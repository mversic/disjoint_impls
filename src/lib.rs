mod disjoint;
mod main_trait;
mod validate;

use param::ParamResolver;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use proc_macro_error::{abort, proc_macro_error, OptionExt};
use quote::{format_ident, quote};
use rustc_hash::{FxHashMap, FxHashSet};
use syn::visit::{visit_predicate_type, visit_trait_bound, visit_type_param, Visit};
use syn::ItemImpl;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, ItemTrait,
};

/// AST node type of self type such as `u32` in `impl Clone for u32`
type SelfType = syn::Type;

/// A collection of {Option<ItemTrait>, [`ItemImpl`]}.
/// Trait definition can be absent if dealing with inherent impls.
#[derive(Debug, PartialEq, Eq)]
struct ItemImpls {
    /// Definition of the main trait.
    /// [`None`] for inherent impl blocks.
    item_trait_: Option<ItemTrait>,
    /// impls map as in: (self type -> ItemImpl)
    item_impls: FxHashMap<SelfType, Vec<ItemImpl>>,
}

/// AST node type of the trait identifier such as 'Deref<Target = u32>' in `impl<T: Deref<Target = u32>> Clone for T`.
/// Equality of this type doesn't compare associated bounds. Therefore `Deref<Target = u32>` == `Deref<Target = u64>`.
#[derive(Debug, Clone, Copy, Eq)]
struct TraitBound<'ast>(pub &'ast syn::Path);

/// Unique name based identifier of the associated type bound such as:
///     `(T, Deref, Deref::Target)` in `impl<T: Deref<Target = bool>> for Clone for T`
type AssocBoundIdent<'ast> = (syn::Type, TraitBound<'ast>, &'ast syn::Ident);

/// AST node type of the associated bound constraint such as:
///     `bool` in `impl<T: Deref<Target = bool>> for Clone for T`
// TODO: how to support GATs? make a test
type AssocBoundPayload = syn::Type;

impl PartialEq for TraitBound<'_> {
    fn eq(&self, other: &Self) -> bool {
        if self.0.leading_colon != other.0.leading_colon {
            return false;
        }

        let mut first_iter = self.0.segments.iter().rev();
        let mut second_iter = other.0.segments.iter().rev();

        let first_elem = first_iter.next().unwrap();
        let second_elem = second_iter.next().unwrap();

        if first_elem.ident != second_elem.ident || !first_iter.eq(second_iter) {
            return false;
        }

        match (&first_elem.arguments, &second_elem.arguments) {
            (
                syn::PathArguments::AngleBracketed(first_args),
                syn::PathArguments::AngleBracketed(second_args),
            ) => {
                if first_args.colon2_token != second_args.colon2_token {
                    return false;
                }

                let first_args = first_args
                    .args
                    .iter()
                    .filter(|arg| !matches!(arg, syn::GenericArgument::AssocType(_)))
                    .collect::<Vec<_>>();
                let second_args = second_args
                    .args
                    .iter()
                    .filter(|arg| !matches!(arg, syn::GenericArgument::AssocType(_)))
                    .collect::<Vec<_>>();

                if first_args.len() != second_args.len() {
                    return false;
                }

                first_args
                    .iter()
                    .zip(&second_args)
                    .all(|zipped_args| match zipped_args {
                        (syn::GenericArgument::AssocType(_), _)
                        | (_, syn::GenericArgument::AssocType(_)) => unreachable!(),
                        _ => zipped_args.0 == zipped_args.1,
                    })
            }
            _ => first_elem.arguments == second_elem.arguments,
        }
    }
}

impl core::hash::Hash for TraitBound<'_> {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.0.leading_colon.hash(state);

        let mut iter = self.0.segments.iter().rev();
        let first_elem = iter.next().unwrap();

        iter.rev().for_each(|elem| elem.hash(state));
        first_elem.ident.hash(state);

        match &first_elem.arguments {
            syn::PathArguments::AngleBracketed(first_args) => {
                first_args.colon2_token.hash(state);

                first_args.args.iter().for_each(|args| match args {
                    syn::GenericArgument::AssocType(_) => {}
                    _ => args.hash(state),
                })
            }
            _ => first_elem.arguments.hash(state),
        }
    }
}

impl quote::ToTokens for TraitBound<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        self.0.leading_colon.to_tokens(tokens);

        let mut iter = self.0.segments.iter().rev();
        let first_elem = iter.next().unwrap();

        iter.rev().for_each(|elem| elem.to_tokens(tokens));
        first_elem.ident.to_tokens(tokens);

        match &first_elem.arguments {
            syn::PathArguments::AngleBracketed(first_args) => {
                first_args.colon2_token.to_tokens(tokens);

                quote!(<).to_tokens(tokens);
                first_args.args.iter().for_each(|args| match args {
                    syn::GenericArgument::AssocType(_) => {}
                    _ => args.to_tokens(tokens),
                });
                quote!(>).to_tokens(tokens);
            }
            _ => first_elem.arguments.to_tokens(tokens),
        }
    }
}

struct AssocBounds<'ast> {
    /// Collection of associated bound identifiers
    type_param_idents: Vec<AssocBoundIdent<'ast>>,
    /// Collection of associated bounds for every implementation
    type_params: Vec<FxHashMap<AssocBoundIdent<'ast>, &'ast AssocBoundPayload>>,
}

impl<'ast> AssocBounds<'ast> {
    fn find(impls: &'ast [ItemImpl]) -> Self {
        let (mut type_param_idents, mut type_params) = (FxHashSet::default(), Vec::new());

        for impl_ in impls {
            let mut visitor = AssocBoundsVisitor::new();

            visitor.visit_generics(&impl_.generics);
            type_params.push(visitor.type_params);

            type_param_idents.extend(visitor.type_param_idents);
        }

        AssocBounds {
            type_param_idents: type_param_idents.into_iter().collect(),
            type_params,
        }
    }
}

struct AssocBoundsVisitor<'ast> {
    /// Type parameter identifier currently being visited
    curr_type_param: Option<syn::Type>,
    /// Trait bound currently being visited
    curr_trait_bound: Option<TraitBound<'ast>>,

    /// Collection of associated bound identifiers
    type_param_idents: FxHashSet<AssocBoundIdent<'ast>>,
    /// Collection of associated bounds for every implementation
    type_params: FxHashMap<AssocBoundIdent<'ast>, &'ast AssocBoundPayload>,
}

impl<'ast> AssocBoundsVisitor<'ast> {
    fn new() -> Self {
        Self {
            curr_type_param: None,
            curr_trait_bound: None,

            type_param_idents: FxHashSet::default(),
            type_params: FxHashMap::default(),
        }
    }

    fn make_assoc_param_ident(&self, assoc_param_name: &'ast syn::Ident) -> AssocBoundIdent<'ast> {
        (
            self.curr_type_param.clone().unwrap(),
            self.curr_trait_bound.unwrap(),
            assoc_param_name,
        )
    }
}

impl<'ast> Visit<'ast> for AssocBoundsVisitor<'ast> {
    fn visit_item_impl(&mut self, node: &'ast ItemImpl) {
        self.visit_generics(&node.generics);
    }

    fn visit_type_param(&mut self, node: &'ast syn::TypeParam) {
        let param = &node.ident;
        self.curr_type_param = Some(syn::parse_quote!(#param));
        visit_type_param(self, node);
    }
    fn visit_predicate_type(&mut self, node: &'ast syn::PredicateType) {
        self.curr_type_param = Some(node.bounded_ty.clone());
        visit_predicate_type(self, node);
    }

    fn visit_trait_bound(&mut self, node: &'ast syn::TraitBound) {
        self.curr_trait_bound = Some(TraitBound(&node.path));
        visit_trait_bound(self, node);
    }

    fn visit_assoc_type(&mut self, node: &'ast syn::AssocType) {
        let assoc_bound_ident = self.make_assoc_param_ident(&node.ident);
        self.type_params.insert(assoc_bound_ident.clone(), &node.ty);
        self.type_param_idents.insert(assoc_bound_ident);
    }
}

/// Unlock support for a variety of mutually disjoint implementations.
///
/// This library enables you to write certain types of disjoint impls that Rust compiler doesn't (yet?) allow.
/// Namely, disjoint impls where a type is bounded by an associated type. One would expect the following
/// syntax to compile without the need to invoke `disjoint_impls!`, but it doesn't:
///
/// ```
/// use disjoint_impls::disjoint_impls;
///
/// pub trait Dispatch {
///     type Group;
/// }
///
/// pub enum GroupA {}
/// impl Dispatch for String {
///     type Group = GroupA;
/// }
///
/// pub enum GroupB {}
/// impl Dispatch for i32 {
///     type Group = GroupB;
/// }
///
/// disjoint_impls! {
///     pub trait Kita {
///         const NAME: &'static str;
///
///         fn name() -> &'static str {
///             "Default blanket"
///         }
///     }
///
///     impl<T: Dispatch<Group = GroupA>> Kita for T {
///         const NAME: &'static str = "Blanket A";
///     }
///     impl<U: Dispatch<Group = GroupB>> Kita for U {
///         const NAME: &'static str = "Blanket B";
///
///         fn name() -> &'static str {
///             "Blanket B"
///         }
///     }
/// }
///
/// fn main() {
///     assert_eq!("Blanket A", String::NAME);
///     assert_eq!("Blanket B", i32::NAME);
///
///     assert_eq!("Default blanket", String::name());
///     assert_eq!("Blanket B", i32::name());
/// }
/// ```
///
/// Other much more complex examples can be found in `tests`
#[proc_macro]
#[proc_macro_error]
pub fn disjoint_impls(input: TokenStream) -> TokenStream {
    let impls: ItemImpls = parse_macro_input!(input);

    let mut helper_traits = Vec::new();
    let mut main_trait_impl = None;
    let mut item_impls = Vec::new();

    let main_trait = impls.item_trait_;
    for (_, per_self_ty_impls) in impls.item_impls {
        // TODO: Assoc bounds are computed multiple times
        helper_traits.push(helper_trait::gen(main_trait.as_ref(), &per_self_ty_impls));
        main_trait_impl = Some(main_trait::gen(main_trait.as_ref(), &per_self_ty_impls));
        item_impls.extend(disjoint::gen(per_self_ty_impls));
    }

    quote! {
        #main_trait

        const _: () = {
            #( #helper_traits )*
            #( #item_impls )*

            #main_trait_impl
        };
    }
    .into()
}

impl Parse for ItemImpls {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        let mut trait_ = input.parse::<ItemTrait>().ok();

        trait_
            .as_mut()
            .map(ParamResolver::resolve_non_predicate_params);

        let mut item_impls = FxHashMap::default();
        while let Ok(mut item) = input.parse::<ItemImpl>() {
            // TODO: Resolve predicate param idents
            item.resolve_non_predicate_params();

            item_impls
                .entry((*item.self_ty).clone())
                .or_insert_with(Vec::new)
                .push(item);
        }

        Ok(ItemImpls::new(trait_, item_impls))
    }
}

impl ItemImpls {
    fn new(item_trait_: Option<ItemTrait>, item_impls: FxHashMap<SelfType, Vec<ItemImpl>>) -> Self {
        if let Some(trait_) = &item_trait_ {
            validate::validate_trait_impls(trait_, &item_impls);
        } else {
            validate::validate_inherent_impls(&item_impls);
        }

        Self {
            item_trait_,
            item_impls,
        }
    }
}

mod helper_trait {
    use super::*;

    /// Generate helper trait
    ///
    /// Helper trait contains all items of the main trait but is parametrized with
    /// type parameters corresponding to a minimal set of associated bounds
    /// required to uniquely identify all of the disjoint impls
    pub fn gen(main_trait: Option<&ItemTrait>, impls: &[ItemImpl]) -> Option<TokenStream2> {
        let assoc_type_param_count = AssocBounds::find(impls).type_param_idents.len();
        let type_param_idents = (0..assoc_type_param_count).map(param::gen_indexed_type_param_name);

        if let Some(mut helper_trait) = main_trait.cloned() {
            helper_trait.vis = syn::Visibility::Public(syn::parse_quote!(pub));
            helper_trait.ident = gen_ident(&helper_trait.ident);
            let start_idx = helper_trait.generics.type_params().count();

            (start_idx..(assoc_type_param_count + start_idx))
                .map(param::gen_indexed_type_param_name)
                .for_each(|type_param_ident| {
                    helper_trait
                        .generics
                        .params
                        .push(syn::parse_quote!(#type_param_ident: ?Sized));
                });

            return Some(quote!(#helper_trait));
        } else if let Some(inherent_impl) = impls.get(0) {
            let items = resolve_inherent_impl_items(&inherent_impl.items);

            if let syn::Type::Path(type_path) = &*inherent_impl.self_ty {
                if let Some(last_seg) = type_path.path.segments.last() {
                    let helper_trait_ident = gen_ident(&last_seg.ident);

                    return Some(quote! {
                        pub trait #helper_trait_ident<#(#type_param_idents: ?Sized),*> {
                            #(#items)*
                        }
                    });
                }
            }
        }

        None
    }

    fn resolve_inherent_impl_items(
        impl_items: &[syn::ImplItem],
    ) -> impl Iterator<Item = TokenStream2> + '_ {
        impl_items.iter().map(|impl_item| match impl_item {
            syn::ImplItem::Const(item) => {
                let syn::ImplItemConst {
                    attrs,
                    ident,
                    generics,
                    ty,
                    ..
                } = &item;

                quote! {
                    #(#attrs),*
                    const #ident: #ty #generics;
                }
            }
            syn::ImplItem::Fn(item) => {
                let syn::ImplItemFn { attrs, sig, .. } = &item;
                quote! {
                    #(#attrs),*
                    #sig;
                }
            }
            syn::ImplItem::Type(item) => {
                let syn::ImplItemType {
                    attrs,
                    ident,
                    generics,
                    ..
                } = &item;

                let (impl_generics, _, where_clause) = generics.split_for_impl();

                quote! {
                    #(#attrs),*
                    type #ident #impl_generics #where_clause;
                }
            }
            syn::ImplItem::Macro(item) => quote! { #item },
            syn::ImplItem::Verbatim(item) => item.clone(),
            _ => unimplemented!(),
        })
    }

    /// Generate identifier of helper trait
    pub fn gen_ident(ident: &syn::Ident) -> syn::Ident {
        format_ident!("_{}", &ident)
    }
}

mod param {
    //! Contains logic related to uniform position based (re-)naming of parameters

    use rustc_hash::FxHashMap;

    use quote::format_ident;
    use syn::{
        visit::{visit_path, Visit},
        visit_mut::{visit_const_param_mut, visit_path_mut, visit_type_param_mut, VisitMut},
        ItemImpl,
    };

    /// Resolve lifetimes, type params and const params into position based identifiers
    pub trait ParamResolver {
        /// Replaces all param identifiers with a position based identifier.
        /// This makes easier to compare params across different impls.
        ///
        /// For:
        ///     `impl<U, T: IntoIterator<Item = V>, V> Trait<u32, T> for U`
        /// resolved impl signature would be:
        ///     `impl<_T2, _T1: IntoIterator<Item = V>, V> Trait<u32, _T1> for _T2`
        fn resolve_non_predicate_params(&mut self);
    }

    struct NonPredicateParamResolver {
        lifetimes: FxHashMap<syn::Ident, usize>,
        type_params: FxHashMap<syn::Ident, usize>,
        const_params: FxHashMap<syn::Ident, usize>,
    }

    /// Indexer for params used in traits, impl trait or self type, but not predicates.
    ///
    /// For `impl<U, T: IntoIterator<Item = V>, V> Trait<T> for U` resolved indices would be:
    /// `T` = 0,
    /// `U` = 1,
    /// `V` = undetermined
    struct NonPredicateParamIndexer<'ast> {
        lifetime_params: FxHashMap<&'ast syn::Ident, Option<usize>>,
        type_params: FxHashMap<&'ast syn::Ident, Option<usize>>,
        const_params: FxHashMap<&'ast syn::Ident, Option<usize>>,

        curr_lifetime_param_pos_idx: usize,
        curr_type_param_pos_idx: usize,
        curr_const_param_pos_idx: usize,
    }

    impl ParamResolver for ItemImpl {
        fn resolve_non_predicate_params(&mut self) {
            let mut non_predicate_param_indexer = NonPredicateParamIndexer::new(&self.generics);
            non_predicate_param_indexer.visit_item_impl(self);
            let mut param_resolver = NonPredicateParamResolver::new(non_predicate_param_indexer);
            param_resolver.visit_item_impl_mut(self);

            // TODO: Add unnamed lifetimes (&u32) or elided lifetimes (&'_ u32)
            // TODO: Remove unused lifetimes. Example where 'b is unused:
            // impl<'a: 'b, 'b: 'a, T: 'b > Kara<'a, T> for &'a T {
            //
            //self.generics.params = self
            //    .generics
            //    .params
            //    .into_iter()
            //    .filter(|param| match param {
            //        syn::GenericParam::Lifetime(lifetime)
            //            if param_resolver.0.get(&lifetime.lifetime.ident).1 =>
            //        {
            //            syn::GenericParam::Lifetime(lifetime)
            //        }
            //        param => param,
            //    })
            //    .collect();
        }
    }

    impl ParamResolver for syn::ItemTrait {
        fn resolve_non_predicate_params(&mut self) {
            let mut non_predicate_param_indexer = NonPredicateParamIndexer::new(&self.generics);
            non_predicate_param_indexer.visit_item_trait(self);
            let mut param_resolver = NonPredicateParamResolver::new(non_predicate_param_indexer);
            param_resolver.visit_item_trait_mut(self);
        }
    }

    fn gen_indexed_lifetime_param_name(idx: usize) -> syn::Ident {
        format_ident!("_LŠČ{idx}")
    }

    pub(super) fn gen_indexed_type_param_name(idx: usize) -> syn::Ident {
        format_ident!("_TŠČ{idx}")
    }

    fn gen_indexed_const_param_name(idx: usize) -> syn::Ident {
        format_ident!("_CŠČ{idx}")
    }

    impl<'ast> NonPredicateParamIndexer<'ast> {
        fn new(generics: &'ast syn::Generics) -> Self {
            let lifetime_params = generics
                .lifetimes()
                .map(|param| (&param.lifetime.ident, None))
                .collect();
            let type_params = generics
                .type_params()
                .map(|param| (&param.ident, None))
                .collect();
            let const_params = generics
                .const_params()
                .map(|param| (&param.ident, None))
                .collect();

            Self {
                lifetime_params,
                type_params,
                const_params,

                curr_lifetime_param_pos_idx: 0,
                curr_type_param_pos_idx: 0,
                curr_const_param_pos_idx: 0,
            }
        }
    }

    impl<'ast> Visit<'ast> for NonPredicateParamIndexer<'ast> {
        fn visit_item_trait(&mut self, node: &'ast syn::ItemTrait) {
            self.visit_generics(&node.generics);
        }

        fn visit_item_impl(&mut self, node: &'ast ItemImpl) {
            if let Some((_, trait_, _)) = &node.trait_ {
                // NOTE: Calling `visit_path` on a trait would conflict
                // with resolving params on `TypePath` so it's not done
                if let Some(last_seg) = &trait_.segments.last() {
                    self.visit_path_arguments(&last_seg.arguments);
                }
            }

            self.visit_type(&node.self_ty);
        }

        // Called only for a trait definition, never for impl block
        fn visit_lifetime_param(&mut self, node: &'ast syn::LifetimeParam) {
            self.visit_lifetime(&node.lifetime);
        }

        // Called only for a trait definition, never for impl block
        fn visit_type_param(&mut self, node: &'ast syn::TypeParam) {
            *self.type_params.get_mut(&node.ident).unwrap() = Some(self.curr_type_param_pos_idx);

            if let Some(curr_pos_idx) = self.curr_type_param_pos_idx.checked_add(1) {
                self.curr_type_param_pos_idx = curr_pos_idx;
            }
        }

        // Called only for a trait definition, never for impl block
        fn visit_const_param(&mut self, node: &'ast syn::ConstParam) {
            *self.const_params.get_mut(&node.ident).unwrap() = Some(self.curr_const_param_pos_idx);

            if let Some(curr_pos_idx) = self.curr_const_param_pos_idx.checked_add(1) {
                self.curr_const_param_pos_idx = curr_pos_idx;
            }
        }

        fn visit_lifetime(&mut self, node: &'ast syn::Lifetime) {
            *self.lifetime_params.get_mut(&node.ident).unwrap() =
                Some(self.curr_lifetime_param_pos_idx);

            if let Some(curr_pos_idx) = self.curr_lifetime_param_pos_idx.checked_add(1) {
                self.curr_lifetime_param_pos_idx = curr_pos_idx;
            }
        }

        fn visit_path(&mut self, node: &'ast syn::Path) {
            if let Some(param_idx) = node.get_ident().and_then(|i| self.const_params.get_mut(&i)) {
                *param_idx = Some(self.curr_const_param_pos_idx);

                if let Some(curr_pos_idx) = self.curr_const_param_pos_idx.checked_add(1) {
                    self.curr_const_param_pos_idx = curr_pos_idx;
                }
            } else if let Some(first_segment) = node.segments.first() {
                self.type_params
                    .entry(&first_segment.ident)
                    .and_modify(|param_idx| {
                        if param_idx.is_none() {
                            *param_idx = Some(self.curr_type_param_pos_idx);

                            if let Some(pos_idx) = self.curr_type_param_pos_idx.checked_add(1) {
                                self.curr_type_param_pos_idx = pos_idx;
                            }
                        }
                    });
            }

            visit_path(self, node);
        }

        fn visit_where_clause(&mut self, _node: &'ast syn::WhereClause) {}
    }

    impl NonPredicateParamResolver {
        fn new(indexer: NonPredicateParamIndexer) -> Self {
            Self {
                lifetimes: indexer
                    .lifetime_params
                    .into_iter()
                    .filter_map(|(param, idx)| idx.map(|idx| (param.clone(), idx)))
                    .collect(),
                type_params: indexer
                    .type_params
                    .into_iter()
                    .filter_map(|(param, idx)| idx.map(|idx| (param.clone(), idx)))
                    .collect(),
                const_params: indexer
                    .const_params
                    .into_iter()
                    .filter_map(|(param, idx)| idx.map(|idx| (param.clone(), idx)))
                    .collect(),
            }
        }
    }

    impl VisitMut for NonPredicateParamResolver {
        fn visit_lifetime_mut(&mut self, node: &mut syn::Lifetime) {
            if let Some(&idx) = self.lifetimes.get(&node.ident) {
                node.ident = gen_indexed_lifetime_param_name(idx);
            }
        }

        fn visit_type_param_mut(&mut self, node: &mut syn::TypeParam) {
            if let Some(&idx) = self.type_params.get(&node.ident) {
                node.ident = gen_indexed_type_param_name(idx);
            }

            visit_type_param_mut(self, node);
        }

        fn visit_const_param_mut(&mut self, node: &mut syn::ConstParam) {
            if let Some(&idx) = self.const_params.get(&node.ident) {
                node.ident = gen_indexed_const_param_name(idx);
            }

            visit_const_param_mut(self, node);
        }

        fn visit_path_mut(&mut self, node: &mut syn::Path) {
            if let Some(first_segment) = node.segments.first_mut() {
                if let Some(&idx) = self.type_params.get(&first_segment.ident) {
                    first_segment.ident = gen_indexed_type_param_name(idx);
                } else if let Some(&idx) = self.const_params.get(&first_segment.ident) {
                    first_segment.ident = gen_indexed_const_param_name(idx);
                }
            }

            visit_path_mut(self, node);
        }
    }

    //struct PredicateIndexer<'ast> {
    //    type_params: FxHashMap<&'ast syn::Ident, Option<usize>>,
    //    curr_pos_idx: usize,
    //}
    //impl<'ast> PredicateIndexer<'ast> {
    //    fn new(type_params: FxHashMap<&'ast syn::Ident, Option<usize>>) -> Self {
    //        let curr_pos_idx: usize = type_params
    //            .values()
    //            .filter_map(|x| *x)
    //            .reduce(|acc, x| x.max(acc))
    //            .unwrap_or(0);
    //
    //        Self {
    //            type_params,
    //            curr_pos_idx,
    //        }
    //    }
    //}
    //impl<'ast> Visit<'ast> for PredicateIndexer<'ast> {
    //    fn visit_item_impl(&mut self, node: &'ast ItemImpl) {
    //        self.visit_generics(&node.generics);
    //    }
    //
    //    fn visit_path_segment(&mut self, node: &'ast syn::PathSegment) {
    //        self.type_params.entry(&node.ident).and_modify(|param_idx| {
    //            if param_idx.is_none() {
    //                // Param encountered for the first time
    //                *param_idx = Some(self.curr_pos_idx);
    //            }
    //        });
    //
    //        if let Some(curr_pos_idx) = self.curr_pos_idx.checked_add(1) {
    //            self.curr_pos_idx = curr_pos_idx;
    //        }
    //    }
    //}
}
