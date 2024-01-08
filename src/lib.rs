mod disjoint;
mod main_trait;
mod validate;

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

type ImplGroupId = (Option<syn::Path>, syn::Type);

/// A collection of {Option<ItemTrait>, [`ItemImpl`]}.
/// Trait definition can be absent if dealing with inherent impls.
#[derive(Debug, PartialEq, Eq)]
struct ItemImpls {
    /// Definition of the main trait.
    /// [`None`] for inherent impl blocks.
    item_trait_: Option<ItemTrait>,
    /// impls map as in: ((trait path, self type) -> ItemImpl)
    item_impls: FxHashMap<ImplGroupId, Vec<ItemImpl>>,
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

        let (Some(first_elem), Some(second_elem)) = (first_iter.next(), second_iter.next()) else {
            return true;
        };

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
                first_args
                    .args
                    .iter()
                    .filter_map(|arg| match arg {
                        syn::GenericArgument::AssocType(_) => None,
                        _ => Some(arg),
                    })
                    .collect::<syn::punctuated::Punctuated<_, syn::Token![,]>>()
                    .to_tokens(tokens);
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
    fn find(impl_group: &'ast [ItemImpl]) -> Self {
        let (mut type_param_idents, mut type_params) = (FxHashSet::default(), Vec::new());

        for impl_ in impl_group {
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
/// // Basic example
/// disjoint_impls! {
///     pub trait BasicKita {
///         const BASIC_NAME: &'static str;
///
///         fn basic_name() -> &'static str {
///             "Default blanket"
///         }
///     }
///
///     impl<T: Dispatch<Group = GroupA>> BasicKita for T {
///         const BASIC_NAME: &'static str = "Blanket A";
///     }
///     impl<U: Dispatch<Group = GroupB>> BasicKita for U {
///         const BASIC_NAME: &'static str = "Blanket B";
///
///         fn basic_name() -> &'static str {
///             "Blanket B"
///         }
///     }
/// }
///
/// // Complex example
/// disjoint_impls! {
///     pub trait ComplexKita {
///         const COMPLEX_NAME: &'static str;
///     }
///
///     impl<T: Dispatch<Group = GroupA>, U: Dispatch<Group = GroupA>> ComplexKita for (T, U) {
///         const COMPLEX_NAME: &'static str = "Blanket AA";
///     }
///     impl<U, T> ComplexKita for (U, T)
///     where
///         U: Dispatch<Group = GroupA>,
///         T: Dispatch<Group = GroupB>
///     {
///         const COMPLEX_NAME: &'static str = "Blanket AB";
///     }
///     impl<T: Dispatch<Group = GroupB>, U> ComplexKita for (T, U) {
///         const COMPLEX_NAME: &'static str = "Blanket B*";
///     }
///
///     impl<T: Dispatch<Group = GroupA>> ComplexKita for T {
///         const COMPLEX_NAME: &'static str = "Blanket A";
///     }
///     impl<U: Dispatch<Group = GroupB>> ComplexKita for U {
///         const COMPLEX_NAME: &'static str = "Blanket B";
///     }
/// }
///
/// fn main() {
///     assert_eq!("Blanket A", String::BASIC_NAME);
///     assert_eq!("Blanket B", i32::BASIC_NAME);
///
///     assert_eq!("Default blanket", String::basic_name());
///     assert_eq!("Blanket B", i32::basic_name());
///
///     assert_eq!("Blanket A", String::COMPLEX_NAME);
///     assert_eq!("Blanket B", i32::COMPLEX_NAME);
///
///     assert_eq!("Blanket AA", <(String, String)>::COMPLEX_NAME);
///     assert_eq!("Blanket AB", <(String, i32)>::COMPLEX_NAME);
///     assert_eq!("Blanket B*", <(i32, String)>::COMPLEX_NAME);
/// }
/// ```
///
/// Other much more complex examples can be found in `tests`
#[proc_macro]
#[proc_macro_error]
pub fn disjoint_impls(input: TokenStream) -> TokenStream {
    let impls: ItemImpls = parse_macro_input!(input);

    let mut helper_traits = Vec::new();
    let mut main_trait_impls = Vec::new();
    let mut item_impls = Vec::new();

    let main_trait = impls.item_trait_;
    for (idx, (impl_group_id, impl_group)) in impls.item_impls.into_iter().enumerate() {
        // TODO: Assoc bounds are computed multiple times
        helper_traits.push(helper_trait::gen(
            main_trait.as_ref(),
            &impl_group_id,
            &impl_group,
            idx,
        ));
        main_trait_impls.push(main_trait::gen(
            main_trait.as_ref(),
            &impl_group_id,
            &impl_group,
            idx,
        ));
        item_impls.extend(disjoint::gen(impl_group, idx));
    }

    quote! {
        #main_trait

        const _: () = {
            #( #helper_traits )*
            #( #item_impls )*
            #( #main_trait_impls )*
        };
    }
    .into()
}

impl Parse for ItemImpls {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        let main_trait = input.parse::<ItemTrait>().ok();

        let mut item_impls = FxHashMap::default();
        while let Ok(mut item) = input.parse::<ItemImpl>() {
            // TODO: Resolve predicate param idents
            param::resolve_non_predicate_params(&mut item);

            item_impls
                .entry((
                    item.trait_.as_ref().map(|trait_| trait_.1.clone()),
                    (*item.self_ty).clone(),
                ))
                .or_insert_with(Vec::new)
                .push(item);
        }

        Ok(ItemImpls::new(main_trait, item_impls))
    }
}

impl ItemImpls {
    fn new(
        item_trait_: Option<ItemTrait>,
        item_impls: FxHashMap<ImplGroupId, Vec<ItemImpl>>,
    ) -> Self {
        if let Some(trait_) = &item_trait_ {
            validate::validate_trait_impls(trait_, item_impls.values().flatten());
        } else {
            validate::validate_inherent_impls(item_impls.values().flatten());
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
    pub fn gen(
        main_trait: Option<&ItemTrait>,
        impl_group_id: &ImplGroupId,
        impl_group: &[ItemImpl],
        idx: usize,
    ) -> Option<TokenStream2> {
        let assoc_type_param_count = AssocBounds::find(impl_group).type_param_idents.len();
        let type_param_idents = (0..assoc_type_param_count).map(param::gen_indexed_param_name);

        if let Some(mut helper_trait) = main_trait.cloned() {
            helper_trait.vis = syn::Visibility::Public(syn::parse_quote!(pub));
            helper_trait.ident = gen_ident(&helper_trait.ident, idx);

            helper_trait.generics.params = type_param_idents
                .map(|type_param_ident| syn::parse_quote!(#type_param_ident: ?Sized))
                .chain(helper_trait.generics.params)
                .collect();

            return Some(quote!(#helper_trait));
        } else if let Some(inherent_impl) = impl_group.get(0) {
            let items = resolve_inherent_impl_items(&inherent_impl.items);

            let self_ty = &impl_group_id.1;
            if let syn::Type::Path(type_path) = self_ty {
                let path = type_path.path.segments.last().unwrap();
                let helper_trait_ident = gen_ident(&path.ident, idx);

                return Some(quote! {
                    pub trait #helper_trait_ident<#(#type_param_idents: ?Sized),*> {
                        #(#items)*
                    }
                });
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
    pub fn gen_ident(ident: &syn::Ident, idx: usize) -> syn::Ident {
        format_ident!("_{}{}", ident, idx)
    }
}

mod param {
    //! Contains logic related to uniform position based (re-)naming of parameters

    use rustc_hash::FxHashMap;

    use quote::format_ident;
    use syn::{visit::Visit, visit_mut::VisitMut};

    /// Indexer for params used in traits, impl trait or self type, but not predicates.
    ///
    /// For `impl<U, T: IntoIterator<Item = V>, V> Trait<T> for U` resolved indices would be:
    /// `T` = 0,
    /// `U` = 1,
    /// `V` = undetermined
    struct NonPredicateParamIndexer {
        indexed_params: FxHashMap<syn::Ident, (usize, syn::GenericParam)>,
        unindexed_params: FxHashMap<syn::Ident, syn::GenericParam>,
        curr_param_pos_idx: usize,
    }

    struct NonPredicateParamResolver {
        params: FxHashMap<syn::Ident, usize>,
    }

    pub fn resolve_non_predicate_params(item_impl: &mut syn::ItemImpl) {
        let item_impl_generics = item_impl.generics.params.iter().cloned();

        let mut non_predicate_param_indexer = NonPredicateParamIndexer::new(
            item_impl_generics
                .map(|param| (get_param_ident(&param).clone(), param))
                .collect(),
            0,
        );

        non_predicate_param_indexer.visit_item_impl(item_impl);

        let mut prev_unindexed_params_count = usize::MAX;
        let mut indexed_params = non_predicate_param_indexer.indexed_params;
        let mut curr_unindexed_params_count = non_predicate_param_indexer.unindexed_params.len();

        while !non_predicate_param_indexer.unindexed_params.is_empty()
            // NOTE: This discards parameters only used in where clause
            && prev_unindexed_params_count != curr_unindexed_params_count
        {
            non_predicate_param_indexer = NonPredicateParamIndexer::new(
                non_predicate_param_indexer.unindexed_params,
                non_predicate_param_indexer.curr_param_pos_idx,
            );

            non_predicate_param_indexer.visit_indexed_params(
                indexed_params
                    .iter()
                    .map(|(_, (idx, param))| (*idx, param))
                    .collect(),
            );

            prev_unindexed_params_count = curr_unindexed_params_count;
            indexed_params.extend(non_predicate_param_indexer.indexed_params);
            curr_unindexed_params_count = non_predicate_param_indexer.unindexed_params.len();
        }

        NonPredicateParamResolver::new(
            indexed_params
                .into_iter()
                .map(|(ident, (idx, _))| (ident, idx)),
        )
        .visit_item_impl_mut(item_impl);

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

    pub(super) fn gen_indexed_param_name(idx: usize) -> syn::Ident {
        format_ident!("_{idx}")
    }

    impl NonPredicateParamIndexer {
        fn new(
            unindexed_params: FxHashMap<syn::Ident, syn::GenericParam>,
            curr_param_pos_idx: usize,
        ) -> Self {
            let indexed_params = FxHashMap::default();

            Self {
                indexed_params,
                unindexed_params,
                curr_param_pos_idx,
            }
        }

        fn visit_param_ident(&mut self, param_ident: &syn::Ident) -> bool {
            if let Some(removed) = self.unindexed_params.remove(param_ident) {
                self.indexed_params
                    .insert(param_ident.clone(), (self.curr_param_pos_idx, removed));
                self.curr_param_pos_idx = self.curr_param_pos_idx.checked_add(1).unwrap();

                return true;
            }

            false
        }

        fn visit_indexed_params(&mut self, node: FxHashMap<usize, &syn::GenericParam>) {
            let mut indexed_params = node.into_iter().collect::<Vec<_>>();
            indexed_params.sort_by_key(|(k, _)| *k);

            for (_, param) in indexed_params {
                self.visit_generic_param(param);
            }
        }
    }

    impl Visit<'_> for NonPredicateParamIndexer {
        fn visit_item_impl(&mut self, node: &syn::ItemImpl) {
            if let Some((_, trait_, _)) = &node.trait_ {
                // NOTE: Calling `visit_path` on a trait would conflict
                // with resolving params on `TypePath` so it's not done
                //
                // # Example
                //
                // ```
                // trait T<T> {}
                // ```
                //
                // had `Visit::visit_path` been used on `T<T>` to resolve
                // trait generics it would also rename the trait ident itself
                let path = trait_.segments.last().unwrap();
                self.visit_path_arguments(&path.arguments);
            }

            self.visit_type(&node.self_ty);
        }

        fn visit_lifetime(&mut self, node: &syn::Lifetime) {
            self.visit_param_ident(&node.ident);
        }

        fn visit_path(&mut self, node: &syn::Path) {
            if !self.visit_param_ident(&node.segments.first().unwrap().ident) {
                syn::visit::visit_path(self, node);
            }
        }

        // TODO: Is this required? I don't think it is anymore
        //fn visit_expr(&mut self, node: &syn::Expr) {
        //    if let syn::Expr::Path(path) = node {
        //        self.visit_expr_path(path);
        //    } else {
        //        self.curr_param_pos_idx = self.curr_param_pos_idx.checked_add(1).unwrap();
        //    }
        //}
    }

    impl NonPredicateParamResolver {
        fn new(params: impl IntoIterator<Item = (syn::Ident, usize)>) -> Self {
            Self {
                params: params.into_iter().collect(),
            }
        }
    }

    impl VisitMut for NonPredicateParamResolver {
        fn visit_item_impl_mut(&mut self, node: &mut syn::ItemImpl) {
            for attr in &mut node.attrs {
                self.visit_attribute_mut(attr);
            }

            self.visit_generics_mut(&mut node.generics);
            if let Some((_, trait_, _)) = &mut node.trait_ {
                // NOTE: Calling `visit_path` on a trait would conflict
                // with resolving params on `TypePath` so it's not done
                //
                // # Example
                //
                // ```
                // trait T<T> {}
                // ```
                //
                // had `Visit::visit_path` been used on `T<T>` to resolve
                // trait generics it would also rename the trait ident itself
                let path = trait_.segments.last_mut().unwrap();
                self.visit_path_arguments_mut(&mut path.arguments);
            }

            self.visit_type_mut(&mut node.self_ty);

            for item in &mut node.items {
                self.visit_impl_item_mut(item);
            }
        }

        fn visit_lifetime_mut(&mut self, node: &mut syn::Lifetime) {
            if let Some(&idx) = self.params.get(&node.ident) {
                node.ident = gen_indexed_param_name(idx);
            }

            syn::visit_mut::visit_lifetime_mut(self, node);
        }

        fn visit_type_param_mut(&mut self, node: &mut syn::TypeParam) {
            if let Some(&idx) = self.params.get(&node.ident) {
                node.ident = gen_indexed_param_name(idx);
            }

            syn::visit_mut::visit_type_param_mut(self, node);
        }

        fn visit_const_param_mut(&mut self, node: &mut syn::ConstParam) {
            if let Some(&idx) = self.params.get(&node.ident) {
                node.ident = gen_indexed_param_name(idx);
            }

            syn::visit_mut::visit_const_param_mut(self, node);
        }

        fn visit_path_mut(&mut self, node: &mut syn::Path) {
            let path = node.segments.first_mut().unwrap();

            if let Some(&idx) = self.params.get(&path.ident) {
                path.ident = gen_indexed_param_name(idx);
            } else {
                syn::visit_mut::visit_path_mut(self, node);
            }
        }
    }

    pub fn get_param_ident(generic_param: &syn::GenericParam) -> &syn::Ident {
        match generic_param {
            syn::GenericParam::Lifetime(syn::LifetimeParam { lifetime, .. }) => &lifetime.ident,
            syn::GenericParam::Type(syn::TypeParam { ident, .. }) => ident,
            syn::GenericParam::Const(syn::ConstParam { ident, .. }) => ident,
        }
    }
}
