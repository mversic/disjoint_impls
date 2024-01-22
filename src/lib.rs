mod disjoint;
mod main_trait;
mod validate;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use proc_macro_error::{abort, proc_macro_error, OptionExt};
use quote::{format_ident, quote};
use rustc_hash::{FxHashMap, FxHashSet};
use syn::visit::Visit;
use syn::ItemImpl;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, ItemTrait,
};

/// Unique id of an impl group, i.e. ([`ItemImpl::trait_`], [`ItemImpl::self_ty`]).
/// All [`ImplItem`]s that have matching group ids are handled by one main trait impl.
type ImplGroupId = (Option<syn::Path>, syn::Type);

/// Body of the [`disjoint_impls`] macro
#[derive(Debug, PartialEq, Eq)]
struct ItemImpls {
    /// Definition of the main trait. [`None`] for inherent impls
    item_trait_: Option<ItemTrait>,
    /// Collection of [`ItemImpl`] blocks grouped by [`ImplGroupId`]
    item_impls: FxHashMap<ImplGroupId, Vec<ItemImpl>>,
}

/// AST node type of the trait identifier such as 'IntoIterator<Item = u32>' in `impl<T: IntoIterator<Item = u32>> Clone for T`.
/// Equality of the type doesn't compare associated bounds. Therefore `IntoIterator<Item = u32>` == `IntoIterator<IntoIter = Vec<u32>>`.
#[derive(Debug, Clone, Copy, Eq)]
struct TraitBound<'ast>(&'ast syn::Path);

/// Bounded param/type
#[derive(Debug, Clone, Copy, Eq)]
enum Bounded<'ast> {
    /// Holds the value of [`syn::TypeParam::ident`]
    Param(&'ast syn::Ident),
    /// Holds the value of [`syn::PredicateType::bounded_ty`]
    Type(&'ast syn::Type),
}

/// Unique name based identifier of the associated type bound such as:
///     `(T, Deref, Deref::Target)` in `impl<T: Deref<Target = bool>> for Clone for T`
type AssocBoundIdent<'ast> = (Bounded<'ast>, TraitBound<'ast>, &'ast syn::Ident);

/// AST node type of the associated bound constraint such as:
///     `bool` in `impl<T: Deref<Target = bool>> for Clone for T`
// TODO: how to support GATs? make a test
type AssocBoundPayload = syn::Type;

impl PartialEq for Bounded<'_> {
    fn eq(&self, other: &Self) -> bool {
        use Bounded::*;

        match (self, other) {
            (Param(x), Param(y)) => x == y,
            (Type(x), Type(y)) => x == y,

            (Param(x), Type(syn::Type::Path(ty))) => ty.path.get_ident() == Some(x),
            (Type(syn::Type::Path(ty)), Param(y)) => ty.path.get_ident() == Some(y),

            _ => false,
        }
    }
}

impl core::hash::Hash for Bounded<'_> {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        use Bounded::*;

        match self {
            Param(ident) => ident.hash(state),
            Type(syn::Type::Path(ty)) => {
                if let Some(ident) = ty.path.get_ident() {
                    ident.hash(state);
                } else {
                    ty.hash(state);
                }
            }
            Type(ty) => ty.hash(state),
        }
    }
}

impl<'ast> From<&'ast syn::Ident> for Bounded<'ast> {
    fn from(source: &'ast syn::Ident) -> Self {
        Self::Param(source)
    }
}

impl<'ast> From<&'ast syn::Type> for Bounded<'ast> {
    fn from(source: &'ast syn::Type) -> Self {
        Self::Type(source)
    }
}

impl quote::ToTokens for Bounded<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            Bounded::Param(ident) => ident.to_tokens(tokens),
            Bounded::Type(type_) => type_.to_tokens(tokens),
        }
    }
}

impl PartialEq for TraitBound<'_> {
    fn eq(&self, other: &Self) -> bool {
        let mut first_iter = self.0.segments.iter().rev();
        let mut second_iter = other.0.segments.iter().rev();

        let (Some(first_elem), Some(second_elem)) = (first_iter.next(), second_iter.next()) else {
            return true;
        };

        if first_elem.ident != second_elem.ident || !first_iter.eq(second_iter) {
            return false;
        }

        match (&first_elem.arguments, &second_elem.arguments) {
            (syn::PathArguments::None, syn::PathArguments::None) => true,
            (syn::PathArguments::AngleBracketed(bracketed), syn::PathArguments::None)
            | (syn::PathArguments::None, syn::PathArguments::AngleBracketed(bracketed)) => {
                !bracketed
                    .args
                    .iter()
                    .any(|arg| !matches!(arg, syn::GenericArgument::AssocType(_)))
            }
            (
                syn::PathArguments::AngleBracketed(first_args),
                syn::PathArguments::AngleBracketed(second_args),
            ) => {
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
                    .all(|zipped_args| zipped_args.0 == zipped_args.1)
            }
            (_, syn::PathArguments::Parenthesized(_))
            | (syn::PathArguments::Parenthesized(_), _) => {
                unreachable!()
            }
        }
    }
}

impl core::hash::Hash for TraitBound<'_> {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        let mut iter = self.0.segments.iter().rev();
        let first_elem = iter.next().unwrap();

        iter.rev().for_each(|elem| elem.hash(state));
        first_elem.ident.hash(state);

        match &first_elem.arguments {
            syn::PathArguments::None => {}
            syn::PathArguments::AngleBracketed(first_args) => {
                first_args.args.iter().for_each(|args| match args {
                    syn::GenericArgument::AssocType(_) => {}
                    _ => args.hash(state),
                })
            }
            syn::PathArguments::Parenthesized(_) => unreachable!(),
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
                    .filter(|arg| !matches!(arg, syn::GenericArgument::AssocType(_)))
                    .collect::<syn::punctuated::Punctuated<_, syn::Token![,]>>()
                    .to_tokens(tokens);
                quote!(>).to_tokens(tokens);
            }
            _ => first_elem.arguments.to_tokens(tokens),
        }
    }
}

#[derive(Default)]
struct AssocBounds<'ast> {
    /// Collection of associated bound identifiers
    assoc_bound_idents: Vec<AssocBoundIdent<'ast>>,
    /// Collection of associated bounds for every implementation
    assoc_bounds: Vec<FxHashMap<AssocBoundIdent<'ast>, &'ast AssocBoundPayload>>,
}

impl<'ast> AssocBounds<'ast> {
    fn find(impl_group: &'ast [ItemImpl]) -> Self {
        let mut type_param_bounds: FxHashSet<_> = if let Some(first_impl) = impl_group.first() {
            let mut visitor = TraitBoundsVisitor::new();
            visitor.visit_generics(&first_impl.generics);
            visitor.type_param_bounds
        } else {
            FxHashSet::default()
        };

        let assoc_bounds: Vec<_> = impl_group
            .iter()
            .map(|impl_| {
                let mut visitor = TraitBoundsVisitor::new();
                visitor.visit_generics(&impl_.generics);
                type_param_bounds = &type_param_bounds & &visitor.type_param_bounds;

                visitor.assoc_bounds
            })
            .collect::<Vec<_>>()
            .into_iter()
            .map(|assoc_bounds| {
                assoc_bounds
                    .into_iter()
                    .filter_map(|(id, payload)| {
                        let (param, trait_bound, _) = id;

                        if type_param_bounds.contains(&(param, trait_bound)) {
                            return Some((id, payload));
                        }

                        None
                    })
                    .collect::<FxHashMap<_, _>>()
            })
            .collect();

        let assoc_bound_idents = assoc_bounds
            .iter()
            .fold(FxHashSet::default(), |mut acc, e| {
                acc.extend(e.keys());
                acc
            })
            .into_iter()
            .collect();

        Self {
            assoc_bound_idents,
            assoc_bounds,
        }
    }
}

struct TraitBoundsVisitor<'ast> {
    /// Type parameter identifier currently being visited
    curr_type_param: Option<Bounded<'ast>>,
    /// Trait bound currently being visited
    curr_trait_bound: Option<TraitBound<'ast>>,

    type_param_bounds: FxHashSet<(Bounded<'ast>, TraitBound<'ast>)>,
    /// Collection of associated bounds for every implementation
    assoc_bounds: Vec<(AssocBoundIdent<'ast>, &'ast AssocBoundPayload)>,
}

impl<'ast> TraitBoundsVisitor<'ast> {
    fn new() -> Self {
        Self {
            curr_type_param: None,
            curr_trait_bound: None,

            type_param_bounds: FxHashSet::default(),
            assoc_bounds: Vec::default(),
        }
    }

    fn make_assoc_param_ident(
        &mut self,
        assoc_param_name: &'ast syn::Ident,
    ) -> AssocBoundIdent<'ast> {
        (
            self.curr_type_param.unwrap(),
            self.curr_trait_bound.unwrap(),
            assoc_param_name,
        )
    }
}

impl<'ast> Visit<'ast> for TraitBoundsVisitor<'ast> {
    fn visit_item_impl(&mut self, node: &'ast ItemImpl) {
        self.visit_generics(&node.generics);
    }

    fn visit_type_param(&mut self, node: &'ast syn::TypeParam) {
        self.curr_type_param = Some((&node.ident).into());
        syn::visit::visit_type_param(self, node);
    }
    fn visit_predicate_type(&mut self, node: &'ast syn::PredicateType) {
        self.curr_type_param = Some((&node.bounded_ty).into());
        syn::visit::visit_predicate_type(self, node);
    }

    fn visit_trait_bound(&mut self, node: &'ast syn::TraitBound) {
        self.curr_trait_bound = Some(TraitBound(&node.path));
        syn::visit::visit_trait_bound(self, node);

        self.type_param_bounds.insert((
            self.curr_type_param.unwrap(),
            self.curr_trait_bound.unwrap(),
        ));
    }

    fn visit_assoc_type(&mut self, node: &'ast syn::AssocType) {
        let assoc_bound_ident = self.make_assoc_param_ident(&node.ident);
        self.assoc_bounds.push((assoc_bound_ident, &node.ty));
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
///     impl<T: Dispatch<Group = GroupB>, U: Dispatch> ComplexKita for (T, U) {
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
    for (impl_group_idx, (_, impl_group)) in impls.item_impls.into_iter().enumerate() {
        // TODO: Assoc bounds are computed multiple times
        helper_traits.push(helper_trait::gen(
            main_trait.as_ref(),
            impl_group_idx,
            &impl_group,
        ));

        main_trait_impls.push(main_trait::gen(
            main_trait.as_ref(),
            impl_group_idx,
            &impl_group,
        ));

        item_impls.extend(disjoint::gen(impl_group_idx, impl_group));
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
        let mut item_impls = FxHashMap::<_, Vec<_>>::default();

        let main_trait = input.parse::<ItemTrait>().ok();
        while let Ok(mut item) = input.parse::<ItemImpl>() {
            // TODO: Resolve predicate param idents
            param::resolve_non_predicate_params(&mut item);

            let impl_group_id = (
                item.trait_.as_ref().map(|trait_| &trait_.1).cloned(),
                (*item.self_ty).clone(),
            );

            item_impls.entry(impl_group_id).or_default().push(item);
        }

        Ok(ItemImpls::new(main_trait, item_impls))
    }
}

fn gen_inherent_self_ty_args(self_ty: &mut syn::TypePath, generics: &syn::Generics) {
    let last_seg = &mut self_ty.path.segments.last_mut().unwrap();

    if let syn::PathArguments::AngleBracketed(bracketed) = &mut last_seg.arguments {
        let mut lifetimes = Vec::new();
        let mut params = Vec::new();

        generics.params.iter().for_each(|param| match param {
            syn::GenericParam::Lifetime(syn::LifetimeParam { lifetime, .. }) => {
                lifetimes.push(lifetime);
            }
            syn::GenericParam::Type(syn::TypeParam { ident, .. }) => params.push(ident),
            syn::GenericParam::Const(syn::ConstParam { ident, .. }) => params.push(ident),
        });

        lifetimes.sort();
        params.sort();

        *bracketed = syn::parse_quote!(<#(#lifetimes,)* #(#params),*>);
    } else {
        unreachable!()
    }
}

impl ItemImpls {
    fn new(
        item_trait_: Option<ItemTrait>,
        item_impls: FxHashMap<ImplGroupId, Vec<ItemImpl>>,
    ) -> Self {
        if let Some(trait_) = &item_trait_ {
            for item_impls in item_impls.values() {
                validate::validate_trait_impls(trait_, item_impls);
            }
        } else {
            for item_impls in item_impls.values() {
                validate::validate_inherent_impls(item_impls);
            }
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
        impl_group_idx: usize,
        impl_group: &[ItemImpl],
    ) -> Option<syn::ItemTrait> {
        let assoc_bound_idents = AssocBounds::find(impl_group).assoc_bound_idents;

        let mut helper_trait = if let Some(helper_trait) = main_trait {
            helper_trait.clone()
        } else if let Some(helper_trait) = impl_group.first().cloned() {
            let syn::ItemImpl {
                attrs,
                unsafety,
                mut generics,
                mut self_ty,
                items,
                ..
            } = helper_trait.clone();

            let items = gen_inherent_impl_items(&items);
            if let syn::Type::Path(type_path) = &mut *self_ty {
                type_path.path.segments.last_mut().unwrap().arguments = syn::PathArguments::None;
            }

            remove_param_bounds(&mut generics);
            let (impl_generics, _, _) = generics.split_for_impl();

            syn::parse_quote! {
                #(#attrs)*
                #unsafety trait #self_ty #impl_generics {
                    #(#items)*
                }
            }
        } else {
            return None;
        };

        helper_trait.vis = syn::Visibility::Public(syn::parse_quote!(pub));
        helper_trait.ident = gen_ident(&helper_trait.ident, impl_group_idx);

        let start_idx = helper_trait.generics.params.len();
        helper_trait.generics.params = combine_generic_args(
            (start_idx..start_idx + assoc_bound_idents.len()).map(param::gen_indexed_param_name),
            &helper_trait.generics,
        )
        .map(|arg| -> syn::GenericParam { syn::parse_quote!(#arg) })
        .collect();

        Some(helper_trait)
    }

    pub fn remove_param_bounds(generics: &mut syn::Generics) {
        generics.params.iter_mut().for_each(|param| match param {
            syn::GenericParam::Lifetime(syn::LifetimeParam { bounds, .. }) => {
                *bounds = syn::punctuated::Punctuated::default()
            }
            syn::GenericParam::Type(syn::TypeParam { bounds, .. }) => {
                *bounds = syn::punctuated::Punctuated::default()
            }
            syn::GenericParam::Const(_) => {}
        });

        generics.where_clause = None;
    }

    fn combine_generic_args(
        assoc_param_bounds: impl IntoIterator<Item = syn::Ident>,
        generics: &syn::Generics,
    ) -> impl Iterator<Item = TokenStream2> {
        let mut generic_args: Vec<_> = assoc_param_bounds
            .into_iter()
            .map(|param| quote!(#param: ?Sized))
            .collect();

        let mut lifetimes = vec![];
        for arg in &generics.params {
            if matches!(arg, syn::GenericParam::Lifetime(_)) {
                lifetimes.push(quote!(#arg));
            } else {
                generic_args.push(quote!(#arg));
            }
        }

        lifetimes.into_iter().chain(generic_args)
    }

    fn gen_inherent_impl_items(
        impl_items: &[syn::ImplItem],
    ) -> impl Iterator<Item = syn::ImplItem> + '_ {
        impl_items.iter().map(|impl_item| match impl_item {
            syn::ImplItem::Const(item) => {
                let syn::ImplItemConst {
                    attrs,
                    ident,
                    generics,
                    ty,
                    ..
                } = &item;

                syn::parse_quote! {
                    #(#attrs),*
                    const #ident: #ty #generics;
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

                syn::parse_quote! {
                    #(#attrs),*
                    type #ident #impl_generics #where_clause;
                }
            }
            syn::ImplItem::Fn(item) => {
                let syn::ImplItemFn { attrs, sig, .. } = &item;

                syn::parse_quote! {
                    #(#attrs),*
                    #sig;
                }
            }
            item => abort!(item, "Not supported"),
        })
    }

    /// Generate ident of the helper trait
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
        format_ident!("_ŠČ{idx}")
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

        fn visit_trait_bound(&mut self, node: &syn::TraitBound) {
            self.visit_trait_bound_modifier(&node.modifier);

            if let Some(lifetime) = &node.lifetimes {
                self.visit_bound_lifetimes(lifetime);
            }

            let path = node.path.segments.last().unwrap();
            self.visit_path_arguments(&path.arguments);
        }
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

        fn visit_trait_bound_mut(&mut self, node: &mut syn::TraitBound) {
            self.visit_trait_bound_modifier_mut(&mut node.modifier);

            if let Some(lifetime) = &mut node.lifetimes {
                self.visit_bound_lifetimes_mut(lifetime);
            }

            let path = node.path.segments.last_mut().unwrap();
            self.visit_path_arguments_mut(&mut path.arguments);
        }
    }

    // TODO: Lifetimes as type/const params can have the same ident
    pub fn get_param_ident(generic_param: &syn::GenericParam) -> &syn::Ident {
        match generic_param {
            syn::GenericParam::Lifetime(syn::LifetimeParam { lifetime, .. }) => &lifetime.ident,
            syn::GenericParam::Type(syn::TypeParam { ident, .. }) => ident,
            syn::GenericParam::Const(syn::ConstParam { ident, .. }) => ident,
        }
    }
}
