mod disjoint;
mod main_trait;
mod validate;

use param::get_param_ident;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use proc_macro_error::{abort, proc_macro_error};
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
    unsized_type_params: FxHashSet<Bounded<'ast>>,
}

impl<'ast> AssocBounds<'ast> {
    fn find(impl_group: &'ast [ItemImpl]) -> Self {
        let mut unsized_type_params = FxHashSet::default();

        let first_type_param_bounds = impl_group
            .first()
            .map(|first_impl| {
                let mut visitor = TraitBoundsVisitor::new();
                visitor.visit_generics(&first_impl.generics);
                visitor.type_param_bounds
            })
            .unwrap_or_default();

        let mut type_param_bounds = first_type_param_bounds
            .iter()
            .cloned()
            .collect::<FxHashSet<_>>();

        let assoc_bounds = impl_group
            .iter()
            .map(|impl_| {
                let mut visitor = TraitBoundsVisitor::new();
                visitor.visit_generics(&impl_.generics);

                type_param_bounds = &type_param_bounds
                    & &visitor
                        .type_param_bounds
                        .into_iter()
                        .collect::<FxHashSet<_>>();

                unsized_type_params.extend(visitor.unsized_type_params);

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
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();

        let assoc_bound_idents = {
            let mut seen_assoc_bound_idents = FxHashSet::default();
            let mut seen_type_param_bounds = FxHashSet::default();

            let assoc_bound_idents = assoc_bounds
                .iter()
                .flatten()
                .map(|(id, _)| *id)
                .filter(|&id| seen_assoc_bound_idents.insert(id))
                .fold(FxHashMap::<_, Vec<_>>::default(), |mut acc, id| {
                    acc.entry((id.0, id.1)).or_default().push(id);
                    acc
                });

            first_type_param_bounds
                .into_iter()
                .filter(|&bound| {
                    seen_type_param_bounds.insert(bound) && assoc_bound_idents.contains_key(&bound)
                })
                .flat_map(|type_param_bound| assoc_bound_idents.get(&type_param_bound).unwrap())
                .cloned()
                .collect()
        };

        Self {
            assoc_bound_idents,
            assoc_bounds: assoc_bounds
                .into_iter()
                .map(|impl_assoc_bounds| impl_assoc_bounds.into_iter().collect())
                .collect(),
            unsized_type_params,
        }
    }
}

struct TraitBoundsVisitor<'ast> {
    /// Type parameter identifier currently being visited
    curr_type_param: Option<Bounded<'ast>>,
    /// Trait bound currently being visited
    curr_trait_bound: Option<TraitBound<'ast>>,

    type_param_bounds: Vec<(Bounded<'ast>, TraitBound<'ast>)>,
    /// Collection of associated bounds for every implementation
    assoc_bounds: Vec<(AssocBoundIdent<'ast>, &'ast AssocBoundPayload)>,

    unsized_type_params: FxHashSet<Bounded<'ast>>,
}

impl<'ast> TraitBoundsVisitor<'ast> {
    fn new() -> Self {
        Self {
            curr_type_param: None,
            curr_trait_bound: None,

            type_param_bounds: Vec::default(),
            assoc_bounds: Vec::default(),

            unsized_type_params: FxHashSet::default(),
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

    fn visit_generics(&mut self, node: &'ast syn::Generics) {
        let mut params: Vec<_> = node
            .params
            .iter()
            .map(|param| (get_param_ident(param), param))
            .collect();

        // NOTE: Iterate in predictable order
        params.sort_by_key(|(ident, _)| *ident);

        for (_, param) in params {
            self.visit_generic_param(param);
        }
        if let Some(where_clause) = &node.where_clause {
            self.visit_where_clause(where_clause);
        }
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

        if node.modifier == syn::parse_quote!(?) {
            self.unsized_type_params
                .insert(self.curr_type_param.unwrap());
        }

        self.type_param_bounds.push((
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
    for (impl_group_idx, impl_group) in impls.item_impls.into_values().enumerate() {
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
            let impl_generics = generics.split_for_impl().0;

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
            (start_idx..start_idx + assoc_bound_idents.len()).map(param::gen_indexed_param_ident),
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

    use proc_macro2::Span;
    use proc_macro_error::abort_call_site;
    use rustc_hash::FxHashMap;

    use quote::format_ident;
    use syn::{visit::Visit, visit_mut::VisitMut};

    /// Indexer for params used in traits, impl trait or self type, but not predicates.
    ///
    /// For `impl<U, T: IntoIterator<Item = V>, V> Trait<T> for U` resolved indices would be:
    /// `T` = 0,
    /// `U` = 1,
    /// `V` = undetermined
    pub struct NonPredicateParamIndexer<'a> {
        unindexed_lifetimes: FxHashMap<&'a syn::Ident, &'a syn::LifetimeParam>,
        unindexed_type_params: FxHashMap<&'a syn::Ident, &'a syn::TypeParam>,
        unindexed_const_params: FxHashMap<&'a syn::Ident, &'a syn::ConstParam>,

        pub indexed_lifetimes: FxHashMap<&'a syn::Ident, (usize, &'a syn::LifetimeParam)>,
        pub indexed_type_params: FxHashMap<&'a syn::Ident, (usize, &'a syn::TypeParam)>,
        pub indexed_const_params: FxHashMap<&'a syn::Ident, (usize, &'a syn::ConstParam)>,

        curr_param_pos_idx: usize,
    }

    pub struct NonPredicateParamResolver<'a> {
        lifetime_replacements: FxHashMap<syn::Ident, &'a syn::Lifetime>,
        type_param_replacements: FxHashMap<syn::Ident, &'a syn::Type>,
        const_param_replacements: FxHashMap<syn::Ident, &'a syn::Expr>,
    }

    pub fn resolve_non_predicate_params(item_impl: &mut syn::ItemImpl) {
        let mut non_predicate_param_indexer = NonPredicateParamIndexer::new(
            item_impl
                .generics
                .lifetimes()
                .map(|param| (&param.lifetime.ident, param)),
            item_impl
                .generics
                .type_params()
                .map(|param| (&param.ident, param)),
            item_impl
                .generics
                .const_params()
                .map(|param| (&param.ident, param)),
            0,
        );

        non_predicate_param_indexer.visit_item_impl(item_impl);

        let mut prev_unindexed_params_count = usize::MAX;
        let mut curr_unindexed_params_count = non_predicate_param_indexer.len();
        let mut indexed_lifetimes = non_predicate_param_indexer.indexed_lifetimes;
        let mut indexed_type_params = non_predicate_param_indexer.indexed_type_params;
        let mut indexed_const_params = non_predicate_param_indexer.indexed_const_params;

        while prev_unindexed_params_count > 0
            // NOTE: This discards parameters only used in where clause
            && prev_unindexed_params_count != curr_unindexed_params_count
        {
            non_predicate_param_indexer = NonPredicateParamIndexer::new(
                non_predicate_param_indexer.unindexed_lifetimes,
                non_predicate_param_indexer.unindexed_type_params,
                non_predicate_param_indexer.unindexed_const_params,
                non_predicate_param_indexer.curr_param_pos_idx,
            );

            non_predicate_param_indexer.visit_indexed_params(
                indexed_type_params
                    .iter()
                    .map(|(_, (idx, param))| (*idx, *param)),
                indexed_const_params
                    .iter()
                    .map(|(_, (idx, param))| (*idx, *param)),
                item_impl.generics.where_clause.as_ref(),
            );

            prev_unindexed_params_count = curr_unindexed_params_count;
            curr_unindexed_params_count = non_predicate_param_indexer.len();
            indexed_lifetimes.extend(non_predicate_param_indexer.indexed_lifetimes);
            indexed_type_params.extend(non_predicate_param_indexer.indexed_type_params);
            indexed_const_params.extend(non_predicate_param_indexer.indexed_const_params);
        }

        let lifetimes = indexed_lifetimes
            .into_iter()
            .map(|(ident, (idx, _))| (ident.clone(), gen_indexed_param_ident(idx)))
            .collect::<FxHashMap<_, _>>();
        let type_params = indexed_type_params
            .into_iter()
            .map(|(ident, (idx, _))| (ident.clone(), gen_indexed_param_ident(idx)))
            .collect::<FxHashMap<_, _>>();
        let const_params = indexed_const_params
            .into_iter()
            .map(|(ident, (idx, _))| (ident.clone(), gen_indexed_param_ident(idx)))
            .collect::<FxHashMap<_, _>>();

        for param in &mut item_impl.generics.params {
            match param {
                syn::GenericParam::Lifetime(syn::LifetimeParam { lifetime, .. }) => {
                    if let Some(new_lifetime) = lifetimes.get(&lifetime.ident) {
                        lifetime.ident = new_lifetime.clone();
                    }
                }
                syn::GenericParam::Type(syn::TypeParam { ident, .. }) => {
                    if let Some(new_ident) = type_params.get(ident) {
                        *ident = new_ident.clone();
                    }
                }
                syn::GenericParam::Const(syn::ConstParam { ident, .. }) => {
                    if let Some(new_ident) = const_params.get(ident) {
                        *ident = new_ident.clone();
                    }
                }
            }
        }

        let lifetimes = lifetimes
            .into_iter()
            .map(|(old_lifetime, new_lifetime)| {
                let new_lifetime = syn::Lifetime {
                    apostrophe: Span::call_site(),
                    ident: new_lifetime,
                };

                (old_lifetime, new_lifetime)
            })
            .collect::<FxHashMap<_, _>>();
        let type_params = type_params
            .into_iter()
            .map(|(old_type_param, new_type_param)| {
                (old_type_param, syn::parse_quote!(#new_type_param))
            })
            .collect::<FxHashMap<_, _>>();
        let const_params = const_params
            .into_iter()
            .map(|(old_const_param, new_const_param)| {
                (old_const_param, syn::parse_quote!(#new_const_param))
            })
            .collect::<FxHashMap<_, _>>();
        NonPredicateParamResolver::new(
            lifetimes.iter().map(|(old, new)| (old.clone(), new)),
            type_params.iter().map(|(old, new)| (old.clone(), new)),
            const_params.iter().map(|(old, new)| (old.clone(), new)),
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

    pub(super) fn gen_indexed_param_ident(idx: usize) -> syn::Ident {
        format_ident!("_ŠČ{idx}")
    }

    impl<'a> NonPredicateParamIndexer<'a> {
        pub fn new(
            unindexed_lifetimes: impl IntoIterator<Item = (&'a syn::Ident, &'a syn::LifetimeParam)>,
            unindexed_type_params: impl IntoIterator<Item = (&'a syn::Ident, &'a syn::TypeParam)>,
            unindexed_const_params: impl IntoIterator<Item = (&'a syn::Ident, &'a syn::ConstParam)>,
            curr_param_pos_idx: usize,
        ) -> Self {
            Self {
                indexed_lifetimes: FxHashMap::default(),
                indexed_type_params: FxHashMap::default(),
                indexed_const_params: FxHashMap::default(),
                unindexed_lifetimes: unindexed_lifetimes.into_iter().collect(),
                unindexed_type_params: unindexed_type_params.into_iter().collect(),
                unindexed_const_params: unindexed_const_params.into_iter().collect(),
                curr_param_pos_idx,
            }
        }

        fn len(&self) -> usize {
            self.unindexed_lifetimes.len()
                + self.unindexed_type_params.len()
                + self.unindexed_const_params.len()
        }

        pub fn visit_indexed_params(
            &mut self,
            indexed_type_params: impl IntoIterator<Item = (usize, &'a syn::TypeParam)>,
            indexed_const_params: impl IntoIterator<Item = (usize, &'a syn::ConstParam)>,
            where_clause: Option<&'a syn::WhereClause>,
        ) {
            enum GenericParamRef<'a> {
                Type(&'a syn::TypeParam),
                Const(&'a syn::ConstParam),
            }

            let node = indexed_type_params
                .into_iter()
                .map(|(idx, type_param)| (idx, GenericParamRef::Type(type_param)))
                .chain(
                    indexed_const_params
                        .into_iter()
                        .map(|(idx, const_param)| (idx, GenericParamRef::Const(const_param))),
                )
                .collect::<FxHashMap<_, _>>();

            let mut indexed_params = node.into_iter().collect::<Vec<_>>();
            indexed_params.sort_by_key(|(k, _)| *k);

            for (_, param) in indexed_params {
                match param {
                    GenericParamRef::Type(type_param) => self.visit_type_param(type_param),
                    GenericParamRef::Const(const_param) => self.visit_const_param(const_param),
                }
            }

            if let Some(where_clause) = where_clause {
                let prev_unindexed_param_count =
                    self.unindexed_type_params.len() + self.unindexed_const_params.len();
                syn::visit::visit_where_clause(self, where_clause);
                let curr_unindexed_param_count =
                    self.unindexed_type_params.len() + self.unindexed_const_params.len();

                if prev_unindexed_param_count < curr_unindexed_param_count {
                    abort_call_site!("Generics bounded only in where clause are not supported");
                }
            }
        }

        fn visit_lifetime_ident(&mut self, lifetime_ident: &'a syn::Ident) -> bool {
            if let Some(removed) = self.unindexed_lifetimes.remove(lifetime_ident) {
                self.indexed_lifetimes
                    .insert(lifetime_ident, (self.curr_param_pos_idx, removed));
                self.curr_param_pos_idx = self.curr_param_pos_idx.checked_add(1).unwrap();
            }

            false
        }

        fn visit_type_param_ident(&mut self, param_ident: &'a syn::Ident) -> bool {
            if let Some(removed) = self.unindexed_type_params.remove(param_ident) {
                self.indexed_type_params
                    .insert(param_ident, (self.curr_param_pos_idx, removed));
                self.curr_param_pos_idx = self.curr_param_pos_idx.checked_add(1).unwrap();

                return true;
            }

            false
        }

        fn visit_const_param_ident(&mut self, param_ident: &'a syn::Ident) -> bool {
            if let Some(removed) = self.unindexed_const_params.remove(param_ident) {
                self.indexed_const_params
                    .insert(param_ident, (self.curr_param_pos_idx, removed));
                self.curr_param_pos_idx = self.curr_param_pos_idx.checked_add(1).unwrap();

                return true;
            }

            false
        }
    }

    impl<'a> Visit<'a> for NonPredicateParamIndexer<'a> {
        fn visit_generics(&mut self, _: &syn::Generics) {}

        fn visit_lifetime(&mut self, node: &'a syn::Lifetime) {
            self.visit_lifetime_ident(&node.ident);
        }

        fn visit_type(&mut self, node: &'a syn::Type) {
            match node {
                syn::Type::Path(ty) => {
                    if let Some(qself) = &ty.qself {
                        self.visit_qself(qself);
                    }

                    let first_seg = ty.path.segments.first().unwrap();
                    self.visit_type_param_ident(&first_seg.ident);
                    syn::visit::visit_path(self, &ty.path);
                }
                _ => syn::visit::visit_type(self, node),
            }
        }

        fn visit_expr(&mut self, node: &'a syn::Expr) {
            match node {
                syn::Expr::Path(ty) => {
                    if let Some(qself) = &ty.qself {
                        self.visit_qself(qself);
                    }

                    let first_seg = ty.path.segments.first().unwrap();
                    if !self.visit_type_param_ident(&first_seg.ident) {
                        self.visit_const_param_ident(&first_seg.ident);
                    }

                    syn::visit::visit_path(self, &ty.path);
                }
                _ => syn::visit::visit_expr(self, node),
            }
        }
    }

    impl<'a> NonPredicateParamResolver<'a> {
        pub fn new(
            lifetime_replacements: impl IntoIterator<Item = (syn::Ident, &'a syn::Lifetime)>,
            type_param_replacements: impl IntoIterator<Item = (syn::Ident, &'a syn::Type)>,
            const_param_replacements: impl IntoIterator<Item = (syn::Ident, &'a syn::Expr)>,
        ) -> Self {
            Self {
                lifetime_replacements: lifetime_replacements.into_iter().collect(),
                type_param_replacements: type_param_replacements.into_iter().collect(),
                const_param_replacements: const_param_replacements.into_iter().collect(),
            }
        }

        fn try_replace_type_path_with_type(&self, path: &syn::Path) -> Option<syn::Type> {
            let mut segments = path.segments.iter();
            let ident = &segments.next().unwrap().ident;

            if let Some(&replacement) = self.type_param_replacements.get(ident) {
                return Some(if path.segments.len() > 1 {
                    syn::parse_quote!(<#replacement> #(::#segments)*)
                } else {
                    syn::parse_quote!(#replacement)
                });
            }

            None
        }

        fn try_replace_expr_path_with_type(&self, path: &mut syn::Path) {
            let first_seg = path.segments.first_mut().unwrap();

            if let Some(replacement) = self.const_param_replacements.get(&first_seg.ident) {
                *first_seg = syn::parse_quote!(#replacement);
            }
        }
    }

    impl VisitMut for NonPredicateParamResolver<'_> {
        fn visit_lifetime_mut(&mut self, node: &mut syn::Lifetime) {
            if let Some(&replace_with) = self.lifetime_replacements.get(&node.ident) {
                *node = replace_with.clone();
            }
        }

        fn visit_type_mut(&mut self, node: &mut syn::Type) {
            match node {
                syn::Type::Path(ty) => {
                    syn::visit_mut::visit_type_path_mut(self, ty);

                    if let Some(new_ty) = self.try_replace_type_path_with_type(&ty.path) {
                        *node = new_ty;
                    }
                }
                _ => syn::visit_mut::visit_type_mut(self, node),
            }
        }

        fn visit_expr_mut(&mut self, node: &mut syn::Expr) {
            match node {
                syn::Expr::Path(ty) => {
                    syn::visit_mut::visit_expr_path_mut(self, ty);

                    // TODO: struct name can clash with type/const param name
                    if let Some(new_ty) = self.try_replace_type_path_with_type(&ty.path) {
                        *ty = syn::parse_quote!(#new_ty);
                    } else {
                        self.try_replace_expr_path_with_type(&mut ty.path);
                    }
                }
                _ => syn::visit_mut::visit_expr_mut(self, node),
            }
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
