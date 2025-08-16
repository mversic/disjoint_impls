use core::iter::zip;

use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use param::get_param_ident;
use proc_macro::TokenStream;
use proc_macro_error2::{abort, proc_macro_error};
use proc_macro2::Span;
use proc_macro2::TokenStream as TokenStream2;
use quote::{ToTokens, format_ident, quote};
use superset::Substitutions;
use syn::ItemImpl;
use syn::parse_quote;
use syn::visit::Visit;
use syn::visit::visit_path;
use syn::visit_mut::VisitMut;
use syn::{
    ItemTrait,
    parse::{Parse, ParseStream},
    parse_macro_input,
};

use crate::superset::Superset;

mod disjoint;
mod helper_trait;
mod main_trait;
mod param;
mod superset;
mod unconstrained;
mod validate;

type TraitBoundIdent = (Bounded, TraitBound);

// TODO: Remove this and implement proper Ord
#[derive(Debug, Clone)]
struct Tokenized<T: ?Sized>(T);

/// AST node type of the trait identifier such as 'IntoIterator<Item = u32>' in `impl<T: IntoIterator<Item = u32>> Clone for T`.
/// Equality of the type doesn't compare associated bounds. Therefore `IntoIterator<Item = u32>` == `IntoIterator<IntoIter = Vec<u32>>`.
#[derive(Debug, Clone)]
struct TraitBound(syn::Path);

/// Unique id of an impl group, i.e. ([`ItemImpl::trait_`], [`ItemImpl::self_ty`]).
/// All [`ItemImpl`]s that have matching group ids are handled by one main trait impl.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ImplGroupId {
    trait_: Option<syn::Path>,
    self_ty: syn::Type,
}

/// Body of the [`disjoint_impls`] macro
#[derive(Debug)]
struct ImplGroups {
    /// Definition of the main trait. [`None`] for inherent impls
    item_trait_: Option<ItemTrait>,
    /// Collection of [`ItemImpl`] blocks grouped by [`ImplGroupId`]
    /// Each impl group is dispatched on a set of associated bounds
    impl_groups: IndexMap<ImplGroupId, ImplGroup>,
}

/// Bounded param/type
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Bounded(syn::Type);

impl<T: ToTokens> PartialEq for Tokenized<T> {
    fn eq(&self, other: &Self) -> bool {
        let first_token_stream = self.0.to_token_stream().to_string();
        let second_token_stream = other.0.to_token_stream().to_string();

        first_token_stream == second_token_stream
    }
}

impl<T: ToTokens> Eq for Tokenized<T> {}

impl<T: Eq + ToTokens> PartialOrd for Tokenized<T> {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: Eq + ToTokens> Ord for Tokenized<T> {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        let first_token_stream = self.0.to_token_stream().to_string();
        let second_token_stream = other.0.to_token_stream().to_string();

        first_token_stream.cmp(&second_token_stream)
    }
}

impl<T: ToTokens> core::hash::Hash for Tokenized<T> {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.0.to_token_stream().to_string().hash(state)
    }
}

impl From<syn::Path> for TraitBound {
    fn from(source: syn::Path) -> Self {
        Self(source)
    }
}

impl PartialEq for TraitBound {
    fn eq(&self, other: &Self) -> bool {
        let mut first_iter = self.0.segments.iter().rev();
        let mut second_iter = other.0.segments.iter().rev();

        let first_last_elem = first_iter.next().unwrap();
        let second_last_elem = second_iter.next().unwrap();

        if !first_iter
            .rev()
            .map(Tokenized)
            .eq(second_iter.rev().map(Tokenized))
        {
            return false;
        }

        if first_last_elem.ident != second_last_elem.ident {
            return false;
        }

        match (&first_last_elem.arguments, &second_last_elem.arguments) {
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
                    .map(Tokenized)
                    .collect::<Vec<_>>();
                let second_args = second_args
                    .args
                    .iter()
                    .filter(|arg| !matches!(arg, syn::GenericArgument::AssocType(_)))
                    .map(Tokenized)
                    .collect::<Vec<_>>();

                if first_args.len() != second_args.len() {
                    return false;
                }

                first_args.iter().eq(&second_args)
            }
            (_, syn::PathArguments::Parenthesized(_))
            | (syn::PathArguments::Parenthesized(_), _) => {
                unreachable!()
            }
        }
    }
}

impl Eq for TraitBound {}

impl PartialOrd for TraitBound {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TraitBound {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        let mut first_iter = self.0.segments.iter().rev();
        let mut second_iter = other.0.segments.iter().rev();

        let first_last_elem = first_iter.next().unwrap();
        let second_last_elem = second_iter.next().unwrap();

        let res = first_iter
            .rev()
            .map(Tokenized)
            .cmp(second_iter.rev().map(Tokenized));
        if res != core::cmp::Ordering::Equal {
            return res;
        }

        let res = first_last_elem.ident.cmp(&second_last_elem.ident);
        if res != core::cmp::Ordering::Equal {
            return res;
        }

        match (&first_last_elem.arguments, &second_last_elem.arguments) {
            (syn::PathArguments::None, syn::PathArguments::None) => core::cmp::Ordering::Equal,
            (syn::PathArguments::AngleBracketed(bracketed), syn::PathArguments::None)
            | (syn::PathArguments::None, syn::PathArguments::AngleBracketed(bracketed)) => {
                if bracketed
                    .args
                    .iter()
                    .any(|arg| !matches!(arg, syn::GenericArgument::AssocType(_)))
                {
                    core::cmp::Ordering::Less
                } else {
                    core::cmp::Ordering::Equal
                }
            }
            (
                syn::PathArguments::AngleBracketed(first_args),
                syn::PathArguments::AngleBracketed(second_args),
            ) => {
                let first_args = first_args
                    .args
                    .iter()
                    .filter(|arg| !matches!(arg, syn::GenericArgument::AssocType(_)))
                    .map(Tokenized)
                    .collect::<Vec<_>>();
                let second_args = second_args
                    .args
                    .iter()
                    .filter(|arg| !matches!(arg, syn::GenericArgument::AssocType(_)))
                    .map(Tokenized)
                    .collect::<Vec<_>>();

                first_args.iter().cmp(&second_args)
            }
            (_, syn::PathArguments::Parenthesized(_))
            | (syn::PathArguments::Parenthesized(_), _) => {
                unreachable!()
            }
        }
    }
}

impl core::hash::Hash for TraitBound {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        let mut iter = self.0.segments.iter().rev();
        let last_elem = iter.next().unwrap();

        iter.rev().for_each(|elem| Tokenized(elem).hash(state));
        last_elem.ident.hash(state);

        match &last_elem.arguments {
            syn::PathArguments::None => {}
            syn::PathArguments::AngleBracketed(first_args) => {
                first_args.args.iter().for_each(|arg| match arg {
                    syn::GenericArgument::AssocType(_) => {}
                    _ => Tokenized(arg).hash(state),
                })
            }
            syn::PathArguments::Parenthesized(_) => unreachable!(),
        }
    }
}

impl quote::ToTokens for TraitBound {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        self.0.leading_colon.to_tokens(tokens);

        let mut iter = self.0.segments.iter();
        let last_elem = iter.next_back().unwrap();
        let last_ident = &last_elem.ident;

        quote!(#(#iter::)* #last_ident).to_tokens(tokens);

        match &last_elem.arguments {
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
            _ => last_elem.arguments.to_tokens(tokens),
        }
    }
}

impl From<&syn::Ident> for Bounded {
    fn from(source: &syn::Ident) -> Self {
        Self(parse_quote!(#source))
    }
}

impl From<&syn::Type> for Bounded {
    fn from(source: &syn::Type) -> Self {
        Self(source.clone())
    }
}

impl quote::ToTokens for Bounded {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        self.0.to_tokens(tokens);
    }
}

/// Param bounds
#[derive(Debug, Clone, Default)]
struct ItemImplBounds(
    // NOTE: Order is preserved as found in the implementation
    Vec<(TraitBoundIdent, Vec<(syn::Ident, AssocBindingPayload)>)>,
);

#[derive(Debug, Clone)]
struct AssocBindingsGroup {
    // TODO: Move to Vec of payloads
    //bindings: IndexMap<TraitBoundIdent, IndexMap<syn::Ident, Vec<AssocBindingPayload>>>,
    bindings: IndexMap<TraitBoundIdent, Vec<IndexMap<syn::Ident, AssocBindingPayload>>>,
}

impl AssocBindingsGroup {
    fn new(impl_bounds: ItemImplBounds) -> Self {
        let bindings = impl_bounds.0.into_iter().fold(
            IndexMap::<_, Vec<_>>::new(),
            |mut acc, (trait_bound_id, assoc_bindings)| {
                acc.entry(trait_bound_id)
                    .or_insert_with(|| vec![IndexMap::new()])[0]
                    .extend(assoc_bindings);

                acc
            },
        );

        Self { bindings }
    }

    /// Removes all assoc bounds that are mandated by the main trait.
    /// These bounds are always the same for all impllementations.
    ///
    /// # Example
    ///
    /// ```ignore
    /// impl trait Kita<T: Bound<Type = u32>> {}
    /// impl<T> Kita for T where T: Bound<Type = u32> {}
    /// ```
    ///
    /// here `Bound<Type = u32>` shouldn't be taken as part of the dispatch assoc bounds
    //fn prune_main_trait_assoc(
    //    &mut self,
    //    main_trait_generics: &syn::Generics,
    //    impl_trait: &syn::Path,
    //) {
    //    let mut type_params = IndexMap::new();

    //    match &impl_trait.segments.last().unwrap().arguments {
    //        syn::PathArguments::None => {}
    //        syn::PathArguments::AngleBracketed(bracketed) => {
    //            main_trait_generics
    //                .params
    //                .iter()
    //                .zip(&bracketed.args)
    //                .for_each(|(param, arg)| {
    //                    if let (syn::GenericParam::Type(param), syn::GenericArgument::Type(arg)) =
    //                        (param, arg)
    //                    {
    //                        type_params.insert(param.ident.clone(), arg);
    //                    }
    //                });
    //        }
    //        syn::PathArguments::Parenthesized(_) => unreachable!(),
    //    }

    //    let mut main_trait_generics = main_trait_generics.clone();
    //    rewrite_main_trait_bounds_to_where_clause(&mut main_trait_generics);
    //    if let Some(where_clause) = &mut main_trait_generics.where_clause {
    //        let mut param_resolver =
    //            NonPredicateParamResolver::new(IndexMap::new(), type_params, IndexMap::new());
    //        param_resolver.visit_where_clause_mut(where_clause);
    //        let main_trait_param_bounds = TraitBoundsVisitor::find(&main_trait_generics);

    //        self.bounds = core::mem::take(&mut self.bounds)
    //            .into_iter()
    //            .map(|(assoc_binding, assoc_binding_payload)| {
    //                if let Some((_, matching_bound_payload)) = main_trait_param_bounds
    //                    .0
    //                    .iter()
    //                    .find(|(bound, _)| bound == &assoc_binding)
    //                {
    //                    let assoc_binding_payload = assoc_binding_payload
    //                        .into_iter()
    //                        .filter(|per_impl_payloads| {
    //                            !matching_bound_payload
    //                                .iter()
    //                                .any(|(ident, _)| per_impl_payloads.contains_key(ident))
    //                        })
    //                        .collect();

    //                    (assoc_binding, assoc_binding_payload)
    //                } else {
    //                    (assoc_binding, assoc_binding_payload)
    //                }
    //            })
    //            .collect();

    //        self.prune_non_assoc();
    //    }
    //}

    /// Removes all trait bounds that don't have an associated type in any of the impls
    ///
    /// # Example
    ///
    /// ```ignore
    /// impl<T: Dispatch<Group = GroupA> + Dispatch2> Kita for T {
    ///     const NAME: &'static str = "Blanket A";
    /// }
    /// impl<T: Dispatch<Group = GroupB> + Dispatch2, U> Kita for (T, U) where {
    ///     const NAME: &'static str = "Blanket C";
    /// }
    /// ```
    ///
    /// `Dispatch2` is considered as an associated bound because it isn't known ahead of time
    /// if some of the impls will use it as a differentiator. After all impls are processed
    /// `Dispatch2` should be removed since no impl was found where it had an associated bound
    fn prune_non_assoc(&mut self) {
        self.bindings = core::mem::take(&mut self.bindings)
            .into_iter()
            .filter(|(_, impl_assoc_bindings)| {
                impl_assoc_bindings
                    .iter()
                    .any(|assoc_bindings| !assoc_bindings.is_empty())
            })
            .collect();
    }

    /// Returns true if all associated bounds of any 2 impls are overlapping
    ///
    /// # Example
    ///
    /// ```ignore
    /// impl<T: Dispatch<Group = GroupB>> Kita for T {
    ///     const NAME: &'static str = "Blanket B";
    /// }

    /// impl<T: Dispatch<Group = GroupA>, U: Dispatch<Group = GroupA>> Kita for (T, U) {
    ///     const NAME: &'static str = "Blanket AA";
    /// }
    /// impl<T: Dispatch<Group = GroupA>, U: Dispatch<Group = GroupB>> Kita for (T, U) {
    ///     const NAME: &'static str = "Blanket AB";
    /// }
    /// ```
    ///
    /// since `T` is a superset of `(T, U)` when the above 3 impls form impl group `impl Kita for T`
    /// they will do so on the associated bound `T: Dispatch<Group = x>`. Notice, however, that the
    /// last 2 impls are dispatched on the same payload `GroupA` of the chosen associated bound,
    /// i.e. they are overlapping. Therefore this impl group is invalid and should be discarded
    fn is_overlapping(&mut self) -> bool {
        let assoc_payloads = self.payloads().collect::<Vec<_>>();

        assoc_payloads
            .iter()
            .enumerate()
            .flat_map(|(i, a1)| {
                let iter2 = assoc_payloads.iter().enumerate();

                iter2.filter_map(move |(j, a2)| {
                    if i == j {
                        return None;
                    }

                    Some((a1, a2))
                })
            })
            .any(|(a1, a2)| {
                assert_eq!(a1.len(), a2.len());

                zip(a1, a2).all(|(e1, e2)| match (e1, e2) {
                    (Some(e1), Some(e2)) => e1.is_superset(e2).is_some(),
                    (None, _) => true,
                    _ => false,
                })
            })
    }

    fn is_empty(&self) -> bool {
        self.bindings.is_empty()
    }

    /// Returns an ordered list of associated bound identifiers
    ///
    /// # Example
    ///
    /// ```ignore
    /// impl<T: Dispatch1<Group = GroupA> + Dispatch2<Group = GroupA>> Kita for T {}
    /// impl<T: Dispatch1<Group = GroupB> + Dispatch2> Kita for T {}
    /// impl<T: Dispatch1 + Dispatch2<Group = GroupB>> Kita for T {}
    /// ```
    ///
    /// would return: `[(Dispatch1, Group), (Dispatch2, Group)]`
    fn idents(&self) -> impl Iterator<Item = AssocBindingIdent<'_>> {
        self.bindings
            .iter()
            .flat_map(|(trait_bound, impl_assoc_bindings)| {
                impl_assoc_bindings
                    .iter()
                    .fold(IndexSet::new(), |mut acc, assoc_bindings| {
                        assoc_bindings.iter().for_each(|assoc_binding| {
                            acc.insert(assoc_binding.0);
                        });

                        acc
                    })
                    .into_iter()
                    .map(move |ident| (trait_bound, ident))
            })
    }

    /// Returns an ordered list of associated bound payloads for every impl item
    ///
    /// # Example
    ///
    /// ```ignore
    /// impl<T: Dispatch<Group = GroupA>> Kita for T {}
    /// impl<T: Dispatch<Group = GroupB>> Kita for T {}
    /// impl<T: Dispatch> Kita for T {}
    /// ```
    ///
    /// would return: `[Some([GroupA]), Some([GroupB]), None]`
    fn payloads(&self) -> impl Iterator<Item = Vec<Option<&AssocBindingPayload>>> {
        let mut impl_item_idx = 0;

        core::iter::from_fn(move || {
            let mut assoc_bindings_map = IndexMap::new();

            if impl_item_idx > 0 && self.bindings.is_empty() {
                return None;
            }

            for (trait_bound_id, impl_assoc_bindings) in &self.bindings {
                if impl_item_idx >= impl_assoc_bindings.len() {
                    return None;
                }

                assoc_bindings_map.extend(
                    impl_assoc_bindings[impl_item_idx]
                        .iter()
                        .map(|(ident, payload)| ((trait_bound_id, ident), Some(payload))),
                );
            }

            impl_item_idx += 1;
            let impl_payloads = self.idents().map(|assoc_binding_ident| {
                assoc_bindings_map
                    .swap_remove(&assoc_binding_ident)
                    .unwrap_or_default()
            });

            Some(Iterator::collect::<Vec<_>>(impl_payloads))
        })
    }

    fn intersection(
        &self,
        other: &ItemImplBounds,
        substitutions: &Substitutions,
    ) -> impl Iterator<Item = Self> + use<> {
        let other = other.0.iter().fold(
            IndexMap::<_, IndexMap<_, _>>::new(),
            |mut acc, (trait_bound_id, assoc_bindings)| {
                let entry = acc.entry(trait_bound_id).or_default();
                entry.extend(assoc_bindings.iter().map(|(k, v)| (k, v)));

                acc
            },
        );

        other
            .into_iter()
            .map(|(other_trait_bound, other_assoc_bindings)| {
                substitutions
                    .substitute(other_trait_bound)
                    .map(|subs_trait_bound| {
                        let found_bound = self.bindings.get(&subs_trait_bound).cloned();

                        found_bound.map(|mut impl_assoc_bindings| {
                            let other_assoc_bindings = other_assoc_bindings
                                .iter()
                                .map(|(&ident, &payload)| (ident.clone(), payload.clone()))
                                .collect();

                            impl_assoc_bindings.push(other_assoc_bindings);
                            (subs_trait_bound, impl_assoc_bindings)
                        })
                    })
                    .collect::<Vec<_>>()
            })
            .multi_cartesian_product()
            .map(move |bounds| Self {
                bindings: bounds.into_iter().flatten().collect(),
            })
    }
}

/// Unique name based identifier of the associated type bound such as:
///     `(T, Deref, Deref::Target)` in `impl<T: Deref<Target = bool>> for Clone for T`
type AssocBindingIdent<'a> = (&'a TraitBoundIdent, &'a syn::Ident);

/// AST node type of the associated bound constraint such as:
///     `bool` in `impl<T: Deref<Target = bool>> for Clone for T`
type AssocBindingPayload = syn::Type;

struct ImplGroupParamsPruner {
    impl_types: IndexMap<syn::Ident, bool>,
    impl_consts: IndexSet<syn::Ident>,

    id_params: Vec<syn::GenericParam>,
}

impl ImplGroupParamsPruner {
    fn new(type_params: IndexMap<syn::Ident, bool>, const_params: IndexSet<syn::Ident>) -> Self {
        Self {
            impl_types: type_params,
            impl_consts: const_params,

            id_params: Vec::new(),
        }
    }

    fn prune(
        mut self,
        impl_group_id: &ImplGroupId,
        assoc_bindings: &AssocBindingsGroup,
    ) -> Vec<syn::GenericParam> {
        if let Some(trait_) = &impl_group_id.trait_ {
            self.visit_path(trait_);
        }

        self.visit_type(&impl_group_id.self_ty);
        for (Bounded(bounded), _) in assoc_bindings.bindings.keys() {
            self.visit_type(bounded);
        }

        self.id_params
    }
}

impl Visit<'_> for ImplGroupParamsPruner {
    fn visit_path(&mut self, node: &syn::Path) {
        if let Some(ident) = node.get_ident() {
            if let Some(is_unsized) = self.impl_types.swap_remove(ident) {
                let param: syn::TypeParam = if is_unsized {
                    parse_quote! { #node: ?Sized }
                } else {
                    parse_quote! { #node }
                };

                self.id_params.push(param.into());
            } else if self.impl_consts.swap_remove(ident) {
                let param: syn::ConstParam = parse_quote! { #node };
                self.id_params.push(param.into());
            }
        } else {
            visit_path(self, node);
        }
    }
}

struct TraitBoundsVisitor<'a> {
    /// Bounded type currently being visited
    curr_bounded_ty: Option<Bounded>,
    /// Trait bound currently being visited
    curr_trait_bound: Option<TraitBound>,

    /// Bounds of an impl block
    param_bounds: ItemImplBounds,
    /// Resolved self type
    self_ty: &'a syn::Type,
}

impl<'a> TraitBoundsVisitor<'a> {
    fn find(item_impl: &'a ItemImpl) -> ItemImplBounds {
        let mut visitor = Self {
            curr_bounded_ty: None,
            curr_trait_bound: None,

            param_bounds: ItemImplBounds::default(),
            self_ty: &*item_impl.self_ty,
        };
        visitor.visit_generics(&item_impl.generics);
        visitor.param_bounds
    }
}

impl Visit<'_> for TraitBoundsVisitor<'_> {
    fn visit_item_impl(&mut self, node: &ItemImpl) {
        self.visit_generics(&node.generics);
    }

    fn visit_generics(&mut self, node: &syn::Generics) {
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

    fn visit_type_param(&mut self, node: &syn::TypeParam) {
        self.curr_bounded_ty = Some((&node.ident).into());
        syn::visit::visit_type_param(self, node);
    }

    fn visit_predicate_type(&mut self, node: &syn::PredicateType) {
        struct GenericFinder(bool);

        impl Visit<'_> for GenericFinder {
            fn visit_path(&mut self, node: &syn::Path) {
                if let Some(ident) = node.get_ident()
                    && ident.to_string().starts_with("_ŠČ")
                {
                    self.0 = true;
                } else {
                    visit_path(self, node);
                }
            }
        }
        let bounded_ty = if node.bounded_ty == parse_quote!(Self) {
            &self.self_ty
        } else {
            &node.bounded_ty
        };

        let mut generic_finder = GenericFinder(false);
        generic_finder.visit_type(bounded_ty);

        if generic_finder.0 {
            self.curr_bounded_ty = Some(bounded_ty.into());
            syn::visit::visit_predicate_type(self, node);
        }
    }

    fn visit_trait_bound(&mut self, node: &syn::TraitBound) {
        self.curr_trait_bound = Some(node.path.clone().into());

        let curr_trait_bound_ident = (
            self.curr_bounded_ty.clone().unwrap(),
            self.curr_trait_bound.clone().unwrap(),
        );

        self.param_bounds
            .0
            .push((curr_trait_bound_ident, Vec::new()));

        syn::visit::visit_trait_bound(self, node);
    }

    fn visit_assoc_type(&mut self, node: &syn::AssocType) {
        self.param_bounds
            .0
            .last_mut()
            .unwrap()
            .1
            .push((node.ident.clone(), node.ty.clone()));
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
    let impls: ImplGroups = parse_macro_input!(input);

    let mut helper_traits = Vec::new();
    let mut main_trait_impls = Vec::new();
    let mut item_impls = Vec::new();
    let mut constrain_traits = Vec::new();

    let main_trait = impls.item_trait_;
    for (impl_group_idx, (impl_group_id, mut impl_group)) in
        impls.impl_groups.into_iter().enumerate()
    {
        if impl_group.item_impls.len() > 1 {
            helper_traits.push(helper_trait::generate(
                main_trait.as_ref(),
                impl_group_idx,
                &impl_group,
            ));

            if let Some(mut main_trait_impl) = main_trait::generate(
                main_trait.as_ref(),
                impl_group_idx,
                &impl_group_id,
                &impl_group,
            ) {
                constrain_traits.push(unconstrained::generate(
                    main_trait.as_ref(),
                    &mut main_trait_impl,
                    impl_group_idx,
                    &impl_group,
                ));

                main_trait_impls.push(main_trait_impl);
            }

            item_impls.extend(disjoint::generate(impl_group_idx, impl_group));
        } else if let Some(main_trait_impl) = impl_group.item_impls.pop() {
            main_trait_impls.push(main_trait_impl);
        }
    }

    quote! {
        #main_trait

        const _: () = {
            #( #helper_traits )*
            #( #item_impls )*
            #( #main_trait_impls )*

            #( #constrain_traits )*
        };
    }
    .into()
}

#[derive(Debug)]
struct ImplGroup {
    /// Id of this impl group
    id: ImplGroupId,
    /// All [impl blocks](syn::ItemImpl) that are part of this group (in the order of appearance)
    item_impls: Vec<syn::ItemImpl>,
    /// Associated bindings that impls of this group are dispatched on (i.e. differentiated by)
    assoc_bindings: AssocBindingsGroup,
    /// Generic parameters used in the impl group id or the bounded type of associated type binding
    params: Vec<syn::GenericParam>,
}

impl Parse for ImplGroups {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        let mut impl_groups = IndexMap::<_, IndexMap<_, _>>::new();

        let mut impl_group_lifetimes = IndexMap::<_, IndexSet<_>>::new();
        let mut impl_group_type_params = IndexMap::<_, IndexMap<_, _>>::new();
        let mut impl_group_const_params = IndexMap::<_, IndexSet<_>>::new();

        let main_trait = input.parse::<ItemTrait>().ok();
        while let Ok(mut item) = input.parse::<ItemImpl>() {
            param::index(&item).resolve(&mut item);

            let impl_group_id = ImplGroupId {
                trait_: item.trait_.as_ref().map(|trait_| &trait_.1).cloned(),
                self_ty: (*item.self_ty).clone(),
            };
            let impl_group_type_params = impl_group_type_params
                .entry(impl_group_id.clone())
                .or_default();

            let item_lifetimes = item
                .generics
                .lifetimes()
                .map(|param| &param.lifetime.ident)
                .cloned();
            let item_type_params = item.generics.type_params().cloned().map(|param| {
                let is_unsized = param.bounds.into_iter().any(|bound| {
                    matches!(
                        bound,
                        syn::TypeParamBound::Trait(syn::TraitBound {
                            modifier: syn::TraitBoundModifier::Maybe(_),
                            ..
                        })
                    )
                });

                (param.ident.clone(), is_unsized)
            });
            let item_const_params = item
                .generics
                .const_params()
                .map(|param| &param.ident)
                .cloned();

            impl_group_lifetimes
                .entry(impl_group_id.clone())
                .or_default()
                .extend(item_lifetimes);
            item_type_params.for_each(|(type_param, is_unsized)| {
                if let Some(entry) = impl_group_type_params.get_mut(&type_param) {
                    *entry |= is_unsized;
                } else {
                    impl_group_type_params.insert(type_param, is_unsized);
                }
            });
            impl_group_const_params
                .entry(impl_group_id.clone())
                .or_default()
                .extend(item_const_params);

            let param_bounds = TraitBoundsVisitor::find(&item);
            impl_groups
                .entry(impl_group_id.clone())
                .or_default()
                .insert(item, param_bounds);
        }

        let (mut supersets, subsets) = make_sets(impl_groups.keys());

        let impl_groups = supersets
            .iter()
            .filter_map(|(&impl_group_id, superset_count)| {
                if *superset_count == 0 {
                    return Some(impl_group_id);
                }

                None
            })
            .collect::<Vec<_>>()
            .into_iter()
            .flat_map(|impl_group_id| {
                let impl_group = impl_groups[impl_group_id].keys().collect::<Vec<_>>();

                find_impl_groups(
                    (impl_group_id, &impl_group),
                    &subsets,
                    &mut supersets,
                    &impl_groups,
                )
                // TODO: Handle error properly
                .unwrap_or_else(|| {
                    let trait_ = &impl_group_id.trait_;
                    let ty = &impl_group_id.self_ty;

                    let impl_group = trait_.as_ref().map_or_else(
                        || format!("`impl {}`", quote!(#ty)),
                        |trait_| format!("`impl {} for {}`", quote!(#trait_), quote!(#ty)),
                    );

                    panic!("Conflicting implementations of {impl_group}");
                })
            })
            .map(|(impl_group_id, (assoc_bindings, impl_group))| {
                let params_pruner = ImplGroupParamsPruner::new(
                    impl_group_type_params.swap_remove(impl_group_id).unwrap(),
                    impl_group_const_params.swap_remove(impl_group_id).unwrap(),
                );

                //if let Some(main_trait) = &main_trait {
                //    assoc_bindings.prune_main_trait_assoc(
                //        &main_trait.generics,
                //        impl_group_id.0.as_ref().unwrap(),
                //    );
                //}

                (
                    impl_group_id.clone(),
                    ImplGroup {
                        id: impl_group_id.clone(),
                        item_impls: impl_group.into_iter().cloned().collect(),
                        params: impl_group_lifetimes
                            .swap_remove(impl_group_id)
                            .unwrap()
                            .into_iter()
                            .map(|ident| {
                                syn::LifetimeParam::new(syn::Lifetime::new(
                                    &format!("'{}", ident),
                                    Span::call_site(),
                                ))
                                .into()
                            })
                            .chain(params_pruner.prune(impl_group_id, &assoc_bindings))
                            .collect(),
                        assoc_bindings,
                    },
                )
            })
            .collect();

        Ok(ImplGroups::new(main_trait, impl_groups))
    }
}

fn find_impl_groups<'a, 'b>(
    curr_impl_group: (&'a ImplGroupId, &[&'b ItemImpl]),

    impl_group_id_subsets: &Subsets<'a>,
    impl_group_id_supersets: &mut Supersets<'a>,

    item_impls: &'b IndexMap<ImplGroupId, IndexMap<ItemImpl, ItemImplBounds>>,
) -> Option<IndexMap<&'a ImplGroupId, (AssocBindingsGroup, Vec<&'b ItemImpl>)>> {
    find_impl_groups_rec(
        curr_impl_group,
        impl_group_id_subsets,
        impl_group_id_supersets,
        item_impls,
        &mut IndexMap::new(),
    )
}

fn find_impl_groups_rec<'a, 'b>(
    curr_impl_group: (&'a ImplGroupId, &[&'b ItemImpl]),

    impl_group_id_subsets: &Subsets<'a>,
    impl_group_id_supersets: &mut Supersets<'a>,

    item_impls: &'b IndexMap<ImplGroupId, IndexMap<ItemImpl, ItemImplBounds>>,
    impl_groups: &mut IndexMap<&'a ImplGroupId, (AssocBindingsGroup, Vec<&'b ItemImpl>)>,
) -> Option<IndexMap<&'a ImplGroupId, (AssocBindingsGroup, Vec<&'b ItemImpl>)>> {
    let curr_impl_group_id = curr_impl_group.0;

    if let Some((&curr_impl, other)) = curr_impl_group.1.split_first() {
        let mut new_supersets = IndexMap::new();

        let curr_impl_bounds = &item_impls[curr_impl_group_id][curr_impl];
        let impl_group_ids = impl_groups.keys().cloned().collect::<Vec<_>>();

        let mut acc = None::<IndexMap<_, _>>;
        for impl_group_id in impl_group_ids {
            let impl_group = &impl_groups[impl_group_id];

            if let Some(subs) = impl_group_id_subsets[impl_group_id].get(curr_impl_group_id) {
                impl_group.0.intersection(curr_impl_bounds, subs)
            } else if impl_group_id == curr_impl_group_id {
                let subs = &impl_group_id.is_superset(curr_impl_group_id).unwrap();
                impl_group.0.intersection(curr_impl_bounds, subs)
            } else {
                // NOTE: It's possible that current impl group is not a subset
                // of a group that comes before it in the topological order
                continue;
            }
            .for_each(|intersection| {
                let impl_group = impl_groups.get_mut(impl_group_id).unwrap();
                let prev_assoc_bindings = core::mem::replace(&mut impl_group.0, intersection);
                impl_group.1.push(curr_impl);

                let mut supersets = impl_group_id_supersets.clone();
                let res = find_impl_groups_rec(
                    (curr_impl_group_id, other),
                    impl_group_id_subsets,
                    &mut supersets,
                    item_impls,
                    impl_groups,
                )
                .and_then(|mut impl_groups| {
                    for (assoc_bindings_group, impls) in impl_groups.values_mut() {
                        assoc_bindings_group.prune_non_assoc();

                        if impls.len() > 1 && assoc_bindings_group.is_empty() {
                            return None;
                        }
                        if assoc_bindings_group.is_overlapping() {
                            return None;
                        }
                    }

                    Some(impl_groups)
                });

                match (res, &mut acc) {
                    (Some(res), Some(acc)) if res.len() < acc.len() => {
                        new_supersets = supersets;
                        *acc = res;
                    }
                    (Some(res), None) => {
                        new_supersets = supersets;
                        acc = Some(res);
                    }
                    _ => {}
                }

                // NOTE: Restore changes to the impl groups set for the next iteration
                let impl_group_to_restore = impl_groups.get_mut(impl_group_id).unwrap();
                impl_group_to_restore.0 = prev_assoc_bindings;
                impl_group_to_restore.1.pop();
            });
        }

        if acc.is_some() {
            *impl_group_id_supersets = new_supersets;
        // NOTE: Create a new group if it doesn't exist yet
        } else if !impl_groups.contains_key(curr_impl_group_id) {
            impl_groups.insert(
                curr_impl_group_id,
                (
                    AssocBindingsGroup::new(curr_impl_bounds.clone()),
                    vec![curr_impl],
                ),
            );

            acc = find_impl_groups_rec(
                (curr_impl_group_id, other),
                impl_group_id_subsets,
                impl_group_id_supersets,
                item_impls,
                impl_groups,
            );

            // NOTE: Restore changes to impl groups
            impl_groups.shift_remove(curr_impl_group_id);
        }

        return acc;
    }

    unlock_subset_impl_groups(
        curr_impl_group_id,
        impl_group_id_subsets,
        impl_group_id_supersets,
        item_impls,
        impl_groups,
    )
}

fn unlock_subset_impl_groups<'a, 'b>(
    curr_impl_group_id: &'a ImplGroupId,

    impl_group_id_subsets: &Subsets<'a>,
    impl_group_id_supersets: &mut Supersets<'a>,

    item_impls: &'b IndexMap<ImplGroupId, IndexMap<ItemImpl, ItemImplBounds>>,
    impl_groups: &mut IndexMap<&'a ImplGroupId, (AssocBindingsGroup, Vec<&'b ItemImpl>)>,
) -> Option<IndexMap<&'a ImplGroupId, (AssocBindingsGroup, Vec<&'b ItemImpl>)>> {
    let mut acc = Some(impl_groups.clone());

    for (&subset_impl_group_id, _) in &impl_group_id_subsets[curr_impl_group_id] {
        let subset = &item_impls[subset_impl_group_id].keys().collect::<Vec<_>>();

        let superset_count = impl_group_id_supersets
            .get_mut(subset_impl_group_id)
            .unwrap();

        *superset_count -= 1;
        if *superset_count == 0 {
            acc = acc.and_then(|mut impl_groups| {
                find_impl_groups_rec(
                    (subset_impl_group_id, subset),
                    impl_group_id_subsets,
                    impl_group_id_supersets,
                    item_impls,
                    &mut impl_groups,
                )
            });
        }
    }

    acc
}

fn gen_inherent_self_ty_args(self_ty: &mut syn::TypePath, generics: &[syn::GenericParam]) {
    let last_seg = &mut self_ty.path.segments.last_mut().unwrap();

    if let syn::PathArguments::AngleBracketed(bracketed) = &mut last_seg.arguments {
        let mut lifetimes = Vec::new();
        let mut params = Vec::new();

        generics.iter().for_each(|param| match param {
            syn::GenericParam::Lifetime(syn::LifetimeParam { lifetime, .. }) => {
                lifetimes.push(lifetime);
            }
            syn::GenericParam::Type(syn::TypeParam { ident, .. }) => params.push(ident),
            syn::GenericParam::Const(syn::ConstParam { ident, .. }) => params.push(ident),
        });

        lifetimes.sort();
        params.sort();

        *bracketed = parse_quote!(<#(#lifetimes,)* #(#params),*>);
    } else {
        unreachable!()
    }
}

impl ImplGroups {
    fn new(item_trait_: Option<ItemTrait>, impl_groups: IndexMap<ImplGroupId, ImplGroup>) -> Self {
        if let Some(trait_) = &item_trait_ {
            for ImplGroup { item_impls, .. } in impl_groups.values() {
                validate::validate_trait_impls(trait_, item_impls);
            }
        } else {
            for ImplGroup { item_impls, .. } in impl_groups.values() {
                validate::validate_inherent_impls(item_impls);
            }
        }

        Self {
            item_trait_,
            impl_groups,
        }
    }
}

type Supersets<'a> = IndexMap<&'a ImplGroupId, usize>;
type Subsets<'a> = IndexMap<&'a ImplGroupId, IndexMap<&'a ImplGroupId, Substitutions<'a>>>;

fn make_sets<'a, I>(impl_groups: I) -> (Supersets<'a>, Subsets<'a>)
where
    I: IntoIterator<Item = &'a ImplGroupId, IntoIter: Clone>,
{
    let impl_groups = impl_groups.into_iter();

    impl_groups
        .clone()
        .cartesian_product(impl_groups.clone())
        .filter(|(g1, g2)| g1 != g2)
        .filter_map(|(g1, g2)| g1.is_superset(g2).map(|subs| (g1, g2, subs)))
        .fold(
            (
                impl_groups
                    .clone()
                    .map(|id| (id, 0))
                    .collect::<IndexMap<_, _>>(),
                impl_groups
                    .map(|id| (id, IndexMap::new()))
                    .collect::<IndexMap<_, _>>(),
            ),
            |(mut supersets, mut subsets), (g1, g2, subs)| {
                subsets.get_mut(g1).unwrap().insert(g2, subs);
                *supersets.get_mut(g2).unwrap() += 1;
                (supersets, subsets)
            },
        )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_sets_checked<'a, I>(impl_groups: I) -> (Supersets<'a>, Subsets<'a>)
    where
        I: IntoIterator<Item = &'a ImplGroupId, IntoIter: Clone>,
    {
        let impl_groups = impl_groups.into_iter().collect::<Vec<_>>();
        let (supersets, subsets) = make_sets(impl_groups.clone());

        let expected_supersets = [
            (impl_groups[0], 0),
            (impl_groups[1], 1),
            (impl_groups[2], 1),
            (impl_groups[3], 3),
        ];

        assert_eq!(
            supersets,
            expected_supersets
                .iter()
                .cloned()
                .collect::<IndexMap<_, _>>()
        );

        (supersets, subsets)
    }

    #[test]
    fn find_impl_group_candidates_works_correctly() {
        let impl_groups: IndexMap<ImplGroupId, IndexMap<ItemImpl, ItemImplBounds>> = [
            (
                quote!((_ŠČ0, _ŠČ1)),
                quote!((_ŠČ0, _ŠČ1)),
                quote!(GroupA),
                "Blanket A",
            ),
            (
                quote!((Vec<_ŠČ0>, _ŠČ1)),
                quote!((Vec<_ŠČ0>, _ŠČ1)),
                quote!(GroupB),
                "Blanket B",
            ),
            (
                quote!((_ŠČ0, Vec<_ŠČ1>)),
                quote!((_ŠČ0, Vec<_ŠČ1>)),
                quote!(GroupC),
                "Blanket C",
            ),
            (
                quote!((Vec<_ŠČ0>, Vec<_ŠČ1>)),
                quote!((Vec<_ŠČ0>, Vec<_ŠČ1>)),
                quote!(GroupD),
                "Blanket D",
            ),
        ]
        .iter()
        .map(|(ty, bounded_ty, assoc_ty, msg)| {
            (
                ImplGroupId {
                    trait_: Some(syn::parse_quote!(Kita)),
                    self_ty: syn::parse_quote!(#ty),
                },
                parse_quote! {
                    impl<_ŠČ> Kita for #ty where #bounded_ty: Dispatch<Group = #assoc_ty> {
                        const NAME: &'static str = #msg;
                    }
                },
                ItemImplBounds(vec![(
                    (
                        Bounded(parse_quote!(#bounded_ty)),
                        TraitBound(parse_quote!(Dispatch<Group = #assoc_ty>)),
                    ),
                    vec![(parse_quote!(Group), parse_quote!(#assoc_ty))],
                )]),
            )
        })
        .map(|(group_id, impl_item, assoc_bindings)| {
            let mut map = IndexMap::new();
            map.insert(impl_item, assoc_bindings);
            (group_id, map)
        })
        .collect();

        let (mut supersets, subsets) = make_sets_checked(impl_groups.keys());

        let impl_groups = supersets
            .iter()
            .filter_map(|(&impl_group_id, superset_count)| {
                if *superset_count == 0 {
                    return Some(impl_group_id);
                }

                None
            })
            .collect::<Vec<_>>()
            .into_iter()
            .flat_map(|impl_group_id| {
                let impl_group = impl_groups[impl_group_id].keys().collect::<Vec<_>>();

                find_impl_groups(
                    (impl_group_id, &impl_group),
                    &subsets,
                    &mut supersets,
                    &impl_groups,
                )
                .unwrap()
            })
            .collect::<Vec<_>>();

        assert_eq!(impl_groups.len(), 1);
        // TODO: Check the output as well
    }
}
