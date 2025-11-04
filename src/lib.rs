//! Unlock support for a variety of mutually disjoint implementations that the Rust compiler
//! [does not (yet?) support](https://github.com/rust-lang/rust/issues/20400).
//!
//! Works for trait and inherent implementations alike (no special syntax).
//!
//! # Trait implementations
//!
//! ```
//! use disjoint_impls::disjoint_impls;
//!
//! pub trait Dispatch {
//!     type Group;
//! }
//!
//! disjoint_impls! {
//!     pub trait Kita {}
//!
//!     impl<T: Dispatch<Group = u32>> Kita for T {}
//!     impl<T: Dispatch<Group = i32>> Kita for T {}
//! }
//! ```
//!
//! # Inherent implementations
//!
//! ```
//! use disjoint_impls::disjoint_impls;
//!
//! pub trait Dispatch {
//!     type Group;
//! }
//!
//! struct Wrapper<T>(T);
//!
//! disjoint_impls! {
//!     impl<T: Dispatch<Group = u32>> Wrapper<T> {}
//!     impl<T: Dispatch<Group = i32>> Wrapper<T> {}
//! }
//! ```
//!
//! See the [`disjoint_impls!`] macro for details.

use generalize::Generalizations;
use indexmap::{IndexMap, IndexSet};
use itertools::Itertools as _;
use proc_macro::TokenStream;
use proc_macro_error2::{abort, proc_macro_error};
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::{
    ItemImpl, ItemTrait,
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote,
    visit::{Visit, visit_trait_bound},
};

use crate::{
    generalize::{Generalize, GenericParam, Params, Sizedness, as_generics},
    main_trait::is_remote,
    validate::validate_impl_syntax,
};

mod disjoint;
mod generalize;
mod helper_trait;
mod main_trait;
mod normalize;
mod validate;

/// Identifier of a type bounded with a trait such as:
///     `Option<T>: Kita` in `impl<T> Foo for T where Option<T>: Kita`
type TraitBoundIdent = (Bounded, TraitBound);

/// AST node type of the associated bound constraint such as:
///     `bool` in `impl<T: Deref<Target = bool>> for Clone for T`
type AssocBindingPayload = syn::Type;

/// Mapping from an associated type identifier to it's payload such as:
///     `Target = bool` in `impl<T: Deref<Target = bool>> Clone for T`
///
/// This mapping tracks generalized payload (payload to be used in the main trait impl)
/// and a concrete payload of every impl
type AssocBindings = IndexMap<
    syn::Ident,
    (
        Option<AssocBindingPayload>,
        Vec<Option<AssocBindingPayload>>,
    ),
>;

/// Builder for [`AssocBindings`].
type AssocBindingsBuilder =
    IndexMap<syn::Ident, (AssocBindingPayload, Vec<Option<AssocBindingPayload>>)>;

/// All generalized trait bounded types of the impl group with their associated bindings.
#[derive(Debug, Clone)]
struct TraitBounds(IndexMap<TraitBoundIdent, (Vec<TraitBoundIdent>, AssocBindings)>);

/// Builder for [`TraitBoundGroup`]
#[derive(Debug, Clone)]
struct TraitBoundsBuilder(IndexMap<TraitBoundIdent, (Vec<TraitBoundIdent>, AssocBindingsBuilder)>);

/// A this wrapper around [`syn::PredicateType::bounded_ty`](syn::PredicateType::bounded_ty).
/// Note, however, that generic type parameters are also considered to be bounded types
///
/// # Example
///
/// ```ignore
/// impl<T: Dispatch<Group = GroupA>> Kita for T {}
/// impl<T> Kita for T where T: Dispatch<Group = GroupB> {}
/// ```
///
/// both have `T` as a bounded type
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
struct Bounded(syn::Type);

/// A thin wrapper around [`syn::TraitBound`](syn::TraitBound) but with associated bindings removed
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
struct TraitBound(syn::TraitBound);

/// Unique id of an impl group, i.e. ([`ItemImpl::trait_`], [`ItemImpl::self_ty`]).
/// All [`ItemImpl`]s that have matching group ids are handled by one main trait impl.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ImplGroupId {
    trait_: Option<syn::Path>,
    self_ty: syn::Type,
}

#[derive(Debug, Clone, Default)]
struct ImplItems {
    /// Fns that are contained in this group
    fns: IndexMap<syn::Ident, syn::Signature>,
    /// Associated types that are contained in this group
    assoc_types: IndexSet<syn::Ident>,
    /// Associated constants that are contained in this group
    assoc_consts: IndexMap<syn::Ident, syn::Type>,
}

/// [`syn::ItemImpl`] descriptor
#[derive(Debug)]
struct ItemImplDesc {
    /// Id of this impl
    id: ImplGroupId,
    /// List of all type and const parameters of this impl
    params: Params,

    /// List of all trait bounds of this impl (optionally with associated bindings)
    trait_bounds: IndexMap<TraitBoundIdent, IndexMap<syn::Ident, AssocBindingPayload>>,

    /// Items of this impl
    items: ImplItems,
}

/// Builder for [`ImplGroup`]
#[derive(Debug, Clone)]
struct ImplGroupBuilder {
    /// Id of this impl group
    id: ImplGroupId,

    /// List of lifetime parameters common to this impl
    lifetimes: Vec<syn::LifetimeParam>,
    /// List of type and const parameters common to this impl
    params: Params,

    /// List of all trait bounds common to this impl (optionally with associated bindings)
    trait_bounds: TraitBoundsBuilder,

    /// Generalized items of the impl group if the impls are inherent, `None` otherwise
    items: Option<ImplItems>,
}

/// Collection of [`ItemImpl`]s grouped by [`ImplGroupId`] and dispatched on associated bindings
struct ImplGroup {
    /// Id of this impl group
    id: ImplGroupId,

    /// Generic parameters used in the impl group id or the bounded type of associated type binding
    params: Vec<syn::GenericParam>,

    /// List of all trait bounds common to this group (optionally with associated bindings)
    ///
    /// Contains associated bindings impls of this group are dispatched on (i.e. differentiated by)
    trait_bounds: TraitBounds,

    /// Generalized items of this group if the impls are inherent, `None` otherwise
    items: Option<ImplItems>,

    /// All [impl blocks](syn::ItemImpl) that are part of this group (in the order of appearance)
    impls: Vec<(Vec<syn::GenericArgument>, syn::ItemImpl)>,
}

/// Body of the [`disjoint_impls`] macro
struct ImplGroups {
    /// Definition of the main trait. [`None`] for inherent impls
    item_trait_: Option<ItemTrait>,
    /// Collection of [`ItemImpl`] blocks grouped by [`ImplGroupId`]
    /// Each impl group is dispatched on a set of associated bounds
    impl_groups: Vec<ImplGroup>,
}

struct ItemImplDescVisitor {
    /// Bounded type currently being visited
    curr_bounded_ty: Option<Bounded>,
    /// Trait bound currently being visited
    curr_trait_bound: Option<TraitBound>,

    /// Bounds of an impl block
    impl_desc: ItemImplDesc,
}

impl ImplGroupId {
    fn is_inherent(&self) -> bool {
        self.trait_.is_none()
    }
}

impl From<&syn::TypeParam> for Bounded {
    fn from(source: &syn::TypeParam) -> Self {
        let ident = &source.ident;
        Self(parse_quote!(#ident))
    }
}

impl From<syn::Type> for Bounded {
    fn from(source: syn::Type) -> Self {
        Self(source.clone())
    }
}

impl From<syn::TraitBound> for TraitBound {
    fn from(mut source: syn::TraitBound) -> Self {
        source.lifetimes = Default::default();

        source
            .path
            .segments
            .last_mut()
            .into_iter()
            .for_each(|segment| {
                if let syn::PathArguments::AngleBracketed(bracketed) = &mut segment.arguments {
                    bracketed.args = core::mem::take(&mut bracketed.args)
                        .into_iter()
                        .filter(|arg| {
                            matches!(
                                arg,
                                syn::GenericArgument::Lifetime(_)
                                    | syn::GenericArgument::Type(_)
                                    | syn::GenericArgument::Const(_)
                            )
                        })
                        .collect();

                    if bracketed.args.is_empty() {
                        segment.arguments = syn::PathArguments::None;
                    }
                }
            });

        Self(source)
    }
}

impl syn::parse::Parse for Bounded {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self::from(input.parse::<syn::Type>()?))
    }
}

impl syn::parse::Parse for TraitBound {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self::from(input.parse::<syn::TraitBound>()?))
    }
}

impl quote::ToTokens for Bounded {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        self.0.to_tokens(tokens);
    }
}

impl quote::ToTokens for TraitBound {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        self.0.to_tokens(tokens);
    }
}

impl quote::ToTokens for ImplGroupId {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        quote!(impl).to_tokens(tokens);

        if let Some(trait_) = &self.trait_ {
            trait_.to_tokens(tokens);
            quote!(for).to_tokens(tokens);
        }

        self.self_ty.to_tokens(tokens);
    }
}

impl TraitBoundsBuilder {
    /// Try to remove all unconstrained params because Rust's trait solver can overflow
    /// when exploring valid implementations that have too many degrees of freedom.
    ///
    /// Prefer `impl<T> Kita for T where Self: Kita0<<T as Dispatch>::Group>, T: Dispatch {}`
    /// instead of: `impl<T, U> Kita for T where Self: Kita0<<U>, T: Dispatch<Group = U> {}`
    fn build(self, impl_group_id: &ImplGroupId, params: &mut Params) -> TraitBounds {
        struct ParamsPartitioner<'a>(IndexSet<&'a syn::Ident>, IndexSet<&'a syn::Ident>);
        struct UnboundedParamFinder<'a>(IndexSet<&'a syn::Ident>, IndexSet<&'a syn::Ident>);

        struct RequiredPayloadDetector<'a> {
            unbounded_params: &'a mut IndexSet<syn::Ident>,
            required_params: IndexSet<&'a syn::Ident>,
            all_params: &'a IndexSet<syn::Ident>,

            is_required: bool,
        }

        impl<'a> Visit<'a> for ParamsPartitioner<'a> {
            fn visit_path(&mut self, node: &'a syn::Path) {
                let first_seg = node.segments.first().unwrap();

                if self.0.swap_remove(&first_seg.ident) {
                    self.1.insert(&first_seg.ident);
                } else {
                    syn::visit::visit_path(self, node);
                }
            }
        }

        impl<'a> Visit<'a> for UnboundedParamFinder<'a> {
            fn visit_path(&mut self, node: &'a syn::Path) {
                let first_seg = node.segments.first().unwrap();

                if self.0.contains(&first_seg.ident) {
                    self.1.insert(&first_seg.ident);
                } else {
                    syn::visit::visit_path(self, node);
                }
            }
        }

        impl<'a> Visit<'a> for RequiredPayloadDetector<'a> {
            fn visit_path(&mut self, node: &'a syn::Path) {
                let first_seg = node.segments.first().unwrap();

                let ident = &first_seg.ident;
                if self.unbounded_params.swap_remove(ident) {
                    self.is_required = true;
                } else if self.all_params.contains(ident) {
                    self.required_params.insert(ident);
                } else {
                    syn::visit::visit_path(self, node);
                }
            }
        }

        let mut params_partitioner = ParamsPartitioner(
            params.iter().map(|(ident, _)| ident).collect(),
            IndexSet::new(),
        );

        params_partitioner.visit_type(&impl_group_id.self_ty);
        if let Some(trait_) = &impl_group_id.trait_ {
            params_partitioner.visit_path(trait_);
        }

        let (maybe_redundant_params, mut unbounded_params) = if impl_group_id.is_inherent() {
            let unbounded_params = params_partitioner.0.into_iter().cloned().collect();

            (IndexSet::new(), unbounded_params)
        } else {
            let mut unbounded_param_finder =
                UnboundedParamFinder(params_partitioner.0, IndexSet::new());
            for (bounded, trait_bound) in self.0.keys() {
                unbounded_param_finder.visit_type(&bounded.0);
                unbounded_param_finder.visit_trait_bound(&trait_bound.0);
            }

            let maybe_redundant_params = unbounded_param_finder
                .0
                .into_iter()
                .cloned()
                .collect::<IndexSet<_>>();
            let unbounded_params = unbounded_param_finder
                .1
                .into_iter()
                .cloned()
                .collect::<IndexSet<_>>();

            (maybe_redundant_params, unbounded_params)
        };

        let mut required_params = params_partitioner
            .1
            .into_iter()
            .cloned()
            .chain(unbounded_params.clone())
            .collect::<IndexSet<_>>();

        let trait_bounds = self
            .0
            .into_iter()
            .map(|(bound_id, (orig_idents, bindings))| {
                let bindings = bindings
                    .into_iter()
                    .map(|(ident, (payload, payloads))| {
                        let mut required_payload_detector = RequiredPayloadDetector {
                            unbounded_params: &mut unbounded_params,
                            all_params: &maybe_redundant_params,
                            required_params: IndexSet::new(),

                            is_required: false,
                        };

                        required_payload_detector.visit_type(&payload);
                        let payload = if required_payload_detector.is_required {
                            required_params.extend(
                                required_payload_detector
                                    .required_params
                                    .into_iter()
                                    .cloned(),
                            );

                            Some(payload)
                        } else {
                            None
                        };

                        (ident, (payload, payloads))
                    })
                    .collect();

                (bound_id, (orig_idents, bindings))
            })
            .collect();

        *params = core::mem::take(params)
            .into_iter()
            .filter(|(ident, _)| required_params.contains(ident))
            .collect();

        TraitBounds(trait_bounds)
    }
}

impl TraitBounds {
    fn generalized_idents(&self) -> impl Iterator<Item = (&TraitBoundIdent, &syn::Ident)> {
        self.0
            .iter()
            .flat_map(|(trait_bound_ident, (_, bindings))| {
                bindings
                    .iter()
                    .map(move |(ident, _)| (trait_bound_ident, ident))
            })
    }

    fn payloads(&self) -> impl Iterator<Item = syn::Type> {
        self.0
            .iter()
            .flat_map(|((bounded, trait_bound), (_, bindings))| {
                bindings.iter().map(move |(ident, (payload, _))| {
                    payload.clone().unwrap_or_else(|| {
                        parse_quote! { <#bounded as #trait_bound>::#ident }
                    })
                })
            })
    }
}

impl ImplGroupBuilder {
    fn new(impl_desc: &ItemImplDesc) -> Self {
        let mut subs = Default::default();

        let generalized_id = Generalize::generalize(
            &impl_desc.id,
            &impl_desc.id,
            &impl_desc.params,
            &impl_desc.params,
            &mut subs,
        )
        .unwrap();

        for (assoc_ty1, assoc_ty2) in impl_desc
            .trait_bounds
            .values()
            .zip_eq(impl_desc.trait_bounds.values())
            .flat_map(|(group_bound, other_bound)| {
                group_bound.values().zip_eq(other_bound.values())
            })
        {
            let _ = assoc_ty1
                .generalize(assoc_ty2, &impl_desc.params, &impl_desc.params, &mut subs)
                .unwrap();
        }

        let group_bounds = impl_desc
            .trait_bounds
            .iter()
            .zip_eq(&impl_desc.trait_bounds)
            .map(
                |((group_bound_id, group_bound), (other_bound_id, other_bound))| {
                    let group_bound_id = group_bound_id
                        .generalize(
                            other_bound_id,
                            &impl_desc.params,
                            &impl_desc.params,
                            &mut subs,
                        )
                        .unwrap();

                    let group_bound = group_bound
                        .iter()
                        .zip_eq(other_bound.iter())
                        .map(|((ident, group_payload), (_, other_payload))| {
                            let group_payload = group_payload
                                .generalize(
                                    other_payload,
                                    &impl_desc.params,
                                    &impl_desc.params,
                                    &mut subs,
                                )
                                .unwrap();

                            (
                                ident.clone(),
                                (group_payload, vec![Some(other_payload.clone())]),
                            )
                        })
                        .collect();

                    (group_bound_id, (vec![other_bound_id.clone()], group_bound))
                },
            )
            .collect();

        let impl_items = impl_desc.id.is_inherent().then(|| {
            impl_desc
                .items
                .generalize(
                    &impl_desc.items,
                    &impl_desc.params,
                    &impl_desc.params,
                    &subs,
                )
                .unwrap()
        });

        let (lifetimes, params) = subs.build_params();

        Self {
            id: generalized_id,
            lifetimes,
            params,
            trait_bounds: TraitBoundsBuilder(group_bounds),
            items: impl_items,
        }
    }

    fn find_valid_trait_bound_groups<'a>(
        &'a self,
        other: &'a ItemImplDesc,
        id_substitutions: &Generalizations<'a>,
        implicit_params_container: &'a mut Vec<Vec<syn::Type>>,
    ) -> Vec<(TraitBoundsBuilder, Generalizations<'a>)> {
        let group_bounds: Vec<_> = (0..self.trait_bounds.0.len()).collect();
        let other_bounds: Vec<_> = (0..other.trait_bounds.len()).collect();

        let mut results = Vec::new();
        let mut stack = Vec::new();

        // TODO: Use stack or queue? eliminate redundant results
        stack.push((group_bounds, other_bounds, Vec::new()));
        while let Some((mut rem_group_bounds, rem_other_bounds, acc)) = stack.pop() {
            let Some(group_bound_idx) = rem_group_bounds.pop() else {
                if acc
                    .iter()
                    .map(|(_, _, bindings, _, _)| bindings)
                    .any(|bindings: &IndexMap<_, _>| !bindings.is_empty())
                {
                    results.push(acc);
                }

                continue;
            };

            let (group_bound_id, (rows, group_bound)) =
                self.trait_bounds.0.get_index(group_bound_idx).unwrap();

            let nrows = rows.len();
            let mut substack = Vec::new();
            for j in 0..rem_other_bounds.len() {
                let mut rem_other_bounds = rem_other_bounds.clone();
                let other_bound_idx = rem_other_bounds.swap_remove(j);

                let (other_bound_id, other_bound) =
                    other.trait_bounds.get_index(other_bound_idx).unwrap();

                let mut bound_subs = id_substitutions.clone();
                let mut all_payload_subs = id_substitutions.clone();

                let mut new_group_bound = group_bound.clone();
                let Some(_) = group_bound_id.generalize(
                    other_bound_id,
                    &self.params,
                    &other.params,
                    &mut bound_subs,
                ) else {
                    continue;
                };

                for (ident, payload) in other_bound
                    .iter()
                    .filter(|(ident, _)| !group_bound.contains_key(*ident))
                {
                    let payloads = core::iter::repeat_n(None, nrows);
                    new_group_bound.insert(ident.clone(), (payload.clone(), payloads.collect()));
                }

                let new_group_bound = new_group_bound
                    .into_iter()
                    .filter_map(|(ident, (_, payloads))| {
                        let generalized_payload = group_bound
                            .get(&ident)
                            .map(|(generalized_payload, _)| generalized_payload)
                            .or_else(|| other_bound.get(&ident))
                            .unwrap();

                        let payload = if let Some(payload) = other_bound.get(&ident) {
                            let mut curr_payload_subs = all_payload_subs.clone();

                            let _ = generalized_payload.generalize(
                                payload,
                                &self.params,
                                &other.params,
                                &mut curr_payload_subs,
                            )?;

                            all_payload_subs = curr_payload_subs;
                            Some(payload)
                        } else {
                            None
                        };

                        let mut payloads = payloads.clone();
                        payloads.push(payload.cloned());

                        Some((ident, (generalized_payload, payload, payloads)))
                    })
                    .collect::<IndexMap<_, _>>();

                let mut acc = acc.clone();
                let bound_subs_diff = bound_subs.difference(id_substitutions);
                let payload_subs_diff = all_payload_subs.difference(id_substitutions);

                acc.push((
                    group_bound_id,
                    other_bound_id,
                    new_group_bound,
                    bound_subs_diff,
                    payload_subs_diff,
                ));

                substack.push((rem_group_bounds.clone(), rem_other_bounds, acc));
            }

            stack.push((rem_group_bounds, rem_other_bounds, acc));
            stack.extend(substack);
        }

        // NOTE: Only pick out trait bounds that, when generalized, don't introduce a new type
        // unbounded parameter that is not found in any of the associated binding payloads
        let results = results
            .into_iter()
            .filter_map(|mut remaining| {
                let mut subs = id_substitutions.clone();
                let mut result = Vec::new();

                loop {
                    let ready: Vec<_> = remaining
                        .iter()
                        .enumerate()
                        .filter_map(|(i, (_, _, _, bound_subs, _))| {
                            bound_subs.difference(&subs).is_empty().then_some(i)
                        })
                        .collect();

                    if ready.is_empty() {
                        break;
                    }

                    for idx in ready.into_iter().rev() {
                        let (group_bound_id, other_bound_id, bound, _, payload_subs) =
                            remaining.swap_remove(idx);

                        result.push((group_bound_id, other_bound_id, bound));
                        subs = subs.unify(payload_subs);
                    }
                }

                (!result.is_empty()).then_some((result, subs))
            })
            .collect::<Vec<_>>();

        for (result, _) in &results {
            implicit_params_container.push(
                result
                    .iter()
                    .flat_map(|(_, _, bound)| {
                        bound
                            .values()
                            .filter(|(_, other_payload, _)| other_payload.is_none())
                    })
                    .enumerate()
                    .map(|(i, _)| {
                        let ident = format_ident!("_MŠČ{}", i);
                        parse_quote!(#ident)
                    })
                    .collect::<Vec<_>>(),
            );
        }

        results
            .into_iter()
            .enumerate()
            .map(|(i, (result, subs))| {
                // NOTE: After finding out which substitutions are valid,
                // the generalization is done once again with results kept
                let mut subs = id_substitutions.clone().unify(subs);

                let mut implicit_params = implicit_params_container[i].iter();
                let mut trait_bounds = IndexMap::new();
                for (group_bound_id, other_bound_id, bound) in result {
                    let generalized_bound_id = group_bound_id
                        .generalize(other_bound_id, &self.params, &other.params, &mut subs)
                        .unwrap();

                    let mut generalized_bound = IndexMap::new();
                    for (ident, (generalized_payload, other_payload, payloads)) in bound {
                        let other_payload =
                            other_payload.unwrap_or_else(|| implicit_params.next().unwrap());

                        let generalized_payload = generalized_payload
                            .generalize(other_payload, &self.params, &other.params, &mut subs)
                            .unwrap();

                        generalized_bound.insert(ident, (generalized_payload, payloads));
                    }

                    let mut bounds = self.trait_bounds.0[group_bound_id].0.clone();
                    bounds.push(other_bound_id.clone());
                    trait_bounds.insert(generalized_bound_id, (bounds, generalized_bound));
                }

                (TraitBoundsBuilder(trait_bounds), subs)
            })
            .collect()
    }

    fn intersection<'a>(
        &self,
        other_idx: usize,
        other: &'a ItemImplDesc,
        mut impls: IndexMap<usize, &'a ItemImplDesc>,
    ) -> Vec<Self> {
        let mut implicit_params_container = vec![];
        let mut id_subs = Default::default();

        let Some(generalized_id) = Generalize::generalize(
            &self.id,
            &other.id,
            &self.params,
            &other.params,
            &mut id_subs,
        ) else {
            return vec![];
        };

        self.find_valid_trait_bound_groups(other, &id_subs, &mut implicit_params_container)
            .into_iter()
            .filter_map(|(group, subs)| {
                let nrows = group.0[0].0.len();

                let mut payload_rows = core::iter::repeat_n(Vec::new(), nrows).collect::<Vec<_>>();
                group.0.iter().for_each(|(_, (_, bindings))| {
                    for (_, payload_cols) in bindings.values() {
                        for (i, col) in payload_cols.iter().enumerate() {
                            payload_rows[i].push(col);
                        }
                    }
                });

                impls.insert(other_idx, other);
                for (i, p1) in payload_rows.iter().enumerate() {
                    for (j, p2) in payload_rows.iter().enumerate().skip(i + 1) {
                        let mut subs = Default::default();
                        if i == j {
                            continue;
                        }

                        let impl_id1 = &impls[i].id;
                        let impl_id2 = &impls[j].id;

                        let params1 = &impls[i].params;
                        let params2 = &impls[j].params;

                        if impl_id1
                            .generalize(impl_id2, params1, params2, &mut subs)
                            .is_none()
                            || subs.is_disjoint(params1, params2)
                        {
                            continue;
                        }

                        if p1.iter().zip_eq(p2).all(|(&p1, &p2)| {
                            p1.generalize(p2, params1, params2, &mut subs).is_none()
                                || !subs.is_disjoint(params1, params2)
                        }) {
                            return None;
                        }
                    }
                }

                impls.pop();
                let impl_items = if self.id.is_inherent() {
                    Some(self.items.as_ref().unwrap().generalize(
                        &other.items,
                        &self.params,
                        &other.params,
                        &subs,
                    )?)
                } else {
                    None
                };

                let (lifetimes, params) = subs.build_params();

                Some(Self {
                    id: generalized_id.clone(),
                    lifetimes,
                    params,
                    trait_bounds: group,
                    items: impl_items,
                })
            })
            .collect()
    }
}

impl ImplGroups {
    fn new(item_trait_: Option<ItemTrait>, impl_groups: Vec<ImplGroup>) -> Self {
        if let Some(trait_) = &item_trait_ {
            for ImplGroup { impls, .. } in &impl_groups {
                validate::validate_trait_impls(trait_, impls.iter().map(|(_, impl_)| impl_));
            }
        } else {
            for ImplGroup { impls, .. } in &impl_groups {
                validate::validate_inherent_impls(impls.iter().map(|(_, impl_)| impl_));
            }
        }

        Self {
            item_trait_,
            impl_groups,
        }
    }
}

impl ItemImplDescVisitor {
    fn find(item_impl: &ItemImpl) -> ItemImplDesc {
        let trait_ = item_impl.trait_.as_ref().map(|(_, trait_, _)| trait_);

        let mut visitor =
            Self {
                curr_bounded_ty: None,
                curr_trait_bound: None,

                impl_desc: ItemImplDesc {
                    id: ImplGroupId {
                        trait_: trait_.cloned(),
                        self_ty: (*item_impl.self_ty).clone(),
                    },
                    params: item_impl
                        .generics
                        .type_params()
                        .map(|param| {
                            let ident = param.ident.clone();

                            let sizedness = if param.bounds.iter().any(|bound| {
                                matches!(
                                    bound,
                                    syn::TypeParamBound::Trait(syn::TraitBound {
                                        modifier: syn::TraitBoundModifier::Maybe(_),
                                        ..
                                    })
                                )
                            }) {
                                Sizedness::Unsized
                            } else {
                                Sizedness::Sized
                            };

                            (ident, GenericParam::Type(sizedness, IndexSet::new()))
                        })
                        .chain(item_impl.generics.const_params().map(|param| {
                            (param.ident.clone(), GenericParam::Const(param.ty.clone()))
                        }))
                        .collect(),
                    trait_bounds: IndexMap::new(),
                    items: ImplItems {
                        fns: item_impl
                            .items
                            .iter()
                            .filter_map(|item| match item {
                                syn::ImplItem::Fn(item) => {
                                    Some((item.sig.ident.clone(), item.sig.clone()))
                                }
                                _ => None,
                            })
                            .collect(),
                        assoc_types: item_impl
                            .items
                            .iter()
                            .filter_map(|item| match item {
                                syn::ImplItem::Type(item) => Some(item.ident.clone()),
                                _ => None,
                            })
                            .collect(),
                        assoc_consts: item_impl
                            .items
                            .iter()
                            .filter_map(|item| match item {
                                syn::ImplItem::Const(item) => {
                                    Some((item.ident.clone(), item.ty.clone()))
                                }
                                _ => None,
                            })
                            .collect(),
                    },
                },
            };
        visitor.visit_generics(&item_impl.generics);
        visitor.impl_desc
    }
}

impl Visit<'_> for ItemImplDescVisitor {
    fn visit_item_impl(&mut self, node: &ItemImpl) {
        self.visit_generics(&node.generics);
    }

    fn visit_constraint(&mut self, node: &syn::Constraint) {
        let curr_bounded_ty = self.curr_bounded_ty.take().unwrap();
        let curr_trait_bound = self.curr_trait_bound.take().unwrap();

        let ident = &node.ident;
        let generics = &node.generics;

        self.curr_bounded_ty = Some(parse_quote! {
            <#curr_bounded_ty as #curr_trait_bound>::#ident #generics
        });

        for bound in &node.bounds {
            syn::visit::visit_type_param_bound(self, bound);
        }

        self.curr_bounded_ty = Some(curr_bounded_ty);
        self.curr_trait_bound = Some(curr_trait_bound);
    }

    fn visit_type_param(&mut self, node: &syn::TypeParam) {
        self.curr_bounded_ty = Some(node.into());
        syn::visit::visit_type_param(self, node);
        self.curr_bounded_ty = None;
    }

    fn visit_predicate_type(&mut self, node: &syn::PredicateType) {
        self.curr_bounded_ty = Some(node.bounded_ty.clone().into());
        syn::visit::visit_predicate_type(self, node);
        self.curr_bounded_ty = None;
    }

    fn visit_trait_bound(&mut self, node: &syn::TraitBound) {
        self.curr_trait_bound = Some(node.clone().into());

        let trait_bound_ident = (
            self.curr_bounded_ty.clone().unwrap(),
            self.curr_trait_bound.clone().unwrap(),
        );

        self.impl_desc
            .trait_bounds
            .entry(trait_bound_ident)
            .or_default();

        visit_trait_bound(self, node);
        self.curr_trait_bound = None;
    }

    fn visit_assoc_type(&mut self, node: &syn::AssocType) {
        let trait_bound_ident = (
            // FIXME: clone seems unnecessary?
            self.curr_bounded_ty.clone().unwrap(),
            self.curr_trait_bound.clone().unwrap(),
        );

        self.impl_desc.trait_bounds[&trait_bound_ident].insert(node.ident.clone(), node.ty.clone());
    }
}

/// Enables writing non-overlapping (*disjoint*) impls distinguished by a set of associated types.
///
/// # Trait implementations
///
/// ```
/// use disjoint_impls::disjoint_impls;
///
/// pub trait Dispatch {
///     type Group;
/// }
///
/// pub enum GroupA {}
/// pub enum GroupB {}
///
/// impl Dispatch for u32 {
///     type Group = GroupA;
/// }
/// impl Dispatch for i32 {
///     type Group = GroupB;
/// }
///
/// impl Dispatch for Option<u32> {
///     type Group = GroupA;
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
///     impl<T, U> Kita for (T, U)
///     where
///         T: Dispatch<Group = GroupA>,
///         U: Dispatch<Group = GroupA>,
///     {
///         const NAME: &'static str = "Blanket AA";
///     }
///
///     impl<T, U> Kita for (T, U)
///     where
///         T: Dispatch<Group = GroupA>,
///         U: Dispatch<Group = GroupB>,
///     {
///         const NAME: &'static str = "Blanket AB";
///     }
///
///     impl<T, U> Kita for (T, U)
///     where
///         T: Dispatch<Group = GroupB>,
///         U: Dispatch,
///     {
///         const NAME: &'static str = "Blanket B*";
///     }
///
///     impl<T> Kita for T
///     where
///         Option<T>: Dispatch<Group = GroupA>,
///     {
///         const NAME: &'static str = "Option blanket";
///
///         fn name() -> &'static str {
///             <Self as Kita>::NAME
///         }
///     }
/// }
///
/// fn main() {
///     assert_eq!("Blanket AA", <(u32, u32)>::NAME);
///     assert_eq!("Blanket AB", <(u32, i32)>::NAME);
///     assert_eq!("Blanket B*", <(i32, u32)>::NAME);
///
///     assert_eq!("Option blanket", u32::name());
/// }
/// ```
///
/// # Inherent implementations
///
/// ```
/// use disjoint_impls::disjoint_impls;
///
/// pub trait Dispatch {
///     type Group;
/// }
///
/// impl Dispatch for u32 {
///     type Group = Self;
/// }
/// impl Dispatch for i32 {
///     type Group = Self;
/// }
///
/// struct Wrapper<T>(T);
///
/// disjoint_impls! {
///     impl<T: Dispatch<Group = U>, U: Dispatch<Group = u32>> Wrapper<T> {
///         const NAME: &'static str = "Blanket A";
///     }
///     impl<T: Dispatch<Group = U>, U: Dispatch<Group = i32>> Wrapper<T> {
///         const NAME: &'static str = "Blanket B";
///     }
/// }
///
/// fn main() {
///     assert_eq!("Blanket A", Wrapper::<u32>::NAME);
///     assert_eq!("Blanket B", Wrapper::<i32>::NAME);
/// }
/// ```
///
/// # Foreign(remote) traits
///
/// For traits defined outside the current crate (a.k.a. foreign or remote traits), duplicate
/// the trait definition inside the macro and annotate it with `#[disjoint_impls(remote)]`.
///
/// ```
/// use disjoint_impls::disjoint_impls;
/// // A foreign trait must be brought into scope so
/// // the `disjoint_impls!` macro can refer to it.
/// use remote_trait::ForeignKita;
///
/// pub trait Dispatch {
///     type Group;
/// }
///
/// pub enum GroupA {}
/// pub enum GroupB {}
///
/// impl Dispatch for u32 {
///     type Group = GroupA;
/// }
/// impl Dispatch for i32 {
///     type Group = GroupB;
/// }
///
/// // (orphan rule): You can define blanket impls only
/// // for types that are defined in the current crate
/// pub struct LocalType<T>(T);
///
/// disjoint_impls! {
///     // Trait annotated with `#[disjoint_impls(remote)]` must be an exact duplicate of
///     // the foreign/remote trait it refers to (default values and fn bodies excluded)
///     #[disjoint_impls(remote)]
///     pub trait ForeignKita<U> {
///         fn kita() -> &'static str;
///     }
///
///     impl<T: Dispatch<Group = GroupA>> ForeignKita<T> for LocalType<T> {
///         fn kita() -> &'static str {
///             "Blanket A"
///         }
///     }
///     impl<T: Dispatch<Group = GroupB>> ForeignKita<T> for LocalType<T> {
///         fn kita() -> &'static str {
///             "Blanket B"
///         }
///     }
/// }
///
/// fn main() {
///     assert_eq!("Blanket A", LocalType::<u32>::kita());
///     assert_eq!("Blanket B", LocalType::<i32>::kita());
/// }
/// ```
///
/// Other, much more complex examples, can be found in tests.
#[proc_macro]
#[proc_macro_error]
pub fn disjoint_impls(input: TokenStream) -> TokenStream {
    let impls: ImplGroups = parse_macro_input!(input);

    let mut helper_traits = Vec::new();
    let mut main_trait_impls = Vec::new();
    let mut item_impls = Vec::new();

    let main_trait = impls.item_trait_;
    for (impl_group_idx, mut impl_group) in impls.impl_groups.into_iter().enumerate() {
        if impl_group.impls.len() > 1 {
            helper_traits.push(helper_trait::generate(
                main_trait.as_ref(),
                impl_group_idx,
                &impl_group,
            ));

            if let Some(main_trait_impl) =
                main_trait::generate(main_trait.as_ref(), impl_group_idx, &impl_group)
            {
                main_trait_impls.push(main_trait_impl);
            }

            item_impls.extend(disjoint::generate(impl_group_idx, impl_group));
        } else if let Some((_, main_trait_impl)) = impl_group.impls.pop() {
            main_trait_impls.push(main_trait_impl);
        }
    }

    let main_trait = main_trait.filter(|main_trait_| !main_trait_.attrs.iter().any(is_remote));

    quote! {
        #main_trait

        #[allow(clippy::needless_lifetimes)]
        const _: () = {
            #( #helper_traits )*
            #( #item_impls )*
            #( #main_trait_impls )*

        };
    }
    .into()
}

struct Dsu {
    parent: Vec<usize>,
}

impl Dsu {
    fn new(n: usize) -> Self {
        Self {
            parent: (0..n).collect(),
        }
    }

    fn find(&mut self, x: usize) -> usize {
        if self.parent[x] != x {
            let root = self.find(self.parent[x]);
            self.parent[x] = root;
        }

        self.parent[x]
    }

    fn union(&mut self, a: usize, b: usize) {
        let ra = self.find(a);
        let rb = self.find(b);

        if ra != rb {
            self.parent[rb] = ra;
        }
    }

    fn groups(mut self) -> Vec<Vec<usize>> {
        let mut map: IndexMap<_, Vec<_>> = IndexMap::new();

        for i in 0..self.parent.len() {
            let root = self.find(i);
            map.entry(root).or_default().push(i);
        }

        map.into_values().collect()
    }
}

impl Parse for ImplGroups {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        let main_trait = input.parse::<ItemTrait>().ok();

        let mut impls = Vec::new();
        while let Ok(item) = input.parse::<ItemImpl>() {
            validate_impl_syntax(&item);

            let item = normalize::normalize(item);
            let param_bounds = ItemImplDescVisitor::find(&item);
            impls.push((param_bounds, item));
        }

        let mut dsu = Dsu::new(impls.len());
        for i in 0..impls.len() {
            for j in (i + 1)..impls.len() {
                let (desc_i, _) = &impls[i];
                let (desc_j, _) = &impls[j];
                let mut subs = Default::default();

                if Generalize::generalize(
                    &desc_i.id,
                    &desc_j.id,
                    &desc_i.params,
                    &desc_j.params,
                    &mut subs,
                )
                .is_some()
                    && !subs.is_disjoint(&desc_i.params, &desc_j.params)
                {
                    dsu.union(i, j);
                }
            }
        }

        let impl_groups = dsu
            .groups()
            .iter()
            .flat_map(|subset| partition_impl_groups(subset, &impls))
            .collect::<Vec<_>>();

        let args = impl_groups
            .iter()
            .map(|(impl_group_builder, impl_group)| {
                let impls = impl_group.iter().map(|&i| &impls[i]).collect::<Vec<_>>();

                if impl_group_builder.id.is_inherent() && impl_group.len() > 1 {
                    let nrows = impl_group_builder.trait_bounds.0[0].0.len();

                    let payloads = impl_group_builder
                        .trait_bounds
                        .0
                        .values()
                        .flat_map(|(_, bindings)| bindings.values())
                        .collect::<Vec<_>>();

                    let mut args = vec![];
                    for i in 0..nrows {
                        let mut subs = Default::default();
                        let (impl_desc, _) = &impls[i];

                        if Generalize::generalize(
                            &impl_group_builder.id,
                            &impl_desc.id,
                            &impl_group_builder.params,
                            &impl_desc.params,
                            &mut subs,
                        )
                        .is_none()
                        {
                            continue;
                        }

                        payloads.iter().for_each(|(generalized_payload, payloads)| {
                            if let Some(payload) = &payloads[i] {
                                let _ = generalized_payload
                                    .generalize(
                                        payload,
                                        &impl_group_builder.params,
                                        &impls[i].0.params,
                                        &mut subs,
                                    )
                                    .unwrap();
                            }
                        });

                        args.push(subs.generic_args().map(|(_, arg)| arg).collect::<Vec<_>>());
                    }

                    args
                } else {
                    Vec::new()
                }
            })
            .collect::<Vec<_>>();

        let mut impls = impls
            .into_iter()
            .map(|(_, impl_)| Some(impl_))
            .collect::<Vec<_>>();

        let impl_groups = impl_groups
            .into_iter()
            .zip_eq(args)
            .map(|((mut impl_group_builder, impl_group), args)| {
                let impls = impl_group
                    .iter()
                    .map(|&i| impls[i].take().unwrap())
                    .collect::<Vec<_>>();

                let impl_group_id = impl_group_builder.id;
                let impls = if impl_group_id.is_inherent() && impl_group.len() > 1 {
                    args.into_iter().zip_eq(impls).collect()
                } else {
                    impls.into_iter().map(|impl_| (vec![], impl_)).collect()
                };

                let assoc_bindings = impl_group_builder
                    .trait_bounds
                    .build(&impl_group_id, &mut impl_group_builder.params);

                let params = impl_group_builder
                    .lifetimes
                    .into_iter()
                    .map(syn::GenericParam::Lifetime)
                    .chain(as_generics(&impl_group_builder.params))
                    .collect();

                ImplGroup {
                    id: impl_group_id,
                    params,

                    trait_bounds: assoc_bindings,
                    items: impl_group_builder.items,

                    impls,
                }
            })
            .collect();

        Ok(Self::new(main_trait, impl_groups))
    }
}

/// Further partitions the given subset into smaller, non-overlapping groups of impls.
///
/// The input `subset` comes from an initial partition where any two impls in **different** subsets
/// are guaranteed by the Rust compiler to be non-overlapping. However, impls within the **same**
/// subset may still potentially overlap. This function refines those subsets by detecting and
/// separating impls that are found not to overlap.
fn partition_impl_groups(
    subset: &[usize],
    item_impls: &[(ItemImplDesc, ItemImpl)],
) -> Vec<(ImplGroupBuilder, Vec<usize>)> {
    partition_impl_groups_rec(subset, item_impls, &mut Vec::new())
        // FIXME: I don't think it does
        .expect("At least one solution must always exist")
}

fn partition_impl_groups_rec(
    subset: &[usize],
    item_impls: &[(ItemImplDesc, ItemImpl)],
    impl_groups: &mut Vec<(ImplGroupBuilder, Vec<usize>)>,
) -> Option<Vec<(ImplGroupBuilder, Vec<usize>)>> {
    if subset.is_empty() {
        return Some(impl_groups.to_vec());
    }

    let curr_impl_idx = subset[0];
    let mut min = None::<Vec<_>>;

    let curr_impl = &item_impls[curr_impl_idx];
    'OUT: for impl_group_idx in 0..impl_groups.len() {
        let impl_group = &impl_groups[impl_group_idx];

        let group_impls = impl_group
            .1
            .iter()
            .map(|&i| (i, &item_impls[i].0))
            .collect();

        let intersections = impl_group
            .0
            .intersection(curr_impl_idx, &curr_impl.0, group_impls);

        for intersection in intersections {
            let impl_group = &mut impl_groups[impl_group_idx];

            let prev_assoc_bindings = core::mem::replace(&mut impl_group.0, intersection);
            impl_group.1.push(curr_impl_idx);

            let res = partition_impl_groups_rec(&subset[1..], item_impls, impl_groups);

            match (res, &mut min) {
                (Some(res), Some(min)) if res.len() < min.len() => *min = res,
                (Some(res), None) => min = Some(res),
                _ => {}
            }

            // NOTE: Restore changes to the impl group set for the next iteration
            let impl_group_to_restore = impl_groups.get_mut(impl_group_idx).unwrap();
            impl_group_to_restore.0 = prev_assoc_bindings;
            impl_group_to_restore.1.pop();

            if min.as_ref().is_some_and(|m| m.len() == impl_groups.len()) {
                // NOTE: One of the shortest solutions has been found
                break 'OUT;
            }
        }
    }

    // NOTE: Try create a new group if shorter solution is possible
    if min.as_ref().is_none_or(|m| m.len() > 1 + impl_groups.len())
        && (curr_impl.0.id.is_inherent() || impl_groups.iter().all(|g| g.0.id != curr_impl.0.id))
    {
        impl_groups.push((ImplGroupBuilder::new(&curr_impl.0), vec![curr_impl_idx]));
        let res = partition_impl_groups_rec(&subset[1..], item_impls, impl_groups);

        match (res, &mut min) {
            (Some(res), Some(min)) if res.len() < min.len() => *min = res,
            (Some(res), None) => min = Some(res),
            _ => {}
        }

        // NOTE: Restore changes to impl groups
        impl_groups.pop();
    }

    min
}
