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
use proc_macro_error2::{OptionExt, abort, proc_macro_error};
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::{
    ItemImpl, ItemTrait, Token,
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote,
    punctuated::Punctuated,
    visit::{Visit, visit_trait_bound},
};

use crate::{
    disjoint::traitize_inherent_impl,
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
///
/// All [`ItemImpl`]s that have matching group ids are handled by one main trait impl.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ImplGroupId {
    trait_: Option<syn::Path>,
    self_ty: syn::Type,
}

/// [`syn::ImplItem`]s descriptor
#[derive(Debug, Clone, Default)]
struct ImplItemsDesc {
    /// Fns that are contained in this group
    fns: IndexMap<syn::Ident, syn::Signature>,
    /// Associated types that are contained in this group
    assoc_types: IndexSet<syn::Ident>,
    /// Associated constants that are contained in this group
    assoc_consts: IndexMap<syn::Ident, syn::Type>,
}

/// [`syn::ItemImpl`] descriptor
#[derive(Debug, Clone)]
struct ItemImplDesc {
    /// Id of this impl
    id: ImplGroupId,
    /// Type and const parameters of this impl
    params: Params,
    /// All trait bounds of this impl (optionally with associated bindings)
    trait_bounds: IndexMap<TraitBoundIdent, IndexMap<syn::Ident, AssocBindingPayload>>,
    /// [Items](syn::ImplItem) of this impl if the impl is inherent, [`None`] otherwise
    items: Option<ImplItemsDesc>,
}

/// Builder for [`ImplGroup`]
#[derive(Debug, Clone)]
struct ImplGroupBuilder {
    /// Id of this group
    id: ImplGroupId,
    /// Lifetime parameters common to this group
    lifetimes: Vec<syn::LifetimeParam>,
    /// Type and const parameters common to this group
    params: Params,
    /// All trait bounds common to this impl group (optionally with associated bindings)
    trait_bounds: TraitBoundsBuilder,
    /// Generalized items of the impl group if the impls are inherent, [`None`] otherwise
    items: Option<ImplItemsDesc>,
    /// All impls that overlap on associated bindings this group is dispatched on.
    subgroups: Vec<(Self, Vec<usize>, TraitBoundsBuilder)>,
}

/// Collection of disjoint [`ItemImpl`]s grouped by [`ImplGroupId`] and dispatched on associated bindings
#[derive(Debug, Clone)]
struct ImplGroup {
    /// Id of this group
    id: ImplGroupId,
    /// Type and const parameters common to this group
    params: Punctuated<syn::GenericParam, Token![,]>,
    /// All trait bounds common to this impl group (optionally with associated bindings)
    trait_bounds: TraitBounds,
    /// Generalized items of the impl group if the impls are inherent, [`None`] otherwise
    items: Option<ImplItemsDesc>,

    /// All disjoint [impls](syn::ItemImpl) that are part of this group
    impls: Vec<(syn::ItemImpl, Vec<syn::GenericArgument>)>,

    /// All remaining impls that overlap on associated bindings this group is dispatched on.
    ///
    /// For instance, the following 3 impls will form a group where the last 2 impls
    /// overlap on `Group1 = GroupB` but form a subgroup on `Group2` assoc binding:
    ///
    /// ```ignore
    /// impl<T> Kita for T where T: Dispatch<Group1 = GroupA> {}
    /// impl<T> Kita for T where T: Dispatch<Group1 = GroupB, Group2 = GroupA> {}
    /// impl<T> Kita for T where T: Dispatch<Group1 = GroupB, Group2 = GroupB> {}
    /// ```
    subgroups: Vec<(ImplGroup, Vec<AssocBindingPayload>)>,
}

/// Body of the [`disjoint_impls`] macro
#[derive(Debug, Clone)]
struct ImplGroups {
    /// Definition of the trait current group is implementing. [`None`] for inherent impls
    trait_: Option<ItemTrait>,
    /// Collection of [`ItemImpl`] blocks grouped by [`ImplGroupId`].
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

/// Helper struct for disjoint-set union algorithm
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

impl TraitBoundsBuilder {
    /// Removes impls at positions not in `keep`. Returns removed impls.
    ///
    /// The remove is stable. This means that relative order of impls of both original
    /// and removed impls is preserved. Total order of trait bounds is also preserved.
    fn retain_impl_by_pos(&mut self, keep: &[usize]) -> Self {
        let keep: IndexSet<_> = keep.iter().copied().collect();

        let mut removed = IndexMap::new();
        for (trait_ident, (orig, bindings)) in self.0.iter_mut() {
            let mut removed_bindings = IndexMap::new();

            let (kept_rows, removed_rows) = core::mem::take(orig)
                .into_iter()
                .enumerate()
                .partition::<Vec<_>, _>(|(pos, _)| keep.contains(pos));

            bindings
                .iter_mut()
                .for_each(|(ident, (payload, payloads))| {
                    let (kept_payloads, removed_payloads) = core::mem::take(payloads)
                        .into_iter()
                        .enumerate()
                        .partition::<Vec<_>, _>(|(pos, _)| keep.contains(pos));

                    *payloads = kept_payloads
                        .into_iter()
                        .map(|(_, binding)| binding)
                        .collect();

                    let removed_payloads = removed_payloads
                        .into_iter()
                        .map(|(_, binding)| binding)
                        .collect::<Vec<_>>();

                    if !removed_payloads.is_empty() {
                        removed_bindings.insert(ident.clone(), (payload.clone(), removed_payloads));
                    }
                });

            if !removed_rows.is_empty() {
                removed.insert(
                    trait_ident.clone(),
                    (
                        removed_rows.into_iter().map(|(_, row)| row).collect(),
                        removed_bindings,
                    ),
                );
            }

            *orig = kept_rows.into_iter().map(|(_, row)| row).collect();
        }

        Self(removed)
    }

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
        self.0.iter().flat_map(|(bounded, (_, bindings))| {
            bindings.iter().map(move |(ident, (payload, _))| {
                payload
                    .clone()
                    .unwrap_or_else(|| assoc_binding_default(bounded, ident))
            })
        })
    }
}

impl ImplGroupBuilder {
    fn new(impl_desc: &ItemImplDesc) -> Self {
        let mut subs = Default::default();

        let generalized_id = impl_desc
            .id
            .generalize(
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
                .as_ref()
                .unwrap()
                .generalize(
                    impl_desc.items.as_ref().unwrap(),
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
            subgroups: vec![],
        }
    }

    fn find_valid_trait_bound_groups<'a>(
        &'a self,
        other: &'a ItemImplDesc,
        id_substitutions: &Generalizations<'a>,
        implicit_params_container: &'a mut Vec<syn::Type>,
    ) -> Option<(TraitBoundsBuilder, Generalizations<'a>)> {
        let mut generalized_bounds = Vec::new();

        for (group_bound_id, (rows, group_bound)) in &self.trait_bounds.0 {
            let nrows = rows.len();

            for (other_bound_id, other_bound) in &other.trait_bounds {
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

                let bound_subs_diff = bound_subs.difference(id_substitutions);
                let payload_subs_diff = all_payload_subs.difference(id_substitutions);

                generalized_bounds.push((
                    group_bound_id,
                    other_bound_id,
                    new_group_bound,
                    bound_subs_diff,
                    payload_subs_diff,
                ));
            }
        }

        // NOTE: Only pick out trait bounds that, when generalized, don't introduce a new type
        // unbounded parameter that is not found in any of the associated binding payloads
        let mut subs = id_substitutions.clone();
        let mut result = Vec::new();

        loop {
            let ready: Vec<_> = generalized_bounds
                .iter()
                .enumerate()
                .filter_map(|(i, (_, _, _, bound_subs, _))| {
                    bound_subs.difference(&subs).is_empty().then_some(i)
                })
                .collect();

            if ready.is_empty() {
                if result.is_empty() {
                    return None;
                }

                break;
            }

            for idx in ready.into_iter().rev() {
                let (group_bound_id, other_bound_id, bound, _, payload_subs) =
                    generalized_bounds.swap_remove(idx);

                result.push((group_bound_id, other_bound_id, bound));
                subs = subs.unify(payload_subs);
            }
        }

        *implicit_params_container = result
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
            .collect::<Vec<_>>();

        // NOTE: After finding out which substitutions are valid,
        // the generalization is done once again with results kept
        let mut subs = id_substitutions.clone().unify(subs);

        let mut implicit_params = implicit_params_container.iter();
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

        Some((TraitBoundsBuilder(trait_bounds), subs))
    }

    fn intersection(&self, other: &ItemImplDesc) -> Option<Self> {
        let mut implicit_params_container = vec![];
        let mut id_subs = Default::default();

        let generalized_id =
            self.id
                .generalize(&other.id, &self.params, &other.params, &mut id_subs)?;

        let (group, subs) =
            self.find_valid_trait_bound_groups(other, &id_subs, &mut implicit_params_container)?;

        let impl_items = if self.id.is_inherent() {
            Some(self.items.as_ref().unwrap().generalize(
                other.items.as_ref().unwrap(),
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
            subgroups: vec![],
        })
    }

    /// Indices of ipmls contained in this builder's nested subgroups.
    fn subgroup_impls(&self) -> Vec<usize> {
        let mut all = vec![];

        for (sub_builder, sub_indices, _) in &self.subgroups {
            all.extend(sub_indices);
            all.extend(sub_builder.subgroup_impls());
        }

        all
    }
}

impl ImplGroups {
    fn new(trait_: Option<ItemTrait>, impl_groups: Vec<ImplGroup>) -> Self {
        if let Some(trait_) = &trait_ {
            for ImplGroup { impls, .. } in &impl_groups {
                validate::validate_trait_impls(trait_, impls.iter().map(|(impl_, _)| impl_));
            }
        } else {
            for ImplGroup { impls, .. } in &impl_groups {
                validate::validate_inherent_impls(impls.iter().map(|(impl_, _)| impl_));
            }
        }

        Self {
            trait_,
            impl_groups,
        }
    }
}

fn compute_inherent_args(
    builder: &ImplGroupBuilder,
    impl_indices: &[usize],
    all_descs: &[ItemImplDesc],
) -> Vec<Vec<syn::GenericArgument>> {
    if !builder.id.is_inherent() || impl_indices.len() <= 1 && builder.subgroups.is_empty() {
        return Vec::new();
    }

    let Some((&_, (rows, _))) = builder.trait_bounds.0.get_index(0) else {
        return Vec::new();
    };

    let group_impls = impl_indices
        .iter()
        .map(|&idx| &all_descs[idx])
        .collect::<Vec<_>>();

    let nrows = rows.len().min(group_impls.len());
    if nrows == 0 {
        return Vec::new();
    }

    let payloads = builder
        .trait_bounds
        .0
        .values()
        .flat_map(|(_, bindings)| bindings.values())
        .collect::<Vec<_>>();

    let mut args = Vec::with_capacity(nrows);
    for row in 0..nrows {
        let impl_desc = group_impls[row];
        let mut subs = Generalizations::default();

        if builder
            .id
            .generalize(&impl_desc.id, &builder.params, &impl_desc.params, &mut subs)
            .is_none()
        {
            continue;
        }

        for (generalized_payload, payloads_vec) in &payloads {
            if let Some(payload) = &payloads_vec[row] {
                let _ = generalized_payload
                    .generalize(payload, &builder.params, &impl_desc.params, &mut subs)
                    .unwrap();
            }
        }

        args.push(subs.generic_args().map(|(_, arg)| arg).collect::<Vec<_>>());
    }

    args
}

fn instantiate_impl_group(
    builder: ImplGroupBuilder,
    impl_indices: Vec<usize>,
    all_impls: &mut [Option<ItemImpl>],
    all_descs: &[ItemImplDesc],
) -> ImplGroup {
    let args = compute_inherent_args(&builder, &impl_indices, all_descs);

    let ImplGroupBuilder {
        id,
        lifetimes,
        mut params,
        trait_bounds,
        items,
        subgroups,
    } = builder;

    let subgroups = subgroups
        .into_iter()
        .map(|(mut sub_builder, indices, overlapping_bounds)| {
            let common_types = find_common_types(&overlapping_bounds, &sub_builder.trait_bounds);
            remove_trait_bounds(&overlapping_bounds, &mut sub_builder);

            (
                instantiate_impl_group(sub_builder, indices, all_impls, all_descs),
                common_types,
            )
        })
        .collect::<Vec<_>>();

    let trait_bounds = trait_bounds.build(&id, &mut params);
    let params = lifetimes
        .into_iter()
        .map(syn::GenericParam::Lifetime)
        .chain(as_generics(&params))
        .collect();

    let impl_items = impl_indices
        .iter()
        .map(|&idx| all_impls[idx].take().unwrap())
        .collect::<Vec<_>>();

    let impls = if id.is_inherent() && (impl_items.len() > 1 || !subgroups.is_empty()) {
        impl_items.into_iter().zip_eq(args).collect()
    } else {
        impl_items
            .into_iter()
            .map(|impl_| (impl_, Vec::new()))
            .collect()
    };

    ImplGroup {
        id,
        params,
        trait_bounds,
        items,
        impls,
        subgroups,
    }
}

fn build_disjoint_impl_group(
    trait_: Option<&ItemTrait>,
    mut impl_group: ImplGroup,
    group_idx: usize,
) -> (TokenStream2, Vec<ItemImpl>) {
    let mut helper_traits = Vec::new();
    let mut trait_impls = Vec::new();
    let mut item_impls = Vec::new();
    let mut subgroup_tokens = Vec::new();

    if impl_group.impls.len() > 1 || !impl_group.subgroups.is_empty() {
        let helper_trait = helper_trait::generate(trait_, group_idx, &impl_group);
        helper_traits.push(helper_trait.clone());

        if let Some(main_impl) = main_trait::generate_impl(trait_, group_idx, &impl_group) {
            trait_impls.push(main_impl);
        }

        let mut dispatch_bindings_cnt: usize = impl_group
            .trait_bounds
            .0
            .iter()
            .map(|(_, bindings)| bindings.1.len())
            .sum();

        let mut subgroup_trait = helper_trait;
        subgroup_trait.generics.params = subgroup_trait
            .generics
            .params
            .into_iter()
            .filter(|param| {
                if let syn::GenericParam::Type(_) = param
                    && dispatch_bindings_cnt > 0
                {
                    dispatch_bindings_cnt -= 1;
                    return false;
                }

                true
            })
            .collect();

        let subgroup_impls = core::mem::take(&mut impl_group.subgroups);
        for (subgroup_idx, (mut subgroup, common_args)) in subgroup_impls.into_iter().enumerate() {
            let subgroup_trait_ident = &subgroup_trait.ident;

            let is_trait_inherent = subgroup.id.is_inherent();
            if let Some(trait_) = &mut subgroup.id.trait_ {
                trait_.segments.last_mut().unwrap().ident = subgroup_trait_ident.clone();
            } else {
                let helper_trait_args = &subgroup.params;

                subgroup.id.trait_ =
                    Some(parse_quote! { #subgroup_trait_ident<#helper_trait_args> });

                for (impl_, args) in &mut subgroup.impls {
                    traitize_inherent_impl(args, impl_, &subgroup.id.self_ty);
                }
            }

            let (tokens, mut subgroup_main_impls) =
                build_disjoint_impl_group(Some(&subgroup_trait), subgroup, subgroup_idx);

            subgroup_tokens.push(tokens);
            subgroup_main_impls.iter_mut().for_each(|trait_impl| {
                let trait_path = &mut trait_impl.trait_.as_mut().unwrap().1;
                let last_seg = trait_path.segments.last_mut().unwrap();

                prepend_args(&mut last_seg.arguments, &common_args);

                if is_trait_inherent {
                    match &mut last_seg.arguments {
                        syn::PathArguments::None => {}
                        syn::PathArguments::AngleBracketed(bracketed) => {
                            let replace_at = bracketed.args.len() - common_args.len();
                            let mut args =
                                core::mem::take(&mut bracketed.args).into_iter().collect::<Vec<_>>();

                            args.splice(
                                replace_at..,
                                common_args
                                    .iter()
                                    .cloned()
                                    .map(syn::GenericArgument::Type),
                            );

                            bracketed.args = args.into_iter().collect();
                        }
                        syn::PathArguments::Parenthesized(_) => {
                            unreachable!("Not a valid trait name")
                        }
                    }
                }
            });

            subgroup_tokens.extend(
                subgroup_main_impls
                    .into_iter()
                    .map(|trait_impl| quote!(#trait_impl)),
            );
        }

        item_impls.extend(disjoint::generate(group_idx, impl_group));
    } else if let Some((main_impl, _)) = impl_group.impls.pop() {
        trait_impls.push(main_impl);
    }

    let tokens = quote! {
        #( #helper_traits )*
        #( #item_impls )*

        #( #subgroup_tokens )*
    };

    (tokens, trait_impls)
}

fn prepend_args<'a>(
    arguments: &mut syn::PathArguments,
    types: impl IntoIterator<Item = &'a syn::Type>,
) {
    let types = types.into_iter();

    match arguments {
        syn::PathArguments::None => {
            let bracketed = parse_quote! { <#( #types ),*> };
            *arguments = syn::PathArguments::AngleBracketed(bracketed);
        }
        syn::PathArguments::AngleBracketed(bracketed) => {
            bracketed.args = types
                .map(|param| parse_quote!(#param))
                .chain(core::mem::take(&mut bracketed.args))
                .collect();
        }
        syn::PathArguments::Parenthesized(_) => unreachable!("Not a valid trait name"),
    }
}

impl ItemImplDescVisitor {
    fn find(item_impl: &ItemImpl) -> ItemImplDesc {
        let trait_ = item_impl.trait_.as_ref().map(|(_, trait_, _)| trait_);

        let items = trait_.is_none().then(|| ImplItemsDesc {
            fns: item_impl
                .items
                .iter()
                .filter_map(|item| match item {
                    syn::ImplItem::Fn(item) => Some((item.sig.ident.clone(), item.sig.clone())),
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
                    syn::ImplItem::Const(item) => Some((item.ident.clone(), item.ty.clone())),
                    _ => None,
                })
                .collect(),
        });

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
                    items,
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
    let ImplGroups {
        trait_,
        impl_groups,
    } = parse_macro_input!(input);

    let mut trait_impls_tokens = Vec::new();
    let groups = impl_groups
        .into_iter()
        .enumerate()
        .map(|(idx, impl_group)| {
            let (tokens, trait_impls) = build_disjoint_impl_group(trait_.as_ref(), impl_group, idx);
            trait_impls_tokens.extend(trait_impls);
            tokens
        })
        .collect::<Vec<_>>();

    let groups = quote! { #(#groups)* };
    let trait_ = trait_.filter(|trait_| !trait_.attrs.iter().any(is_remote));

    let trait_impls = trait_impls_tokens
        .into_iter()
        .map(|trait_impl| quote!(#trait_impl))
        .collect::<Vec<_>>();

    quote! {
        #trait_

        #[allow(clippy::needless_lifetimes)]
        const _: () = {
            #groups
            #( #trait_impls )*
        };
    }
    .into()
}

impl Parse for ImplGroups {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        let main_trait = input.parse::<ItemTrait>().ok();

        let mut impls = Vec::new();
        let mut descs = Vec::new();

        while let Ok(item) = input.parse::<ItemImpl>() {
            validate_impl_syntax(&item);

            let impl_ = normalize::normalize(item);
            let desc = ItemImplDescVisitor::find(&impl_);

            impls.push(impl_);
            descs.push(desc);
        }

        let mut dsu = Dsu::new(impls.len());
        for i in 0..impls.len() {
            for j in (i + 1)..impls.len() {
                let mut subs = Default::default();

                let desc_i = &descs[i];
                let desc_j = &descs[j];

                if desc_i
                    .id
                    .generalize(&desc_j.id, &desc_i.params, &desc_j.params, &mut subs)
                    .is_some()
                    && !subs.is_disjoint(&desc_i.params, &desc_j.params)
                {
                    dsu.union(i, j);
                }
            }
        }

        let impl_group_builders = dsu
            .groups()
            .iter()
            .flat_map(|subset| {
                // TODO: Write better error message
                partition_impl_groups(subset, &descs).expect_or_abort("Impls overlap")
            })
            .collect::<Vec<_>>();

        let mut impls = impls.into_iter().map(Some).collect::<Vec<_>>();

        let impl_groups = impl_group_builders
            .into_iter()
            .map(|(builder, impl_group)| {
                instantiate_impl_group(builder, impl_group, &mut impls, &descs)
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
    impls: &[ItemImplDesc],
) -> Option<Vec<(ImplGroupBuilder, Vec<usize>)>> {
    partition_impl_groups_rec(subset, impls, &mut Vec::new())
}

fn partition_impl_groups_rec(
    subset: &[usize],
    impls: &[ItemImplDesc],
    impl_groups: &mut Vec<(ImplGroupBuilder, Vec<usize>)>,
) -> Option<Vec<(ImplGroupBuilder, Vec<usize>)>> {
    let Some((&curr_impl_idx, rest)) = subset.split_first() else {
        return build_impl_groups(impls, impl_groups).collect();
    };

    let mut min = None::<Vec<_>>;
    let curr_impl = &impls[curr_impl_idx];

    for impl_group_idx in 0..impl_groups.len() {
        let impl_group = &impl_groups[impl_group_idx];

        if let Some(intersection) = impl_group.0.intersection(curr_impl) {
            let (builder, impl_group) = &mut impl_groups[impl_group_idx];
            let prev_builder = core::mem::replace(builder, intersection);
            impl_group.push(curr_impl_idx);

            match (
                partition_impl_groups_rec(rest, impls, impl_groups),
                &mut min,
            ) {
                (Some(res), Some(min)) if res.len() < min.len() => *min = res,
                (Some(res), None) => min = Some(res),
                _ => {}
            }

            // NOTE: Restore impl group changes before the next iteration
            let impl_group_to_restore = &mut impl_groups[impl_group_idx];
            impl_group_to_restore.0 = prev_builder;
            impl_group_to_restore.1.pop();

            if min.as_ref().is_some_and(|m| m.len() == impl_groups.len()) {
                // NOTE: One of the shortest solutions has been found
                break;
            }
        }
    }

    // NOTE: Try create a new group if shorter solution is possible
    if min.as_ref().is_none_or(|m| m.len() > 1 + impl_groups.len())
        && (curr_impl.id.is_inherent() || impl_groups.iter().all(|g| g.0.id != curr_impl.id))
    {
        impl_groups.push((ImplGroupBuilder::new(curr_impl), vec![curr_impl_idx]));

        match (
            partition_impl_groups_rec(rest, impls, impl_groups),
            &mut min,
        ) {
            (Some(res), Some(min)) if res.len() < min.len() => *min = res,
            (Some(res), None) => min = Some(res),
            _ => {}
        }

        // NOTE: Restore changes to impl groups
        impl_groups.pop();
    }

    min
}

/// Separates the given impls into two groups: those that don't overlap and those that do.
/// Overlapping impls are then recursively partitioned into smaller, non-overlapping groups.
fn build_impl_groups(
    impls: &[ItemImplDesc],
    impl_groups: &[(ImplGroupBuilder, Vec<usize>)],
) -> impl Iterator<Item = Option<(ImplGroupBuilder, Vec<usize>)>> {
    impl_groups.iter().map(|(builder, impl_group)| {
        let (non_overlapping, overlapping) = split_overlapping_impls(impls, builder, impl_group);
        let non_overlapping_set = non_overlapping.iter().copied().collect::<IndexSet<_>>();

        if non_overlapping_set.is_empty() {
            return None;
        }

        let subgroups = partition_impl_groups(&overlapping, impls)?;
        let mut non_overlapping_pos: Vec<_> = impl_group
            .iter()
            .enumerate()
            .filter(|&(_, idx)| non_overlapping_set.contains(idx))
            .map(|(pos, _)| pos)
            .collect();
        non_overlapping_pos.sort_unstable();

        let mut builder = builder.clone();
        let mut overlapping_trait_bounds = builder
            .trait_bounds
            .retain_impl_by_pos(&non_overlapping_pos);

        builder.subgroups = subgroups
            .into_iter()
            .map(|(sub_builder, idxs)| {
                let mut all_idxs = sub_builder.subgroup_impls();
                all_idxs.extend(idxs.iter().copied());

                let mut other_impl_pos: Vec<_> = overlapping
                    .iter()
                    .enumerate()
                    .filter(|&(_, idx)| !all_idxs.contains(idx))
                    .map(|(pos, _)| pos)
                    .collect();
                other_impl_pos.sort_unstable();

                (
                    sub_builder,
                    idxs,
                    overlapping_trait_bounds.retain_impl_by_pos(&other_impl_pos),
                )
            })
            .collect();

        Some((builder, non_overlapping))
    })
}

fn split_overlapping_impls(
    impls: &[ItemImplDesc],
    builder: &ImplGroupBuilder,
    group: &[usize],
) -> (Vec<usize>, Vec<usize>) {
    let mut dsu = Dsu::new(group.len());

    for i in 0..group.len() {
        for j in (i + 1)..group.len() {
            let mut subs = Generalizations::default();

            let desc_i = &impls[group[i]];
            let desc_j = &impls[group[j]];

            if desc_i
                .id
                .generalize(&desc_j.id, &desc_i.params, &desc_j.params, &mut subs)
                .is_none()
            {
                continue;
            }

            if !subs.is_disjoint(&desc_i.params, &desc_j.params) {
                let assoc_bindings = builder
                    .trait_bounds
                    .0
                    .values()
                    .map(|(_, bindings)| bindings);

                let is_overlapping = assoc_bindings
                    .flat_map(|assoc_bindings| assoc_bindings.values())
                    .all(|(_, payloads)| {
                        let (binding_i, binding_j) = (&payloads[i], &payloads[j]);

                        if binding_i
                            .generalize(binding_j, &desc_i.params, &desc_j.params, &mut subs)
                            .is_none()
                        {
                            return false;
                        }

                        !subs.is_disjoint(&desc_i.params, &desc_j.params)
                    });

                if is_overlapping {
                    dsu.union(i, j);
                }
            }
        }
    }

    let mut non_overlapping = Vec::new();
    let mut overlapping = Vec::new();

    for component in dsu.groups() {
        if component.len() > 1 {
            overlapping.extend(component.into_iter().map(|pos| group[pos]));
        } else {
            non_overlapping.push(group[component[0]]);
        }
    }

    (non_overlapping, overlapping)
}

fn remove_trait_bounds(trait_bounds: &TraitBoundsBuilder, impl_group: &mut ImplGroupBuilder) {
    for (parent_orig, _) in trait_bounds.0.values() {
        impl_group
            .subgroups
            .iter_mut()
            .for_each(|(subgroup, _, common_bounds)| {
                remove_trait_bounds(trait_bounds, subgroup);

                common_bounds.0.retain(|_, target_orig| {
                    target_orig
                        .0
                        .iter()
                        .any(|target_bound| !parent_orig.contains(target_bound))
                });
            });

        impl_group.trait_bounds.0.retain(|_, target_orig| {
            target_orig
                .0
                .iter()
                .any(|target_bound| !parent_orig.contains(target_bound))
        });
    }
}

fn find_common_types(
    parent_trait_bounds: &TraitBoundsBuilder,
    subgroup_trait_bounds: &TraitBoundsBuilder,
) -> Vec<AssocBindingPayload> {
    let mut result = Vec::new();

    for (parent_bound_id, (parent_orig, parent_bindings)) in &parent_trait_bounds.0 {
        if let Some((_, (_, sub_bindings))) =
            subgroup_trait_bounds.0.iter().find(|(_, (sub_orig, _))| {
                sub_orig
                    .iter()
                    .all(|sub_bound| parent_orig.contains(sub_bound))
            })
        {
            result.extend(parent_bindings.iter().map(|(ident, _)| {
                sub_bindings
                    .get(ident)
                    .map(|(payload, _)| payload.clone())
                    .unwrap_or_else(|| assoc_binding_default(parent_bound_id, ident))
            }));
        }
    }

    result
}

fn assoc_binding_default(trait_ident: &TraitBoundIdent, ident: &syn::Ident) -> syn::Type {
    let Bounded(bounded_ty) = &trait_ident.0;
    let TraitBound(trait_bound) = &trait_ident.1;
    parse_quote!(<#bounded_ty as #trait_bound>::#ident)
}
