//! Unlock support for a variety of mutually disjoint implementations that the Rust compiler
//! [does not (yet?) support](https://github.com/rust-lang/rust/issues/20400).
//!
//! # Example
//!
//! ```
//! use disjoint_impls::disjoint_impls;
//!
//! pub enum GroupA {}
//! pub enum GroupB {}
//!
//! pub trait Dispatch {
//!     type Group;
//! }
//!
//! disjoint_impls! {
//!     pub trait Kita {}
//!
//!     impl<T: Dispatch<Group = GroupA>> Kita for T {}
//!     impl<U: Dispatch<Group = GroupB>> Kita for U {}
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
    generalize::{Generalize, GenericParam, Params, Sizedness, as_generics, is_superset},
    main_trait::is_remote,
    validate::validate_impl_syntax,
};

mod disjoint;
mod generalize;
mod helper_trait;
mod main_trait;
mod normalize;
mod validate;

/// Per impl count of supersets.
///
/// Indices in this list correspond with the total order of impls
type Supersets = Vec<usize>;

/// Per impl list of subsets with generalizations converting from subset to the current item.
///
/// For superset relations generalizations of the superset impl are always identifiers, never complex types.
/// Indices in this list correspond with the total order of impls
type Subsets<'a> = Vec<IndexMap<usize, Generalizations<'a>>>;

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

    /// Generalized items of this impl group
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

    /// Generalized items of this group
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
    fn build(self, impl_group_id: &ImplGroupId, params: &mut Params) -> TraitBounds {
        struct GenericParamFinder<'a>(IndexSet<syn::Ident>, IndexSet<&'a syn::Ident>);
        struct GenericParamMatcher<'a>(IndexSet<&'a syn::Ident>, bool);

        impl<'a> Visit<'a> for GenericParamFinder<'a> {
            fn visit_path(&mut self, node: &'a syn::Path) {
                let first_seg = node.segments.first().unwrap();

                if self.0.contains(&first_seg.ident) {
                    self.1.insert(&first_seg.ident);
                }

                syn::visit::visit_path(self, node);
            }
        }

        impl<'a> Visit<'a> for GenericParamMatcher<'a> {
            fn visit_path(&mut self, node: &'a syn::Path) {
                let first_seg = node.segments.first().unwrap();

                if !self.0.contains(&first_seg.ident) {
                    self.1 = true;
                }

                syn::visit::visit_path(self, node);
            }
        }

        let (valid, maybe) = self.0.into_iter().partition::<Vec<_>, _>(|(bound_id, _)| {
            let last_seg = bound_id.1.0.path.segments.last().unwrap();

            if let syn::PathArguments::AngleBracketed(bracketed) = &last_seg.arguments {
                return !matches!(
                    bracketed.args.first(),
                    Some(syn::GenericArgument::Lifetime(_))
                );
            }

            true
        });

        let valid_payloads = valid
            .iter()
            .flat_map(|(_, (_, bindings))| bindings.iter().map(|(_, (p, _))| p))
            .collect::<Vec<_>>();

        let validated_maybe = maybe
            .into_iter()
            .map(|(bound_id, (orig_bounds, bindings))| {
                let bindings = bindings
                    .into_iter()
                    .map(|(ident, (p, payloads))| {
                        let ty_params = params
                            .iter()
                            .filter_map(|(ident, param)| match param {
                                GenericParam::Type(_, _) => Some(ident),
                                _ => None,
                            })
                            .cloned()
                            .collect();
                        let mut param_finder = GenericParamFinder(ty_params, IndexSet::new());

                        param_finder.visit_type(&p);
                        let unbound_params = param_finder.1;

                        let x = if unbound_params.is_empty() {
                            Some(p)
                        } else {
                            let mut matcher = GenericParamMatcher(unbound_params.clone(), true);

                            matcher.visit_type(&impl_group_id.self_ty);
                            if let Some(trait_) = &impl_group_id.trait_ {
                                matcher.visit_path(trait_);
                            }

                            for &valid_payload in &valid_payloads {
                                matcher.visit_type(valid_payload);
                            }

                            if !matcher.1 {
                                Some(p)
                            } else {
                                for &unbound_param in &unbound_params {
                                    params.swap_remove(unbound_param);
                                }

                                None
                            }
                        };

                        (ident, (x, payloads))
                    })
                    .collect::<IndexMap<_, _>>();

                (bound_id, (orig_bounds, bindings))
            })
            .collect::<Vec<_>>();

        TraitBounds(
            valid
                .into_iter()
                .map(|(bound_id, (orig_bounds, bindings))| {
                    let bindings = bindings
                        .into_iter()
                        .map(|(ident, (p, payloads))| (ident, (Some(p), payloads)))
                        .collect::<IndexMap<_, _>>();

                    (bound_id, (orig_bounds, bindings))
                })
                .chain(validated_maybe)
                .collect(),
        )
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
    fn new<'a>(impl_desc: &ItemImplDesc, identity_subs: &Generalizations<'a>) -> Self {
        let mut subs = identity_subs.clone();

        for (assoc_ty1, assoc_ty2) in impl_desc
            .trait_bounds
            .values()
            .zip_eq(impl_desc.trait_bounds.values())
            .flat_map(|(group_bound, other_bound)| {
                group_bound.values().zip_eq(other_bound.values())
            })
        {
            assoc_ty1
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

        let impl_items = impl_desc.id.trait_.is_none().then(|| {
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
            id: impl_desc.id.clone(),
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

        // FIXME: Use stack or queue? the order matters very much!
        // how to eliminate redundant results?
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
                let mut subs = Generalizations::default();
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
        subsets: &Subsets<'_>,
        mut impls: IndexMap<usize, &'a ItemImplDesc>,
    ) -> Vec<Self> {
        let mut implicit_params_container = vec![];

        let id_subs = &subsets[*impls.first().unwrap().0][&other_idx];
        self.find_valid_trait_bound_groups(other, id_subs, &mut implicit_params_container)
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

                // NOTE: After adding new impl into this group all pairs of impls have to be checked
                // because a dispatch trait could have been removed thus resulting in an overlap
                impls.insert(other_idx, other);
                for (i, p1) in payload_rows.iter().enumerate() {
                    for (j, p2) in payload_rows.iter().enumerate() {
                        if i == j {
                            continue;
                        }

                        let impl_id1 = &impls[i].id;
                        let impl_id2 = &impls[j].id;

                        let params1 = &impls[i].params;
                        let params2 = &impls[j].params;

                        if is_superset(impl_id1, impl_id2, params1, params2).is_none() {
                            continue;
                        }

                        if p1.iter().zip_eq(p2).all(|(&p1, &p2)| {
                            is_superset(p1, p2, params1, params2)
                                .is_some_and(|subs| subs.is_empty())
                        }) {
                            return None;
                        }
                    }
                }

                impls.pop();
                let impl_items = if self.id.trait_.is_none() {
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
                    id: self.id.clone(),
                    lifetimes,
                    params,
                    trait_bounds: group,
                    items: impl_items,
                })
            })
            .collect()
    }
}

impl ImplItems {
    fn generalize(
        &self,
        other: &Self,
        params1: &Params,
        params2: &Params,
        subs: &Generalizations<'_>,
    ) -> Option<Self> {
        let mut new_subs = subs.clone();

        let generalized_fns = self
            .fns
            .iter()
            .map(|(ident, sig)| {
                let other_sig = other.fns.get(ident)?;

                Some((
                    ident.clone(),
                    sig.generalize(other_sig, params1, params2, &mut new_subs)?,
                ))
            })
            .collect::<Option<_>>()?;

        let generalized_assoc_types = self
            .assoc_types
            .iter()
            .all(|ident| other.assoc_types.contains(ident))
            .then_some(self.assoc_types.clone())?;

        let generalized_assoc_consts = self
            .assoc_consts
            .iter()
            .map(|(ident, self_ty)| {
                let other_ty = other.assoc_consts.get(ident)?;

                Some((
                    ident.clone(),
                    self_ty.generalize(other_ty, params1, params2, &mut new_subs)?,
                ))
            })
            .collect::<Option<_>>()?;

        if !new_subs.difference(subs).is_empty() {
            return None;
        }

        Some(ImplItems {
            fns: generalized_fns,
            assoc_types: generalized_assoc_types,
            assoc_consts: generalized_assoc_consts,
        })
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
/// # Example
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
/// impl Dispatch for String {
///     type Group = GroupA;
/// }
/// impl Dispatch for i32 {
///     type Group = GroupB;
/// }
///
/// impl Dispatch for Option<String> {
///     type Group = GroupA;
/// }
/// impl Dispatch for Option<i32> {
///     type Group = GroupB;
/// }
///
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
///     impl<T> ComplexKita for T where Option<T>: Dispatch<Group = GroupA> {
///         const COMPLEX_NAME: &'static str = "Blanket A";
///     }
///     impl<U> ComplexKita for U where Option<U>: Dispatch<Group = GroupB> {
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
/// Other, much more complex examples can be found in tests.
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
/// impl Dispatch for String {
///     type Group = GroupA;
/// }
///
/// pub enum GroupB {}
/// impl Dispatch for i32 {
///     type Group = GroupB;
/// }
///
/// // (orphan rule): You can define blanket impls only
/// // for types that are defined in the current crate
/// pub struct LocalType<T>(T);
///
/// disjoint_impls! {
///     // Trait annotated with `#[disjoint_impls(remote)]` must be an exact duplicate
///     // of the foreign/remote trait it refers to (values and function bodies excluded)
///     #[disjoint_impls(remote)]
///     pub trait ForeignKita<U> {
///         fn kita() -> &'static str;
///     }
///
///     impl<T: Dispatch<Group = GroupA>, U> ForeignKita<U> for LocalType<T> {
///         fn kita() -> &'static str {
///             "Blanket A"
///         }
///     }
///     impl<T: Dispatch<Group = GroupB>, U> ForeignKita<U> for LocalType<T> {
///         fn kita() -> &'static str {
///             "Blanket B"
///         }
///     }
/// }
/// ```
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

        const _: () = {
            #( #helper_traits )*
            #( #item_impls )*
            #( #main_trait_impls )*

        };
    }
    .into()
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

        let (mut supersets, subsets) = make_sets(&impls);

        let impl_groups = supersets
            .iter()
            .enumerate()
            .filter_map(|(i, superset_count)| (*superset_count == 0).then_some(i))
            .collect::<Vec<_>>()
            .into_iter()
            .flat_map(|impl_group_idx| {
                find_impl_groups(impl_group_idx, &mut supersets, &subsets, &impls).unwrap_or_else(
                    || {
                        let impl_group_id = &impls[impl_group_idx].0.id;

                        let msg = format!(
                            "Conflicting implementations of `{}`",
                            quote!(#impl_group_id)
                        );

                        abort!(impl_group_id, msg)
                    },
                )
            })
            .collect::<IndexMap<_, _>>();

        let args = impl_groups
            .iter()
            .map(|(impl_group_id, (impl_group_builder, impl_group))| {
                let impls = impl_group.iter().map(|&i| &impls[i]).collect::<Vec<_>>();

                if impl_group_id.trait_.is_none() && impl_group.len() > 1 {
                    let nrows = impl_group_builder.trait_bounds.0[0].0.len();

                    let payloads = impl_group_builder
                        .trait_bounds
                        .0
                        .values()
                        .flat_map(|(_, payloads)| payloads.values())
                        .collect::<Vec<_>>();

                    (0..nrows)
                        .map(|i| {
                            let mut subs = subsets[impl_group[0]][&impl_group[i]].clone();

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

                            subs.generic_args().map(|(_, arg)| arg).collect::<Vec<_>>()
                        })
                        .collect()
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
            .map(
                |((impl_group_id, (mut impl_group_builder, impl_group)), args)| {
                    let impls = impl_group
                        .iter()
                        .map(|&i| impls[i].take().unwrap())
                        .collect::<Vec<_>>();

                    let impls = if impl_group_id.trait_.is_none() && impl_group.len() > 1 {
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
                },
            )
            .collect();

        Ok(Self::new(main_trait, impl_groups))
    }
}

fn find_impl_groups<'a, 'b>(
    curr_item_idx: usize,

    supersets: &mut Supersets,
    subsets: &Subsets<'a>,

    item_impls: &'b [(ItemImplDesc, ItemImpl)],
) -> Option<IndexMap<ImplGroupId, (ImplGroupBuilder, Vec<usize>)>> {
    find_impl_groups_rec(
        curr_item_idx,
        supersets,
        subsets,
        item_impls,
        &mut IndexMap::new(),
    )
}

fn find_impl_groups_rec<'a, 'b>(
    curr_item: usize,

    supersets: &mut Supersets,
    subsets: &Subsets<'a>,

    item_impls: &'b [(ItemImplDesc, ItemImpl)],
    impl_groups: &mut IndexMap<ImplGroupId, (ImplGroupBuilder, Vec<usize>)>,
) -> Option<IndexMap<ImplGroupId, (ImplGroupBuilder, Vec<usize>)>> {
    let curr_impl = &item_impls[curr_item];

    let mut min = None::<IndexMap<_, _>>;
    let mut new_supersets = core::iter::repeat_n(0, supersets.len()).collect();

    let identity_subs = &subsets[curr_item][&curr_item];
    'OUT: for impl_group_id in impl_groups.keys().cloned().collect::<Vec<_>>() {
        let impl_group = &impl_groups[&impl_group_id];

        let group_impls = impl_group
            .1
            .iter()
            .map(|&i| (i, &item_impls[i].0))
            .collect();

        let intersections =
            impl_group
                .0
                .intersection(curr_item, &curr_impl.0, subsets, group_impls);

        for intersection in intersections {
            let impl_group = &mut impl_groups[&impl_group_id];

            let prev_assoc_bindings = core::mem::replace(&mut impl_group.0, intersection);
            impl_group.1.push(curr_item);

            let mut supersets = supersets.clone();
            let res = unlock_subset_impl_groups(
                curr_item,
                &mut supersets,
                subsets,
                item_impls,
                impl_groups,
            );

            match (res, &mut min) {
                (Some(res), Some(min)) if res.len() < min.len() => {
                    new_supersets = supersets;
                    *min = res;
                }
                (Some(res), None) => {
                    new_supersets = supersets;
                    min = Some(res);
                }
                _ => {}
            }

            // NOTE: Restore changes to the impl group set for the next iteration
            let impl_group_to_restore = impl_groups.get_mut(&impl_group_id).unwrap();
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
        && !impl_groups.contains_key(&curr_impl.0.id)
    {
        impl_groups.insert(
            curr_impl.0.id.clone(),
            (
                ImplGroupBuilder::new(&curr_impl.0, identity_subs),
                vec![curr_item],
            ),
        );

        let mut supersets = supersets.clone();
        let res =
            unlock_subset_impl_groups(curr_item, &mut supersets, subsets, item_impls, impl_groups);

        match (res, &mut min) {
            (Some(res), Some(min)) if res.len() < min.len() => {
                new_supersets = supersets;
                *min = res;
            }
            (Some(res), None) => {
                new_supersets = supersets;
                min = Some(res);
            }
            _ => {}
        }

        // NOTE: Restore changes to impl groups
        impl_groups.shift_remove(&curr_impl.0.id);
    }

    if min.is_some() {
        *supersets = new_supersets;
    }

    min
}

fn unlock_subset_impl_groups<'a, 'b>(
    curr_item: usize,

    supersets: &mut Supersets,
    subsets: &Subsets<'a>,

    item_impls: &'b [(ItemImplDesc, ItemImpl)],
    impl_groups: &mut IndexMap<ImplGroupId, (ImplGroupBuilder, Vec<usize>)>,
) -> Option<IndexMap<ImplGroupId, (ImplGroupBuilder, Vec<usize>)>> {
    let mut acc = impl_groups.clone();

    for &subset_idx in subsets[curr_item].keys().filter(|&i| *i != curr_item) {
        let superset_count = supersets.get_mut(subset_idx).unwrap();

        *superset_count -= 1;
        if *superset_count == 0 {
            acc = find_impl_groups_rec(subset_idx, supersets, subsets, item_impls, &mut acc)?;
        }
    }

    Some(acc)
}

fn make_sets(impl_groups: &[(ItemImplDesc, ItemImpl)]) -> (Supersets, Subsets<'_>) {
    let n = impl_groups.len();

    let mut supersets: Supersets = core::iter::repeat_n(0, n).collect();
    let mut subsets: Subsets = core::iter::repeat_n(Default::default(), n).collect();

    let iter = impl_groups.iter().enumerate();
    for ((i1, (g1, _)), (i2, (g2, _))) in iter.clone().cartesian_product(iter) {
        // FIXME: Too much cloning
        let params1 = g1.params.clone();
        let params2 = g2.params.clone();

        if !subsets[i2].contains_key(&i1)
            && let Some(subs) = is_superset(&g1.id, &g2.id, &params1, &params2)
        {
            subsets[i1].insert(i2, subs);

            if i1 != i2 {
                *supersets.get_mut(i2).unwrap() += 1;
            }
        }
    }

    (supersets, subsets)
}
