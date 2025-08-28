use core::iter::zip;

use indexmap::{IndexMap, IndexSet};
use itertools::{Itertools, zip_eq};
use proc_macro::TokenStream;
use proc_macro_error2::{abort, proc_macro_error};
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use superset::Substitutions;
use syn::{
    ItemImpl, ItemTrait,
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote,
    visit::{Visit, visit_path},
    visit_mut::VisitMut,
};

use crate::{
    param::{IsUnsizedBound as _, prune_unused_generics},
    superset::Superset,
};

mod disjoint;
mod helper_trait;
mod main_trait;
mod param;
mod superset;
mod unconstrained;
mod validate;

type Supersets<'a> = IndexMap<&'a ImplGroupId, usize>;
type Subsets<'a> = IndexMap<&'a ImplGroupId, IndexMap<&'a ImplGroupId, Substitutions<'a>>>;

type TraitBoundIdent = (Bounded, TraitBound);

/// Unique name based identifier of the associated type bound such as:
///     `(T, Deref, Deref::Target)` in `impl<T: Deref<Target = bool>> for Clone for T`
type AssocBindingIdent = (TraitBoundIdent, syn::Ident);

/// AST node type of the associated bound constraint such as:
///     `bool` in `impl<T: Deref<Target = bool>> for Clone for T`
type AssocBindingPayload = syn::Type;

#[derive(Debug, Clone)]
struct AssocBindingsGroup(IndexMap<AssocBindingIdent, Vec<Option<AssocBindingPayload>>>);

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

#[derive(Debug)]
struct ImplGroup {
    /// Id of this impl group
    id: ImplGroupId,
    /// All [impl blocks](syn::ItemImpl) that are part of this group (in the order of appearance)
    item_impls: Vec<syn::ItemImpl>,
    /// Associated bindings that impls of this group are dispatched on (i.e. differentiated by)
    assoc_bindings: AssocBindingsGroup,
    /// Generic parameters used in the impl group id or the bounded type of associated type binding
    generics: syn::Generics,
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

#[derive(Debug, Clone, Default)]
struct ItemImplDescriptor {
    trait_bounds: IndexSet<TraitBoundIdent>,
    assoc_bindings: IndexMap<AssocBindingIdent, AssocBindingPayload>,
}

#[derive(Debug, Clone)]
struct AssocBindingsGroupBuilder {
    trait_bounds: IndexSet<TraitBoundIdent>,
    assoc_bindings_group: AssocBindingsGroup,
}

struct TraitBoundsVisitor {
    /// Bounded type currently being visited
    curr_bounded_ty: Option<Bounded>,
    /// Trait bound currently being visited
    curr_trait_bound: Option<TraitBound>,

    /// Bounds of an impl block
    param_bounds: ItemImplDescriptor,
}

impl From<syn::Ident> for Bounded {
    fn from(source: syn::Ident) -> Self {
        Self(parse_quote!(#source))
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

impl AssocBindingsGroup {
    fn idents(&self) -> impl Iterator<Item = &AssocBindingIdent> {
        self.0.keys()
    }

    /// Returns a per row list of associated binding payloads
    ///
    /// # Example
    ///
    /// ```ignore
    /// impl<T: Dispatch<Group = GroupA>, U: Dispatch<Group = GroupA>> Kita for (T, U) {
    ///     const NAME: &'static str = "Blanket AA";
    /// }
    ///
    /// impl<T: Dispatch<Group = GroupB>, U: Dispatch<Group = GroupB>> Kita for (T, U) {
    ///     const NAME: &'static str = "Tuple AB";
    /// }
    /// ```
    ///
    /// would return: `[[GroupA, GroupA], [GroupB, GroupB]]`
    fn payload_rows(&self) -> Vec<Vec<Option<&AssocBindingPayload>>> {
        if self.0.is_empty() {
            return vec![];
        }

        let n_rows = self.0[0].len();
        let n_cols = self.0.len();

        self.0.values().fold(
            core::iter::repeat_n(Vec::with_capacity(n_cols), n_rows).collect::<Vec<_>>(),
            |mut rows, cols| {
                cols.iter()
                    .enumerate()
                    .for_each(|(i, col)| rows[i].push(col.as_ref()));

                rows
            },
        )
    }

    /// Returns true if all associated bounds of any 2 impls are overlapping
    ///
    /// # Example
    ///
    /// ```ignore
    /// impl<T: Dispatch<Group = GroupB>> Kita for T {
    ///     const NAME: &'static str = "Blanket B";
    /// }
    ///
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
    fn is_overlapping(&self, item_impls: &[&ItemImpl]) -> bool {
        if self.0.is_empty() {
            return true;
        }

        !self
            .find_overlapping_before(item_impls.len() - 1, item_impls)
            .is_empty()
    }

    /// Checks for overlapping associated bindings, but only the ones
    /// that come before `item_impl_idx` in the total ordering
    fn find_overlapping_before(
        &self,
        item_impl_idx: usize,
        item_impls: &[&ItemImpl],
    ) -> IndexSet<usize> {
        fn is_superset(
            id1: &ItemImpl,
            id2: &ItemImpl,
            lhs: &[Option<&syn::Type>],
            rhs: &[Option<&syn::Type>],
        ) -> bool {
            zip_eq(lhs, rhs).all(|(e1, e2)| {
                id1.trait_.as_ref().zip(id2.trait_.as_ref()).is_some_and(
                    |((_, trait1, _), (_, trait2, _))| trait1.is_superset(trait2).is_some(),
                ) && id1.self_ty.is_superset(&id2.self_ty).is_some()
                    && e1 == e2
            })
        }

        let payload_rows = self.payload_rows();
        let impl_item_id = &item_impls[item_impl_idx];
        let impl_payload = &payload_rows[item_impl_idx];

        let nrows = item_impls.len();

        if item_impls
            .iter()
            .zip(&payload_rows)
            .rev()
            .skip(nrows - item_impl_idx)
            .any(|(item_id, row)| is_superset(item_id, impl_item_id, row, impl_payload))
        {
            return [item_impl_idx].into_iter().collect();
        }

        item_impls
            .iter()
            .zip(&payload_rows)
            .enumerate()
            .rev()
            .skip(nrows - item_impl_idx)
            .filter_map(|(i, (item_id, row))| {
                is_superset(impl_item_id, item_id, impl_payload, row).then_some(i)
            })
            .collect()
    }
}

impl AssocBindingsGroupBuilder {
    fn new(impl_item_trait_bounds: ItemImplDescriptor) -> Self {
        let assoc_bindings_group = impl_item_trait_bounds
            .assoc_bindings
            .into_iter()
            .map(|(ident, payload)| (ident, vec![Some(payload)]))
            .collect();

        Self {
            trait_bounds: impl_item_trait_bounds.trait_bounds,
            assoc_bindings_group: AssocBindingsGroup(assoc_bindings_group),
        }
    }

    fn intersection<'a>(
        &self,
        impl_group: &'a [&ItemImpl],
        other: (&'a ItemImplDescriptor, &'a ItemImpl),
        substitutions: &Substitutions,
    ) -> impl Iterator<Item = Self> + use<'a> {
        let mut impl_group = impl_group.to_vec();
        let (other, other_impl) = other;

        impl_group.push(other_impl);
        let nrows = impl_group.len();

        let other = other
            .trait_bounds
            .iter()
            .map(|trait_bound| substitutions.substitute(trait_bound).collect::<Vec<_>>())
            .multi_cartesian_product()
            .map(|trait_bounds| {
                let trait_bounds = other
                    .trait_bounds
                    .iter()
                    .zip_eq(trait_bounds)
                    .collect::<IndexMap<_, _>>();

                let assoc_bindings = other
                    .assoc_bindings
                    .iter()
                    .map(|(other_assoc_binding_ident, other_assoc_binding_payload)| {
                        let assoc_bound_ident = (
                            trait_bounds[&other_assoc_binding_ident.0].clone(),
                            other_assoc_binding_ident.1.clone(),
                        );

                        // FIXME: How to substitute payloads???
                        (assoc_bound_ident, other_assoc_binding_payload.clone())
                    })
                    .collect();

                ItemImplDescriptor {
                    trait_bounds: trait_bounds
                        .into_iter()
                        .map(|(_, substituted_trait_bound)| substituted_trait_bound)
                        .collect(),
                    assoc_bindings,
                }
            });

        let trait_bounds = self.trait_bounds.clone();
        let mut assoc_bindings = self.assoc_bindings_group.clone();
        for assoc_binding_payload in assoc_bindings.0.values_mut() {
            assoc_binding_payload.push(None);
        }

        let intersections = other.map(move |other| {
            let trait_bounds = trait_bounds
                .iter()
                .filter(|&trait_bound| trait_bound.1.is_unsized())
                .chain(
                    other
                        .trait_bounds
                        .iter()
                        .filter(|(_, bound)| bound.is_unsized()),
                )
                .chain(
                    trait_bounds
                        .iter()
                        .filter(|&trait_bound| other.trait_bounds.contains(trait_bound)),
                )
                .cloned()
                .collect::<IndexSet<_>>();

            let mut assoc_bindings_group = assoc_bindings
                .0
                .iter()
                .filter_map(|(assoc_binding_ident, assoc_binding_payload)| {
                    if trait_bounds.contains(&assoc_binding_ident.0) {
                        return Some((assoc_binding_ident.clone(), assoc_binding_payload.clone()));
                    }

                    None
                })
                .collect::<IndexMap<_, _>>();

            for (other_assoc_binding_ident, other_assoc_binding_payload) in other.assoc_bindings {
                if !trait_bounds.contains(&other_assoc_binding_ident.0) {
                    continue;
                }

                let found_bound = assoc_bindings_group
                    .entry(other_assoc_binding_ident)
                    .or_insert_with(|| core::iter::repeat_n(None, nrows).collect())
                    .last_mut()
                    .unwrap();

                *found_bound = Some(other_assoc_binding_payload);
            }

            Self {
                trait_bounds,
                assoc_bindings_group: AssocBindingsGroup(assoc_bindings_group),
            }
        });

        intersections.filter(move |intersection| {
            !intersection
                .assoc_bindings_group
                .is_overlapping(&impl_group)
        })
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

impl<'a> TraitBoundsVisitor {
    fn find(item_impl: &'a ItemImpl) -> ItemImplDescriptor {
        let mut visitor = Self {
            curr_bounded_ty: None,
            curr_trait_bound: None,

            param_bounds: ItemImplDescriptor::default(),
        };
        visitor.visit_generics(&item_impl.generics);
        visitor.param_bounds
    }
}

impl Visit<'_> for TraitBoundsVisitor {
    fn visit_item_impl(&mut self, node: &ItemImpl) {
        self.visit_generics(&node.generics);
    }

    fn visit_generics(&mut self, node: &syn::Generics) {
        let mut params: Vec<_> = node
            .params
            .iter()
            .map(|param| {
                use syn::GenericParam::*;

                let ident = match param {
                    Lifetime(syn::LifetimeParam { lifetime, .. }) => &lifetime.ident,
                    Type(syn::TypeParam { ident, .. }) => ident,
                    Const(syn::ConstParam { ident, .. }) => ident,
                };

                (ident, param)
            })
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
        self.curr_bounded_ty = Some(node.ident.clone().into());
        syn::visit::visit_type_param(self, node);
    }

    fn visit_predicate_type(&mut self, node: &syn::PredicateType) {
        struct GenericFinder(bool);

        impl Visit<'_> for GenericFinder {
            fn visit_path(&mut self, node: &syn::Path) {
                let first_seg = node.segments.first().unwrap();

                if first_seg.ident.to_string().starts_with("_ŠČ") {
                    self.0 = true;
                } else {
                    visit_path(self, node);
                }
            }
        }

        let mut generic_finder = GenericFinder(false);
        generic_finder.visit_type(&node.bounded_ty);

        if generic_finder.0 {
            self.curr_bounded_ty = Some(node.bounded_ty.clone().into());
            syn::visit::visit_predicate_type(self, node);
        }
    }

    fn visit_trait_bound(&mut self, node: &syn::TraitBound) {
        self.curr_trait_bound = Some(node.clone().into());

        self.param_bounds.trait_bounds.insert((
            self.curr_bounded_ty.clone().unwrap(),
            self.curr_trait_bound.clone().unwrap(),
        ));

        syn::visit::visit_trait_bound(self, node);
    }

    fn visit_assoc_type(&mut self, node: &syn::AssocType) {
        let trait_bound_ident = (
            self.curr_bounded_ty.clone().unwrap(),
            self.curr_trait_bound.clone().unwrap(),
        );

        self.param_bounds
            .assoc_bindings
            .insert((trait_bound_ident, node.ident.clone()), node.ty.clone());
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

impl Parse for ImplGroups {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        let mut impl_groups = IndexMap::<_, Vec<_>>::new();

        let main_trait = input.parse::<ItemTrait>().ok();
        while let Ok(mut item) = input.parse::<ItemImpl>() {
            param::index(&item).resolve(&mut item);

            let impl_group_id = ImplGroupId {
                trait_: item.trait_.as_ref().map(|trait_| &trait_.1).cloned(),
                self_ty: (*item.self_ty).clone(),
            };

            let param_bounds = TraitBoundsVisitor::find(&item);

            impl_groups
                .entry(impl_group_id.clone())
                .or_default()
                .push((param_bounds, item));
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
                find_impl_groups(impl_group_id, &subsets, &mut supersets, &impl_groups)
                    .unwrap_or_else(|| {
                        let msg = format!(
                            "Conflicting implementations of `{}`",
                            quote!(#impl_group_id)
                        );

                        abort!(impl_group_id, msg)
                    })
            })
            .map(|(impl_group_id, (assoc_bindings_builder, impl_group))| {
                let mut generics = syn::Generics::default();

                if let Some(&item) = impl_group.first() {
                    generics.params = item.generics.params.clone();

                    generics.params.iter_mut().for_each(|mut param| {
                        if let syn::GenericParam::Type(syn::TypeParam { bounds, .. }) = &mut param {
                            *bounds = core::iter::empty::<syn::TypeParamBound>().collect();
                        }
                    });
                }

                generics.make_where_clause().predicates = assoc_bindings_builder
                    .trait_bounds
                    .into_iter()
                    .fold(
                        IndexMap::<_, IndexSet<_>>::new(),
                        |mut acc, (ident, bound)| {
                            acc.entry(ident).or_default().insert(bound);
                            acc
                        },
                    )
                    .into_iter()
                    .map(|(ident, bounds)| -> syn::WherePredicate {
                        let bounds = bounds.iter();
                        parse_quote! { #ident: #(#bounds) + * }
                    })
                    .collect();

                if impl_group_id.trait_.is_some() {
                    let assoc_bindings = &assoc_bindings_builder.assoc_bindings_group;
                    prune_unused_generics(&mut generics, impl_group_id, assoc_bindings);
                }

                (
                    impl_group_id.clone(),
                    ImplGroup {
                        id: impl_group_id.clone(),
                        item_impls: impl_group.into_iter().cloned().collect(),
                        assoc_bindings: assoc_bindings_builder.assoc_bindings_group,
                        generics,
                    },
                )
            })
            .collect();

        Ok(Self::new(main_trait, impl_groups))
    }
}

fn find_impl_groups<'a, 'b>(
    curr_impl_group_id: &'a ImplGroupId,

    impl_group_id_subsets: &Subsets<'a>,
    impl_group_id_supersets: &mut Supersets<'a>,

    item_impls: &'b IndexMap<ImplGroupId, Vec<(ItemImplDescriptor, ItemImpl)>>,
) -> Option<IndexMap<&'a ImplGroupId, (AssocBindingsGroupBuilder, Vec<&'b ItemImpl>)>> {
    find_impl_groups_rec(
        (curr_impl_group_id, 0),
        impl_group_id_subsets,
        impl_group_id_supersets,
        item_impls,
        &mut IndexMap::new(),
    )
}

fn find_impl_groups_rec<'a, 'b>(
    curr_impl: (&'a ImplGroupId, usize),

    impl_group_id_subsets: &Subsets<'a>,
    impl_group_id_supersets: &mut Supersets<'a>,

    item_impls: &'b IndexMap<ImplGroupId, Vec<(ItemImplDescriptor, ItemImpl)>>,
    impl_groups: &mut IndexMap<&'a ImplGroupId, (AssocBindingsGroupBuilder, Vec<&'b ItemImpl>)>,
) -> Option<IndexMap<&'a ImplGroupId, (AssocBindingsGroupBuilder, Vec<&'b ItemImpl>)>> {
    let (curr_impl_group_id, item_idx) = curr_impl;
    let curr_group = &item_impls[curr_impl_group_id];

    if let Some((curr_desc, curr_impl_item)) = curr_group.get(item_idx) {
        let impl_group_ids = impl_groups.keys().cloned().collect::<Vec<_>>();

        let mut acc = None::<IndexMap<_, _>>;
        let mut new_supersets = IndexMap::new();
        for impl_group_id in impl_group_ids {
            let impl_group = &impl_groups[impl_group_id];

            let impl_ = (curr_desc, curr_impl_item);
            if let Some(subs) = impl_group_id_subsets[impl_group_id].get(curr_impl_group_id) {
                impl_group.0.intersection(&impl_group.1, impl_, subs)
            } else if impl_group_id == curr_impl_group_id {
                let subs = &impl_group_id.is_superset(curr_impl_group_id).unwrap();
                impl_group.0.intersection(&impl_group.1, impl_, subs)
            } else {
                // NOTE: It's possible that current impl group is not a subset
                // of a group that comes before it in the topological order
                continue;
            }
            .collect::<Vec<_>>()
            .into_iter()
            .for_each(|intersection| {
                let impl_group = impl_groups.get_mut(impl_group_id).unwrap();
                let prev_assoc_bindings = core::mem::replace(&mut impl_group.0, intersection);
                impl_group.1.push(curr_impl_item);

                let mut supersets = impl_group_id_supersets.clone();
                let res = find_impl_groups_rec(
                    (curr_impl_group_id, item_idx + 1),
                    impl_group_id_subsets,
                    &mut supersets,
                    item_impls,
                    impl_groups,
                );

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
                    AssocBindingsGroupBuilder::new(curr_desc.clone()),
                    vec![curr_impl_item],
                ),
            );

            acc = find_impl_groups_rec(
                (curr_impl_group_id, item_idx + 1),
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

    item_impls: &'b IndexMap<ImplGroupId, Vec<(ItemImplDescriptor, ItemImpl)>>,
    impl_groups: &mut IndexMap<&'a ImplGroupId, (AssocBindingsGroupBuilder, Vec<&'b ItemImpl>)>,
) -> Option<IndexMap<&'a ImplGroupId, (AssocBindingsGroupBuilder, Vec<&'b ItemImpl>)>> {
    let mut acc = Some(impl_groups.clone());

    for (&subset_impl_group_id, _) in &impl_group_id_subsets[curr_impl_group_id] {
        let superset_count = impl_group_id_supersets
            .get_mut(subset_impl_group_id)
            .unwrap();

        *superset_count -= 1;
        if *superset_count == 0 {
            acc = acc.and_then(|mut impl_groups| {
                find_impl_groups_rec(
                    (subset_impl_group_id, 0),
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

fn gen_inherent_self_ty_args<'a>(
    self_ty: &mut syn::TypePath,
    generics: impl IntoIterator<Item = &'a syn::GenericParam>,
) {
    let last_seg = &mut self_ty.path.segments.last_mut().unwrap();

    if let syn::PathArguments::AngleBracketed(bracketed) = &mut last_seg.arguments {
        let mut lifetimes = Vec::new();
        let mut params = Vec::new();

        generics.into_iter().for_each(|param| match param {
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
        let impl_groups: IndexMap<ImplGroupId, Vec<(ItemImplDescriptor, ItemImpl)>> = [
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
                ItemImplDescriptor {
                    trait_bounds: [(parse_quote!(#bounded_ty), parse_quote!(Dispatch))]
                        .into_iter()
                        .collect(),
                    assoc_bindings: [(
                        (
                            (parse_quote!(#bounded_ty), parse_quote!(Dispatch)),
                            parse_quote!(Group),
                        ),
                        parse_quote!(#assoc_ty),
                    )]
                    .into_iter()
                    .collect(),
                },
                parse_quote! {
                    impl<_ŠČ> Kita for #ty where #bounded_ty: Dispatch<Group = #assoc_ty> {
                        const NAME: &'static str = #msg;
                    }
                },
            )
        })
        .map(|(group_id, impl_item_descriptor, impl_item)| {
            (group_id, vec![(impl_item_descriptor, impl_item)])
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
                find_impl_groups(impl_group_id, &subsets, &mut supersets, &impl_groups).unwrap()
            })
            .collect::<Vec<_>>();

        assert_eq!(impl_groups.len(), 1);
        // TODO: Check the output as well
    }
}
