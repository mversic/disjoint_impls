use core::iter::zip;

use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use param::get_param_ident;
use proc_macro::TokenStream;
use proc_macro_error2::{abort, proc_macro_error};
use proc_macro2::TokenStream as TokenStream2;
use quote::{ToTokens, format_ident, quote};
use superset::Substitutions;
use syn::ItemImpl;
use syn::visit::Visit;
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
mod validate;

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
struct ImplGroupId(Option<syn::Path>, syn::Type);

/// Body of the [`disjoint_impls`] macro
#[derive(Debug)]
struct ImplGroups {
    /// Definition of the main trait. [`None`] for inherent impls
    item_trait_: Option<ItemTrait>,
    /// Collection of [`ItemImpl`] blocks grouped by [`ImplGroupId`]
    /// Each impl group is dispatched on a set of associated bounds
    // TODO: Can it be a Vec?
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
        Self(syn::parse_quote!(#source))
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

type TraitBoundIdent = (Bounded, TraitBound);

#[derive(Debug, Clone, Default)]
struct ItemImplBounds(
    /// Param bounds
    // NOTE: Order is preserved as found in the implementation
    Vec<(TraitBoundIdent, Vec<(syn::Ident, AssocBoundPayload)>)>,
    /// Unsized params, i.e. params that implement ?Sized
    IndexSet<Bounded>,
);

#[derive(Debug, Clone)]
struct AssocBoundsGroup {
    // TODO: Move to Vec of payloads
    //bounds: IndexMap<TraitBoundIdent, IndexMap<syn::Ident, Vec<AssocBoundPayload>>>,
    bounds: IndexMap<TraitBoundIdent, Vec<IndexMap<syn::Ident, AssocBoundPayload>>>,
    /// Params that implement ?Sized
    unsized_params: IndexSet<Bounded>,
}

impl AssocBoundsGroup {
    fn new(impl_bounds: ItemImplBounds) -> Self {
        let bounds = impl_bounds.0.into_iter().fold(
            IndexMap::<_, Vec<_>>::new(),
            |mut acc, (trait_bound_id, assoc_bounds)| {
                acc.entry(trait_bound_id)
                    .or_insert_with(|| vec![IndexMap::new()])[0]
                    .extend(assoc_bounds);

                acc
            },
        );

        Self {
            bounds,
            unsized_params: impl_bounds.1,
        }
    }

    fn prune_non_assoc(&mut self) {
        self.bounds = core::mem::take(&mut self.bounds)
            .into_iter()
            .filter(|(_, impl_assoc_bounds)| {
                impl_assoc_bounds
                    .iter()
                    .any(|assoc_bounds| !assoc_bounds.is_empty())
            })
            .collect();
    }

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
        self.bounds.is_empty()
    }

    fn idents(&self) -> impl Iterator<Item = AssocBoundIdent> {
        self.bounds
            .iter()
            .flat_map(|(trait_bound, impl_assoc_bounds)| {
                impl_assoc_bounds
                    .iter()
                    .fold(IndexSet::new(), |mut acc, assoc_bounds| {
                        assoc_bounds.iter().for_each(|assoc_bound| {
                            acc.insert(assoc_bound.0);
                        });

                        acc
                    })
                    .into_iter()
                    .map(move |ident| (trait_bound, ident))
            })
    }

    fn payloads(&self) -> impl Iterator<Item = Vec<Option<&AssocBoundPayload>>> {
        let mut impl_item_idx = 0;

        core::iter::from_fn(move || {
            let mut assoc_bounds_map = IndexMap::new();

            if impl_item_idx > 0 && self.bounds.is_empty() {
                return None;
            }

            for (trait_bound_id, impl_assoc_bounds) in &self.bounds {
                if impl_item_idx >= impl_assoc_bounds.len() {
                    return None;
                }

                assoc_bounds_map.extend(
                    impl_assoc_bounds[impl_item_idx]
                        .iter()
                        .map(|(ident, payload)| ((trait_bound_id, ident), Some(payload))),
                );
            }

            impl_item_idx += 1;
            let impl_payloads = self.idents().map(|assoc_bound_ident| {
                assoc_bounds_map
                    .swap_remove(&assoc_bound_ident)
                    .unwrap_or_default()
            });

            // TODO: Can I avoid collecting here?
            Some(Iterator::collect::<Vec<_>>(impl_payloads))
        })
    }

    fn intersection(
        &self,
        other: &ItemImplBounds,
        substitutions: &Substitutions,
    ) -> impl Iterator<Item = Self> + use<> {
        let mut unsized_params = self.unsized_params.clone();
        unsized_params.extend(other.1.clone());

        let other = other.0.iter().fold(
            IndexMap::<_, IndexMap<_, _>>::new(),
            |mut acc, (trait_bound_id, assoc_bounds)| {
                let entry = acc.entry(trait_bound_id).or_default();
                entry.extend(assoc_bounds.iter().map(|(k, v)| (k, v)));

                acc
            },
        );

        other
            .into_iter()
            .map(|(other_trait_bound, other_assoc_bounds)| {
                substitutions
                    .substitute(other_trait_bound)
                    .map(|subs_trait_bound| {
                        let found_bound = self.bounds.get(&subs_trait_bound).cloned();

                        found_bound.map(|mut impl_assoc_bounds| {
                            let other_assoc_bounds = other_assoc_bounds
                                .iter()
                                .map(|(&ident, &payload)| (ident.clone(), payload.clone()))
                                .collect();

                            impl_assoc_bounds.push(other_assoc_bounds);
                            (subs_trait_bound, impl_assoc_bounds)
                        })
                    })
                    .collect::<Vec<_>>()
            })
            .multi_cartesian_product()
            .map(move |bounds| {
                let mut unsized_params = unsized_params.clone();

                let bounds: IndexMap<_, _> = bounds.into_iter().flatten().collect();
                let params = bounds.keys().map(|(param, _)| param).collect::<Vec<_>>();

                unsized_params.retain(|param| params.contains(&param));

                Self {
                    bounds,
                    unsized_params,
                }
            })
    }
}

/// Unique name based identifier of the associated type bound such as:
///     `(T, Deref, Deref::Target)` in `impl<T: Deref<Target = bool>> for Clone for T`
type AssocBoundIdent<'a> = (&'a TraitBoundIdent, &'a syn::Ident);

/// AST node type of the associated bound constraint such as:
///     `bool` in `impl<T: Deref<Target = bool>> for Clone for T`
// TODO: how to support GATs? make a test
type AssocBoundPayload = syn::Type;

#[derive(Default)]
struct TraitBoundsVisitor {
    /// Type parameter currently being visited
    curr_type_param: Option<Bounded>,
    /// Trait bound currently being visited
    curr_trait_bound: Option<TraitBound>,

    /// Bounds of an impl block
    param_bounds: ItemImplBounds,
}

impl TraitBoundsVisitor {
    fn find(generics: &syn::Generics) -> ItemImplBounds {
        let mut visitor = TraitBoundsVisitor::default();
        visitor.visit_generics(generics);
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
        self.curr_type_param = Some((&node.ident).into());
        syn::visit::visit_type_param(self, node);
    }

    fn visit_predicate_type(&mut self, node: &syn::PredicateType) {
        self.curr_type_param = Some((&node.bounded_ty).into());
        syn::visit::visit_predicate_type(self, node);
    }

    fn visit_trait_bound(&mut self, node: &syn::TraitBound) {
        self.curr_trait_bound = Some(node.path.clone().into());

        let curr_trait_bound_ident = (
            self.curr_type_param.clone().unwrap(),
            self.curr_trait_bound.clone().unwrap(),
        );

        self.param_bounds
            .0
            .push((curr_trait_bound_ident, Vec::new()));

        if let syn::TraitBoundModifier::Maybe(_) = &node.modifier {
            self.param_bounds
                .1
                .insert(self.curr_type_param.clone().unwrap());
        }

        syn::visit::visit_trait_bound(self, node);
    }

    fn visit_assoc_type(&mut self, node: &syn::AssocType) {
        let curr_trait_bound_ident = (
            self.curr_type_param.take().unwrap(),
            self.curr_trait_bound.take().unwrap(),
        );

        self.param_bounds
            .0
            .last_mut()
            .unwrap()
            .1
            .push((node.ident.clone(), node.ty.clone()));

        self.curr_type_param = Some(curr_trait_bound_ident.0);
        self.curr_trait_bound = Some(curr_trait_bound_ident.1);
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

    let main_trait = impls.item_trait_;
    for (impl_group_idx, impl_group) in impls.impl_groups.into_values().enumerate() {
        helper_traits.push(helper_trait::generate(
            main_trait.as_ref(),
            impl_group_idx,
            &impl_group,
        ));

        main_trait_impls.push(main_trait::generate(
            main_trait.as_ref(),
            impl_group_idx,
            &impl_group,
        ));

        item_impls.extend(disjoint::generate(impl_group_idx, impl_group));
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

#[derive(Debug)]
struct ImplGroup {
    // NOTE: The first impl is the ID of the group
    item_impls: Vec<syn::ItemImpl>,
    assoc_bounds: AssocBoundsGroup,
}

impl Parse for ImplGroups {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        let mut impl_groups = IndexMap::<_, IndexMap<_, _>>::new();

        let main_trait = input.parse::<ItemTrait>().ok();
        while let Ok(mut item) = input.parse::<ItemImpl>() {
            param::resolve_non_predicate_params(&mut item);

            let impl_group_id = ImplGroupId(
                item.trait_.as_ref().map(|trait_| &trait_.1).cloned(),
                (*item.self_ty).clone(),
            );

            let param_bounds = TraitBoundsVisitor::find(&item.generics);

            impl_groups
                .entry(impl_group_id)
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

                find_impl_group_candidates(
                    (impl_group_id, &impl_group),
                    &subsets,
                    &mut supersets,
                    &impl_groups,
                )
                .reduce(|mut acc, impl_groups| {
                    // TODO: How to choose minimal impl group set?
                    // ATM, one of the smallest sets is chosen
                    if acc.len() >= impl_groups.len() {
                        acc = impl_groups;
                    }

                    acc
                })
                // TODO: Handle error properly
                .unwrap_or_else(|| panic!("Unable to form impl group for {:?}", impl_group_id))
            })
            .map(|(impl_group_id, (assoc_bounds, impl_group))| {
                let item_impls = impl_group.into_iter().cloned().collect();
                (
                    impl_group_id.clone(),
                    ImplGroup {
                        assoc_bounds,
                        item_impls,
                    },
                )
            })
            .collect();

        Ok(ImplGroups::new(main_trait, impl_groups))
    }
}

// TODO: Can dynamic programming look up table be applied?
fn find_impl_group_candidates<'a, 'b>(
    curr_impl_group: (&'a ImplGroupId, &[&'b ItemImpl]),

    impl_group_id_subsets: &Subsets<'a>,
    impl_group_id_supersets: &mut Supersets<'a>,

    item_impls: &'b IndexMap<ImplGroupId, IndexMap<ItemImpl, ItemImplBounds>>,
) -> impl Iterator<Item = IndexMap<&'a ImplGroupId, (AssocBoundsGroup, Vec<&'b ItemImpl>)>> {
    find_impl_group_candidates_rec(
        curr_impl_group,
        impl_group_id_subsets,
        impl_group_id_supersets,
        item_impls,
        &mut IndexMap::new(),
    )
    .into_iter()
    .filter_map(|mut impl_groups| {
        for (assoc_bounds_group, impls) in impl_groups.values_mut() {
            assoc_bounds_group.prune_non_assoc();

            if assoc_bounds_group.is_empty() && impls.len() > 1 {
                return None;
            }
            if assoc_bounds_group.is_overlapping() {
                return None;
            }
        }

        Some(impl_groups)
    })
}

fn find_impl_group_candidates_rec<'a, 'b>(
    curr_impl_group: (&'a ImplGroupId, &[&'b ItemImpl]),

    impl_group_id_subsets: &Subsets<'a>,
    impl_group_id_supersets: &mut Supersets<'a>,

    item_impls: &'b IndexMap<ImplGroupId, IndexMap<ItemImpl, ItemImplBounds>>,
    impl_groups: &mut IndexMap<&'a ImplGroupId, (AssocBoundsGroup, Vec<&'b ItemImpl>)>,
) -> Vec<IndexMap<&'a ImplGroupId, (AssocBoundsGroup, Vec<&'b ItemImpl>)>> {
    let curr_impl_group_id = curr_impl_group.0;

    if let Some((&curr_impl, other)) = curr_impl_group.1.split_first() {
        let mut new_supersets = IndexMap::new();

        let curr_impl_bounds = &item_impls[curr_impl_group_id][curr_impl];
        let impl_group_ids = impl_groups.keys().cloned().collect::<Vec<_>>();

        let mut acc = Vec::new();
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
                let prev_assoc_bounds = core::mem::replace(&mut impl_group.0, intersection);
                impl_group.1.push(curr_impl);

                let mut supersets = impl_group_id_supersets.clone();
                let res = find_impl_group_candidates_rec(
                    (curr_impl_group_id, other),
                    impl_group_id_subsets,
                    &mut supersets,
                    item_impls,
                    impl_groups,
                );

                if !res.is_empty() {
                    new_supersets = supersets;
                    acc.extend(res);
                }

                // NOTE: Restore changes to the impl groups set for the next iteration
                let impl_group_to_restore = impl_groups.get_mut(impl_group_id).unwrap();
                impl_group_to_restore.0 = prev_assoc_bounds;
                impl_group_to_restore.1.pop();
            });
        }

        // NOTE: Create a new group if it doesn't exist yet
        if !impl_groups.contains_key(curr_impl_group_id) {
            let mut supersets = impl_group_id_supersets.clone();

            impl_groups.insert(
                curr_impl_group_id,
                (
                    AssocBoundsGroup::new(curr_impl_bounds.clone()),
                    vec![curr_impl],
                ),
            );

            let res = find_impl_group_candidates_rec(
                (curr_impl_group_id, other),
                impl_group_id_subsets,
                &mut supersets,
                item_impls,
                impl_groups,
            );

            if !res.is_empty() {
                new_supersets = supersets;
                acc.extend(res);
            }

            // NOTE: Restore changes to impl groups
            impl_groups.swap_remove(curr_impl_group_id);
        }

        if !acc.is_empty() {
            *impl_group_id_supersets = new_supersets;
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
    impl_groups: &mut IndexMap<&'a ImplGroupId, (AssocBoundsGroup, Vec<&'b ItemImpl>)>,
) -> Vec<IndexMap<&'a ImplGroupId, (AssocBoundsGroup, Vec<&'b ItemImpl>)>> {
    let mut acc = vec![impl_groups.clone()];

    for (&subset_impl_group_id, _) in &impl_group_id_subsets[curr_impl_group_id] {
        let subset = &item_impls[subset_impl_group_id].keys().collect::<Vec<_>>();

        let superset_count = impl_group_id_supersets
            .get_mut(subset_impl_group_id)
            .unwrap();

        *superset_count -= 1;
        if *superset_count == 0 {
            let old_supersets = impl_group_id_supersets.clone();

            acc = acc
                .into_iter()
                .flat_map(|mut impl_groups| {
                    *impl_group_id_supersets = old_supersets.clone();

                    find_impl_group_candidates_rec(
                        (subset_impl_group_id, subset),
                        impl_group_id_subsets,
                        impl_group_id_supersets,
                        item_impls,
                        &mut impl_groups,
                    )
                })
                .collect();
        }
    }

    acc
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
    use syn::parse_quote;

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
                ImplGroupId(Some(syn::parse_quote!(Kita)), parse_quote!(#ty)),
                parse_quote! {
                    impl<_ŠČ> Kita for #ty where #bounded_ty: Dispatch<Group = #assoc_ty> {
                        const NAME: &'static str = #msg;
                    }
                },
                ItemImplBounds(
                    vec![(
                        (
                            Bounded(parse_quote!(#bounded_ty)),
                            TraitBound(parse_quote!(Dispatch<Group = #assoc_ty>)),
                        ),
                        vec![(parse_quote!(Group), parse_quote!(#assoc_ty))],
                    )],
                    IndexSet::default(),
                ),
            )
        })
        .map(|(group_id, impl_item, assoc_bounds)| {
            let mut map = IndexMap::new();
            map.insert(impl_item, assoc_bounds);
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

                find_impl_group_candidates(
                    (impl_group_id, &impl_group),
                    &subsets,
                    &mut supersets,
                    &impl_groups,
                )
                .reduce(|mut acc, impl_groups| {
                    if acc.len() >= impl_groups.len() {
                        acc = impl_groups;
                    }

                    acc
                })
                .unwrap()
            })
            .collect::<Vec<_>>();

        assert_eq!(impl_groups.len(), 1);
        // TODO: Check the output as well
    }
}
