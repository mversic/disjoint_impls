mod disjoint;
mod main_trait;
mod validate;

use core::iter::zip;

use indexmap::{IndexMap, IndexSet};
use param::get_param_ident;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use proc_macro_error::{abort, proc_macro_error};
use quote::{format_ident, quote, ToTokens};
use syn::visit::Visit;
use syn::ItemImpl;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, ItemTrait,
};

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

        let mut iter = self.0.segments.iter().rev();
        let last_elem = iter.next().unwrap();

        iter.rev().for_each(|elem| elem.to_tokens(tokens));
        last_elem.ident.to_tokens(tokens);

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

// NOTE: Order is preserved as found in the implementation
type AssocBounds = Vec<(TraitBoundIdent, Vec<(syn::Ident, AssocBoundPayload)>)>;

#[derive(Debug, Clone)]
struct AssocBoundsGroup {
    bounds: IndexMap<TraitBoundIdent, Vec<IndexMap<syn::Ident, AssocBoundPayload>>>,
}

impl AssocBoundsGroup {
    fn new(impl_bounds: AssocBounds) -> Self {
        let bounds = impl_bounds.into_iter().fold(
            IndexMap::<_, Vec<_>>::new(),
            |mut acc, (trait_bound_id, assoc_bounds)| {
                acc.entry(trait_bound_id)
                    .or_insert_with(|| vec![IndexMap::new()])[0]
                    .extend(assoc_bounds);

                acc
            },
        );

        Self { bounds }
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
            Some(impl_payloads).map(Iterator::collect::<Vec<_>>)
        })
    }

    fn intersection(&self, other: &AssocBounds) -> Option<Self> {
        let other = other.into_iter().fold(
            IndexMap::<_, IndexMap<_, _>>::new(),
            |mut acc, (trait_bound_id, assoc_bounds)| {
                let entry = acc.entry(trait_bound_id).or_default();
                entry.extend(assoc_bounds.iter().map(|(k, v)| (k, v)));

                acc
            },
        );

        let set1 = self.bounds.keys().collect::<IndexSet<_>>();
        let set2 = other
            .iter()
            .map(|(&trait_bound_ident, _)| trait_bound_ident)
            .collect::<IndexSet<_>>();

        let bounds = (&set1 & &set2)
            .into_iter()
            .map(|trait_bound| {
                let mut impl_assoc_bounds = self.bounds[trait_bound].clone();

                impl_assoc_bounds.push(
                    other[trait_bound]
                        .iter()
                        .map(|(&trait_bound_ident, &assoc_bounds)| {
                            (trait_bound_ident.clone(), assoc_bounds.clone())
                        })
                        .collect(),
                );

                (trait_bound.clone(), impl_assoc_bounds)
            })
            .collect::<IndexMap<_, _>>();

        if bounds.is_empty() {
            return None;
        }

        Some(Self { bounds })
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

    /// Collection of type param bounds
    type_param_bounds: AssocBounds,
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

        self.type_param_bounds
            .push((curr_trait_bound_ident, Vec::new()));

        syn::visit::visit_trait_bound(self, node);
    }

    fn visit_assoc_type(&mut self, node: &syn::AssocType) {
        let curr_trait_bound_ident = (
            self.curr_type_param.take().unwrap(),
            self.curr_trait_bound.take().unwrap(),
        );

        self.type_param_bounds
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

#[derive(Debug)]
struct ImplGroup {
    // NOTE: The first impl is the ID of the group
    item_impls: Vec<syn::ItemImpl>,
    assoc_bounds: AssocBoundsGroup,
}

trait IsSuperset {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions>;
}

#[derive(PartialEq, Eq)]
enum SubstitutionValue<'a> {
    Type(&'a syn::Type),
    Expr(&'a syn::Expr),
}

impl<'a> From<&'a syn::Type> for SubstitutionValue<'a> {
    fn from(value: &'a syn::Type) -> Self {
        Self::Type(value)
    }
}

impl<'a> From<&'a syn::Expr> for SubstitutionValue<'a> {
    fn from(value: &'a syn::Expr) -> Self {
        Self::Expr(value)
    }
}

#[derive(Default)]
struct Substitutions<'a>(IndexMap<&'a syn::Ident, SubstitutionValue<'a>>);

impl<'a> Substitutions<'a> {
    fn new(source: &'a syn::Ident, dest: impl Into<SubstitutionValue<'a>>) -> Self {
        let mut substitutions = IndexMap::new();
        substitutions.insert(source, dest.into());

        Self(substitutions)
    }

    #[must_use]
    fn merge(mut self, other: Self) -> Option<Self> {
        for (ident, dest) in other.0.into_iter() {
            match self.0.entry(ident) {
                indexmap::map::Entry::Occupied(val) => {
                    if dest != *val.get() {
                        return None;
                    }
                }
                indexmap::map::Entry::Vacant(entry) => {
                    entry.insert(dest);
                }
            }
        }

        Some(self)
    }
}

impl IsSuperset for syn::Type {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        use syn::Type::*;

        match (self, other) {
            (Ptr(x1), Ptr(x2)) => {
                if x1.const_token != x2.const_token || x1.mutability != x2.mutability {
                    // TODO: Can *mut T be considered a superset of *const T? Make a test
                    return None;
                }

                x1.elem.is_superset(&x2.elem)
            }
            (Reference(x1), Reference(x2)) => {
                if x1.lifetime != x2.lifetime {
                    // TODO: Can &mut T be considered a superset of &T? Make a test
                    // TODO: How do lifetimes come into picture for supersets? Make a test
                    return None;
                }
                if x1.mutability != x2.mutability {
                    return None;
                }

                x1.elem.is_superset(&x2.elem)
            }
            (Tuple(x1), Tuple(x2)) => {
                if x1.elems.len() != x2.elems.len() {
                    return None;
                }

                zip(&x1.elems, &x2.elems).try_fold(Substitutions::default(), |acc, (x1, x2)| {
                    acc.merge(x1.is_superset(x2)?)
                })
            }
            (Array(x1), Array(x2)) => {
                if x1.len != x2.len {
                    return None;
                }

                x1.elem.is_superset(&x2.elem)
            }
            (Path(x1), x2) => {
                if let Some(ident) = matches_param_ident(&x1.path) {
                    return Some(Substitutions::new(ident, x2));
                }

                if let syn::Type::Path(x2) = x2 {
                    // TODO: Equality is not well defined (for example `syn::TypeParen` or `syn::TypeGroup`)
                    if x1.qself != x2.qself {
                        // NOTE: It's not possible to tell whether 2 associated types overlap or not
                        // Is <Vec<T> as Deref>::Target a superset of <Option<T> as Deref>::Target?
                        return None;
                    }

                    return x1.path.is_superset(&x2.path);
                }

                None
            }
            (BareFn(x1), BareFn(x2)) => {
                // TODO: compare properly
                if x1 == x2 {
                    return Some(Substitutions::default());
                }

                None
            }
            (TraitObject(x1), TraitObject(x2)) => {
                // TODO: How to compare trait objects?
                if x1 == x2 {
                    return Some(Substitutions::default());
                }

                None
            }
            (Slice(x1), Slice(x2)) => x1.elem.is_superset(&x2.elem),
            (Paren(x1), x2) => x1.elem.is_superset(x2),
            (x1, Paren(x2)) => x1.is_superset(&x2.elem),
            (Group(x1), x2) => x1.elem.is_superset(x2),
            (x1, Group(x2)) => x1.is_superset(&x2.elem),
            (ImplTrait(_), ImplTrait(_)) | (Infer(_), Infer(_)) => {
                // NOTE: Not possible in impl declaration
                unreachable!()
            }
            (x1, x2) => (x1 == x2).then_some(Substitutions::default()),
        }
    }
}

fn matches_param_ident(path: &syn::Path) -> Option<&syn::Ident> {
    if let Some(ident) = path.get_ident() {
        if ident.to_string().starts_with("_ŠČ") {
            return Some(ident);
        }
    }

    None
}

impl IsSuperset for syn::Path {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.segments.len() != other.segments.len() {
            return None;
        }

        let mut substitutions = Substitutions::default();
        for (x1, x2) in zip(&self.segments, &other.segments) {
            if x1.ident != x2.ident {
                return None;
            }

            use syn::PathArguments;
            match (&x1.arguments, &x2.arguments) {
                (PathArguments::None, PathArguments::None) => {}
                (PathArguments::Parenthesized(x1), PathArguments::Parenthesized(x2)) => {
                    if x1.inputs.len() != x2.inputs.len() {
                        return None;
                    }

                    substitutions = zip(&x1.inputs, &x2.inputs)
                        .try_fold(substitutions, |acc, (x1, x2)| {
                            acc.merge(x1.is_superset(x2)?)
                        })?;

                    match (&x1.output, &x2.output) {
                        (syn::ReturnType::Default, syn::ReturnType::Default) => {}
                        (syn::ReturnType::Type(_, x1), syn::ReturnType::Type(_, x2)) => {
                            substitutions = substitutions.merge(x1.is_superset(x2)?)?;
                        }
                        _ => return None,
                    }
                }
                (PathArguments::AngleBracketed(x1), PathArguments::AngleBracketed(x2)) => {
                    if x1.args.len() != x2.args.len() {
                        return None;
                    }

                    use syn::GenericArgument::*;
                    substitutions = zip(&x1.args, &x2.args).try_fold(
                        substitutions,
                        |acc, (x1, x2)| match (x1, x2) {
                            (Const(x1), Const(x2)) => acc.merge(x1.is_superset(x2)?),
                            (Lifetime(x1), Lifetime(x2)) => (x1 == x2).then_some(acc),
                            (Type(x1), Type(x2)) => acc.merge(x1.is_superset(x2)?),
                            (Type(syn::Type::Path(x1)), Const(x2)) => {
                                if let Some(ident) = matches_param_ident(&x1.path) {
                                    return acc.merge(Substitutions::new(ident, x2));
                                }

                                None
                            }
                            (AssocType(_), _)
                            | (_, AssocType(_))
                            | (AssocConst(_), _)
                            | (_, AssocConst(_))
                            | (Constraint(_), _)
                            | (_, Constraint(_)) => unreachable!(),
                            _ => return None,
                        },
                    )?;
                }
                _ => return None,
            }
        }

        Some(substitutions)
    }
}

impl IsSuperset for syn::Expr {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        use syn::Expr::*;

        match (self, other) {
            (Array(x1), Array(x2)) => {
                if x1.elems.len() != x2.elems.len() {
                    return None;
                }

                zip(&x1.elems, &x2.elems).try_fold(Substitutions::default(), |acc, (x1, x2)| {
                    acc.merge(x1.is_superset(x2)?)
                })
            }
            (Assign(x1), Assign(x2)) => x1
                .left
                .is_superset(&x2.left)?
                .merge(x1.right.is_superset(&x2.right)?),
            (Binary(x1), Binary(x2)) => {
                if x1.op != x2.op {
                    return None;
                }

                x1.left
                    .is_superset(&x2.left)?
                    .merge(x1.right.is_superset(&x2.right)?)
            }
            (Break(x1), Break(x2)) => match (&x1.expr, &x2.expr) {
                (Some(x1), Some(x2)) => x1.is_superset(x2),
                (None, None) => Some(Substitutions::default()),
                _ => None,
            },
            (Call(x1), Call(x2)) => {
                if x1.args.len() != x2.args.len() {
                    return None;
                }

                zip(&x1.args, &x2.args).try_fold(x1.func.is_superset(&x2.func)?, |acc, (x1, x2)| {
                    acc.merge(x1.is_superset(x2)?)
                })
            }
            (Cast(x1), Cast(x2)) => x1
                .expr
                .is_superset(&x2.expr)?
                .merge(x1.ty.is_superset(&x2.ty)?),
            (Path(x1), x2) => {
                if let Some(ident) = matches_param_ident(&x1.path) {
                    return Some(Substitutions::new(ident, x2));
                }

                if let syn::Expr::Path(x2) = x2 {
                    // TODO: Equality is not well defined (for example `syn::TypeParen` or `syn::TypeGroup`)
                    if x1.qself != x2.qself {
                        // NOTE: It's not possible to tell whether 2 associated types overlap or not
                        // Is <Vec<T> as Deref>::Target a superset of <Option<T> as Deref>::Target?
                        return None;
                    }

                    return x1.path.is_superset(&x2.path);
                }

                None
            }
            (Return(x1), Return(x2)) => match (&x1.expr, &x2.expr) {
                (None, None) => Some(Substitutions::default()),
                (Some(x1), Some(x2)) => x1.is_superset(x2),
                _ => None,
            },
            (Tuple(x1), Tuple(x2)) => {
                if x1.elems.len() != x2.elems.len() {
                    return None;
                }

                zip(&x1.elems, &x2.elems).try_fold(Substitutions::default(), |acc, (x1, x2)| {
                    acc.merge(x1.is_superset(x2)?)
                })
            }
            (Continue(x1), Continue(x2)) => {
                if x1.label == x2.label {
                    return Some(Substitutions::default());
                }

                None
            }
            (Group(x1), x2) => x1.expr.is_superset(x2),
            (x1, Group(x2)) => x1.is_superset(&x2.expr),
            (Index(x1), Index(x2)) => x1
                .expr
                .is_superset(&x2.expr)?
                .merge(x1.index.is_superset(&x2.index)?),
            (Infer(x1), Infer(x2)) => Some(Substitutions::default()),
            (Field(x1), Field(x2)) => unimplemented!(),
            (ForLoop(x1), ForLoop(x2)) => unimplemented!(),
            (If(x1), If(x2)) => unimplemented!(),
            (Let(x1), Let(x2)) => unimplemented!(),
            (Loop(x1), Loop(x2)) => unimplemented!(),
            (Macro(x1), Macro(x2)) => unimplemented!(),
            (Match(x1), Match(x2)) => unimplemented!(),
            (MethodCall(x1), MethodCall(x2)) => unimplemented!(),
            (Paren(x1), Paren(x2)) => unimplemented!(),
            (Range(x1), Range(x2)) => unimplemented!(),
            (Reference(x1), Reference(x2)) => unimplemented!(),
            (Repeat(x1), Repeat(x2)) => unimplemented!(),
            (Struct(x1), Struct(x2)) => unimplemented!(),
            (Try(x1), Try(x2)) => unimplemented!(),
            (TryBlock(x1), TryBlock(x2)) => unimplemented!(),
            (Unary(x1), Unary(x2)) => unimplemented!(),
            (Unsafe(x1), Unsafe(x2)) => unimplemented!(),
            (Verbatim(x1), Verbatim(x2)) => unimplemented!(),
            (While(x1), While(x2)) => unimplemented!(),
            (Closure(x1), Closure(x2)) => unimplemented!(),
            (Const(x1), Const(x2)) => unimplemented!(),
            (Block(x1), Block(x2)) => unimplemented!(),
            (Yield(x1), Yield(x2)) => unimplemented!(),
            (Async(x1), Async(x2)) => unimplemented!(),
            (Await(x1), Await(x2)) => unimplemented!(),
            (x1, x2) => (x1 == x2).then_some(Substitutions::default()),
        }
    }
}
impl IsSuperset for ImplGroupId {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        match (&self.0, &other.0) {
            (Some(x1), Some(x2)) => x1.is_superset(x2)?,
            (None, None) => Substitutions::default(),
            _ => return None,
        }
        .merge(self.1.is_superset(&other.1)?)
    }
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

            let type_param_bounds = {
                let mut visitor = TraitBoundsVisitor::default();
                visitor.visit_generics(&item.generics);
                visitor.type_param_bounds
            };

            impl_groups
                .entry(impl_group_id)
                .or_default()
                .insert(item, type_param_bounds);
        }

        let (mut supersets, subsets) = impl_groups
            .keys()
            .flat_map(|g1| impl_groups.keys().map(move |g2| (g1, g2)))
            // TODO: Equality is not well defined (for example `syn::TypeParen`)
            .filter(|(g1, g2)| g1 != g2 && g1.is_superset(g2).is_some())
            .fold(
                (
                    impl_groups
                        .keys()
                        .map(|id| (id, 0))
                        .collect::<IndexMap<_, _>>(),
                    impl_groups
                        .keys()
                        .map(|id| (id, Vec::new()))
                        .collect::<IndexMap<_, _>>(),
                ),
                |(mut supersets, mut subsets), (g1, g2)| {
                    let superset_entry = supersets.get_mut(g2).unwrap();
                    let subset_entry = subsets.get_mut(g1).unwrap();

                    *superset_entry += 1;
                    subset_entry.push(g2);

                    (supersets, subsets)
                },
            );

        let impl_groups = {
            let impl_groups = impl_groups.iter().collect::<IndexMap<_, _>>();

            supersets
                .iter()
                .filter_map(|(&impl_group_id, superset_count)| {
                    if *superset_count <= 0 {
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
                        if acc.len() < impl_groups.len() {
                            acc = impl_groups;
                        }

                        acc
                    })
                    // TODO: Handle error properly
                    .expect(&format!(
                        "Unable to form impl group for {:?}",
                        impl_group_id
                    ))
                })
                .into_iter()
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
                .collect()
        };

        Ok(ImplGroups::new(main_trait, impl_groups))
    }
}

// TODO: Can dynamic programming look up table be applied?
fn find_impl_group_candidates<'a, 'b>(
    curr_impl_group: (&'a ImplGroupId, &[&'b ItemImpl]),

    impl_group_id_subsets: &IndexMap<&ImplGroupId, Vec<&'a ImplGroupId>>,
    impl_group_id_supersets: &mut IndexMap<&ImplGroupId, usize>,

    item_impls: &'b IndexMap<&ImplGroupId, &IndexMap<ItemImpl, AssocBounds>>,
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
        for (assoc_bounds_group, _) in impl_groups.values_mut() {
            assoc_bounds_group.prune_non_assoc();

            if assoc_bounds_group.is_empty() {
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

    impl_group_id_subsets: &IndexMap<&ImplGroupId, Vec<&'a ImplGroupId>>,
    impl_group_id_supersets: &mut IndexMap<&ImplGroupId, usize>,

    item_impls: &'b IndexMap<&ImplGroupId, &IndexMap<ItemImpl, AssocBounds>>,
    impl_groups: &mut IndexMap<&'a ImplGroupId, (AssocBoundsGroup, Vec<&'b ItemImpl>)>,
) -> Vec<IndexMap<&'a ImplGroupId, (AssocBoundsGroup, Vec<&'b ItemImpl>)>> {
    let curr_impl_group_id = curr_impl_group.0;

    if let Some((&curr_impl, other)) = curr_impl_group.1.split_first() {
        let mut new_supersets = IndexMap::new();

        let curr_impl_type_param_bounds = &item_impls[curr_impl_group_id][curr_impl];
        let impl_group_ids = impl_groups.keys().cloned().collect::<Vec<_>>();

        let mut acc = Vec::new();
        for impl_group_id in impl_group_ids {
            let impl_group = impl_groups.get_mut(impl_group_id).unwrap();

            // TODO: Do substitution before intersection!
            if let Some(intersection) = impl_group.0.intersection(curr_impl_type_param_bounds) {
                let mut supersets = impl_group_id_supersets.clone();

                let prev_assoc_bounds_group = core::mem::replace(&mut impl_group.0, intersection);
                impl_group.1.push(curr_impl);

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
                impl_group_to_restore.0 = prev_assoc_bounds_group;
                impl_group_to_restore.1.pop();
            }
        }

        // NOTE: Create a new group if it doesn't exist yet
        if !impl_groups.contains_key(curr_impl_group_id) {
            let mut supersets = impl_group_id_supersets.clone();

            impl_groups.insert(
                curr_impl_group_id,
                (
                    AssocBoundsGroup::new(curr_impl_type_param_bounds.clone()),
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

        if !new_supersets.is_empty() {
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

    impl_group_id_subsets: &IndexMap<&ImplGroupId, Vec<&'a ImplGroupId>>,
    impl_group_id_supersets: &mut IndexMap<&ImplGroupId, usize>,

    item_impls: &'b IndexMap<&ImplGroupId, &IndexMap<ItemImpl, AssocBounds>>,
    impl_groups: &mut IndexMap<&'a ImplGroupId, (AssocBoundsGroup, Vec<&'b ItemImpl>)>,
) -> Vec<IndexMap<&'a ImplGroupId, (AssocBoundsGroup, Vec<&'b ItemImpl>)>> {
    let mut acc = vec![impl_groups.clone()];

    for &subset_impl_group_id in &impl_group_id_subsets[curr_impl_group_id] {
        let subset = &item_impls[subset_impl_group_id].keys().collect::<Vec<_>>();

        let superset_count = impl_group_id_supersets
            .get_mut(subset_impl_group_id)
            .unwrap();

        *superset_count -= 1;
        if *superset_count <= 0 {
            acc = acc
                .iter_mut()
                .flat_map(|impl_groups| {
                    find_impl_group_candidates_rec(
                        (subset_impl_group_id, subset),
                        impl_group_id_subsets,
                        impl_group_id_supersets,
                        item_impls,
                        impl_groups,
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
        impl_group: &ImplGroup,
    ) -> Option<syn::ItemTrait> {
        let assoc_bound_count = &impl_group.assoc_bounds.idents().count();

        let mut helper_trait = if let Some(helper_trait) = main_trait {
            helper_trait.clone()
        } else if let Some(helper_trait) = impl_group.item_impls.first().cloned() {
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
            (start_idx..start_idx + assoc_bound_count).map(param::gen_indexed_param_ident),
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

    use indexmap::IndexMap;
    use proc_macro2::Span;
    use proc_macro_error::abort_call_site;

    use quote::format_ident;
    use syn::{visit::Visit, visit_mut::VisitMut};

    /// Indexer for params used in traits, impl trait or self type, but not predicates.
    ///
    /// For `impl<U, T: IntoIterator<Item = V>, V> Trait<T> for U` resolved indices would be:
    /// `T` = 0,
    /// `U` = 1,
    /// `V` = undetermined
    pub struct NonPredicateParamIndexer<'a> {
        unindexed_lifetimes: IndexMap<&'a syn::Ident, &'a syn::LifetimeParam>,
        unindexed_type_params: IndexMap<&'a syn::Ident, &'a syn::TypeParam>,
        unindexed_const_params: IndexMap<&'a syn::Ident, &'a syn::ConstParam>,

        pub indexed_lifetimes: IndexMap<&'a syn::Ident, (usize, &'a syn::LifetimeParam)>,
        pub indexed_type_params: IndexMap<&'a syn::Ident, (usize, &'a syn::TypeParam)>,
        pub indexed_const_params: IndexMap<&'a syn::Ident, (usize, &'a syn::ConstParam)>,

        curr_param_pos_idx: usize,
    }

    pub struct NonPredicateParamResolver<'a> {
        lifetime_replacements: IndexMap<syn::Ident, &'a syn::Lifetime>,
        type_param_replacements: IndexMap<syn::Ident, &'a syn::Type>,
        const_param_replacements: IndexMap<syn::Ident, &'a syn::Expr>,
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
            .collect::<IndexMap<_, _>>();
        let type_params = indexed_type_params
            .into_iter()
            .map(|(ident, (idx, _))| (ident.clone(), gen_indexed_param_ident(idx)))
            .collect::<IndexMap<_, _>>();
        let const_params = indexed_const_params
            .into_iter()
            .map(|(ident, (idx, _))| (ident.clone(), gen_indexed_param_ident(idx)))
            .collect::<IndexMap<_, _>>();

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
            .collect::<IndexMap<_, _>>();
        let type_params = type_params
            .into_iter()
            .map(|(old_type_param, new_type_param)| {
                (old_type_param, syn::parse_quote!(#new_type_param))
            })
            .collect::<IndexMap<_, _>>();
        let const_params = const_params
            .into_iter()
            .map(|(old_const_param, new_const_param)| {
                (old_const_param, syn::parse_quote!(#new_const_param))
            })
            .collect::<IndexMap<_, _>>();
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
                indexed_lifetimes: IndexMap::new(),
                indexed_type_params: IndexMap::new(),
                indexed_const_params: IndexMap::new(),
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
                .collect::<IndexMap<_, _>>();

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
            if let Some(removed) = self.unindexed_lifetimes.swap_remove(lifetime_ident) {
                self.indexed_lifetimes
                    .insert(lifetime_ident, (self.curr_param_pos_idx, removed));
                self.curr_param_pos_idx = self.curr_param_pos_idx.checked_add(1).unwrap();
            }

            false
        }

        fn visit_type_param_ident(&mut self, param_ident: &'a syn::Ident) -> bool {
            if let Some(removed) = self.unindexed_type_params.swap_remove(param_ident) {
                self.indexed_type_params
                    .insert(param_ident, (self.curr_param_pos_idx, removed));
                self.curr_param_pos_idx = self.curr_param_pos_idx.checked_add(1).unwrap();

                return true;
            }

            false
        }

        fn visit_const_param_ident(&mut self, param_ident: &'a syn::Ident) -> bool {
            if let Some(removed) = self.unindexed_const_params.swap_remove(param_ident) {
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
