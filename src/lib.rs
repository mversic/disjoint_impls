use std::collections::BTreeSet;
use std::hash::Hash;

use param::ParamResolver;
use proc_macro::TokenStream;
use proc_macro_error::{abort, proc_macro_error};
use quote::{format_ident, quote};
use rustc_hash::FxHashMap;
use syn::visit::{visit_trait_bound, visit_type_param, Visit};
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, ItemTrait,
};

/// AST node type of self type such as `u32` in `impl Clone for u32`
type SelfType = syn::Type;

/// A collection of {Option<ItemTrait>, [`syn::ItemImpl`]}.
/// Trait definition can be absent if dealing with inherent impls.
#[derive(Debug, PartialEq, Eq)]
struct ItemImpls {
    /// Definition of the main trait
    trait_: Option<ItemTrait>,
    /// impls map as in: (self type -> ItemImpl)
    item_impls: FxHashMap<SelfType, Vec<syn::ItemImpl>>,
}

/// Parameter identifier such as `T` in `impl<T> Clone for T`
type ParamIdent = syn::Ident;

/// AST node type of the trait identifier such as 'Deref<Target = u32>' in `impl<T: Deref<Target = u32>> Clone for T`.
/// Equality of this type doesn't compare associated bounds. Therefore `Deref<Target = u32>` == `Deref<Target = u64>`.
#[derive(Debug, Clone, Copy)]
struct TraitBound<'ast>(pub &'ast syn::Path);

/// Unique name based identifier of the associated type bound such as:
///     `(T, Deref, Deref::Target)` in `impl<T: Deref<Target = bool>> for Clone for T`
type AssocBoundIdent<'ast> = (&'ast ParamIdent, TraitBound<'ast>, &'ast syn::Ident);

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

        let first_elem = first_iter.next().unwrap();
        let second_elem = second_iter.next().unwrap();

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

impl Eq for TraitBound<'_> {}
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
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.0.leading_colon.to_tokens(tokens);

        let mut iter = self.0.segments.iter().rev();
        let first_elem = iter.next().unwrap();

        iter.rev().for_each(|elem| elem.to_tokens(tokens));
        first_elem.ident.to_tokens(tokens);

        match &first_elem.arguments {
            syn::PathArguments::AngleBracketed(first_args) => {
                first_args.colon2_token.to_tokens(tokens);

                quote!(<).to_tokens(tokens);
                first_args.args.iter().for_each(|args| match args {
                    syn::GenericArgument::AssocType(_) => {}
                    _ => args.to_tokens(tokens),
                });
                quote!(>).to_tokens(tokens);
            }
            _ => first_elem.arguments.to_tokens(tokens),
        }
    }
}

mod ord {
    use core::cmp::Ordering;

    use super::TraitBound;

    fn cmp_colons(first: Option<syn::Token![::]>, second: Option<syn::Token![::]>) -> Ordering {
        match (first, second) {
            (None, Some(_)) => Ordering::Less,
            (Some(_), None) => Ordering::Greater,
            (Some(_), Some(_)) | (None, None) => Ordering::Equal,
        }
    }

    fn cmp_angle_bracketed_generic_arguments(
        _first: &syn::AngleBracketedGenericArguments,
        _second: &syn::AngleBracketedGenericArguments,
    ) -> Ordering {
        //let res = cmp_colons(first.colon2_token, second.colon2_token);
        //if res != Ordering::Equal {
        //    return res;
        //}

        //for zipped_args in first.args.iter().zip(&second.args) {
        //    match zipped_args {
        //        (
        //            syn::GenericArgument::AssocType(first_assoc),
        //            syn::GenericArgument::AssocType(second_assoc),
        //        ) => {
        //            let res = first_assoc.ident.cmp(&second_assoc.ident);
        //            if res != Ordering::Equal {
        //                return res;
        //            }

        //            let res = match (first_assoc.generics, second_assoc.generics) {
        //                (None, _) => Ordering::Less,
        //                (_, None) => Ordering::Greater,
        //                (None, None) => Ordering::Equal,
        //                (Some(first_args), Some(second_args)) => {
        //                    cmp_angle_bracketed_generic_arguments(&first_args, &second_args)
        //                }
        //            };
        //            if res != Ordering::Equal {
        //                return res;
        //            }

        //            cmp_types(&first_assoc.ty, &second_assoc.ty)
        //        }
        //        _ => first_args.cmp(second_args),
        //    }
        //}

        // TODO
        core::cmp::Ordering::Equal
        //unimplemented!("Ordering of trait bounds not implemented yet")
    }

    impl PartialOrd for TraitBound<'_> {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            Some(self.cmp(other))
        }
    }
    impl Ord for TraitBound<'_> {
        fn cmp(&self, other: &Self) -> Ordering {
            let res = cmp_colons(self.0.leading_colon, other.0.leading_colon);
            if res != Ordering::Equal {
                return res;
            }

            let mut first_iter = self.0.segments.iter().rev();
            let mut second_iter = other.0.segments.iter().rev();

            let first_elem = first_iter.next().unwrap();
            let second_elem = second_iter.next().unwrap();

            let res = first_iter
                .rev()
                .map(|seg| &seg.ident)
                .cmp(second_iter.rev().map(|seg| &seg.ident));
            if res != Ordering::Equal {
                return res;
            }

            let res = first_elem.ident.cmp(&second_elem.ident);
            if res != Ordering::Equal {
                return res;
            }

            match (&first_elem.arguments, &second_elem.arguments) {
                (
                    syn::PathArguments::AngleBracketed(first_args),
                    syn::PathArguments::AngleBracketed(second_args),
                ) => cmp_angle_bracketed_generic_arguments(first_args, second_args),
                (
                    syn::PathArguments::Parenthesized(_first_args),
                    syn::PathArguments::Parenthesized(_second_args),
                ) => {
                    unimplemented!("Parenthesized args ordering not implemented yet")
                }
                (syn::PathArguments::None, _) => Ordering::Less,
                (_, syn::PathArguments::None) => Ordering::Greater,
                (syn::PathArguments::AngleBracketed(_), _) => Ordering::Less,
                (_, syn::PathArguments::AngleBracketed(_)) => Ordering::Greater,
            }
        }
    }
}

struct AssocBounds<'ast> {
    /// Ordered collection of assoc bound idents
    type_param_idents: Vec<AssocBoundIdent<'ast>>,
    /// Assoc types params for every implementation
    type_params: Vec<FxHashMap<AssocBoundIdent<'ast>, &'ast AssocBoundPayload>>,
}

impl<'ast> AssocBounds<'ast> {
    fn find(impls: &'ast [syn::ItemImpl]) -> Self {
        let (mut type_param_idents, mut type_params) = (BTreeSet::new(), Vec::new());

        for impl_ in impls {
            let mut visitor = AssocBoundsVisitor::new();

            visitor.visit_generics(&impl_.generics);
            type_params.push(visitor.type_params);

            type_param_idents.extend(visitor.type_param_idents);
        }

        AssocBounds {
            // Collect all type param assoc bounds into a sorted list
            type_param_idents: type_param_idents.into_iter().collect(),
            type_params,
        }
    }
}

struct AssocBoundsVisitor<'ast> {
    /// Type parameter identifier currently being visited
    curr_type_param: Option<&'ast syn::Ident>,
    /// Trait bound currently being visited
    curr_trait_bound: Option<TraitBound<'ast>>,

    /// Ordered set of associated bound idents
    type_param_idents: BTreeSet<AssocBoundIdent<'ast>>,
    /// Collection of all associated type param bounds
    type_params: FxHashMap<AssocBoundIdent<'ast>, &'ast AssocBoundPayload>,
}

impl<'ast> AssocBoundsVisitor<'ast> {
    fn new() -> Self {
        Self {
            curr_type_param: None,
            curr_trait_bound: None,

            type_param_idents: BTreeSet::new(),
            type_params: FxHashMap::default(),
        }
    }

    fn make_assoc_param_ident(&self, assoc_param_name: &'ast syn::Ident) -> AssocBoundIdent<'ast> {
        (
            self.curr_type_param.unwrap(),
            self.curr_trait_bound.unwrap(),
            assoc_param_name,
        )
    }
}

impl<'ast> Visit<'ast> for AssocBoundsVisitor<'ast> {
    fn visit_item_impl(&mut self, node: &'ast syn::ItemImpl) {
        self.visit_generics(&node.generics);
    }

    fn visit_type_param(&mut self, node: &'ast syn::TypeParam) {
        self.curr_type_param = Some(&node.ident);
        visit_type_param(self, node);
    }

    fn visit_trait_bound(&mut self, node: &'ast syn::TraitBound) {
        self.curr_trait_bound = Some(TraitBound(&node.path));
        visit_trait_bound(self, node);
    }

    fn visit_assoc_type(&mut self, node: &'ast syn::AssocType) {
        let assoc_bound_ident = self.make_assoc_param_ident(&node.ident);
        self.type_params.insert(assoc_bound_ident, &node.ty);
        self.type_param_idents.insert(assoc_bound_ident);
    }
}

impl Parse for ItemImpls {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        let mut trait_ = input.parse::<ItemTrait>().ok();
        trait_
            .as_mut()
            .map(ParamResolver::resolve_non_predicate_params);

        let mut prev_trait = None;
        let mut item_impls = FxHashMap::default();
        let trait_ident = trait_.as_ref().map(|trait_| &trait_.ident);
        while let Ok(mut item) = input.parse::<syn::ItemImpl>() {
            item.resolve_non_predicate_params();
            // TODO: Resolve predicate param idents

            // TODO: check that all have unsafe, default and the same set of attributes
            // TODO: Improve this trait checking. We have to make sure that all traits
            // have the same signature(including generics) which is consistent with trait definition
            // Maybe we don't have to check they are consistent
            let kita = item.trait_.as_ref().map(|trait_| &trait_.1);
            if let Some(prev_trait) = &prev_trait {
                if Some(prev_trait) != kita {
                    abort!(kita, "Differing traits");
                }
            } else {
                prev_trait = kita.cloned();
            }

            let item_trait_ident = kita
                .and_then(|trait_| trait_.segments.last())
                .map(|seg| &seg.ident);

            if trait_ident != item_trait_ident {
                abort!(item_trait_ident, "Doesn't match trait definition");
            }

            item_impls
                .entry((*item.self_ty).clone())
                .or_insert_with(Vec::new)
                .push(item.into());
        }

        Ok(ItemImpls { trait_, item_impls })
    }
}

#[proc_macro]
#[proc_macro_error]
pub fn impls(input: TokenStream) -> TokenStream {
    let impls: ItemImpls = parse_macro_input!(input);

    let mut helper_traits = Vec::new();
    let mut main_trait_impl = None;
    let mut item_impls = Vec::new();

    let main_trait = impls.trait_;
    for (_, per_self_ty_impls) in impls.item_impls {
        helper_traits.push(gen_helper_trait(&main_trait, &per_self_ty_impls));
        main_trait_impl = Some(main::gen_main_trait_impl(&main_trait, &per_self_ty_impls));
        item_impls.extend(gen_disjoint_impls(per_self_ty_impls));
    }

    let k = quote! {
        #main_trait

        const _: () = {
            #( #helper_traits )*
            #( #item_impls )*

            #main_trait_impl
        };
    };

    println!("{}", quote!(#k));
    k.into()
}

fn gen_disjoint_impls(mut impls: Vec<syn::ItemImpl>) -> Vec<syn::ItemImpl> {
    let AssocBounds {
        type_param_idents,
        type_params,
    } = AssocBounds::find(&impls);

    let type_params = type_params
        .iter()
        .map(|params| {
            type_param_idents
                .iter()
                .map(|param_ident| {
                    if let Some(&param) = params.get(&param_ident) {
                        Some(syn::GenericArgument::Type(param.clone()))
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    impls
        .iter_mut()
        .zip(type_params)
        .for_each(|(impl_, params)| {
            impl_.trait_.as_mut().map(|(_, trait_, _)| {
                if let Some(last_seg) = trait_.segments.last_mut() {
                    match &mut last_seg.arguments {
                        syn::PathArguments::None => {
                            last_seg.arguments = syn::PathArguments::AngleBracketed(
                                syn::parse_quote!(<#(#params),*>),
                            )
                        }
                        syn::PathArguments::AngleBracketed(bracketed) => {
                            bracketed.args.extend(
                                params
                                    .into_iter()
                                    .skip(bracketed.args.len())
                                    .map::<syn::GenericArgument, _>(
                                        |param| syn::parse_quote!(#param),
                                    ),
                            );
                        }
                        syn::PathArguments::Parenthesized(_) => {
                            unreachable!("Not a valid trait name")
                        }
                    }
                }
            });
        });

    impls.iter_mut().for_each(|impl_| {
        impl_.trait_.as_mut().map(|(_, trait_, _)| {
            if let Some(last_seg) = trait_.segments.last_mut() {
                last_seg.ident = format_ident!("_{}", &last_seg.ident);
            }
        });
    });

    impls
}

fn gen_helper_trait(main_trait: &Option<ItemTrait>, impls: &[syn::ItemImpl]) -> Option<ItemTrait> {
    let assoc_type_param_count = AssocBounds::find(impls).type_param_idents.len();

    let Some(mut helper_trait) = main_trait.clone() else {
        return None;
    };

    helper_trait.vis = syn::Visibility::Inherited;
    helper_trait.ident = gen_helper_trait_ident(&helper_trait.ident);
    let start_idx = helper_trait.generics.type_params().count();

    for i in start_idx..assoc_type_param_count {
        let type_param_ident = param::gen_indexed_type_param_name(i);

        helper_trait
            .generics
            .params
            .push(syn::parse_quote!(#type_param_ident));
    }

    Some(helper_trait)
}

fn gen_helper_trait_ident(ident: &syn::Ident) -> syn::Ident {
    format_ident!("_{}", &ident)
}

mod main {
    //! Contains logic related to generating impl of the main trait

    use rustc_hash::FxHashSet;
    use syn::visit_mut::VisitMut;

    use super::*;

    struct ImplItemResolver {
        self_as_helper_trait: proc_macro2::TokenStream,
    }

    pub fn gen_main_trait_impl(
        main_trait: &Option<ItemTrait>,
        impls: &[syn::ItemImpl],
    ) -> Option<syn::ItemImpl> {
        let Some(main_trait) = main_trait else {
            return None;
        };

        let Some(example_impl) = impls.get(0).cloned() else {
            return None;
        };

        let AssocBounds {
            type_param_idents, ..
        } = AssocBounds::find(impls);

        let helper_trait_ident = gen_helper_trait_ident(&main_trait.ident);
        let (impl_generics, where_clause) = make_generics(&helper_trait_ident, &type_param_idents);
        let mut impl_item_resolver = ImplItemResolver::new(&helper_trait_ident, &type_param_idents);

        let syn::ItemImpl {
            attrs,
            defaultness,
            unsafety,
            trait_,
            self_ty,
            mut items,
            ..
        } = example_impl;

        let trait_ = trait_.as_ref().map(|trait_| &trait_.1);
        items
            .iter_mut()
            .for_each(|item| impl_item_resolver.visit_impl_item_mut(item));

        Some(syn::parse_quote! {
            #(#attrs)*
            #defaultness #unsafety impl #impl_generics #trait_ for #self_ty #where_clause {
                #(#items)*
            }
        })
    }

    fn gen_assoc_bounds<'a>(
        type_param_idents: &'a [AssocBoundIdent],
    ) -> impl Iterator<Item = proc_macro2::TokenStream> + 'a {
        type_param_idents
            .iter()
            .map(|(param_ident, trait_bound, assoc_param_name)| {
                quote! { <#param_ident as #trait_bound>::#assoc_param_name }
            })
    }

    fn make_generics(
        helper_trait_ident: &syn::Ident,
        type_param_idents: &[AssocBoundIdent],
    ) -> (proc_macro2::TokenStream, proc_macro2::TokenStream) {
        let mut assoc_bounds = FxHashMap::<_, FxHashSet<_>>::default();

        type_param_idents
            .iter()
            .for_each(|(param_ident, trait_bound, _)| {
                assoc_bounds
                    .entry(param_ident)
                    .or_default()
                    .insert(trait_bound);
            });

        let type_params = type_param_idents
            .iter()
            .map(|(param_ident, _, _)| param_ident);

        let where_clause_predicates = assoc_bounds
            .into_iter()
            .map(|(param_ident, trait_bounds)| {
                let trait_bounds = trait_bounds.into_iter();
                quote! { #param_ident: #(#trait_bounds)+* }
            })
            .chain(core::iter::once_with(|| {
                let assoc_bounds = gen_assoc_bounds(type_param_idents);
                quote! { Self: #helper_trait_ident<#(#assoc_bounds),*> }
            }));

        (
            quote! { <#(#type_params),*> },
            quote! { where #(#where_clause_predicates),* },
        )
    }

    impl ImplItemResolver {
        fn new(helper_trait_ident: &syn::Ident, type_param_idents: &[AssocBoundIdent]) -> Self {
            let assoc_bounds = gen_assoc_bounds(type_param_idents);

            Self {
                self_as_helper_trait: quote! {
                    <Self as #helper_trait_ident<#(#assoc_bounds),*>>
                },
            }
        }
    }

    impl VisitMut for ImplItemResolver {
        fn visit_impl_item_const_mut(&mut self, node: &mut syn::ImplItemConst) {
            let self_as_helper_trait = &self.self_as_helper_trait;

            let ident = &node.ident;
            node.expr = syn::parse_quote!(
                #self_as_helper_trait::#ident
            );
        }

        fn visit_impl_item_fn_mut(&mut self, node: &mut syn::ImplItemFn) {
            let self_as_helper_trait = &self.self_as_helper_trait;

            let syn::Signature {
                ident,
                inputs,
                variadic,
                ..
            } = &node.sig;

            let inputs = inputs.iter().map(|input| match input {
                syn::FnArg::Receiver(_) => syn::parse_quote!(self),
                syn::FnArg::Typed(arg) => arg.pat.clone(),
            });
            node.block = syn::parse_quote!({
                #self_as_helper_trait::#ident(#(#inputs),*, #variadic)
            });
        }

        fn visit_impl_item_type_mut(&mut self, node: &mut syn::ImplItemType) {
            let self_as_helper_trait = &self.self_as_helper_trait;

            let ident = &node.ident;
            node.ty = syn::parse_quote!(
                #self_as_helper_trait::#ident
            );
        }

        fn visit_impl_item_macro_mut(&mut self, _node: &mut syn::ImplItemMacro) {
            unimplemented!("Trait macros are not supported")
        }
    }
}

mod param {
    //! Contains logic related to uniform position based (re-)naming of parameters

    use rustc_hash::FxHashMap;

    use quote::format_ident;
    use syn::{
        visit::{visit_path, Visit},
        visit_mut::{visit_const_param_mut, visit_path_mut, visit_type_param_mut, VisitMut},
    };

    /// Resolve lifetimes, type params and const params into position based identifiers
    pub trait ParamResolver {
        /// Replaces all param identifiers with a position based identifier.
        /// This makes easier to compare params across different impls.
        ///
        /// For:
        ///     `impl<U, T: IntoIterator<Item = V>, V> Trait<u32, T> for U`
        /// resolved impl signature would be:
        ///     `impl<_T2, _T1: IntoIterator<Item = V>, V> Trait<u32, _T1> for _T2`
        fn resolve_non_predicate_params(&mut self);
    }

    struct NonPredicateParamResolver {
        lifetimes: FxHashMap<syn::Ident, usize>,
        type_params: FxHashMap<syn::Ident, usize>,
        const_params: FxHashMap<syn::Ident, usize>,
    }

    /// Indexer for params used in traits, impl trait or self type, but not predicates.
    ///
    /// For `impl<U, T: IntoIterator<Item = V>, V> Trait<T> for U` resolved indices would be:
    /// `T` = 0,
    /// `U` = 1,
    /// `V` = undetermined
    struct NonPredicateParamIndexer<'ast> {
        lifetime_params: FxHashMap<&'ast syn::Ident, Option<usize>>,
        type_params: FxHashMap<&'ast syn::Ident, Option<usize>>,
        const_params: FxHashMap<&'ast syn::Ident, Option<usize>>,

        curr_lifetime_param_pos_idx: usize,
        curr_type_param_pos_idx: usize,
        curr_const_param_pos_idx: usize,
    }

    impl ParamResolver for syn::ItemImpl {
        fn resolve_non_predicate_params(&mut self) {
            let mut non_predicate_param_indexer = NonPredicateParamIndexer::new(&self.generics);
            non_predicate_param_indexer.visit_item_impl(&self);
            let mut param_resolver = NonPredicateParamResolver::new(non_predicate_param_indexer);
            param_resolver.visit_item_impl_mut(self);

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
    }

    impl ParamResolver for syn::ItemTrait {
        fn resolve_non_predicate_params(&mut self) {
            let mut non_predicate_param_indexer = NonPredicateParamIndexer::new(&self.generics);
            non_predicate_param_indexer.visit_item_trait(&self);
            let mut param_resolver = NonPredicateParamResolver::new(non_predicate_param_indexer);
            param_resolver.visit_item_trait_mut(self);
        }
    }

    pub fn gen_indexed_lifetime_param_name(idx: usize) -> syn::Ident {
        format_ident!("_LŠČ{idx}")
    }

    pub fn gen_indexed_type_param_name(idx: usize) -> syn::Ident {
        format_ident!("_TŠČ{idx}")
    }

    pub fn gen_indexed_const_param_name(idx: usize) -> syn::Ident {
        format_ident!("_CŠČ{idx}")
    }

    impl<'ast> NonPredicateParamIndexer<'ast> {
        fn new(generics: &'ast syn::Generics) -> Self {
            let lifetime_params = generics
                .lifetimes()
                .map(|param| (&param.lifetime.ident, None))
                .collect();
            let type_params = generics
                .type_params()
                .map(|param| (&param.ident, None))
                .collect();
            let const_params = generics
                .const_params()
                .map(|param| (&param.ident, None))
                .collect();

            Self {
                lifetime_params,
                type_params,
                const_params,

                curr_lifetime_param_pos_idx: 0,
                curr_type_param_pos_idx: 0,
                curr_const_param_pos_idx: 0,
            }
        }
    }

    impl<'ast> Visit<'ast> for NonPredicateParamIndexer<'ast> {
        fn visit_item_trait(&mut self, node: &'ast syn::ItemTrait) {
            self.visit_generics(&node.generics);
        }

        fn visit_item_impl(&mut self, node: &'ast syn::ItemImpl) {
            if let Some((_, trait_, _)) = &node.trait_ {
                // NOTE: Calling `visit_path` on a trait would conflict
                // with resolving params on `TypePath` so it's not done
                if let Some(last_seg) = &trait_.segments.last() {
                    self.visit_path_arguments(&last_seg.arguments);
                }
            }

            self.visit_type(&*node.self_ty);
        }

        // Called only for a trait definition, never for impl block
        fn visit_lifetime_param(&mut self, node: &'ast syn::LifetimeParam) {
            self.visit_lifetime(&node.lifetime);
        }

        // Called only for a trait definition, never for impl block
        fn visit_type_param(&mut self, node: &'ast syn::TypeParam) {
            *self.type_params.get_mut(&node.ident).unwrap() = Some(self.curr_type_param_pos_idx);

            if let Some(curr_pos_idx) = self.curr_type_param_pos_idx.checked_add(1) {
                self.curr_type_param_pos_idx = curr_pos_idx;
            }
        }

        // Called only for a trait definition, never for impl block
        fn visit_const_param(&mut self, node: &'ast syn::ConstParam) {
            *self.const_params.get_mut(&node.ident).unwrap() = Some(self.curr_const_param_pos_idx);

            if let Some(curr_pos_idx) = self.curr_const_param_pos_idx.checked_add(1) {
                self.curr_const_param_pos_idx = curr_pos_idx;
            }
        }

        fn visit_lifetime(&mut self, node: &'ast syn::Lifetime) {
            *self.lifetime_params.get_mut(&node.ident).unwrap() =
                Some(self.curr_lifetime_param_pos_idx);

            if let Some(curr_pos_idx) = self.curr_lifetime_param_pos_idx.checked_add(1) {
                self.curr_lifetime_param_pos_idx = curr_pos_idx;
            }
        }

        fn visit_path(&mut self, node: &'ast syn::Path) {
            if let Some(param_idx) = node.get_ident().and_then(|i| self.const_params.get_mut(&i)) {
                *param_idx = Some(self.curr_const_param_pos_idx);

                if let Some(curr_pos_idx) = self.curr_const_param_pos_idx.checked_add(1) {
                    self.curr_const_param_pos_idx = curr_pos_idx;
                }
            } else if let Some(first_segment) = node.segments.first() {
                self.type_params
                    .entry(&first_segment.ident)
                    .and_modify(|param_idx| {
                        if param_idx.is_none() {
                            *param_idx = Some(self.curr_type_param_pos_idx);
                        }
                    });

                if let Some(pos_idx) = self.curr_type_param_pos_idx.checked_add(1) {
                    self.curr_type_param_pos_idx = pos_idx;
                }
            }

            visit_path(self, node);
        }

        fn visit_where_clause(&mut self, _node: &'ast syn::WhereClause) {}
    }

    impl NonPredicateParamResolver {
        fn new<'ast>(indexer: NonPredicateParamIndexer) -> Self {
            Self {
                lifetimes: indexer
                    .lifetime_params
                    .into_iter()
                    .filter_map(|(param, idx)| idx.map(|idx| (param.clone(), idx)))
                    .collect(),
                type_params: indexer
                    .type_params
                    .into_iter()
                    .filter_map(|(param, idx)| idx.map(|idx| (param.clone(), idx)))
                    .collect(),
                const_params: indexer
                    .const_params
                    .into_iter()
                    .filter_map(|(param, idx)| idx.map(|idx| (param.clone(), idx)))
                    .collect(),
            }
        }
    }

    impl VisitMut for NonPredicateParamResolver {
        fn visit_lifetime_mut(&mut self, node: &mut syn::Lifetime) {
            if let Some(&idx) = self.lifetimes.get(&node.ident) {
                node.ident = gen_indexed_lifetime_param_name(idx);
            }
        }

        fn visit_type_param_mut(&mut self, node: &mut syn::TypeParam) {
            if let Some(&idx) = self.type_params.get(&node.ident) {
                node.ident = gen_indexed_type_param_name(idx);
            }

            visit_type_param_mut(self, node);
        }

        fn visit_const_param_mut(&mut self, node: &mut syn::ConstParam) {
            if let Some(&idx) = self.const_params.get(&node.ident) {
                node.ident = gen_indexed_const_param_name(idx);
            }

            visit_const_param_mut(self, node);
        }

        fn visit_path_mut(&mut self, node: &mut syn::Path) {
            if let Some(first_segment) = node.segments.first_mut() {
                if let Some(&idx) = self.type_params.get(&first_segment.ident) {
                    first_segment.ident = gen_indexed_type_param_name(idx);
                } else if let Some(&idx) = self.const_params.get(&first_segment.ident) {
                    first_segment.ident = gen_indexed_const_param_name(idx);
                }
            }

            visit_path_mut(self, node);
        }
    }

    //struct PredicateIndexer<'ast> {
    //    type_params: FxHashMap<&'ast syn::Ident, Option<usize>>,
    //    curr_pos_idx: usize,
    //}
    //impl<'ast> PredicateIndexer<'ast> {
    //    fn new(type_params: FxHashMap<&'ast syn::Ident, Option<usize>>) -> Self {
    //        let curr_pos_idx: usize = type_params
    //            .values()
    //            .filter_map(|x| *x)
    //            .reduce(|acc, x| x.max(acc))
    //            .unwrap_or(0);
    //
    //        Self {
    //            type_params,
    //            curr_pos_idx,
    //        }
    //    }
    //}
    //impl<'ast> Visit<'ast> for PredicateIndexer<'ast> {
    //    fn visit_item_impl(&mut self, node: &'ast syn::ItemImpl) {
    //        self.visit_generics(&node.generics);
    //    }
    //
    //    fn visit_path_segment(&mut self, node: &'ast syn::PathSegment) {
    //        self.type_params.entry(&node.ident).and_modify(|param_idx| {
    //            if param_idx.is_none() {
    //                // Param encountered for the first time
    //                *param_idx = Some(self.curr_pos_idx);
    //            }
    //        });
    //
    //        if let Some(curr_pos_idx) = self.curr_pos_idx.checked_add(1) {
    //            self.curr_pos_idx = curr_pos_idx;
    //        }
    //    }
    //}
}
