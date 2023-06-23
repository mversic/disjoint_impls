use std::collections::HashMap;

use proc_macro::TokenStream;
use proc_macro_error::abort;
use quote::{format_ident, quote};
use syn::visit::{visit_trait_bound, visit_type_param, Visit};
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, ItemTrait,
};

/// AST node type of self type such as `u32` in `impl Clone for u32`
type SelfType = syn::Type;

/// AST node type of associated (type or const) bound such as:
///
/// 1. `Target` in `impl<T: Deref<Target = bool>> for Clone for T`
/// 2. `ConstParam` in `impl<T: WithAssocConst<ConstParam = true>> for T`
trait AssocBound {
    /// AST node type of the associated bound constraint such as:
    ///
    /// 1. `bool` in `impl<T: Deref<Target = bool>> for Clone for T`
    /// 2. `true` in `impl<T: WithAssocConst<ConstParam = true>> for T`
    type Payload;
}
impl AssocBound for syn::ConstParam {
    type Payload = syn::Expr;
}
impl AssocBound for syn::TypeParam {
    type Payload = syn::Type;
}

/// A collection of {Option<ItemTrait>, [`syn::ItemImpl`]}.
/// Trait definition can be absent if dealing with inherent impls.
#[derive(Debug, PartialEq, Eq)]
struct ItemImpls {
    /// Definition of the main trait
    trait_: Option<ItemTrait>,
    /// impls map as in: (self type -> ItemImpl)
    item_impls: HashMap<SelfType, Vec<syn::ItemImpl>>,
}

/// Parameter identifier such as `T` in `impl<T> Clone for T`
type ParamIdent = syn::Ident;

/// AST node type of the trait identifier such as 'Deref<Target = u32>' in `impl<T: Deref<Target = u32>> Clone for T`.
/// Equality of this type doesn't compare associated bounds. Therefore `Deref<Target = u32>` == `Deref<Target = u64>`.
#[derive(Clone, Copy)]
struct TraitBound<'ast>(&'ast syn::Path);

/// Unique name based identifier of the associated type bound such as:
///
/// 1. `(T, Deref, Deref::Target)` in `impl<T: Deref<Target = bool>> for Clone for T`
/// 2. `(T, WitAssocConst, ConstBound)` in `impl<T: WithAssocConst<ConstBound = true>> for T`
type AssocBoundIdent<'ast> = (&'ast ParamIdent, TraitBound<'ast>, &'ast syn::Ident);

// TODO: Is this needed? how to support GATs? make a test
//&'ast Option<syn::AngleBracketedGenericArguments>,
type AssocBoundPayload<T> = <T as AssocBound>::Payload;

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

        // TODO: Also compare non associated bounds
        //match (first_elem.arguments, second_elem.arguments) {
        //    (syn::PathArguments::AngleBracketed(first_args), syn::PathArguments::AngleBracketed(second_args)) => {
        //        if first_args.colon2_token != second_args.colon2_token {
        //            return false;
        //        }

        //        first_args.args
        //    }
        //    _ => false
        //}
        true
    }
}
impl Eq for TraitBound<'_> {}
impl core::hash::Hash for TraitBound<'_> {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.0.leading_colon.hash(state);

        let mut iter = self.0.segments.iter().rev();
        let first_elem = iter.next().unwrap();

        first_elem.ident.hash(state);
        iter.for_each(|elem| elem.hash(state));
        // TODO: Apply the same TODO suggestion as in eq
    }
}

struct AssocBounds<'ast> {
    /// Ordered set of associated bound idents
    type_param_idents: Vec<AssocBoundIdent<'ast>>,
    /// Ordered set of associated bound idents
    const_param_idents: Vec<AssocBoundIdent<'ast>>,

    /// Assoc types params for every implementation
    type_params: Vec<HashMap<AssocBoundIdent<'ast>, &'ast AssocBoundPayload<syn::TypeParam>>>,
    /// Assoc const params for every implementation
    const_params: Vec<HashMap<AssocBoundIdent<'ast>, &'ast AssocBoundPayload<syn::ConstParam>>>,
}

impl<'ast> AssocBounds<'ast> {
    fn find(impls: &'ast [syn::ItemImpl]) -> Self {
        let mut type_params = Vec::new();
        let mut const_params = Vec::new();

        let mut type_param_idents = Vec::new();
        let mut const_param_idents = Vec::new();

        for impl_ in impls {
            let mut visitor = AssocBoundsVisitor::new();
            visitor.visit_generics(&impl_.generics);

            type_params.push(visitor.type_params);
            const_params.push(visitor.const_params);

            // NOTE: All impls have the same param sets
            type_param_idents = visitor.type_param_idents;
            const_param_idents = visitor.const_param_idents;
        }

        AssocBounds {
            type_param_idents,
            const_param_idents,

            type_params,
            const_params,
        }
    }
}

struct AssocBoundsVisitor<'ast> {
    /// Type parameter identifier currently being visited
    curr_type_param: Option<&'ast syn::Ident>,
    /// Trait bound currently being visited
    curr_trait_bound: Option<TraitBound<'ast>>,

    /// Collection of all associated type param bounds
    type_params: HashMap<AssocBoundIdent<'ast>, &'ast AssocBoundPayload<syn::TypeParam>>,
    /// Collection of all associated const param bounds
    const_params: HashMap<AssocBoundIdent<'ast>, &'ast AssocBoundPayload<syn::ConstParam>>,

    /// Ordered set of associated bound idents
    type_param_idents: Vec<AssocBoundIdent<'ast>>,
    /// Ordered set of associated bound idents
    const_param_idents: Vec<AssocBoundIdent<'ast>>,
}

impl<'ast> AssocBoundsVisitor<'ast> {
    fn new() -> Self {
        Self {
            curr_type_param: None,
            curr_trait_bound: None,

            type_params: HashMap::new(),
            const_params: HashMap::new(),

            type_param_idents: Vec::new(),
            const_param_idents: Vec::new(),
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
        self.type_param_idents.push(assoc_bound_ident);
    }

    fn visit_assoc_const(&mut self, node: &'ast syn::AssocConst) {
        let assoc_bound_ident = self.make_assoc_param_ident(&node.ident);
        self.const_params.insert(assoc_bound_ident, &node.value);
        self.const_param_idents.push(assoc_bound_ident);
    }
}

impl Parse for ItemImpls {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        let trait_ = input.parse::<ItemTrait>().ok();
        let mut item_impls = HashMap::new();

        // TODO: What if parsing fails? Will this happily continue?
        let trait_ident = trait_.as_ref().map(|trait_| &trait_.ident);
        let mut prev_trait = None;
        while let Ok(mut item) = input.parse::<syn::ItemImpl>() {
            param::resolve_non_predicate_param_idents_for_item(&mut item);
            // TODO: Resolve predicate param idents

            // TODO: check that all have unsafe default and the same set of attributes
            // TODO: Improve this trait checking. We have to make sure that all traits
            // have the same signature which is consistent with trait definition
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
pub fn impls(input: TokenStream) -> TokenStream {
    let impls: ItemImpls = parse_macro_input!(input);

    let mut helper_traits = Vec::new();
    let mut item_impls = Vec::new();

    let main_trait = impls.trait_;
    for (_, per_self_ty_impls) in impls.item_impls {
        helper_traits.push(gen_helper_trait(&main_trait, &per_self_ty_impls));
        item_impls.extend(gen_disjoint_impls(per_self_ty_impls));
    }

    let k = quote! {
        #main_trait

        const _: () = {
            #( #helper_traits )*
            #( #item_impls )*
        };
    };

    println!("{}", quote!(#k));
    k.into()
}

// TODO: Make a macro to dedup this?
fn gen_disjoint_impls(mut impls: Vec<syn::ItemImpl>) -> Vec<syn::ItemImpl> {
    let AssocBounds {
        type_params,
        const_params,

        type_param_idents,
        const_param_idents,
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

    let const_params = const_params
        .iter()
        .map(|params| {
            const_param_idents
                .iter()
                .map(|param_ident| {
                    if let Some(&param) = params.get(&param_ident) {
                        Some(syn::GenericArgument::Const(param.clone()))
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
                                syn::parse_quote!(<#(#params,)*>),
                            )
                        }
                        syn::PathArguments::AngleBracketed(bracketed) => {
                            bracketed.args.extend(
                                params.into_iter().map::<syn::GenericArgument, _>(
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

    impls
        .iter_mut()
        .zip(const_params)
        .for_each(|(impl_, params)| {
            impl_.trait_.as_mut().map(|(_, trait_, _)| {
                if let Some(last_seg) = trait_.segments.last_mut() {
                    match &mut last_seg.arguments {
                        syn::PathArguments::None => {
                            last_seg.arguments = syn::PathArguments::AngleBracketed(
                                syn::parse_quote!(<#(#params,)*>),
                            )
                        }
                        syn::PathArguments::AngleBracketed(bracketed) => {
                            bracketed.args.extend(
                                params.into_iter().map::<syn::GenericArgument, _>(
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
    let (assoc_type_param_count, assoc_const_param_count) = count_assoc_in_bounds(impls);

    let Some(mut helper_trait) = main_trait.clone() else {
        return None;
    };

    helper_trait.ident = gen_helper_trait_ident(&helper_trait.ident);

    for i in 0..assoc_type_param_count {
        let type_param_ident = format_ident!("_T{i}");

        helper_trait
            .generics
            .params
            .push(syn::parse_quote!(#type_param_ident));
    }
    // TODO: Do I have to watch the order so that const params are last?
    for _ in 0..assoc_const_param_count {
        unimplemented!("Const generics not yet supported")
        //let const_param_ident = format_ident!("_C{i}");
        //let const_param_ty = const_param.ty;
        //helper_trait
        //    .generics
        //    .params
        //    .push(syn::parse_quote!(const #const_param_ident: #const_param_ty));
    }

    Some(helper_trait)
}

fn count_assoc_in_bounds(impls: &[syn::ItemImpl]) -> (usize, usize) {
    let assoc_bounds = AssocBounds::find(impls);

    (
        assoc_bounds.type_param_idents.len(),
        assoc_bounds.const_param_idents.len(),
    )
}

fn gen_helper_trait_ident(ident: &syn::Ident) -> syn::Ident {
    format_ident!("_{}", &ident)
}

mod param {
    use std::collections::HashMap;

    use quote::format_ident;
    use syn::{
        visit::Visit,
        visit_mut::{visit_path_mut, visit_type_param_mut, VisitMut},
    };

    /// Replaces all parameter identifiers with a position based identifier. This makes easier
    /// to compare type params across different impls.
    ///
    /// For:
    ///     `impl<U, T: IntoIterator<Item = V>, V> Trait<T> for U`
    /// resolved impl signature would be:
    ///     `impl<_T1, _T0: IntoIterator<Item = V>, V> Trait<_T0> for _T1`
    pub fn resolve_non_predicate_param_idents_for_item(item: &mut syn::ItemImpl) {
        let mut non_predicate_param_indexer = NonPredicateParamIndexer::new(item);
        non_predicate_param_indexer.visit_item_impl(&item);

        NonPredicateParamResolver(
            non_predicate_param_indexer
                .type_params
                .into_iter()
                .filter_map(|(param_ident, idx)| idx.map(|idx| (param_ident.clone(), idx)))
                .collect(),
        )
        .visit_item_impl_mut(item);
    }

    struct NonPredicateParamResolver(HashMap<syn::Ident, usize>);
    impl VisitMut for NonPredicateParamResolver {
        fn visit_type_param_mut(&mut self, node: &mut syn::TypeParam) {
            if let Some(&idx) = self.0.get(&node.ident) {
                node.ident = gen_indexed_param_name(idx);
            }

            visit_type_param_mut(self, node);
        }
        fn visit_path_mut(&mut self, node: &mut syn::Path) {
            if let Some(first_segment) = node.segments.first_mut() {
                if let Some(&idx) = self.0.get(&first_segment.ident) {
                    first_segment.ident = gen_indexed_param_name(idx);
                }
            }

            visit_path_mut(self, node);
        }
    }

    fn gen_indexed_param_name(idx: usize) -> syn::Ident {
        format_ident!("_T{idx}")
    }

    /// Indexer for params used in impl trait and self type, but not predicates. This indexer
    /// finds indices of params used only in impl trait and self type.
    ///
    /// For `impl<U, T: IntoIterator<Item = V>, V> Trait<T> for U` resolved indices would be:
    /// `T` = 0,
    /// `U` = 1,
    /// `V` = unknown at this stage
    struct NonPredicateParamIndexer<'ast> {
        type_params: HashMap<&'ast syn::Ident, Option<usize>>,
        curr_pos_idx: usize,
    }
    impl<'ast> NonPredicateParamIndexer<'ast> {
        fn new(item: &'ast syn::ItemImpl) -> Self {
            let type_params = item
                .generics
                .type_params()
                .map(|param| (&param.ident, None))
                .collect();

            Self {
                type_params,
                curr_pos_idx: 0,
            }
        }
    }
    impl<'ast> Visit<'ast> for NonPredicateParamIndexer<'ast> {
        fn visit_item_impl(&mut self, node: &'ast syn::ItemImpl) {
            if let Some((_, trait_, _)) = &node.trait_ {
                self.visit_path(trait_);
            }
            self.visit_type(&*node.self_ty);
        }

        fn visit_path(&mut self, node: &'ast syn::Path) {
            if let Some(first_segment) = node.segments.first() {
                self.type_params
                    .entry(&first_segment.ident)
                    .and_modify(|param_idx| {
                        if param_idx.is_none() {
                            // Param encountered for the first time
                            *param_idx = Some(self.curr_pos_idx);

                            if let Some(curr_pos_idx) = self.curr_pos_idx.checked_add(1) {
                                self.curr_pos_idx = curr_pos_idx;
                            }
                        }
                    });
            }

            //visit_path(self, node);
        }
    }
    //struct PredicateIndexer<'ast> {
    //    type_params: HashMap<&'ast syn::Ident, Option<usize>>,
    //    curr_pos_idx: usize,
    //}
    //impl<'ast> PredicateIndexer<'ast> {
    //    fn new(type_params: HashMap<&'ast syn::Ident, Option<usize>>) -> Self {
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
