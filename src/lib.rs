use std::collections::{HashMap, HashSet};

use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::visit::{
    visit_assoc_const, visit_assoc_type, visit_const_param, visit_generic_argument, visit_path,
    visit_trait_bound, visit_type_param, Visit,
};
use syn::visit_mut::VisitMut;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, ItemTrait,
};

/// A collection of {Option<ItemTrait>, [`syn::ItemImpl`]}.
/// Trait definition can be absent if dealing with inherent impls.
#[derive(Debug, PartialEq, Eq)]
struct ItemImpls(
    Option<ItemTrait>,
    /// impls map as in: (self type -> ItemImpl)
    // TODO: Check that all impls refer to main trait
    HashMap<syn::Type, Vec<syn::ItemImpl>>,
);

trait AssocParam {
    type Payload;
}
impl AssocParam for syn::ConstParam {
    type Payload = syn::Expr;
}
impl AssocParam for syn::TypeParam {
    type Payload = syn::Type;
}

/// Position of the parameter in the impl block as used (i.e. not the declaration order)
///
/// `impl<V, T, U> Kita<U> for Result<T, V>`
///            |        0             1  2
type ParamIdx = usize;

#[derive(Clone, Copy)]
struct TraitIdent<'ast>(&'ast syn::Path);

type ParamIdent<'ast> = &'ast syn::Ident;
type AssocParamIdent<'ast> = (ParamIdent<'ast>, TraitIdent<'ast>, &'ast syn::Ident);
type AssocParamId<'ast> = (ParamIdx, TraitIdent<'ast>, &'ast syn::Ident);

// TODO: Is this needed? how to support GATs? make a test
//&'ast Option<syn::AngleBracketedGenericArguments>,
type AssocParamPayload<'ast, T> = &'ast <T as AssocParam>::Payload;

impl PartialEq for TraitIdent<'_> {
    fn eq(&self, other: &Self) -> bool {
        if self.0.leading_colon != other.0.leading_colon {
            return false;
        }

        let mut first_iter = self.0.segments.iter().rev();
        let mut second_iter = other.0.segments.iter().rev();

        let first_elem = first_iter.next().unwrap();
        let second_elem = second_iter.next().unwrap();

        first_elem.ident == second_elem.ident && first_iter.eq(second_iter)
    }
}
impl Eq for TraitIdent<'_> {}
impl core::hash::Hash for TraitIdent<'_> {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.0.leading_colon.hash(state);

        let mut iter = self.0.segments.iter().rev();
        let first_elem = iter.next().unwrap();

        first_elem.ident.hash(state);
        iter.for_each(|elem| elem.hash(state));
    }
}

struct AssocBounds<'ast> {
    /// Globally unique set of type param idents in the order as they occur in the impls
    type_param_idents: Vec<AssocParamId<'ast>>,
    /// Globally unique set of const param idents in the order as they occur in the impls
    const_param_idents: Vec<AssocParamId<'ast>>,

    /// Assoc types params for every implementation
    type_params: Vec<HashMap<AssocParamId<'ast>, AssocParamPayload<'ast, syn::TypeParam>>>,
    /// Assoc const params for every implementation
    const_params: Vec<HashMap<AssocParamId<'ast>, AssocParamPayload<'ast, syn::ConstParam>>>,
}

impl<'ast> AssocBounds<'ast> {
    fn find(impls: &'ast [syn::ItemImpl]) -> Self {
        let mut all_param_idents = HashSet::new();

        let mut type_param_idents = Vec::new();
        let mut const_param_idents = Vec::new();
        let mut type_params = Vec::new();
        let mut const_params = Vec::new();

        for impl_ in impls {
            let mut visitor = AssocIdentVisitor::new();
            visitor.visit_item_impl(&impl_);

            for (&type_param_ident, _) in &visitor.idx_type_params {
                if all_param_idents.insert(type_param_ident) {
                    type_param_idents.push(type_param_ident);
                }
            }
            for (&const_param_ident, _) in &visitor.idx_const_params {
                if all_param_idents.insert(const_param_ident) {
                    const_param_idents.push(const_param_ident);
                }
            }

            type_params.push(visitor.idx_type_params);
            const_params.push(visitor.idx_const_params);
        }

        AssocBounds {
            type_param_idents,
            const_param_idents,
            type_params,
            const_params,
        }
    }
}

struct AssocIdentVisitor<'ast> {
    visiting_generics: bool,

    local_type_param_idents: HashSet<AssocParamIdent<'ast>>,
    local_const_param_idents: HashSet<AssocParamIdent<'ast>>,

    current_param_idx: usize,
    current_type_param_ident: Option<&'ast syn::Ident>,
    current_trait_bound: Option<TraitIdent<'ast>>,

    ident_type_params: HashMap<
        ParamIdent<'ast>,
        (
            TraitIdent<'ast>,
            &'ast syn::Ident,
            AssocParamPayload<'ast, syn::TypeParam>,
        ),
    >,
    ident_const_params: HashMap<
        ParamIdent<'ast>,
        (
            TraitIdent<'ast>,
            &'ast syn::Ident,
            AssocParamPayload<'ast, syn::ConstParam>,
        ),
    >,

    idx_type_params: HashMap<AssocParamId<'ast>, AssocParamPayload<'ast, syn::TypeParam>>,
    idx_const_params: HashMap<AssocParamId<'ast>, AssocParamPayload<'ast, syn::ConstParam>>,
}

impl<'ast> AssocIdentVisitor<'ast> {
    fn new() -> Self {
        Self {
            visiting_generics: false,

            local_type_param_idents: HashSet::new(),
            local_const_param_idents: HashSet::new(),

            current_param_idx: 0,
            current_trait_bound: None,
            current_type_param_ident: None,

            ident_type_params: HashMap::new(),
            ident_const_params: HashMap::new(),

            idx_type_params: HashMap::new(),
            idx_const_params: HashMap::new(),
        }
    }

    fn make_assoc_param_ident(&self, assoc_param_name: &'ast syn::Ident) -> AssocParamIdent<'ast> {
        (
            self.current_type_param_ident.unwrap(),
            self.current_trait_bound.unwrap(),
            assoc_param_name,
        )
    }

    fn make_assoc_param_id(
        &mut self,
        trait_ident: TraitIdent<'ast>,
        assoc_param_name: &'ast syn::Ident,
    ) -> AssocParamId<'ast> {
        let id = (self.current_param_idx, trait_ident, assoc_param_name);
        self.current_param_idx += 1;
        id
    }
}

impl<'ast> Visit<'ast> for AssocIdentVisitor<'ast> {
    fn visit_item_impl(&mut self, node: &'ast syn::ItemImpl) {
        for it in &node.attrs {
            self.visit_attribute(it);
        }

        self.visiting_generics = true;
        self.visit_generics(&node.generics);
        self.visiting_generics = false;

        if let Some(it) = &node.trait_ {
            self.visit_path(&(it).1);
        }
        self.visit_type(&*node.self_ty);
        //for it in &node.items {
        //    self.visit_impl_item(it);
        //}
    }

    fn visit_type_param(&mut self, node: &'ast syn::TypeParam) {
        self.current_type_param_ident = Some(&node.ident);
        visit_type_param(self, node);
    }

    fn visit_trait_bound(&mut self, node: &'ast syn::TraitBound) {
        self.current_trait_bound = Some(TraitIdent(&node.path));
        visit_trait_bound(self, node);
    }

    fn visit_assoc_type(&mut self, node: &'ast syn::AssocType) {
        let assoc_param_ident = self.make_assoc_param_ident(&node.ident);

        if self.local_type_param_idents.insert(assoc_param_ident) {
            self.ident_type_params.insert(
                self.current_type_param_ident.unwrap(),
                (self.current_trait_bound.unwrap(), &node.ident, &node.ty),
            );
        }
    }

    fn visit_assoc_const(&mut self, node: &'ast syn::AssocConst) {
        let assoc_param_ident = self.make_assoc_param_ident(&node.ident);

        if self.local_const_param_idents.insert(assoc_param_ident) {
            self.ident_const_params.insert(
                self.current_type_param_ident.unwrap(),
                (self.current_trait_bound.unwrap(), &node.ident, &node.value),
            );
        }
    }

    fn visit_path(&mut self, node: &'ast syn::Path) {
        if self.visiting_generics {
            visit_path(self, node);
        } else if let Some(param_ident) = node.get_ident() {
            if let Some(&(trait_ident, assoc_param_name, type_param_payload)) =
                self.ident_type_params.get(param_ident)
            {
                let param_id = self.make_assoc_param_id(trait_ident, assoc_param_name);
                self.idx_type_params.insert(param_id, type_param_payload);
            }
            if let Some(&(trait_ident, assoc_param_name, const_param_payload)) =
                self.ident_const_params.get(param_ident)
            {
                let param_id = self.make_assoc_param_id(trait_ident, assoc_param_name);
                self.idx_const_params.insert(param_id, const_param_payload);
            }
        }
    }
}

impl Parse for ItemImpls {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        let trait_ = input.parse::<ItemTrait>().ok();
        let mut disjoint_impls = HashMap::new();

        while let Ok(item) = input.parse::<syn::ItemImpl>() {
            disjoint_impls
                .entry((*item.self_ty).clone())
                .or_insert_with(Vec::new)
                .push(item.into());
        }

        Ok(ItemImpls(trait_, disjoint_impls))
    }
}

#[proc_macro]
pub fn impls(input: TokenStream) -> TokenStream {
    let impls: ItemImpls = parse_macro_input!(input);

    let mut helper_traits = Vec::new();
    //let mut main_trait_impls = Vec::new();
    let mut disjoint_impls = Vec::new();

    let main_trait = impls.0;
    for (_, per_self_ty_impls) in impls.1 {
        helper_traits.push(gen_helper_trait(&main_trait, &per_self_ty_impls));
        //main_trait_impls.push(gen_main_trait_impls(&per_self_ty_impls));
        disjoint_impls.extend(gen_disjoint_impls(per_self_ty_impls));
    }

    let k = quote! {
        #main_trait

        const _: () = {
            #( #helper_traits )*
            //#( #main_trait_impls )*
            #( #disjoint_impls )*
        };
    };

    println!("{}", quote!(#k));
    k.into()
}

//fn gen_main_trait_impls(impls: &[syn::ItemImpl]) -> TokenStream {
//    let AssocBounds {
//        type_params,
//        const_params,
//        type_param_idents,
//        const_param_idents,
//    } = AssocBounds::find(&impls);
//
//    let items = impls[0].items.iter().map(|item| match item {
//        let mut impl_item_resolver = MainTraitImplItemResolver(k);
//        impl_item_resolver.visit_impl_item(item);
//        item
//    });
//
//    quote! {
//        impl X for #self_ty {
//            #(#items)*
//        }
//    }
//}

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

fn gen_helper_trait(main_trait: &Option<ItemTrait>, impls: &[syn::ItemImpl]) -> ItemTrait {
    let (assoc_type_param_count, assoc_const_param_count) = count_assoc_in_bounds(impls);

    let helper_trait = main_trait.clone().map_or_else(
        || {
            syn::parse_quote! {}
        },
        |mut helper_trait| {
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

            helper_trait
        },
    );

    helper_trait
}

fn count_assoc_in_bounds(impls: &[syn::ItemImpl]) -> (usize, usize) {
    let assoc_bounds = AssocBounds::find(impls);

    (
        assoc_bounds.type_param_idents.len(),
        assoc_bounds.const_param_idents.len(),
    )
}

//struct MainTraitImplItemResolver<'a>(&'a syn::Ident, &'a syn::Type);
//impl VisitMut for MainTraitImplItemResolver<'_> {
//    fn visit_impl_item_macro_mut(&mut self, _node: &mut syn::ImplItemMacro) {
//        unimplemented!()
//    }
//    fn visit_impl_item_mut(&mut self, node: &mut syn::ImplItem) {
//    }
//    fn visit_impl_item_const_mut(&mut self, node: &mut syn::ImplItemConst) {
//        let self_ty = self.1;
//        let associated_const_ident = node.ident;
//        let helper_trait_ident = gen_helper_trait_ident(self.0);
//
//        node.expr = syn::parse_quote! {
//            <#self_ty as #helper_trait_ident<#self_ty::Group>>::#associated_const_ident
//        };
//    }
//}

fn gen_helper_trait_ident(ident: &syn::Ident) -> syn::Ident {
    format_ident!("_{}", &ident)
}
