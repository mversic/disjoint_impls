use std::collections::{HashMap, HashSet};

use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::visit::{visit_assoc_const, visit_assoc_type, visit_trait_bound, Visit};
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
    HashMap<syn::Type, Vec<syn::ItemImpl>>,
);

#[derive(Clone, Copy)]
struct TraitIdent<'ast>(&'ast syn::Path);

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

type AssocParamIdent<'ast> = (
    &'ast syn::Ident,
    &'ast Option<syn::AngleBracketedGenericArguments>,
);

type AssocBoundIdent<'ast> = (TraitIdent<'ast>, AssocParamIdent<'ast>);

struct AssocBounds<'ast> {
    /// Globally unique set of type param idents in the order as they occur in the impls
    type_param_idents: Vec<AssocBoundIdent<'ast>>,
    /// Globally unique set of const param idents in the order as they occur in the impls
    const_param_idents: Vec<AssocBoundIdent<'ast>>,

    /// Assoc types params for every implementation
    type_params: Vec<HashMap<AssocBoundIdent<'ast>, &'ast syn::Type>>,
    /// Assoc const params for every implementation
    const_params: Vec<HashMap<AssocBoundIdent<'ast>, &'ast syn::Expr>>,
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

            for (&type_param_ident, _) in &visitor.type_params {
                if all_param_idents.insert(type_param_ident) {
                    type_param_idents.push(type_param_ident);
                }
            }
            for (&const_param_ident, _) in &visitor.const_params {
                if all_param_idents.insert(const_param_ident) {
                    const_param_idents.push(const_param_ident);
                }
            }

            type_params.push(visitor.type_params);
            const_params.push(visitor.const_params);
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
    local_param_idents: HashSet<AssocBoundIdent<'ast>>,
    current_trait_bound: Option<TraitIdent<'ast>>,

    type_params: HashMap<AssocBoundIdent<'ast>, &'ast syn::Type>,
    const_params: HashMap<AssocBoundIdent<'ast>, &'ast syn::Expr>,
}

impl<'ast> AssocIdentVisitor<'ast> {
    fn new() -> Self {
        Self {
            local_param_idents: HashSet::new(),
            current_trait_bound: None,

            type_params: HashMap::new(),
            const_params: HashMap::new(),
        }
    }

    fn make_assoc_bound_ident(&self, assoc_param_ident: AssocParamIdent<'ast>) -> AssocBoundIdent<'ast> {
        (self.current_trait_bound.unwrap(), assoc_param_ident)
    }
}

impl<'ast> Visit<'ast> for AssocIdentVisitor<'ast> {
    fn visit_trait_bound(&mut self, node: &'ast syn::TraitBound) {
        self.current_trait_bound = Some(TraitIdent(&node.path));
        visit_trait_bound(self, node);
    }

    fn visit_assoc_type(&mut self, node: &'ast syn::AssocType) {
        let assoc_bound_id = self.make_assoc_bound_ident((&node.ident, &node.generics));

        if self.local_param_idents.insert(assoc_bound_id) {
            self.type_params.insert(assoc_bound_id, &node.ty);
        }

        visit_assoc_type(self, node);
    }

    fn visit_assoc_const(&mut self, node: &'ast syn::AssocConst) {
        let assoc_bound_id = self.make_assoc_bound_ident((&node.ident, &node.generics));

        if self.local_param_idents.insert(assoc_bound_id) {
            self.const_params.insert(assoc_bound_id, &node.value);
        }

        visit_assoc_const(self, node);
    }

    fn visit_expr(&mut self, _: &syn::Expr) {
        // NOTE: Just in case there is an expression containing an impl block
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

    let main_trait = impls.0;
    let mut helper_traits = Vec::new();
    let mut disjoint_impls = Vec::new();
    for (_, per_self_ty_impls) in impls.1 {
        helper_traits.push(gen_helper_trait(&main_trait, &per_self_ty_impls));
        disjoint_impls.extend(gen_disjoint_impls(per_self_ty_impls));
    }

    let k = quote! {
        #main_trait

        const _: () = {
            #( #helper_traits )*
            #( #disjoint_impls )*

            //impl<T: Dispatch + #helper_trait_name<T::Group>> #trait_name for #self_ty #where_clause {
            //    const NAME: &'static str = <T as _Kita<T::Group>>::_NAME;
            //}
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

    impls.iter_mut().zip(type_params).for_each(|(impl_, params)| {
        impl_.trait_.as_mut().map(|(_, trait_, _)| {
            if let Some(last_seg) = trait_.segments.last_mut() {
                last_seg.ident = format_ident!("_{}", &last_seg.ident);

                match &mut last_seg.arguments {
                    syn::PathArguments::None => {
                        last_seg.arguments =
                            syn::PathArguments::AngleBracketed(syn::parse_quote!(<#(#params,)*>))
                    }
                    syn::PathArguments::AngleBracketed(bracketed) => {
                        bracketed.args.extend(
                            params
                                .into_iter()
                                .map::<syn::GenericArgument, _>(|param| syn::parse_quote!(#param)),
                        );
                    }
                    syn::PathArguments::Parenthesized(_) => {
                        unreachable!("Not a valid trait name")
                    }
                }
            }
        });
    });

    impls.iter_mut().zip(const_params).for_each(|(impl_, params)| {
        impl_.trait_.as_mut().map(|(_, trait_, _)| {
            if let Some(last_seg) = trait_.segments.last_mut() {
                last_seg.ident = format_ident!("_{}", &last_seg.ident);

                match &mut last_seg.arguments {
                    syn::PathArguments::None => {
                        last_seg.arguments =
                            syn::PathArguments::AngleBracketed(syn::parse_quote!(<#(#params,)*>))
                    }
                    syn::PathArguments::AngleBracketed(bracketed) => {
                        bracketed.args.extend(
                            params
                                .into_iter()
                                .map::<syn::GenericArgument, _>(|param| syn::parse_quote!(#param)),
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
}

fn gen_helper_trait(main_trait: &Option<ItemTrait>, impls: &[syn::ItemImpl]) -> ItemTrait {
    let (assoc_type_param_count, assoc_const_param_count) = count_assoc_in_bounds(impls);

    let helper_trait = main_trait.clone().map_or_else(
        || {
            syn::parse_quote! {}
        },
        |mut helper_trait| {
            helper_trait.ident = format_ident!("_{}", &helper_trait.ident);

            for i in 0..assoc_type_param_count {
                let type_param_ident = format_ident!("_T{i}");

                helper_trait
                    .generics
                    .params
                    .push(syn::parse_quote!(#type_param_ident));
            }
            // TODO: Do I have to watch the order so that const params are last?
            for _ in 0..assoc_const_param_count {
                unimplemented!("Const generics not supported")
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
