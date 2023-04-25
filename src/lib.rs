use std::collections::HashMap;

use proc_macro::TokenStream;
use proc_macro_error::{abort, OptionExt};
use quote::quote;
use syn::punctuated::{Pair, Punctuated};
use syn::visit::Visit;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, token,
    visit_mut::VisitMut,
    GenericParam, Ident, Token, WhereClause,
};
use syn::{TypeParamBound, WherePredicate};

/// A collection of [`syn::ItemImpl`]
#[derive(Debug, PartialEq, Eq)]
struct ItemImpls(
    /// Item impls are grouped in a map like: trait name -> (self type -> item impl)
    Option<HashMap<Option<syn::Path>, HashMap<syn::Type, Vec<ItemImpl>>>>,
);

#[derive(Debug, PartialEq, Eq)]
struct ItemImpl {
    item_impl: syn::ItemImpl,

    /// Type params which don't have bound with associated type/const
    extra_type_params: Vec<syn::TypeParam>,
}

impl From<syn::ItemImpl> for ItemImpl {
    fn from(mut item_impl: syn::ItemImpl) -> Self {
        let mut split_assoc_visitor = SplitAssocVisitor::new();
        split_assoc_visitor.visit_item_impl_mut(&mut item_impl);

        ItemImpl {
            item_impl,
            extra_type_params: split_assoc_visitor.extra_type_params,
        }
    }
}

struct SplitAssocVisitor {
    /// Type params which don't have bound with associated type/const
    extra_type_params: Vec<syn::TypeParam>,
    /// Set to true if currently visited bound has associated type
    curr_type_param_bound_has_assoc: bool,
}
impl SplitAssocVisitor {
    fn new() -> Self {
        Self {
            extra_type_params: Vec::new(),
            curr_type_param_bound_has_assoc: false,
        }
    }
}
impl Visit<'_> for SplitAssocVisitor {
    fn visit_assoc_type(&mut self, _: &syn::AssocType) {
        self.curr_type_param_bound_has_assoc = true;
    }
    fn visit_assoc_const(&mut self, _: &syn::AssocConst) {
        self.curr_type_param_bound_has_assoc = true;
    }
}
impl VisitMut for SplitAssocVisitor {
    fn visit_type_param_mut(&mut self, node: &mut syn::TypeParam) {
        let mut bounds_without_assoc = Punctuated::<TypeParamBound, syn::token::Plus>::new();

        let mut bounds_with_assoc = Vec::new();
        while let Some(bound) = node.bounds.pop() {
            self.visit_type_param_bound(bound.value());

            if self.curr_type_param_bound_has_assoc {
                bounds_with_assoc.push(bound);
            } else {
                bounds_without_assoc.push(bound.into_value());
            }

            self.curr_type_param_bound_has_assoc = false;
        }

        if !bounds_without_assoc.is_empty() {
            // TODO: or should I use: `let mut type_param = node.clone();`
            let mut type_param: syn::TypeParam = node.ident.clone().into();
            type_param.bounds = bounds_without_assoc.into_iter().rev().collect();

            self.extra_type_params.push(type_param);
        }

        node.bounds = bounds_with_assoc.into_iter().rev().collect();
    }
    fn visit_generics_mut(&mut self, node: &mut syn::Generics) {
        let mut params = Punctuated::<GenericParam, syn::token::Comma>::new();

        while let Some(param) = node.params.pop() {
            let mut param = param.into_value();

            self.visit_generic_param_mut(&mut param);
            if let GenericParam::Type(type_param) = &param {
                if !type_param.bounds.is_empty() {
                    params.push(param);
                }
            }
        }

        // TODO: Do this here or?
        //if let Some(it) = &mut node.where_clause {
        //    self.visit_where_clause_mut(it);
        //}
    }
}
struct WhereClauseVisitor {
    type_params: Vec<syn::TypeParam>,
    //where_clause: Vec<syn::PredicateType>,
}
impl VisitMut for WhereClauseVisitor {
    //fn visit_predicate_type_mut(&mut self, node: &mut syn::PredicateType) {
    //    let mut where_clause = node.clone();
    //    where_clause.bounds = Punctuated::new();
    //    visit_predicate_type_mut(self, node);
    //}
}

impl Parse for ItemImpls {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        let mut disjoint_impls = HashMap::new();

        // TODO: What if there is some other element instead of ItemImpl
        while let Ok(mut item) = input.parse::<syn::ItemImpl>() {
            disjoint_impls
                .entry(item.trait_.take().map(|trait_| trait_.1))
                .or_insert_with(HashMap::new)
                .entry((*item.self_ty).clone())
                .or_insert_with(Vec::new)
                .push(item.into());
        }

        Ok(if disjoint_impls.is_empty() {
            ItemImpls(None)
        } else {
            ItemImpls(Some(disjoint_impls))
        })
    }
}

#[proc_macro]
pub fn impls(input: TokenStream) -> TokenStream {
    let impls: ItemImpls = parse_macro_input!(input);

    if impls.0.is_none() {
        return quote!().into();
    }

    if let Some(impls) = impls.0 {
        for (trait_name, v) in impls {
            println!("{:?}:", trait_name);
            for (self_ty, v) in v {
                println!("SELF {:?}:", self_ty);
                for v in v {
                    //let k = v.extra_type_params;
                    let v = v.item_impl;
                    //println!("{}", quote!(#k));
                    println!("{}", quote!(#v));
                }
            }
        }
    }

    //let (disjoint_trait_name, specialized_trait_name) = get_trait_names(&impls.0);
    //let specialize_trait_name = Ident::new(
    //    &format!("__Specialize{specialized_trait_name}"),
    //    Span::call_site(),
    //);

    quote! {}.into()
}
