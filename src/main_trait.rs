//! Contains logic related to generating impl of the main trait

use rustc_hash::FxHashSet;
use syn::visit_mut::VisitMut;

use super::*;

struct GenericsResolver<'a> {
    assoc_bound_type_params: FxHashSet<&'a syn::Ident>,
    where_clause_predicates: Vec<TokenStream2>,
}

struct ImplItemResolver {
    self_as_helper_trait: TokenStream2,
}

/// Generate main trait impl
pub fn gen(main_trait: Option<&ItemTrait>, impls: &[ItemImpl]) -> Option<ItemImpl> {
    let Some(example_impl) = impls.get(0).cloned() else {
        return None;
    };

    let helper_trait_ident = main_trait.as_ref().map_or_else(
        || {
            if let syn::Type::Path(type_path) = &*example_impl.self_ty {
                if let Some(last_seg) = type_path.path.segments.last() {
                    return Some(helper_trait::gen_ident(&last_seg.ident));
                }
            }

            None
        },
        |main_trait| Some(helper_trait::gen_ident(&main_trait.ident)),
    )?;

    let ItemImpl {
        attrs,
        defaultness,
        unsafety,
        mut generics,
        trait_,
        self_ty,
        mut items,
        ..
    } = example_impl;

    let AssocBounds {
        type_param_idents, ..
    } = AssocBounds::find(impls);

    let mut generics_resolver =
        GenericsResolver::new(main_trait, &helper_trait_ident, &type_param_idents);

    generics_resolver.visit_generics_mut(&mut generics);
    let trait_ = trait_.as_ref().map(|trait_| &trait_.1);

    let mut impl_item_resolver =
        ImplItemResolver::new(main_trait, &helper_trait_ident, &type_param_idents);
    items
        .iter_mut()
        .for_each(|item| impl_item_resolver.visit_impl_item_mut(item));

    let (impl_generics, _, where_clause) = generics.split_for_impl();
    let for_ = if main_trait.is_some() {
        quote!(for)
    } else {
        quote!()
    };

    Some(syn::parse_quote! {
        #(#attrs)*
        #defaultness #unsafety impl #impl_generics #trait_ #for_ #self_ty #where_clause {
            #(#items)*
        }
    })
}

fn gen_assoc_bounds<'a>(
    type_param_idents: &'a [AssocBoundIdent],
) -> impl Iterator<Item = TokenStream2> + 'a {
    type_param_idents
        .iter()
        .map(|(param_ident, trait_bound, assoc_param_name)| {
            quote! { <#param_ident as #trait_bound>::#assoc_param_name }
        })
}

impl<'a> GenericsResolver<'a> {
    fn new(
        main_trait: Option<&ItemTrait>,
        helper_trait_ident: &syn::Ident,
        type_param_idents: &'a [AssocBoundIdent],
    ) -> Self {
        let mut assoc_bounds = FxHashMap::<_, FxHashSet<_>>::default();

        type_param_idents
            .iter()
            .for_each(|(param_ident, trait_bound, _)| {
                assoc_bounds
                    .entry(param_ident)
                    .or_default()
                    .insert(trait_bound);
            });

        let assoc_bound_type_params = type_param_idents
            .iter()
            .map(|(param_ident, _, _)| *param_ident)
            .collect::<FxHashSet<_>>();

        let where_clause_predicates = assoc_bounds
            .into_iter()
            .map(|(param_ident, trait_bounds)| {
                let trait_bounds = trait_bounds.into_iter();
                quote! { #param_ident: #(#trait_bounds)+* }
            })
            .chain(core::iter::once_with(|| {
                let main_trait_generics = main_trait
                    .map(|main_trait| main_trait.generics.type_params().collect::<Vec<_>>())
                    .unwrap_or_default();
                let assoc_bounds = gen_assoc_bounds(type_param_idents);
                quote! { Self: #helper_trait_ident<#(#main_trait_generics,)* #(#assoc_bounds),*> }
            }))
            .collect();

        Self {
            assoc_bound_type_params,
            where_clause_predicates,
        }
    }
}

impl ImplItemResolver {
    fn new(
        main_trait: Option<&ItemTrait>,
        helper_trait_ident: &syn::Ident,
        type_param_idents: &[AssocBoundIdent],
    ) -> Self {
        let main_trait_generics = main_trait
            .map(|main_trait| main_trait.generics.type_params().collect::<Vec<_>>())
            .unwrap_or_default();
        let assoc_bounds = gen_assoc_bounds(type_param_idents);

        Self {
            self_as_helper_trait: quote! {
                <Self as #helper_trait_ident<#(#main_trait_generics,)* #(#assoc_bounds),*>>
            },
        }
    }
}

impl VisitMut for GenericsResolver<'_> {
    fn visit_generics_mut(&mut self, node: &mut syn::Generics) {
        node.make_where_clause();
        syn::visit_mut::visit_generics_mut(self, node);

        let node_type_params: FxHashSet<_> = node
            .type_params()
            .into_iter()
            .map(|type_param| type_param.ident.clone())
            .collect();

        for assoc_bound_type_param in &self.assoc_bound_type_params {
            if !node_type_params.contains(assoc_bound_type_param) {
                node.params.push(syn::GenericParam::Type(
                    syn::parse_quote!(#assoc_bound_type_param),
                ));
            }
        }
    }

    fn visit_type_param_mut(&mut self, node: &mut syn::TypeParam) {
        node.bounds = syn::punctuated::Punctuated::new();
    }

    fn visit_where_clause_mut(&mut self, node: &mut syn::WhereClause) {
        let predicates = &self.where_clause_predicates;
        node.predicates = syn::parse_quote! {#(#predicates),*};
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
