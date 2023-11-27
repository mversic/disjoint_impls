//! Contains logic related to generating impl of the main trait

use rustc_hash::FxHashSet;
use syn::{parse_quote, visit_mut::VisitMut};

use crate::param::get_param_idents;

use super::*;

struct GenericsResolver {
    assoc_bound_type_params: FxHashSet<syn::Type>,
    where_clause_predicates: Vec<TokenStream2>,
}

struct ImplItemResolver {
    self_as_helper_trait: TokenStream2,
}

/// Generate main trait impl
pub fn gen(main_trait: Option<&ItemTrait>, impls: &[ItemImpl], idx: usize) -> Option<ItemImpl> {
    let example_impl = impls.get(0)?;
    let self_ty = &*example_impl.self_ty;

    let mut main_trait_impl = main_trait
        .map(|main_trait| gen_dummy_impl_from_trait_definition(main_trait, self_ty))
        .unwrap_or_else(|| gen_dummy_impl_from_inherent_impl(example_impl, self_ty));

    let helper_trait_ident = main_trait_impl.trait_.as_ref().map_or_else(
        || {
            if let syn::Type::Path(type_path) = self_ty {
                return Some(helper_trait::gen_ident(
                    &type_path.path.segments.last()?.ident,
                    idx,
                ));
            }

            None
        },
        |main_trait| {
            Some(helper_trait::gen_ident(
                &main_trait.1.segments.last()?.ident,
                idx,
            ))
        },
    )?;

    let AssocBounds {
        type_param_idents, ..
    } = AssocBounds::find(impls);

    let main_trait_impl_param_idents: FxHashSet<_> = get_param_idents(&main_trait_impl.generics)
        .cloned()
        .collect();

    for type_param in example_impl.generics.type_params() {
        if !main_trait_impl_param_idents.contains(&type_param.ident) {
            let mut type_param = type_param.clone();

            type_param.bounds = syn::punctuated::Punctuated::new();
            main_trait_impl.generics.params.push(type_param.into());
        }
    }
    for const_param in example_impl.generics.const_params() {
        if !main_trait_impl_param_idents.contains(&const_param.ident) {
            main_trait_impl.generics.params.extend(
                example_impl
                    .generics
                    .const_params()
                    .cloned()
                    .map(syn::GenericParam::from),
            );
        }
    }
    for lifetime_param in example_impl.generics.lifetimes() {
        if !main_trait_impl_param_idents.contains(&lifetime_param.lifetime.ident) {
            main_trait_impl.generics.params.extend(
                example_impl
                    .generics
                    .lifetimes()
                    .cloned()
                    .map(syn::GenericParam::from),
            );
        }
    }

    let mut generics_resolver =
        GenericsResolver::new(main_trait, &helper_trait_ident, &type_param_idents);
    generics_resolver.visit_generics_mut(&mut main_trait_impl.generics);

    let mut impl_item_resolver =
        ImplItemResolver::new(main_trait, &helper_trait_ident, &type_param_idents);
    main_trait_impl
        .items
        .iter_mut()
        .for_each(|item| impl_item_resolver.visit_impl_item_mut(item));

    Some(main_trait_impl)
}

/// Generates main trait implementation with item values set to dummy values
fn gen_dummy_impl_from_inherent_impl(
    inherent_impl: &ItemImpl,
    self_ty: &SelfType,
) -> syn::ItemImpl {
    let mut main_trait_impl = inherent_impl.clone();

    main_trait_impl.attrs = Vec::new();
    main_trait_impl.self_ty = Box::new(self_ty.clone());
    main_trait_impl
        .items
        .iter_mut()
        .for_each(|item| match item {
            syn::ImplItem::Const(item) => {
                item.expr = parse_quote! { DUMMY };
            }
            syn::ImplItem::Type(item) => {
                item.ty = parse_quote! { DUMMY };
            }
            syn::ImplItem::Fn(item) => {
                item.block = parse_quote! { DUMMY };
            }
            syn::ImplItem::Macro(_) => unimplemented!("Macro expansion not supported yet"),
            syn::ImplItem::Verbatim(_) => unimplemented!("Verbatim not supported yet"),
            _ => unimplemented!("Unknown item"),
        });

    main_trait_impl
}

/// Generates main trait implementation with item values set to dummy values
fn gen_dummy_impl_from_trait_definition(
    main_trait: &ItemTrait,
    self_ty: &SelfType,
) -> syn::ItemImpl {
    let ItemTrait {
        unsafety,
        generics,
        ident,
        items,
        ..
    } = main_trait;

    let items = items.iter().map(|item| {
        let item: syn::ImplItem = match item {
            syn::TraitItem::Const(item) => {
                let syn::TraitItemConst {
                    generics,
                    ident,
                    ty,
                    ..
                } = item;

                let (_, ty_generics, where_clause) = generics.split_for_impl();

                parse_quote! {
                    const #ident #ty_generics: #ty = DUMMY #where_clause;
                }
            }
            syn::TraitItem::Type(item) => {
                let syn::TraitItemType {
                    generics, ident, ..
                } = item;

                let (_, ty_generics, where_clause) = generics.split_for_impl();

                parse_quote! {
                    type #ident #ty_generics = DUMMY #where_clause;
                }
            }
            syn::TraitItem::Fn(item) => {
                let sig = &item.sig;

                parse_quote! {
                    #sig { /* DUMMY */ }
                }
            }
            syn::TraitItem::Macro(_) => unimplemented!("Macro expansion not supported yet"),
            syn::TraitItem::Verbatim(_) => unimplemented!("Verbatim not supported yet"),
            _ => unimplemented!("Unknown item"),
        };

        item
    });

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    parse_quote! {
        #unsafety impl #impl_generics #ident #ty_generics for #self_ty #where_clause {
            #(#items)*
        }
    }
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

impl GenericsResolver {
    fn new(
        main_trait: Option<&ItemTrait>,
        helper_trait_ident: &syn::Ident,
        type_param_idents: &[AssocBoundIdent],
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
            .map(|(param_ident, _, _)| param_ident)
            .cloned()
            .collect::<FxHashSet<_>>();

        let assoc_bound_predicates = assoc_bounds.into_iter().map(|(param_ident, trait_bounds)| {
            let trait_bounds = trait_bounds.into_iter();
            quote! { #param_ident: #(#trait_bounds)+* }
        });
        let where_clause_predicates = if let Some(main_trait) = main_trait {
            let main_trait_predicates = main_trait
                .generics
                .where_clause
                .as_ref()
                .map(|where_clause| {
                    where_clause
                        .predicates
                        .iter()
                        .map(|predicate| quote!(#predicate))
                        .collect::<Vec<_>>()
                })
                .unwrap_or_default();

            main_trait_predicates
                .into_iter()
                .chain(assoc_bound_predicates.chain(core::iter::once_with(|| {
                    let helper_trait_bound = gen_helper_trait_bound(
                        Some(main_trait),
                        helper_trait_ident,
                        type_param_idents,
                    );

                    quote! { Self: #helper_trait_bound }
                })))
                .collect()
        } else {
            assoc_bound_predicates
                .chain(core::iter::once_with(|| {
                    let helper_trait_bound =
                        gen_helper_trait_bound(None, helper_trait_ident, type_param_idents);

                    quote! { Self: #helper_trait_bound }
                }))
                .collect()
        };

        Self {
            assoc_bound_type_params,
            where_clause_predicates,
        }
    }
}

fn gen_helper_trait_bound(
    main_trait: Option<&ItemTrait>,
    helper_trait_ident: &syn::Ident,
    type_param_idents: &[AssocBoundIdent],
) -> TokenStream2 {
    let assoc_bounds = gen_assoc_bounds(type_param_idents);

    if let Some(main_trait) = main_trait {
        let main_trait_ty_generics = main_trait.generics.params.iter().map(|param| match param {
            syn::GenericParam::Lifetime(syn::LifetimeParam { lifetime, .. }) => quote! {#lifetime},
            syn::GenericParam::Type(syn::TypeParam { ident, .. }) => quote! {#ident},
            syn::GenericParam::Const(syn::ConstParam { ident, .. }) => quote! {#ident},
        });

        return quote! { #helper_trait_ident<#(#assoc_bounds,)* #(#main_trait_ty_generics),*> };
    }

    quote! { #helper_trait_ident<#(#assoc_bounds),*> }
}

impl ImplItemResolver {
    fn new(
        main_trait: Option<&ItemTrait>,
        helper_trait_ident: &syn::Ident,
        type_param_idents: &[AssocBoundIdent],
    ) -> Self {
        let helper_trait_bound =
            gen_helper_trait_bound(main_trait, helper_trait_ident, type_param_idents);

        Self {
            self_as_helper_trait: quote! {
                <Self as #helper_trait_bound>
            },
        }
    }
}

impl VisitMut for GenericsResolver {
    fn visit_generics_mut(&mut self, node: &mut syn::Generics) {
        node.make_where_clause();

        let node_type_params: FxHashSet<syn::Type> = node
            .type_params()
            .map(|type_param| {
                let type_param_ident = &type_param.ident;
                parse_quote!(#type_param_ident)
            })
            .collect();

        for assoc_bound_type_param in &self.assoc_bound_type_params {
            if !node_type_params.contains(assoc_bound_type_param) {
                use syn::parse;

                if let Ok(assoc_bound_type_param) = parse(quote!(#assoc_bound_type_param).into()) {
                    node.params
                        .push(syn::GenericParam::Type(assoc_bound_type_param));
                }
            }
        }

        let predicates = &self.where_clause_predicates;
        node.where_clause = parse_quote!(where #(#predicates),*);
    }
}

impl VisitMut for ImplItemResolver {
    fn visit_impl_item_const_mut(&mut self, node: &mut syn::ImplItemConst) {
        let self_as_helper_trait = &self.self_as_helper_trait;

        let ident = &node.ident;
        node.expr = parse_quote!(
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
            syn::FnArg::Receiver(_) => parse_quote!(self),
            syn::FnArg::Typed(arg) => arg.pat.clone(),
        });
        node.block = parse_quote!({
            #self_as_helper_trait::#ident(#(#inputs,)* #variadic)
        });
    }

    fn visit_impl_item_type_mut(&mut self, node: &mut syn::ImplItemType) {
        let self_as_helper_trait = &self.self_as_helper_trait;

        let ident = &node.ident;
        node.ty = parse_quote!(
            #self_as_helper_trait::#ident
        );
    }

    fn visit_impl_item_macro_mut(&mut self, _node: &mut syn::ImplItemMacro) {
        unimplemented!("Trait macros are not supported")
    }
}
