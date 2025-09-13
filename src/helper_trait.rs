use crate::main_trait::is_remote;

use super::*;

/// Generate helper trait
///
/// Helper trait contains all items of the main trait but is parametrized with
/// type parameters corresponding to a minimal set of associated bindings
/// that uniquely identify all of the disjoint impls within an impl group
pub fn generate(
    main_trait: Option<&ItemTrait>,
    impl_group_idx: usize,
    impl_group: &ImplGroup,
) -> Option<syn::ItemTrait> {
    let assoc_binding_count = &impl_group.assoc_bindings.generalized_idents().count();

    let mut helper_trait = if let Some(helper_trait) = main_trait {
        helper_trait.clone()
    } else {
        let impl_items = gen_inherent_trait_items(impl_group.impl_items.as_ref().unwrap());

        let mut self_ty = impl_group.id.self_ty.clone();
        if let syn::Type::Path(type_path) = &mut self_ty {
            type_path.path.segments.last_mut().unwrap().arguments = syn::PathArguments::None;
        }

        let impl_generics = &impl_group.params;

        parse_quote! {
            trait #self_ty <#(#impl_generics,)*> {
                #(#impl_items)*
            }
        }
    };

    helper_trait.attrs = core::mem::take(&mut helper_trait.attrs)
        .into_iter()
        .filter(|attr| !is_remote(attr))
        .collect();

    helper_trait.vis = syn::Visibility::Public(parse_quote!(pub));
    helper_trait.ident = gen_ident(&helper_trait.ident, impl_group_idx);

    let start_idx = helper_trait.generics.params.len();
    helper_trait.generics.params = combine_generic_args(
        (start_idx..start_idx + assoc_binding_count).map(|i| format_ident!("_TŠČ{i}")),
        &helper_trait.generics,
    )
    .map(|arg| -> syn::GenericParam { parse_quote!(#arg) })
    .collect();

    Some(helper_trait)
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

fn gen_inherent_trait_items(impl_items: &ImplItems) -> impl Iterator<Item = syn::TraitItem> {
    let assoc_consts = impl_items
        .assoc_consts
        .iter()
        .map(|(ident, ty)| syn::TraitItem::Const(parse_quote! { const #ident: #ty; }));
    let assoc_types = impl_items
        .assoc_types
        .iter()
        .map(|ident| syn::TraitItem::Type(parse_quote! { const #ident; }));
    let fns = impl_items
        .fns
        .values()
        .map(|sig| syn::TraitItem::Fn(parse_quote! { #sig; }));

    assoc_consts.chain(assoc_types).chain(fns)
}

/// Generate ident of the helper trait
pub fn gen_ident(ident: &syn::Ident, idx: usize) -> syn::Ident {
    format_ident!("{}{}", ident, idx)
}
