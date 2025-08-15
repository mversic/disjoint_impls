use syn::parse_quote;

use super::*;

pub fn generate(impl_group_idx: usize, mut impl_group: ImplGroup) -> Vec<ItemImpl> {
    let assoc_bindings_idents = impl_group.assoc_bindings.idents().collect::<Vec<_>>();

    let Some(example_impl) = impl_group.item_impls.first() else {
        return Vec::new();
    };

    if example_impl.trait_.is_none()
        && let syn::Type::Path(mut self_ty) = (*example_impl.self_ty).clone()
    {
        gen_inherent_self_ty_args(&mut self_ty, &example_impl.generics);

        impl_group
            .item_impls
            .iter_mut()
            .for_each(|syn::ItemImpl { trait_, .. }| {
                *trait_ = Some((None, parse_quote!(#self_ty), parse_quote![for]));
            });

        impl_group
            .item_impls
            .iter_mut()
            .flat_map(|impl_| &mut impl_.items)
            .for_each(|item| {
                let vis = match item {
                    syn::ImplItem::Const(syn::ImplItemConst { vis, .. }) => vis,
                    syn::ImplItem::Type(syn::ImplItemType { vis, .. }) => vis,
                    syn::ImplItem::Fn(syn::ImplItemFn { vis, .. }) => vis,
                    _ => return,
                };

                *vis = syn::Visibility::Inherited;
            });
    }

    let impl_assoc_bindings = impl_group.assoc_bindings.payloads().map(|impl_payloads| {
        impl_payloads
            .iter()
            .enumerate()
            .map(|(i, payload)| {
                payload.map(|payload| quote!(#payload)).unwrap_or_else(|| {
                    let ((param, trait_bound), assoc_type) = assoc_bindings_idents[i];
                    quote!(<#param as #trait_bound>::#assoc_type)
                })
            })
            .collect::<Vec<_>>()
    });

    impl_group
        .item_impls
        .iter_mut()
        .zip(impl_assoc_bindings)
        .for_each(|(impl_, assoc_bindings)| {
            let trait_ = &mut impl_.trait_.as_mut().unwrap().1;
            let path = trait_.segments.last_mut().unwrap();

            match &mut path.arguments {
                syn::PathArguments::None => {
                    let bracketed = syn::parse_quote! { <#( #assoc_bindings ),*> };
                    path.arguments = syn::PathArguments::AngleBracketed(bracketed)
                }
                syn::PathArguments::AngleBracketed(bracketed) => {
                    bracketed.args = assoc_bindings
                        .into_iter()
                        .map::<syn::GenericArgument, _>(|param| syn::parse_quote!(#param))
                        .chain(core::mem::take(&mut bracketed.args))
                        .collect();
                }
                syn::PathArguments::Parenthesized(_) => unreachable!("Not a valid trait name"),
            }

            path.ident = helper_trait::gen_ident(&path.ident, impl_group_idx);
        });

    impl_group.item_impls
}
