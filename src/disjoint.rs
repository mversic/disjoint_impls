use super::*;

pub fn generate(
    impl_group_idx: usize,
    mut impl_group: ImplGroup,
) -> impl Iterator<Item = ItemImpl> {
    if impl_group.id.trait_.is_none() {
        impl_group
            .impls
            .iter_mut()
            .flat_map(|(generic_args, impl_)| {
                let syn::Type::Path(type_path) = &mut impl_group.id.self_ty else {
                    unreachable!()
                };

                let ident = &type_path.path.segments.last_mut().unwrap().ident;

                impl_.trait_ = Some((
                    None,
                    parse_quote!(#ident<#(#generic_args),*>),
                    Default::default(),
                ));

                &mut impl_.items
            })
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

    let mut impl_assoc_bindings = impl_group
        .trait_bounds
        .0
        .values()
        .flat_map(|(orig_trait_bound, assoc_bindings)| {
            assoc_bindings.iter().map(move |(ident, (_, payloads))| {
                payloads.iter().enumerate().map(move |(i, orig_payload)| {
                    orig_payload.as_ref().map_or_else(
                        || {
                            let (bounded, trait_bound) = &orig_trait_bound[i];
                            quote!(<#bounded as #trait_bound>::#ident)
                        },
                        |p| quote!(#p),
                    )
                })
            })
        })
        .collect::<Vec<_>>();

    impl_group.impls.iter_mut().for_each(|(_, impl_)| {
        let assoc_bindings = impl_assoc_bindings.iter_mut().map(|x| x.next().unwrap());

        let trait_ = &mut impl_.trait_.as_mut().unwrap().1;
        let path = trait_.segments.last_mut().unwrap();

        match &mut path.arguments {
            syn::PathArguments::None => {
                let bracketed = parse_quote! { <#( #assoc_bindings ),*> };
                path.arguments = syn::PathArguments::AngleBracketed(bracketed);
            }
            syn::PathArguments::AngleBracketed(bracketed) => {
                bracketed.args = assoc_bindings
                    .map(|param| parse_quote!(#param))
                    .chain(core::mem::take(&mut bracketed.args))
                    .collect();
            }
            syn::PathArguments::Parenthesized(_) => unreachable!("Not a valid trait name"),
        }

        path.ident = helper_trait::gen_ident(&path.ident, impl_group_idx);
    });

    impl_group.impls.into_iter().map(|(_, impl_)| impl_)
}
