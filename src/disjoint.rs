use super::*;

pub fn generate(
    impl_group_idx: usize,
    mut impl_group: ImplGroup,
) -> impl Iterator<Item = ItemImpl> {
    if impl_group.id.is_inherent() {
        for (impl_, args) in &mut impl_group.impls {
            traitize_inherent_impl(args, impl_, &impl_group.id.self_ty);
        }
    }

    let ty_params = impl_group
        .impls
        .iter()
        .map(|(impl_, _)| {
            impl_
                .generics
                .type_params()
                .map(|param| param.ident.clone())
                .collect::<IndexSet<_>>()
        })
        .collect::<Vec<_>>();

    let mut impl_assoc_bindings = impl_group
        .trait_bounds
        .0
        .values()
        .flat_map(|(orig_trait_bound, assoc_bindings)| {
            assoc_bindings.iter().map(|(assoc_ident, (_, payloads))| {
                payloads.iter().enumerate().map(|(i, payload)| {
                    payload.as_ref().map_or_else(
                        || assoc_binding_default(&orig_trait_bound[i], assoc_ident),
                        |payload| {
                            if let syn::Type::Path(syn::TypePath { path, .. }) = payload
                                && let Some(ident) = path.get_ident()
                                && ident.to_string().starts_with("_TŠČ")
                                && !ty_params[i].contains(ident)
                            {
                                return assoc_binding_default(&orig_trait_bound[i], assoc_ident);
                            }

                            payload.clone()
                        },
                    )
                })
            })
        })
        .collect::<Vec<_>>();

    impl_group.impls.iter_mut().for_each(|(impl_, _)| {
        let assoc_bindings = impl_assoc_bindings.iter_mut().map(|x| x.next().unwrap());

        let trait_ = &mut impl_.trait_.as_mut().unwrap().1;
        let path = trait_.segments.last_mut().unwrap();

        prepend_args(&mut path.arguments, &assoc_bindings.collect::<Vec<_>>());
        path.ident = helper_trait::gen_ident(&impl_group.id, impl_group_idx);
    });

    impl_group.impls.into_iter().map(|(impl_, _)| impl_)
}

/// Modify impls into trait impls from inherent impls
pub fn traitize_inherent_impl(
    generic_args: &[syn::GenericArgument],
    impl_: &mut syn::ItemImpl,
    self_ty: &syn::Type,
) {
    let syn::Type::Path(type_path) = &self_ty else {
        unreachable!()
    };

    let ident = &type_path.path.segments.last().unwrap().ident;
    let trait_ = parse_quote! { #ident <#(#generic_args),*> };

    impl_.items.iter_mut().for_each(|item| {
        let vis = match item {
            syn::ImplItem::Const(syn::ImplItemConst { vis, .. }) => vis,
            syn::ImplItem::Type(syn::ImplItemType { vis, .. }) => vis,
            syn::ImplItem::Fn(syn::ImplItemFn { vis, .. }) => vis,
            _ => return,
        };

        *vis = syn::Visibility::Inherited;
    });

    impl_.trait_ = Some((None, trait_, Default::default()));
}
