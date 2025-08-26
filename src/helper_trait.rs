use super::*;

/// Generate helper trait
///
/// Helper trait contains all items of the main trait but is parametrized with
/// type parameters corresponding to a minimal set of associated bounds
/// required to uniquely identify all of the disjoint impls
pub fn generate(
    main_trait: Option<&ItemTrait>,
    impl_group_idx: usize,
    impl_group: &ImplGroup,
) -> Option<syn::ItemTrait> {
    let assoc_binding_count = &impl_group.assoc_bindings.idents().count();

    let mut helper_trait = if let Some(helper_trait) = main_trait {
        helper_trait.clone()
    } else if let Some(helper_trait) = impl_group.item_impls.first().cloned() {
        let syn::ItemImpl {
            attrs,
            unsafety,
            mut self_ty,
            items,
            ..
        } = helper_trait.clone();

        let items = gen_inherent_impl_items(&items);
        let impl_generics = &impl_group.generics.params;

        if let syn::Type::Path(type_path) = &mut *self_ty {
            type_path.path.segments.last_mut().unwrap().arguments = syn::PathArguments::None;
        }

        syn::parse_quote! {
            #(#attrs)*
            #unsafety trait #self_ty <#impl_generics> {
                #(#items)*
            }
        }
    } else {
        return None;
    };

    let mut assoc_binding_idents = impl_group.assoc_bindings.idents().enumerate().map(
        |(i, ((bounded, trait_bound), assoc_binding))| {
            let pos = syn::LitInt::new(&i.to_string(), proc_macro2::Span::call_site());
            quote!(#pos. <#bounded as #trait_bound>::#assoc_binding)
        },
    );

    let doc_str = format!(
        "Helper trait with arguments corresponding to the following associated bindings:\n{}\n",
        assoc_binding_idents.join(",\n")
    );

    helper_trait.attrs.push(parse_quote!(#[doc = #doc_str]));
    helper_trait.vis = syn::Visibility::Public(syn::parse_quote!(pub));
    helper_trait.ident = gen_ident(&helper_trait.ident, impl_group_idx);

    let start_idx = helper_trait.generics.params.len();
    helper_trait.generics.params = combine_generic_args(
        (start_idx..start_idx + assoc_binding_count).map(param::gen_indexed_param_ident),
        &helper_trait.generics,
    )
    .map(|arg| -> syn::GenericParam { syn::parse_quote!(#arg) })
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

fn gen_inherent_impl_items(
    impl_items: &[syn::ImplItem],
) -> impl Iterator<Item = syn::ImplItem> + '_ {
    impl_items.iter().map(|impl_item| match impl_item {
        syn::ImplItem::Const(item) => {
            let syn::ImplItemConst {
                attrs,
                ident,
                generics,
                ty,
                ..
            } = &item;

            syn::parse_quote! {
                #(#attrs),*
                const #ident: #ty #generics;
            }
        }
        syn::ImplItem::Type(item) => {
            let syn::ImplItemType {
                attrs,
                ident,
                generics,
                ..
            } = &item;

            let (impl_generics, _, where_clause) = generics.split_for_impl();

            syn::parse_quote! {
                #(#attrs),*
                type #ident #impl_generics #where_clause;
            }
        }
        syn::ImplItem::Fn(item) => {
            let syn::ImplItemFn { attrs, sig, .. } = &item;

            syn::parse_quote! {
                #(#attrs),*
                #sig;
            }
        }
        item => abort!(item, "Not supported"),
    })
}

/// Generate ident of the helper trait
pub fn gen_ident(ident: &syn::Ident, idx: usize) -> syn::Ident {
    format_ident!("{}{}", ident, idx)
}
