use syn::parse_quote;

use super::*;

pub fn gen(impl_group_idx: usize, mut impls: Vec<ItemImpl>) -> Vec<ItemImpl> {
    let Some(example_impl) = impls.first() else {
        return Vec::new();
    };

    if example_impl.trait_.is_none() {
        if let syn::Type::Path(mut self_ty) = (*example_impl.self_ty).clone() {
            gen_inherent_self_ty_args(&mut self_ty, &example_impl.generics);

            impls.iter_mut().for_each(|syn::ItemImpl { trait_, .. }| {
                *trait_ = Some((None, parse_quote!(#self_ty), parse_quote![for]));
            });

            impls
                .iter_mut()
                .flat_map(|impl_| &mut impl_.items)
                .for_each(|item| match item {
                    syn::ImplItem::Const(item) => item.vis = syn::Visibility::Inherited,
                    syn::ImplItem::Type(item) => item.vis = syn::Visibility::Inherited,
                    syn::ImplItem::Fn(item) => item.vis = syn::Visibility::Inherited,
                    _ => {}
                });
        }
    }

    let AssocBounds {
        assoc_bound_idents,
        assoc_bounds,
        ..
    } = AssocBounds::find(&impls);

    let assoc_bounds = assoc_bounds
        .iter()
        .map(|assoc_bound| {
            assoc_bound_idents
                .iter()
                .map(|param_ident| {
                    let (param, trait_bound, assoc_type) = &param_ident;

                    assoc_bound
                        .get(param_ident)
                        .map(|&payload| quote!(#payload))
                        .unwrap_or_else(|| quote!(<#param as #trait_bound>::#assoc_type))
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    impls
        .iter_mut()
        .zip(assoc_bounds)
        .for_each(|(impl_, assoc_bounds)| {
            let trait_ = &mut impl_.trait_.as_mut().unwrap().1;
            let path = trait_.segments.last_mut().unwrap();

            match &mut path.arguments {
                syn::PathArguments::None => {
                    let bracketed = syn::parse_quote! { <#( #assoc_bounds ),*> };
                    path.arguments = syn::PathArguments::AngleBracketed(bracketed)
                }
                syn::PathArguments::AngleBracketed(bracketed) => {
                    bracketed.args = assoc_bounds
                        .into_iter()
                        .map::<syn::GenericArgument, _>(|param| syn::parse_quote!(#param))
                        .chain(core::mem::take(&mut bracketed.args))
                        .collect();
                }
                syn::PathArguments::Parenthesized(_) => unreachable!("Not a valid trait name"),
            }

            path.ident = helper_trait::gen_ident(&path.ident, impl_group_idx);
        });

    impls
}
