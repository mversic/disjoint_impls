use syn::parse_quote;

use super::*;

fn update_disjoint_impl_generics(
    impl_: &mut ItemImpl,
    params: Vec<Option<syn::Type>>,
) -> Vec<syn::Type> {
    params
        .into_iter()
        .enumerate()
        .map(|(idx, param)| {
            if let Some(param) = param {
                param
            } else {
                let missing_param = format_ident!("_MŠČ{idx}");
                impl_.generics.params.push(parse_quote!(#missing_param));

                parse_quote!(#missing_param)
            }
        })
        .collect()
}

pub fn gen(mut impls: Vec<ItemImpl>, idx: usize) -> Vec<ItemImpl> {
    let AssocBounds {
        type_param_idents,
        type_params,
    } = AssocBounds::find(&impls);

    let type_params = type_params
        .iter()
        .map(|params| {
            type_param_idents
                .iter()
                .map(|param_ident| params.get(param_ident).map(|&param| param.clone()))
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    impls.iter_mut().for_each(|impl_| {
        if let Some((_, trait_, _)) = impl_.trait_.as_mut() {
            let path = trait_.segments.last_mut().unwrap();
            path.ident = helper_trait::gen_ident(&path.ident, idx);
        } else if let syn::Type::Path(type_path) = &*impl_.self_ty {
            let path = type_path.path.segments.last().unwrap();
            let helper_trait_ident = helper_trait::gen_ident(&path.ident, idx);
            impl_.trait_ = Some((None, parse_quote!(#helper_trait_ident), parse_quote![for]));
        }
    });

    impls
        .iter_mut()
        .zip(type_params)
        .for_each(|(impl_, params)| {
            let params = update_disjoint_impl_generics(impl_, params);

            if let Some((_, trait_, _)) = impl_.trait_.as_mut() {
                let path = trait_.segments.last_mut().unwrap();

                match &mut path.arguments {
                    syn::PathArguments::None => {
                        path.arguments =
                            syn::PathArguments::AngleBracketed(syn::parse_quote!(<#(#params),*>))
                    }
                    syn::PathArguments::AngleBracketed(bracketed) => {
                        bracketed.args = params
                            .into_iter()
                            .map::<syn::GenericArgument, _>(|param| syn::parse_quote!(#param))
                            .chain(core::mem::take(&mut bracketed.args))
                            .collect();
                    }
                    syn::PathArguments::Parenthesized(_) => {
                        unreachable!("Not a valid trait name")
                    }
                }
            }
        });

    impls
        .iter_mut()
        .flat_map(|impl_| &mut impl_.items)
        .for_each(|item| match item {
            syn::ImplItem::Const(item) => {
                item.vis = syn::Visibility::Inherited;
            }
            syn::ImplItem::Fn(item) => {
                item.vis = syn::Visibility::Inherited;
            }
            syn::ImplItem::Type(item) => {
                item.vis = syn::Visibility::Inherited;
            }
            syn::ImplItem::Macro(_) => unimplemented!("Macro expansion not supported yet"),
            syn::ImplItem::Verbatim(_) => unimplemented!("Verbatim not supported yet"),
            _ => unimplemented!("Unknown item"),
        });

    impls
}
