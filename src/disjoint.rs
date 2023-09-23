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

pub fn gen(mut impls: Vec<ItemImpl>) -> Vec<ItemImpl> {
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
            if let Some(last_seg) = trait_.segments.last_mut() {
                last_seg.ident = helper_trait::gen_ident(&last_seg.ident);
            }
        } else if let syn::Type::Path(type_path) = &*impl_.self_ty {
            if let Some(last_seg) = type_path.path.segments.last() {
                let helper_trait_ident = helper_trait::gen_ident(&last_seg.ident);
                impl_.trait_ = Some((None, parse_quote!(#helper_trait_ident), parse_quote![for]));
            }
        }
    });

    impls
        .iter_mut()
        .zip(type_params)
        .for_each(|(impl_, params)| {
            let params = update_disjoint_impl_generics(impl_, params);

            if let Some((_, trait_, _)) = impl_.trait_.as_mut() {
                if let Some(last_seg) = trait_.segments.last_mut() {
                    match &mut last_seg.arguments {
                        syn::PathArguments::None => {
                            last_seg.arguments = syn::PathArguments::AngleBracketed(
                                syn::parse_quote!(<#(#params),*>),
                            )
                        }
                        syn::PathArguments::AngleBracketed(bracketed) => {
                            bracketed.args.extend(
                                params.into_iter().map::<syn::GenericArgument, _>(
                                    |param| syn::parse_quote!(#param),
                                ),
                            );
                        }
                        syn::PathArguments::Parenthesized(_) => {
                            unreachable!("Not a valid trait name")
                        }
                    }
                }
            }
        });

    impls
}
