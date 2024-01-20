use syn::parse_quote;

use super::*;

pub fn gen(impl_group_idx: usize, mut impls: Vec<ItemImpl>) -> Vec<ItemImpl> {
    let Some(example_impl) = impls.first() else {
        return Vec::new();
    };

    if example_impl.trait_.is_none() {
        if let syn::Type::Path(mut self_ty) = (*example_impl.self_ty).clone() {
            let generics = &example_impl.generics;

            let ty_params = &mut self_ty.path.segments.last_mut().unwrap().arguments;
            InherentImplGenericArgPruner::new(&generics).visit_path_arguments_mut(ty_params);

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

    impls
        .iter_mut()
        .zip(type_params)
        .for_each(|(impl_, params)| {
            let params = update_disjoint_impl_generics(impl_, params);

            let trait_ = &mut impl_.trait_.as_mut().unwrap().1;
            let path = trait_.segments.last_mut().unwrap();

            match &mut path.arguments {
                syn::PathArguments::None => {
                    let bracketed = syn::parse_quote! { <#( #params ),*> };
                    path.arguments = syn::PathArguments::AngleBracketed(bracketed)
                }
                syn::PathArguments::AngleBracketed(bracketed) => {
                    bracketed.args = params
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

// TODO: Why is this required? Can't I name missing params the same as other params
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
