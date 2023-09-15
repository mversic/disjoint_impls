use super::*;

struct UnconstrainedParamDetector<'ast>(&'ast syn::TypeParam, bool);

impl<'ast> UnconstrainedParamDetector<'ast> {
    fn new(type_param: &'ast syn::TypeParam) -> Self {
        Self(type_param, false)
    }
}

impl<'ast> Visit<'ast> for UnconstrainedParamDetector<'ast> {
    fn visit_path(&mut self, node: &'ast syn::Path) {
        if node.is_ident(&self.0.ident) {
            self.1 = true;
        }
    }
}

fn remove_unconstrained_params(impl_: &mut ItemImpl) {
    impl_.generics.params = core::mem::take(&mut impl_.generics.params)
        .into_iter()
        .filter_map(|param| {
            if let syn::GenericParam::Type(type_param) = param {
                let mut unconstrained_param_detector = UnconstrainedParamDetector::new(&type_param);
                unconstrained_param_detector.visit_type(&*impl_.self_ty);

                if unconstrained_param_detector.1 {
                    return Some(syn::GenericParam::Type(type_param));
                }

                return None;
            }

            Some(param)
        })
        .collect();
}

fn update_disjoint_impl_generics(
    impl_: &mut ItemImpl,
    params: Vec<Option<syn::Type>>,
) -> Vec<syn::Type> {
    remove_unconstrained_params(impl_);

    params
        .into_iter()
        .enumerate()
        .map(|(idx, param)| {
            if let Some(param) = param {
                param
            } else {
                let missing_param = format_ident!("_MŠČ{idx}");

                impl_
                    .generics
                    .params
                    .push(syn::parse_quote!(#missing_param));

                syn::parse_quote!(#missing_param)
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
                .map(|param_ident| {
                    if let Some(&param) = params.get(&param_ident) {
                        Some(param.clone())
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    impls
        .iter_mut()
        .zip(type_params)
        .for_each(|(impl_, params)| {
            let params = update_disjoint_impl_generics(impl_, params);

            impl_.trait_.as_mut().map(|(_, trait_, _)| {
                if let Some(last_seg) = trait_.segments.last_mut() {
                    let params = params.into_iter();

                    match &mut last_seg.arguments {
                        syn::PathArguments::None => {
                            last_seg.arguments = syn::PathArguments::AngleBracketed(
                                syn::parse_quote!(<#(#params),*>),
                            )
                        }
                        syn::PathArguments::AngleBracketed(bracketed) => {
                            bracketed.args = params
                                .map::<syn::GenericArgument, _>(|param| syn::parse_quote!(#param))
                                .collect();
                        }
                        syn::PathArguments::Parenthesized(_) => {
                            unreachable!("Not a valid trait name")
                        }
                    }
                }
            });
        });

    impls.iter_mut().for_each(|impl_| {
        impl_.trait_.as_mut().map(|(_, trait_, _)| {
            if let Some(last_seg) = trait_.segments.last_mut() {
                last_seg.ident = format_ident!("_{}", &last_seg.ident);
            }
        });
    });

    impls
}
