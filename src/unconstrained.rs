use super::*;

struct AssocBindingRemover<'a> {
    assoc_param: &'a TraitBoundIdent,
    assoc_param_bound: &'a syn::Ident,
}

impl VisitMut for AssocBindingRemover<'_> {
    fn visit_type_param_mut(&mut self, node: &mut syn::TypeParam) {
        if let syn::Type::Path(syn::TypePath { path, .. }) = &self.assoc_param.0.0
            && Some(&node.ident) == path.get_ident()
        {
            for bound in &mut node.bounds {
                self.visit_type_param_bound_mut(bound);
            }
        }
    }

    fn visit_predicate_type_mut(&mut self, node: &mut syn::PredicateType) {
        if node.bounded_ty == self.assoc_param.0.0 {
            for bound in &mut node.bounds {
                self.visit_type_param_bound_mut(bound);
            }
        }
    }

    fn visit_trait_bound_mut(&mut self, node: &mut syn::TraitBound) {
        if TraitBound::from(node.clone()) == self.assoc_param.1 {
            self.visit_path_mut(&mut node.path);
        }
    }

    fn visit_angle_bracketed_generic_arguments_mut(
        &mut self,
        node: &mut syn::AngleBracketedGenericArguments,
    ) {
        node.args = core::mem::take(&mut node.args)
            .into_iter()
            .filter(|arg| {
                if let syn::GenericArgument::AssocType(syn::AssocType { ident, .. }) = arg {
                    return ident != self.assoc_param_bound;
                }

                true
            })
            .collect();
    }
}

fn find_unconstrained_type_params(item_impl: &ItemImpl) -> impl Iterator<Item = &syn::Ident> {
    let indexed_params = param::index(item_impl);

    item_impl.generics.type_params().filter_map(move |param| {
        (!indexed_params.type_params.contains_key(&param.ident)).then_some(&param.ident)
    })
}

pub fn generate(
    main_trait: Option<&ItemTrait>,
    main_trait_impl: &mut ItemImpl,
    impl_group_idx: usize,
    impl_group: &ImplGroup,
) -> Option<syn::Macro> {
    let unconstrained = main_trait.map_or_else(
        || {
            let self_ty = &impl_group.id.self_ty;

            if let syn::Type::Path(type_path) = self_ty {
                return Some(gen_ident(&helper_trait::gen_ident(
                    &type_path.path.segments.last().unwrap().ident,
                    impl_group_idx,
                )));
            }

            None
        },
        |main_trait| {
            Some(gen_ident(&helper_trait::gen_ident(
                &main_trait.ident,
                impl_group_idx,
            )))
        },
    )?;

    let mut unconstrained_type_params: IndexSet<_> =
        find_unconstrained_type_params(main_trait_impl)
            .cloned()
            .collect();

    if !unconstrained_type_params.is_empty() {
        let mut item_impls = impl_group.item_impls.clone();

        let mut constrain_trait: ItemTrait = parse_quote! {
            trait #unconstrained {
                type Bound: ?Sized;
            }
        };

        let (last_assoc_param, last_assoc_param_bound) =
            impl_group.assoc_bindings.bindings.last().unwrap();

        let bounded = if let Some(main_trait) = main_trait {
            unconstrained_type_params.pop().unwrap();

            constrain_trait.generics.where_clause = main_trait.generics.where_clause.clone();
            constrain_trait.generics.params = main_trait
                .generics
                .type_params()
                .cloned()
                .map(syn::GenericParam::Type)
                .collect();

            item_impls
                .iter_mut()
                .enumerate()
                .for_each(|(i, item_impl)| {
                    if let Some((assoc_param_bound, _)) = last_assoc_param_bound[i].last() {
                        let mut assoc_binding_remover = AssocBindingRemover {
                            assoc_param: last_assoc_param,
                            assoc_param_bound,
                        };

                        assoc_binding_remover.visit_item_impl_mut(item_impl);
                    }
                    let unconstrained_type_params: IndexSet<_> =
                        find_unconstrained_type_params(item_impl).cloned().collect();
                    item_impl.generics.params = core::mem::take(&mut item_impl.generics.params)
                        .into_iter()
                        .filter(|param| {
                            if let syn::GenericParam::Type(param) = param {
                                return !unconstrained_type_params.contains(&param.ident);
                            }

                            true
                        })
                        .collect();
                });

            main_trait_impl.generics.params = core::mem::take(&mut main_trait_impl.generics.params)
                .into_iter()
                .filter(|param| {
                    if let syn::GenericParam::Type(param) = param {
                        return !unconstrained_type_params.contains(&param.ident);
                    }

                    true
                })
                .collect();

            last_assoc_param.0.0.clone()
        } else {
            let unconstrained_type_params = unconstrained_type_params.into_iter();
            parse_quote!((#(#unconstrained_type_params, )*))
        };

        item_impls.iter_mut().for_each(|item_impl| {
            item_impl.trait_ = Some((None, parse_quote!(#unconstrained), parse_quote![for]));
            item_impl.items = vec![parse_quote!(type Bound = #bounded;)];
        });

        main_trait_impl
            .generics
            .make_where_clause()
            .predicates
            .push(parse_quote! {
                Self: #unconstrained<Bound = #bounded>
            });

        return Some(parse_quote! {
            disjoint_impls::disjoint_impls! {
                #constrain_trait
                #( #item_impls )*
            }
        });
    }

    None
}

/// Generate ident of the helper trait for constraining params
fn gen_ident(ident: &syn::Ident) -> syn::Ident {
    format_ident!("{ident}Constraint")
}
