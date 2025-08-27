use syn::visit_mut::{visit_predicate_type_mut, visit_type_param_mut};

use super::*;

struct AssocGroupBindingRemover<'a> {
    param_to_prune: &'a syn::Ident,
    flagged_to_remove: bool,
}

struct ImplItemAssocBindingRemover<'a> {
    param_to_prune: &'a syn::Ident,
    prune_assoc_bindings: bool,
}

impl<'a> AssocGroupBindingRemover<'a> {
    fn new(assoc_param: &'a syn::Ident) -> Self {
        Self {
            param_to_prune: assoc_param,
            flagged_to_remove: false,
        }
    }
}

impl Visit<'_> for AssocGroupBindingRemover<'_> {
    fn visit_type_path(&mut self, node: &syn::TypePath) {
        if Some(self.param_to_prune) == node.path.get_ident() {
            self.flagged_to_remove = true;
        }
    }
}

impl<'a> ImplItemAssocBindingRemover<'a> {
    fn new(assoc_param: &'a syn::Ident) -> Self {
        Self {
            param_to_prune: assoc_param,
            prune_assoc_bindings: false,
        }
    }
}

impl VisitMut for ImplItemAssocBindingRemover<'_> {
    fn visit_type_param_mut(&mut self, node: &mut syn::TypeParam) {
        if self.param_to_prune == &node.ident {
            self.prune_assoc_bindings = true;
            visit_type_param_mut(self, node);
            self.prune_assoc_bindings = false;
        }
    }

    fn visit_angle_bracketed_generic_arguments_mut(
        &mut self,
        node: &mut syn::AngleBracketedGenericArguments,
    ) {
        if self.prune_assoc_bindings {
            node.args = core::mem::take(&mut node.args)
                .into_iter()
                .filter(|arg| !matches!(arg, syn::GenericArgument::AssocType(_)))
                .collect();
        }
    }

    fn visit_predicate_type_mut(&mut self, node: &mut syn::PredicateType) {
        self.prune_assoc_bindings = false;
        visit_predicate_type_mut(self, node);
    }

    fn visit_type_path_mut(&mut self, node: &mut syn::TypePath) {
        if Some(self.param_to_prune) == node.path.get_ident() {
            self.prune_assoc_bindings = true;
        }
    }
}

fn find_unconstrained_type_params(item_impl: &ItemImpl) -> impl Iterator<Item = &syn::Ident> {
    let indexed_params = param::index(item_impl);

    item_impl.generics.type_params().filter_map(move |param| {
        (!indexed_params.type_params.contains_key(&param.ident)).then_some(&param.ident)
    })
}

pub fn generate(
    main_trait_impl: &mut ItemImpl,
    impl_group_idx: usize,
    impl_group: &ImplGroup,
) -> Option<syn::Macro> {
    let unconstrained = impl_group.id.trait_.as_ref().map_or_else(
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
        |trait_| {
            let path = &trait_.segments.last().unwrap().ident;
            Some(gen_ident(&helper_trait::gen_ident(path, impl_group_idx)))
        },
    )?;

    let mut unconstrained_type_params: IndexSet<_> =
        find_unconstrained_type_params(main_trait_impl)
            .sorted()
            .collect();

    if !unconstrained_type_params.is_empty() {
        let mut item_impls = impl_group.item_impls.clone();

        let bounded = if impl_group.id.trait_.is_none() {
            let unconstrained_type_params = unconstrained_type_params.into_iter();
            quote!((#(#unconstrained_type_params, )*))
        } else {
            let last_assoc_param = unconstrained_type_params.pop().unwrap();

            item_impls.iter_mut().for_each(|item_impl| {
                let mut assoc_binding_remover = ImplItemAssocBindingRemover::new(last_assoc_param);
                assoc_binding_remover.visit_item_impl_mut(item_impl);

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

            let mut constraint_assoc_bindings = impl_group.assoc_bindings.clone();
            constraint_assoc_bindings
                .0
                .retain(|assoc_binding_ident, _| {
                    let mut assoc_binding_remover = AssocGroupBindingRemover::new(last_assoc_param);
                    assoc_binding_remover.visit_type(&assoc_binding_ident.0.0.0);
                    !assoc_binding_remover.flagged_to_remove
                });

            let overlapping = (0..item_impls.len())
                .rev()
                .flat_map(|i| {
                    constraint_assoc_bindings
                        .find_overlapping_before(i)
                        .into_iter()
                        .filter(move |j| i == *j)
                })
                .collect::<IndexSet<_>>();
            item_impls = item_impls
                .into_iter()
                .enumerate()
                .filter_map(|(i, item_impl)| (!overlapping.contains(&i)).then_some(item_impl))
                .collect();

            quote!(#last_assoc_param)
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
                trait #unconstrained {
                    type Bound: ?Sized;
                }

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
