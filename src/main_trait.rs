//! Contains logic related to generating impl of the main trait

use syn::{parse_quote, visit_mut::VisitMut};

use crate::param::IsUnsizedBound;

use super::*;

struct ImplItemResolver {
    self_as_helper_trait: TokenStream2,
}

/// Generate main trait impl
pub fn generate(
    main_trait: Option<&ItemTrait>,
    impl_group_idx: usize,
    impl_group_id: &ImplGroupId,
    impl_group: &ImplGroup,
) -> Option<ItemImpl> {
    let example_impl = impl_group.item_impls.first()?;

    let helper_trait_ident = main_trait.map_or_else(
        || {
            if let syn::Type::Path(type_path) = &impl_group_id.self_ty {
                return Some(helper_trait::gen_ident(
                    &type_path.path.segments.last().unwrap().ident,
                    impl_group_idx,
                ));
            }

            None
        },
        |main_trait| Some(helper_trait::gen_ident(&main_trait.ident, impl_group_idx)),
    )?;

    let mut main_trait_impl = main_trait
        .map(|main_trait| gen_dummy_impl_from_trait_definition(main_trait, impl_group_id))
        .unwrap_or_else(|| gen_dummy_impl_from_inherent_impl(example_impl, impl_group));

    main_trait_impl.generics.params = impl_group.generics.params.iter().cloned().collect();
    let where_clause = main_trait_impl.generics.make_where_clause();
    let mut predicates = core::mem::take(&mut where_clause.predicates)
        .into_iter()
        .collect::<IndexSet<_>>();
    predicates.extend(
        impl_group
            .generics
            .where_clause
            .as_ref()
            .map(|w| &w.predicates)
            .cloned()
            .unwrap_or_default(),
    );
    where_clause.predicates = predicates.into_iter().collect();

    let helper_trait_bound = gen_helper_trait_bound(
        impl_group,
        &helper_trait_ident,
        impl_group.assoc_bindings.idents(),
    );
    where_clause
        .predicates
        .push(parse_quote! { Self: #helper_trait_bound });

    let mut unsized_params = IndexSet::new();
    where_clause.predicates = core::mem::take(&mut where_clause.predicates)
        .into_iter()
        .filter_map(|mut predicate| {
            if let syn::WherePredicate::Type(predicate) = &mut predicate {
                predicate.bounds = core::mem::take(&mut predicate.bounds)
                    .into_iter()
                    .filter(|bound| {
                        let is_unsized = bound.is_unsized();

                        if is_unsized {
                            let unsized_param = match &predicate.bounded_ty {
                                syn::Type::Path(syn::TypePath { path, .. }) => {
                                    path.get_ident().unwrap()
                                }
                                _ => unreachable!(),
                            };

                            unsized_params.insert(unsized_param.clone());
                        }

                        !is_unsized
                    })
                    .collect();

                if predicate.bounds.is_empty() {
                    return None;
                }
            }

            Some(predicate)
        })
        .collect();

    let unsized_params = main_trait_impl
        .generics
        .type_params_mut()
        .filter_map(|param| {
            if unsized_params.contains(&param.ident) {
                return Some(param);
            }

            None
        });

    for unsized_param in unsized_params {
        if !unsized_param.bounds.iter().any(IsUnsizedBound::is_unsized) {
            unsized_param.bounds.push(parse_quote! { ?Sized });
        }
    }

    let mut impl_item_resolver =
        ImplItemResolver::new(impl_group, &helper_trait_ident, &impl_group.assoc_bindings);
    main_trait_impl
        .items
        .iter_mut()
        .for_each(|item| impl_item_resolver.visit_impl_item_mut(item));

    Some(main_trait_impl)
}

/// Generates main trait implementation with item values set to dummy values
fn gen_dummy_impl_from_inherent_impl(
    example_impl: &ItemImpl,
    impl_group: &ImplGroup,
) -> syn::ItemImpl {
    let mut main_trait_impl = example_impl.clone();
    main_trait_impl.generics = impl_group.generics.clone();
    main_trait_impl
}

/// Generates main trait implementation with item values set to dummy values
fn gen_dummy_impl_from_trait_definition(
    main_trait: &ItemTrait,
    impl_group_id: &ImplGroupId,
) -> syn::ItemImpl {
    let mut main_trait = main_trait.clone();

    let self_ty = &impl_group_id.self_ty;
    let trait_ = impl_group_id.trait_.as_ref().unwrap();
    param::resolve_main_trait_params(&mut main_trait, trait_);

    let ItemTrait {
        unsafety,
        generics,
        items,
        ..
    } = main_trait;

    let items = items.iter().map(|item| {
        let item: syn::ImplItem = match item {
            syn::TraitItem::Const(item) => {
                let syn::TraitItemConst {
                    generics,
                    ident,
                    ty,
                    ..
                } = item;

                let (_, ty_generics, where_clause) = generics.split_for_impl();

                parse_quote! {
                    const #ident #ty_generics: #ty = DUMMY #where_clause;
                }
            }
            syn::TraitItem::Type(item) => {
                let syn::TraitItemType {
                    generics, ident, ..
                } = item;

                let (_, ty_generics, where_clause) = generics.split_for_impl();

                parse_quote! {
                    type #ident #ty_generics = DUMMY #where_clause;
                }
            }
            syn::TraitItem::Fn(item) => {
                let sig = &item.sig;

                parse_quote! {
                    #sig { /* DUMMY */ }
                }
            }
            item => abort!(item, "Not supported"),
        };

        item
    });

    let (impl_generics, _, where_clause) = generics.split_for_impl();

    parse_quote! {
        #unsafety impl #impl_generics #trait_ for #self_ty #where_clause {
            #(#items)*
        }
    }
}

fn combine_generic_args<'a>(
    assoc_binding_idents: impl IntoIterator<Item = &'a AssocBindingIdent>,
    path: &syn::Path,
) -> impl Iterator<Item = syn::GenericArgument> {
    let arguments = &path.segments.last().unwrap().arguments;

    let mut generic_args: Vec<_> = assoc_binding_idents
        .into_iter()
        .map(|((bounded, trait_bound), assoc_param_name)| {
            syn::parse_quote! { <#bounded as #trait_bound>::#assoc_param_name }
        })
        .collect();

    let mut lifetimes = Vec::new();
    if let syn::PathArguments::AngleBracketed(bracketed) = &arguments {
        for arg in &bracketed.args {
            if matches!(arg, syn::GenericArgument::Lifetime(_)) {
                lifetimes.push(parse_quote!(#arg));
            } else {
                generic_args.push(parse_quote!(#arg));
            }
        }
    }

    lifetimes.into_iter().chain(generic_args)
}

fn gen_helper_trait_bound<'a>(
    impl_group: &ImplGroup,
    helper_trait_ident: &syn::Ident,
    assoc_binding_idents: impl IntoIterator<Item = &'a AssocBindingIdent>,
) -> syn::Path {
    let mut self_ty = impl_group.id.self_ty.clone();

    let generic_args = if let Some(impl_trait) = &impl_group.id.trait_ {
        combine_generic_args(assoc_binding_idents, impl_trait)
    } else if let syn::Type::Path(self_ty) = &mut self_ty {
        gen_inherent_self_ty_args(self_ty, &impl_group.generics.params);
        combine_generic_args(assoc_binding_idents, &self_ty.path)
    } else {
        unreachable!()
    };

    parse_quote! { #helper_trait_ident<#(#generic_args),*> }
}

impl ImplItemResolver {
    fn new(
        impl_group: &ImplGroup,
        helper_trait_ident: &syn::Ident,
        assoc_bindings: &AssocBindingsGroup,
    ) -> Self {
        let helper_trait_bound =
            gen_helper_trait_bound(impl_group, helper_trait_ident, assoc_bindings.idents());

        Self {
            self_as_helper_trait: quote! {
                <Self as #helper_trait_bound>
            },
        }
    }
}

impl VisitMut for ImplItemResolver {
    fn visit_impl_item_const_mut(&mut self, node: &mut syn::ImplItemConst) {
        let self_as_helper_trait = &self.self_as_helper_trait;

        let ident = &node.ident;
        node.expr = parse_quote!(
            #self_as_helper_trait::#ident
        );
    }

    fn visit_impl_item_type_mut(&mut self, node: &mut syn::ImplItemType) {
        let self_as_helper_trait = &self.self_as_helper_trait;

        let ident = &node.ident;
        node.ty = parse_quote!(
            #self_as_helper_trait::#ident
        );
    }

    fn visit_impl_item_fn_mut(&mut self, node: &mut syn::ImplItemFn) {
        let self_as_helper_trait = &self.self_as_helper_trait;

        let syn::Signature {
            ident,
            inputs,
            variadic,
            ..
        } = &node.sig;

        let inputs = inputs.iter().map(|input| match input {
            syn::FnArg::Receiver(_) => parse_quote!(self),
            syn::FnArg::Typed(arg) => arg.pat.clone(),
        });

        node.block = parse_quote!({
            #self_as_helper_trait::#ident(#(#inputs,)* #variadic)
        });
    }
}

pub mod param {
    use crate::param::NonPredicateParamResolver;

    use super::*;

    struct ParamBoundToWherePredicateMover(Vec<syn::WherePredicate>);

    impl ParamBoundToWherePredicateMover {
        fn new() -> Self {
            Self(Vec::new())
        }
    }

    impl Visit<'_> for ParamBoundToWherePredicateMover {
        fn visit_lifetime_param(&mut self, node: &syn::LifetimeParam) {
            let ty = &node.lifetime;

            let bounds = &node.bounds;
            self.0.push(parse_quote! {
                #ty: #bounds
            });
        }

        fn visit_type_param(&mut self, node: &syn::TypeParam) {
            let ty = &node.ident;

            if !node.bounds.is_empty() {
                let bounds = &node.bounds;

                self.0.push(parse_quote! {
                    #ty: #bounds
                });
            }
        }
    }

    pub fn rewrite_main_trait_bounds_to_where_clause(generics: &mut syn::Generics) {
        let mut param_bound_resolver = ParamBoundToWherePredicateMover::new();

        param_bound_resolver.visit_generics(generics);
        generics.params = core::mem::take(&mut generics.params)
            .into_iter()
            .map(|mut param| {
                match &mut param {
                    syn::GenericParam::Lifetime(param) => {
                        param.bounds = core::iter::empty::<syn::Lifetime>().collect();
                    }
                    syn::GenericParam::Type(param) => {
                        param.bounds = core::iter::empty::<syn::TypeParamBound>().collect();
                    }
                    syn::GenericParam::Const(_) => {}
                }

                param
            })
            .collect();

        let where_clause = generics.make_where_clause();
        where_clause.predicates = core::mem::take(&mut where_clause.predicates)
            .into_iter()
            .chain(param_bound_resolver.0)
            .collect();
    }

    pub fn resolve_main_trait_params(main_trait: &mut syn::ItemTrait, impl_trait: &syn::Path) {
        rewrite_main_trait_bounds_to_where_clause(&mut main_trait.generics);
        let mut main_trait_iter = main_trait.generics.params.iter();

        let mut lifetimes = IndexMap::new();
        let mut type_params = IndexMap::new();
        let mut const_params = IndexMap::new();

        match &impl_trait.segments.last().unwrap().arguments {
            syn::PathArguments::None => {}
            syn::PathArguments::AngleBracketed(bracketed) => {
                main_trait_iter
                    .by_ref()
                    .take(bracketed.args.len())
                    .zip_eq(&bracketed.args)
                    .for_each(|(param, arg)| match (param, arg) {
                        (
                            syn::GenericParam::Lifetime(param),
                            syn::GenericArgument::Lifetime(arg),
                        ) => {
                            lifetimes.insert(param.lifetime.ident.clone(), arg);
                        }
                        (syn::GenericParam::Type(param), syn::GenericArgument::Type(arg)) => {
                            type_params.insert(param.ident.clone(), arg);
                        }
                        (syn::GenericParam::Const(param), syn::GenericArgument::Const(arg)) => {
                            const_params.insert(param.ident.clone(), arg.clone());
                        }
                        (syn::GenericParam::Const(param), syn::GenericArgument::Type(arg)) => {
                            const_params.insert(param.ident.clone(), parse_quote!(#arg));
                        }
                        _ => unreachable!(),
                    });
            }
            syn::PathArguments::Parenthesized(_) => unreachable!(),
        }

        main_trait_iter.for_each(|param| match param {
            syn::GenericParam::Lifetime(_) => {}
            syn::GenericParam::Type(param) => {
                type_params.insert(param.ident.clone(), param.default.as_ref().unwrap());
            }
            syn::GenericParam::Const(param) => {
                const_params.insert(param.ident.clone(), param.default.clone().unwrap());
            }
        });

        let mut non_predicate_param_resolver =
            NonPredicateParamResolver::new(lifetimes, type_params, const_params);
        if let Some(where_clause) = main_trait.generics.where_clause.as_mut() {
            non_predicate_param_resolver.visit_where_clause_mut(where_clause);
            where_clause.predicates = core::mem::take(&mut where_clause.predicates)
                .into_iter()
                .filter_map(|mut predicate| {
                    if let syn::WherePredicate::Type(predicate) = &mut predicate {
                        let is_param = if let syn::Type::Path(syn::TypePath { path, .. }) =
                            &predicate.bounded_ty
                        {
                            path.get_ident()
                                .is_some_and(|ident| ident.to_string().starts_with("_ŠČ"))
                        } else {
                            false
                        };

                        if !is_param {
                            predicate.bounds = core::mem::take(&mut predicate.bounds)
                                .into_iter()
                                .filter(|bound| !bound.is_unsized())
                                .collect();
                        }

                        if predicate.bounds.is_empty() {
                            return None;
                        }
                    }

                    Some(predicate)
                })
                .collect();
        }
        for supertrait in &mut main_trait.supertraits {
            non_predicate_param_resolver.visit_type_param_bound_mut(supertrait);
        }
        for item in &mut main_trait.items {
            non_predicate_param_resolver.visit_trait_item_mut(item);
        }
    }
}
