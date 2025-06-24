//! Contains logic related to generating impl of the main trait

use proc_macro2::Span;
use syn::{parse_quote, visit_mut::VisitMut};

use crate::{helper_trait::remove_param_bounds, param::NonPredicateParamIndexer};

use super::*;

struct ImplItemResolver {
    self_as_helper_trait: TokenStream2,
}

/// Generate main trait impl
pub fn generate(
    main_trait: Option<&ItemTrait>,
    impl_group_idx: usize,
    impl_group: &ImplGroup,
) -> Option<ItemImpl> {
    let example_impl = impl_group.item_impls.first()?;

    let mut main_trait_impl = main_trait
        .map(|main_trait| gen_dummy_impl_from_trait_definition(main_trait, example_impl))
        .unwrap_or_else(|| gen_dummy_impl_from_inherent_impl(example_impl));

    let helper_trait_ident = main_trait_impl.trait_.as_ref().map_or_else(
        || {
            let self_ty = &*example_impl.self_ty;

            if let syn::Type::Path(type_path) = self_ty {
                return Some(helper_trait::gen_ident(
                    &type_path.path.segments.last().unwrap().ident,
                    impl_group_idx,
                ));
            }

            None
        },
        |main_trait| {
            Some(helper_trait::gen_ident(
                &main_trait.1.segments.last().unwrap().ident,
                impl_group_idx,
            ))
        },
    )?;

    main_trait_impl
        .generics
        .make_where_clause()
        .predicates
        .extend(gen_assoc_bound_predicates(
            example_impl,
            &helper_trait_ident,
            &impl_group.assoc_bounds,
        ));

    // Remove unused params
    let mut non_predicate_param_indexer = NonPredicateParamIndexer::new(
        example_impl
            .generics
            .lifetimes()
            .map(|param| (&param.lifetime.ident, param)),
        example_impl
            .generics
            .type_params()
            .map(|param| (&param.ident, param)),
        example_impl
            .generics
            .const_params()
            .map(|param| (&param.ident, param)),
        0,
    );

    non_predicate_param_indexer.visit_item_impl(&main_trait_impl);
    if let Some(where_clause) = &main_trait_impl.generics.where_clause {
        non_predicate_param_indexer.visit_where_clause(where_clause);
    }

    main_trait_impl.generics.params = non_predicate_param_indexer
        .indexed_lifetimes
        .keys()
        .map(|&ident| {
            syn::LifetimeParam::new(syn::Lifetime {
                apostrophe: Span::call_site(),
                ident: ident.clone(),
            })
            .into()
        })
        .chain(
            non_predicate_param_indexer
                .indexed_type_params
                .keys()
                .chain(non_predicate_param_indexer.indexed_const_params.keys())
                .map(|param_ident| -> syn::GenericParam { syn::parse_quote!(#param_ident) }),
        )
        .collect();
    // Remove unused params end

    let mut impl_item_resolver =
        ImplItemResolver::new(example_impl, &helper_trait_ident, &impl_group.assoc_bounds);
    main_trait_impl
        .items
        .iter_mut()
        .for_each(|item| impl_item_resolver.visit_impl_item_mut(item));

    Some(main_trait_impl)
}

/// Generates main trait implementation with item values set to dummy values
fn gen_dummy_impl_from_inherent_impl(example_impl: &ItemImpl) -> syn::ItemImpl {
    let mut main_trait_impl = example_impl.clone();
    remove_param_bounds(&mut main_trait_impl.generics);

    main_trait_impl
}

/// Generates main trait implementation with item values set to dummy values
fn gen_dummy_impl_from_trait_definition(
    main_trait: &ItemTrait,
    example_impl: &ItemImpl,
) -> syn::ItemImpl {
    let mut main_trait = main_trait.clone();

    let self_ty = &*example_impl.self_ty;
    let trait_ = &example_impl.trait_.as_ref().unwrap().1;
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

fn combine_generic_args<'a, T: IntoIterator<Item = AssocBoundIdent<'a>>>(
    assoc_bound_idents: T,
    path: &syn::Path,
) -> impl Iterator<Item = syn::GenericArgument> + use<T> {
    let arguments = &path.segments.last().unwrap().arguments;

    let mut generic_args: Vec<_> = assoc_bound_idents
        .into_iter()
        .map(|((param, trait_bound), assoc_param_name)| {
            syn::parse_quote! { <#param as #trait_bound>::#assoc_param_name }
        })
        .collect();

    let mut lifetimes = Vec::new();
    if let syn::PathArguments::AngleBracketed(bracketed) = &arguments {
        for arg in &bracketed.args {
            if matches!(arg, syn::GenericArgument::Lifetime(_)) {
                lifetimes.push(syn::parse_quote!(#arg));
            } else {
                generic_args.push(syn::parse_quote!(#arg));
            }
        }
    }

    lifetimes.into_iter().chain(generic_args)
}

fn gen_helper_trait_bound<'a>(
    example_impl: &ItemImpl,
    helper_trait_ident: &syn::Ident,
    assoc_bound_idents: impl IntoIterator<Item = AssocBoundIdent<'a>>,
) -> syn::Path {
    let generic_args = if let Some((_, impl_trait, _)) = &example_impl.trait_ {
        combine_generic_args(assoc_bound_idents, impl_trait)
    } else if let syn::Type::Path(mut self_ty) = (*example_impl.self_ty).clone() {
        gen_inherent_self_ty_args(&mut self_ty, &example_impl.generics);
        combine_generic_args(assoc_bound_idents, &self_ty.path)
    } else {
        unreachable!()
    };

    parse_quote! { #helper_trait_ident<#(#generic_args),*> }
}

impl ImplItemResolver {
    fn new(
        example_impl: &ItemImpl,
        helper_trait_ident: &syn::Ident,
        assoc_bounds: &AssocBoundsGroup,
    ) -> Self {
        let helper_trait_bound =
            gen_helper_trait_bound(example_impl, helper_trait_ident, assoc_bounds.idents());

        Self {
            self_as_helper_trait: quote! {
                <Self as #helper_trait_bound>
            },
        }
    }
}

fn gen_assoc_bound_predicates<'a>(
    example_impl: &ItemImpl,
    helper_trait_ident: &syn::Ident,
    assoc_bounds: &'a AssocBoundsGroup,
) -> impl Iterator<Item = syn::WherePredicate> + 'a {
    let helper_trait_bound =
        gen_helper_trait_bound(example_impl, helper_trait_ident, assoc_bounds.idents());

    let type_param_trait_bounds = assoc_bounds.idents().fold(
        IndexMap::<_, IndexSet<_>>::new(),
        |mut acc, ((param_ident, trait_bound), _)| {
            acc.entry(param_ident).or_default().insert(trait_bound);

            acc
        },
    );

    type_param_trait_bounds
        .into_iter()
        .map(move |(param_ident, trait_bounds)| -> syn::WherePredicate {
            let trait_bounds = trait_bounds.into_iter();

            if assoc_bounds.unsized_params.contains(param_ident) {
                parse_quote! { #param_ident: ?Sized + #(#trait_bounds)+* }
            } else {
                parse_quote! { #param_ident: #(#trait_bounds)+* }
            }
        })
        .chain(core::iter::once({
            parse_quote! { Self: #helper_trait_bound }
        }))
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

mod param {
    use crate::param::NonPredicateParamResolver;

    use super::*;

    struct MainTraitParamBoundResolver<'a>(IndexSet<&'a syn::Ident>, Vec<syn::WherePredicate>);

    impl<'a> MainTraitParamBoundResolver<'a> {
        fn new(concrete: impl IntoIterator<Item = &'a syn::Ident>) -> Self {
            Self(concrete.into_iter().collect(), Vec::new())
        }
    }

    impl Visit<'_> for MainTraitParamBoundResolver<'_> {
        fn visit_lifetime_param(&mut self, node: &syn::LifetimeParam) {
            let ty = &node.lifetime;

            let bounds = &node.bounds;
            self.1.push(syn::parse_quote! {
                #ty: #bounds
            });
        }

        fn visit_type_param(&mut self, node: &syn::TypeParam) {
            let ty = &node.ident;

            if self.0.contains(&ty) {
                return;
            }

            if !node.bounds.is_empty() {
                let bounds = &node.bounds;

                self.1.push(syn::parse_quote! {
                    #ty: #bounds
                });
            }
        }
    }

    pub fn resolve_main_trait_params(main_trait: &mut syn::ItemTrait, trait_: &syn::Path) {
        match &trait_.segments.last().unwrap().arguments {
            syn::PathArguments::None => {}
            syn::PathArguments::AngleBracketed(bracketed) => {
                let mut lifetimes = IndexMap::new();
                let mut type_params = IndexMap::new();
                let mut const_params = IndexMap::new();

                main_trait
                    .generics
                    .params
                    .iter()
                    .zip(&bracketed.args)
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
                            const_params.insert(param.ident.clone(), arg);
                        }
                        _ => unreachable!(),
                    });

                let mut param_resolver = MainTraitParamBoundResolver::new(
                    type_params.iter().filter_map(|(param, substitute)| {
                        if let syn::Type::Path(ty) = substitute {
                            if ty.qself.is_some() {
                                return Some(param);
                            }

                            let ident = ty.path.get_ident().map(ToString::to_string);
                            if ident.map(|ident| ident.starts_with("_ŠČ")).unwrap_or(false) {
                                return None;
                            }
                        }

                        Some(param)
                    }),
                );
                param_resolver.visit_generics(&main_trait.generics);
                main_trait.generics.params = syn::punctuated::Punctuated::new();

                let where_clause = main_trait.generics.make_where_clause();
                where_clause.predicates = core::mem::take(&mut where_clause.predicates)
                    .into_iter()
                    .chain(param_resolver.1)
                    .collect();

                let mut non_predicate_param_resolver =
                    NonPredicateParamResolver::new(lifetimes, type_params, const_params);
                non_predicate_param_resolver.visit_item_trait_mut(main_trait);
            }
            syn::PathArguments::Parenthesized(_) => unreachable!(),
        }
    }
}
