//! Contains logic related to generating impl of the main trait

use syn::{parse_quote, visit_mut::VisitMut};

use super::*;

struct ImplItemResolver {
    self_as_helper_trait: TokenStream2,
}

pub fn is_remote(attr: &syn::Attribute) -> bool {
    if let syn::Meta::List(list) = &attr.meta
        && list.path.is_ident("disjoint_impls")
        && list.tokens.to_string() == "remote"
    {
        return true;
    }

    false
}

/// Generate main trait impl
pub fn generate(
    main_trait: Option<&ItemTrait>,
    impl_group_idx: usize,
    impl_group: &ImplGroup,
) -> Option<ItemImpl> {
    let helper_trait_ident = main_trait.map_or_else(
        || {
            let syn::Type::Path(type_path) = &impl_group.id.self_ty else {
                unreachable!()
            };

            Some(helper_trait::gen_ident(
                &type_path.path.segments.last().unwrap().ident,
                impl_group_idx,
            ))
        },
        |main_trait| Some(helper_trait::gen_ident(&main_trait.ident, impl_group_idx)),
    )?;

    let mut main_trait_impl = main_trait
        .map(|main_trait| gen_dummy_impl_from_trait_definition(main_trait, &impl_group.id))
        .unwrap_or_else(|| gen_dummy_impl_from_inherent_impl(impl_group));

    main_trait_impl.generics.params = impl_group.params.iter().cloned().collect();
    let where_clause = main_trait_impl.generics.make_where_clause();

    let helper_trait_bound =
        gen_helper_trait_bound(impl_group, &helper_trait_ident, &impl_group.trait_bounds);
    where_clause
        .predicates
        .push(parse_quote! { Self: #helper_trait_bound });
    where_clause
        .predicates
        .extend(impl_group.trait_bounds.0.iter().map(
            |((bounded, trait_bound), (_, bindings))| -> syn::WherePredicate {
                let mut trait_ = trait_bound.0.path.clone();

                let bindings = bindings.iter().filter_map(|(ident, (payload, _))| {
                    payload.as_ref().map(|p| quote!(#ident = #p))
                });

                trait_.segments.last_mut().unwrap().arguments = syn::PathArguments::None;
                match &trait_bound.0.path.segments.last().unwrap().arguments {
                    syn::PathArguments::None => {
                        parse_quote! { #bounded: #trait_ <#(#bindings),*> }
                    }
                    syn::PathArguments::AngleBracketed(bracketed) => {
                        let args = bracketed.args.iter();
                        parse_quote! { #bounded: #trait_ <#(#args,)* #(#bindings),*> }
                    }
                    syn::PathArguments::Parenthesized(_) => unreachable!("Not a valid trait name"),
                }
            },
        ));

    let mut impl_item_resolver = ImplItemResolver::new(impl_group, &helper_trait_ident);
    main_trait_impl
        .items
        .iter_mut()
        .for_each(|item| impl_item_resolver.visit_impl_item_mut(item));

    Some(main_trait_impl)
}

/// Generates main trait implementation with item values set to dummy values
fn gen_dummy_impl_from_inherent_impl(impl_group: &ImplGroup) -> syn::ItemImpl {
    let ImplGroup {
        id,

        items:
            Some(ImplItems {
                fns,
                assoc_types,
                assoc_consts,
            }),
        ..
    } = &impl_group
    else {
        unreachable!()
    };

    let fns = fns.values().map(|sig| {
        quote! {
            #sig { /* DUMMY */ }
        }
    });
    let assoc_types = assoc_types.iter().map(|ident| {
        quote! {
            type #ident = DUMMY;
        }
    });
    let assoc_consts = assoc_consts.iter().map(|(ident, ty)| {
        quote! {
            const #ident: #ty = DUMMY;
        }
    });

    let mut main_trait_impl: syn::ItemImpl = parse_quote! {
        #id {
            #(#fns)*
            #(#assoc_types)*
            #(#assoc_consts)*
        }
    };
    main_trait_impl.generics.params = impl_group.params.iter().cloned().collect();
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

fn combine_generic_args(
    assoc_bindings: &TraitBounds,
    path: &syn::Path,
) -> impl Iterator<Item = syn::GenericArgument> {
    let arguments = &path.segments.last().unwrap().arguments;

    let mut generic_args: Vec<_> = assoc_bindings
        .payloads()
        .map(|p| syn::parse_quote!(#p))
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

fn gen_helper_trait_bound(
    impl_group: &ImplGroup,
    helper_trait_ident: &syn::Ident,
    assoc_bindings: &TraitBounds,
) -> syn::Path {
    let mut self_ty = impl_group.id.self_ty.clone();

    let x = if let Some(impl_trait) = &impl_group.id.trait_ {
        impl_trait
    } else if let syn::Type::Path(self_ty) = &mut self_ty {
        gen_inherent_self_ty_args(self_ty, &impl_group.params);
        &self_ty.path
    } else {
        unreachable!()
    };

    let generic_args = combine_generic_args(assoc_bindings, x);
    parse_quote! { #helper_trait_ident<#(#generic_args),*> }
}

fn gen_inherent_self_ty_args<'a>(
    self_ty: &mut syn::TypePath,
    generics: impl IntoIterator<Item = &'a syn::GenericParam>,
) {
    let last_seg = &mut self_ty.path.segments.last_mut().unwrap();

    if let syn::PathArguments::AngleBracketed(bracketed) = &mut last_seg.arguments {
        let mut lifetimes = Vec::new();
        let mut params = Vec::new();

        generics.into_iter().for_each(|param| match param {
            syn::GenericParam::Lifetime(syn::LifetimeParam { lifetime, .. }) => {
                lifetimes.push(lifetime);
            }
            syn::GenericParam::Type(syn::TypeParam { ident, .. }) => params.push(ident),
            syn::GenericParam::Const(syn::ConstParam { ident, .. }) => params.push(ident),
        });

        *bracketed = parse_quote!(<#(#lifetimes,)* #(#params),*>);
    } else {
        unreachable!()
    }
}

impl ImplItemResolver {
    fn new(impl_group: &ImplGroup, helper_trait_ident: &syn::Ident) -> Self {
        let helper_trait_bound =
            gen_helper_trait_bound(impl_group, helper_trait_ident, &impl_group.trait_bounds);

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
            unsafety,
            ident,
            inputs,
            variadic,
            ..
        } = &node.sig;

        let inputs = inputs.iter().map(|input| match input {
            syn::FnArg::Receiver(_) => parse_quote!(self),
            syn::FnArg::Typed(arg) => arg.pat.clone(),
        });

        let block = quote! {
            #self_as_helper_trait::#ident(#(#inputs,)* #variadic)
        };

        node.block = if unsafety.is_none() {
            parse_quote!({
                #block
            })
        } else {
            parse_quote!({ #unsafety {
                #block
            }})
        };
    }
}

pub mod param {
    use crate::generalize::Expr;

    use super::*;

    struct ParamBoundToWherePredicateMover(Vec<syn::WherePredicate>);

    pub struct ParamResolver<'a> {
        lifetime_replacements: IndexMap<&'a syn::Ident, &'a syn::Lifetime>,
        type_param_replacements: IndexMap<&'a syn::Ident, &'a syn::Type>,
        const_param_replacements: IndexMap<&'a syn::Ident, Expr<'a>>,
    }

    impl ParamBoundToWherePredicateMover {
        fn new() -> Self {
            Self(Vec::new())
        }
    }

    impl<'a> ParamResolver<'a> {
        pub fn new(
            lifetime_replacements: impl IntoIterator<Item = (&'a syn::Ident, &'a syn::Lifetime)>,
            type_param_replacements: impl IntoIterator<Item = (&'a syn::Ident, &'a syn::Type)>,
            const_param_replacements: impl IntoIterator<Item = (&'a syn::Ident, Expr<'a>)>,
        ) -> Self {
            Self {
                lifetime_replacements: lifetime_replacements.into_iter().collect(),
                type_param_replacements: type_param_replacements.into_iter().collect(),
                const_param_replacements: const_param_replacements.into_iter().collect(),
            }
        }

        fn try_replace_type_path_with_type(&self, ty: &syn::Path) -> Option<syn::Type> {
            let first_seg = ty.segments.first().unwrap();

            if let Some(&replacement) = self.type_param_replacements.get(&first_seg.ident) {
                return Some(crate::normalize::replace_path(ty, replacement));
            }

            None
        }

        fn try_replace_expr_path_with_type(&self, path: &mut syn::Path) {
            let first_seg = path.segments.first_mut().unwrap();

            if let Some(replacement) = self.const_param_replacements.get(&first_seg.ident) {
                *first_seg = parse_quote!(#replacement);
            }
        }
    }

    impl VisitMut for ParamResolver<'_> {
        fn visit_lifetime_mut(&mut self, node: &mut syn::Lifetime) {
            if let Some(&replace_with) = self.lifetime_replacements.get(&node.ident) {
                *node = replace_with.clone();
            }
        }

        fn visit_type_mut(&mut self, node: &mut syn::Type) {
            match node {
                syn::Type::Path(ty) => {
                    syn::visit_mut::visit_type_path_mut(self, ty);

                    if let Some(new_ty) = self.try_replace_type_path_with_type(&ty.path) {
                        *node = new_ty;
                    } else {
                        self.try_replace_expr_path_with_type(&mut ty.path);
                    }
                }
                _ => syn::visit_mut::visit_type_mut(self, node),
            }
        }

        fn visit_expr_mut(&mut self, node: &mut syn::Expr) {
            match node {
                syn::Expr::Path(ty) => {
                    syn::visit_mut::visit_expr_path_mut(self, ty);

                    // TODO: struct name can clash with type/const param name
                    if let Some(new_ty) = self.try_replace_type_path_with_type(&ty.path) {
                        *node = parse_quote!(#new_ty);
                    } else {
                        self.try_replace_expr_path_with_type(&mut ty.path);
                    }
                }
                _ => syn::visit_mut::visit_expr_mut(self, node),
            }
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
                let bounds = node.bounds.iter().filter(|bound| {
                    !matches!(
                        bound,
                        syn::TypeParamBound::Trait(syn::TraitBound {
                            modifier: syn::TraitBoundModifier::Maybe(_),
                            ..
                        })
                    )
                });

                self.0.push(parse_quote! {
                    #ty: #(#bounds + )*
                });
            }
        }

        fn visit_item_trait(&mut self, node: &syn::ItemTrait) {
            for supertrait in &node.supertraits {
                self.0.push(parse_quote!(Self: #supertrait));
            }

            self.visit_generics(&node.generics);
        }
    }

    pub fn rewrite_main_trait_bounds_to_where_clause(trait_: &mut syn::ItemTrait) {
        let mut param_bound_mover = ParamBoundToWherePredicateMover::new();

        param_bound_mover.visit_item_trait(trait_);
        trait_.supertraits = Default::default();

        trait_.generics.params = core::mem::take(&mut trait_.generics.params)
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

        let where_clause = trait_.generics.make_where_clause();
        where_clause.predicates = core::mem::take(&mut where_clause.predicates)
            .into_iter()
            .chain(param_bound_mover.0)
            .collect();
    }

    pub fn resolve_main_trait_params(main_trait: &mut syn::ItemTrait, impl_trait: &syn::Path) {
        rewrite_main_trait_bounds_to_where_clause(main_trait);

        let mut lifetimes = IndexMap::new();
        let mut type_params = IndexMap::new();
        let mut const_params = IndexMap::new();

        let mut main_trait_iter = main_trait.generics.params.iter();
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
                            lifetimes.insert(&param.lifetime.ident, arg);
                        }
                        (syn::GenericParam::Type(param), syn::GenericArgument::Type(arg)) => {
                            type_params.insert(&param.ident, arg);
                        }
                        (syn::GenericParam::Const(param), syn::GenericArgument::Const(arg)) => {
                            const_params.insert(&param.ident, Expr::Expr(arg));
                        }
                        (syn::GenericParam::Const(param), syn::GenericArgument::Type(_)) => {
                            const_params.insert(&param.ident, Expr::Ident(&param.ident));
                        }
                        _ => unreachable!(),
                    });
            }
            syn::PathArguments::Parenthesized(_) => unreachable!(),
        }

        main_trait_iter.for_each(|param| match param {
            syn::GenericParam::Lifetime(_) => {}
            syn::GenericParam::Type(param) => {
                type_params.insert(&param.ident, param.default.as_ref().unwrap());
            }
            syn::GenericParam::Const(param) => {
                const_params.insert(&param.ident, Expr::Expr(param.default.as_ref().unwrap()));
            }
        });

        let mut param_resolver = ParamResolver::new(lifetimes, type_params, const_params);
        if let Some(where_clause) = main_trait.generics.where_clause.as_mut() {
            param_resolver.visit_where_clause_mut(where_clause);
        }
        for supertrait in &mut main_trait.supertraits {
            param_resolver.visit_type_param_bound_mut(supertrait);
        }
        for item in &mut main_trait.items {
            param_resolver.visit_trait_item_mut(item);
        }
    }
}
