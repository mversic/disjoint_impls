use crate::main_trait::is_remote;

use super::*;

struct GenericParamRenamer(IndexMap<syn::Ident, syn::Ident>);

/// Generate helper trait
///
/// Helper trait contains all items of the main trait but is parametrized with
/// type parameters corresponding to a minimal set of associated bindings
/// that uniquely identify all of the disjoint impls within an impl group
pub fn generate(ctx: &DisjointImplCtx<'_>, impl_group: &ImplGroup) -> syn::ItemTrait {
    let assoc_binding_count = impl_group.trait_bounds.generalized_idents().count();

    let mut helper_trait = if let Some(main_trait) = ctx.root_trait {
        let mut helper_trait = main_trait.clone();

        rename_trait_generic_params(&mut helper_trait);
        helper_trait.supertraits = [gen_original_trait_bound(
            &helper_trait.generics,
            &helper_trait.ident,
        )]
        .into_iter()
        .collect();

        helper_trait
    } else {
        let impl_items = gen_inherent_trait_items(impl_group.items.as_ref().unwrap());

        let mut self_ty = impl_group.id.self_ty.clone();
        if let syn::Type::Path(type_path) = &mut self_ty {
            type_path.path.segments.last_mut().unwrap().arguments = syn::PathArguments::None;
        }

        let impl_generics = &impl_group.params;

        parse_quote! {
            trait #self_ty <#impl_generics> {
                #(#impl_items)*
            }
        }
    };

    rename_trait_item_idents(&mut helper_trait);
    helper_trait.attrs = core::mem::take(&mut helper_trait.attrs)
        .into_iter()
        .filter(|attr| !is_remote(attr))
        .collect();

    helper_trait.vis = syn::Visibility::Public(parse_quote!(pub));
    helper_trait.attrs.extend(common_impl_attrs(impl_group));
    helper_trait.ident = ctx.gen_ident(&impl_group.id);

    let start_idx = helper_trait.generics.params.len();
    helper_trait.generics.params = combine_generic_args(
        (start_idx..start_idx + assoc_binding_count).map(|i| format_ident!("_TŠČ{i}")),
        &helper_trait.generics,
    )
    .map(|arg| -> syn::GenericParam { parse_quote!(#arg) })
    .collect();

    helper_trait
}

pub(crate) fn common_impl_attrs(impl_group: &ImplGroup) -> Vec<syn::Attribute> {
    let Some((first_impl, _)) = impl_group.impls.first() else {
        return Vec::new();
    };

    first_impl
        .attrs
        .iter()
        .filter(|attr| {
            let path = attr.path();
            path.is_ident("cfg") || path.is_ident("cfg_attr")
        })
        .filter(|attr| {
            impl_group
                .impls
                .iter()
                .all(|(item_impl, _)| item_impl.attrs.iter().any(|a| a == *attr))
        })
        .cloned()
        .collect()
}

fn rename_trait_generic_params(trait_: &mut syn::ItemTrait) {
    let mut param_idx = 0;

    let mut replacements = IndexMap::new();
    for param in &mut trait_.generics.params {
        let (ident, prefix) = match param {
            syn::GenericParam::Lifetime(_) => continue,
            syn::GenericParam::Type(syn::TypeParam { ident, .. }) => (ident, "_TŠČ"),
            syn::GenericParam::Const(syn::ConstParam { ident, .. }) => (ident, "_CŠČ"),
        };

        let new_ident = format_ident!("{prefix}{param_idx}");
        let old_ident = core::mem::replace(ident, new_ident.clone());
        replacements.insert(old_ident, new_ident);
        param_idx += 1;
    }

    if !replacements.is_empty() {
        let mut renamer = GenericParamRenamer(replacements);
        renamer.visit_item_trait_mut(trait_);
    }
}

fn gen_original_trait_bound(
    generics: &syn::Generics,
    trait_ident: &syn::Ident,
) -> syn::TypeParamBound {
    if generics.params.is_empty() {
        return parse_quote!(#trait_ident);
    }

    let args = generics.params.iter().map(|param| match param {
        syn::GenericParam::Lifetime(param) => {
            syn::GenericArgument::Lifetime(param.lifetime.clone())
        }
        syn::GenericParam::Type(syn::TypeParam { ident, .. }) => {
            syn::GenericArgument::Type(parse_quote!(#ident))
        }
        syn::GenericParam::Const(syn::ConstParam { ident, .. }) => {
            syn::GenericArgument::Const(parse_quote!(#ident))
        }
    });

    parse_quote!(#trait_ident<#(#args),*>)
}

fn combine_generic_args(
    assoc_param_bounds: impl IntoIterator<Item = syn::Ident>,
    generics: &syn::Generics,
) -> impl Iterator<Item = TokenStream2> {
    let mut generic_args: Vec<_> = assoc_param_bounds
        .into_iter()
        .map(|param| quote!(#param: ?Sized))
        .collect();

    let mut lifetimes = vec![];
    for arg in &generics.params {
        if matches!(arg, syn::GenericParam::Lifetime(_)) {
            lifetimes.push(quote!(#arg));
        } else {
            generic_args.push(quote!(#arg));
        }
    }

    lifetimes.into_iter().chain(generic_args)
}

fn gen_inherent_trait_items(impl_items: &ImplItemsDesc) -> impl Iterator<Item = syn::TraitItem> {
    let assoc_consts = impl_items
        .assoc_consts
        .iter()
        .map(|(ident, ty)| syn::TraitItem::Const(parse_quote! { const #ident: #ty; }));
    let assoc_types = impl_items
        .assoc_types
        .iter()
        .map(|ident| syn::TraitItem::Type(parse_quote! { const #ident; }));
    let fns = impl_items
        .fns
        .values()
        .map(|sig| syn::TraitItem::Fn(parse_quote! { #sig; }));

    assoc_consts.chain(assoc_types).chain(fns)
}

fn rename_trait_item_idents(trait_: &mut syn::ItemTrait) {
    for item in &mut trait_.items {
        let ident = match item {
            syn::TraitItem::Const(item) => &mut item.ident,
            syn::TraitItem::Type(item) => &mut item.ident,
            syn::TraitItem::Fn(item) => &mut item.sig.ident,
            _ => continue,
        };

        *ident = format_ident!("{ident}_šč");
    }
}

impl syn::visit_mut::VisitMut for GenericParamRenamer {
    fn visit_path_mut(&mut self, node: &mut syn::Path) {
        syn::visit_mut::visit_path_mut(self, node);

        if let Some(ident) = node.get_ident()
            && let Some(replacement) = self.0.get(ident)
        {
            node.segments[0].ident = replacement.clone();
        }
    }
}
