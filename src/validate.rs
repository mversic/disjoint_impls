use proc_macro_error2::OptionExt;

use super::*;

pub fn validate_trait_impls<'a, I: IntoIterator<Item = &'a ItemImpl>>(
    trait_: &ItemTrait,
    item_impls: I,
) where
    I::IntoIter: Clone,
{
    let item_impls = item_impls.into_iter();

    for item_impl in item_impls.clone() {
        if let Some((_, trait_path, _)) = &item_impl.trait_ {
            let item_trait_ident = trait_path
                .segments
                .last()
                .map(|seg| &seg.ident)
                .expect_or_abort("Trait definition given but found inherent impl");

            if &trait_.ident != item_trait_ident {
                abort!(item_trait_ident, "Doesn't match trait definition");
            }
        } else {
            abort!(item_impl, "Expected trait impl, found inherent impl");
        }

        if trait_.unsafety != item_impl.unsafety {
            abort!(item_impl.unsafety, "Doesn't match trait definition");
        }
    }
    for impl_items in item_impls.map(|item_impl| &item_impl.items) {
        compare_trait_items(&trait_.items, impl_items);
    }
}

pub fn validate_inherent_impls<'a, I: IntoIterator<Item = &'a ItemImpl>>(item_impls: I)
where
    I::IntoIter: Clone,
{
    let item_impls = item_impls.into_iter();

    for item_impl in item_impls.clone() {
        if let Some((_, item_impl_trait, _)) = &item_impl.trait_ {
            abort!(item_impl_trait, "Expected inherent impl but found trait");
        }
    }

    let mut impl_items = item_impls.map(|item_impl| &item_impl.items);
    if let Some(first_items) = impl_items.next() {
        for second_items in impl_items {
            compare_inherent_items(first_items, second_items);
        }
    }
}

fn compare_trait_items(trait_items: &[syn::TraitItem], second: &[syn::ImplItem]) {
    let mut second_consts = IndexMap::new();
    let mut second_types = IndexMap::new();
    let mut second_fns = IndexMap::new();

    second.iter().for_each(|item| match item {
        syn::ImplItem::Const(item) => {
            second_consts.insert(&item.ident, item);
        }
        syn::ImplItem::Type(item) => {
            second_types.insert(&item.ident, item);
        }
        syn::ImplItem::Fn(item) => {
            second_fns.insert(&item.sig.ident, item);
        }
        item => abort!(item, "Not supported"),
    });

    for trait_item in trait_items {
        match trait_item {
            syn::TraitItem::Const(trait_item) => {
                if let Some(second_item) = second_consts.swap_remove(&trait_item.ident) {
                    if trait_item.generics.params.len() != second_item.generics.params.len() {
                        abort!(trait_item.generics, "Doesn't match trait definition");
                    }
                } else if trait_item.default.is_none() {
                    abort!(trait_item, "Missing in one of the impls");
                }
            }
            syn::TraitItem::Type(trait_item) => {
                if second_types.swap_remove(&trait_item.ident).is_none()
                    && trait_item.default.is_none()
                {
                    abort!(trait_item, "Missing in one of the impls");
                }
            }
            syn::TraitItem::Fn(trait_item) => {
                if second_fns.swap_remove(&trait_item.sig.ident).is_none()
                    && trait_item.default.is_none()
                {
                    abort!(trait_item, "Missing in one of the impls");
                }
            }
            item => abort!(item, "Not supported"),
        }
    }

    if let Some((second_item, _)) = second_consts.into_iter().next() {
        abort!(second_item, "Not found in trait definition");
    }
    if let Some((second_item, _)) = second_types.into_iter().next() {
        abort!(second_item, "Not found in trait definition");
    }
    if let Some((second_item, _)) = second_fns.into_iter().next() {
        abort!(second_item, "Not found in trait definition");
    }
}

fn compare_inherent_items(first: &[syn::ImplItem], second: &[syn::ImplItem]) {
    let mut second_consts = IndexMap::new();
    let mut second_types = IndexMap::new();
    let mut second_fns = IndexMap::new();

    second.iter().for_each(|item| match item {
        syn::ImplItem::Const(item) => {
            second_consts.insert(&item.ident, item);
        }
        syn::ImplItem::Type(item) => {
            second_types.insert(&item.ident, item);
        }
        syn::ImplItem::Fn(item) => {
            second_fns.insert(&item.sig.ident, item);
        }
        item => abort!(item, "Not supported"),
    });

    for first_item in first {
        match first_item {
            syn::ImplItem::Const(first_item) => {
                if let Some(second_item) = second_consts.swap_remove(&first_item.ident) {
                    if first_item.generics.params.len() != second_item.generics.params.len() {
                        abort!(first_item.generics, "Generics don't match between impls");
                    }
                } else {
                    abort!(first_item, "Not found in one of the impls");
                }
            }
            syn::ImplItem::Type(first_item) => {
                if second_types.swap_remove(&first_item.ident).is_none() {
                    abort!(first_item, "Not found in one of the impls");
                }
            }
            syn::ImplItem::Fn(first_item) => {
                if second_fns.swap_remove(&first_item.sig.ident).is_none() {
                    abort!(first_item, "Not found in one of the impls");
                }
            }
            item => abort!(item, "Not supported"),
        }
    }

    if let Some((second_item, _)) = second_consts.into_iter().next() {
        abort!(second_item, "Not found in one of the impls");
    }
    if let Some((second_item, _)) = second_types.into_iter().next() {
        abort!(second_item, "Not found in one of the impls");
    }
    if let Some((second_item, _)) = second_fns.into_iter().next() {
        abort!(second_item, "Not found in one of the impls");
    }
}
