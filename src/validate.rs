use super::*;

struct Validator<'a> {
    params: IndexSet<&'a syn::Ident>,
    error: Option<syn::Error>,
}

impl<'a> Validator<'a> {
    fn new(generics: &'a syn::Generics) -> Self {
        Self {
            params: generics.type_params().map(|param| &param.ident).collect(),
            error: None,
        }
    }

    fn error(&mut self, node: impl quote::ToTokens, message: impl Into<String>) {
        if self.error.is_none() {
            self.error = Some(syn::Error::new_spanned(node, message.into()));
        }
    }

    fn finish(self) -> syn::Result<()> {
        self.error.map_or(Ok(()), Err)
    }

    fn is_guard_attr(attr: &syn::Attribute) -> bool {
        attr.path().is_ident("cfg") || attr.path().is_ident("cfg_attr")
    }

    fn has_guard_attr(attrs: &[syn::Attribute]) -> bool {
        attrs.iter().any(Self::is_guard_attr)
    }

    fn trait_bound_has_assoc_binding(bound: &syn::TraitBound) -> bool {
        fn args_have_assoc_bindings(args: &syn::PathArguments) -> bool {
            let syn::PathArguments::AngleBracketed(bracketed) = args else {
                return false;
            };

            for arg in &bracketed.args {
                match arg {
                    syn::GenericArgument::AssocType(_) | syn::GenericArgument::Constraint(_) => {
                        return true;
                    }
                    syn::GenericArgument::Type(syn::Type::Path(type_path))
                        if type_path
                            .path
                            .segments
                            .iter()
                            .any(|seg| args_have_assoc_bindings(&seg.arguments)) =>
                    {
                        return true;
                    }
                    _ => {}
                }
            }

            false
        }

        bound
            .path
            .segments
            .iter()
            .any(|seg| args_have_assoc_bindings(&seg.arguments))
    }
}

impl Visit<'_> for Validator<'_> {
    fn visit_type_param(&mut self, node: &syn::TypeParam) {
        let err_msg = "#cfg attribute on associated type bindings is not supported";

        let has_guard = Self::has_guard_attr(&node.attrs);
        let has_assoc_binding = node.bounds.iter().any(|bound| {
            let syn::TypeParamBound::Trait(bound) = bound else {
                return false;
            };

            Self::trait_bound_has_assoc_binding(bound)
        });

        if has_guard && has_assoc_binding {
            self.error(node, err_msg);
            return;
        }

        syn::visit::visit_type_param(self, node);
    }

    fn visit_type_path(&mut self, node: &syn::TypePath) {
        let mut segments = node.path.segments.iter();
        syn::visit::visit_type_path(self, node);

        let first = match &node.qself {
            None if node.path.segments.len() > 1 => {
                let first = &segments.next().unwrap().ident;

                if !self.params.contains(&first) {
                    return;
                }

                parse_quote!(#first)
            }
            Some(syn::QSelf { ty, position, .. }) if *position == 0 => (*ty).clone(),
            _ => {
                return;
            }
        };

        let err_msg = format!(
            "Ambiguous associated type. Qualify with a trait to disambiguate (e.g. {})",
            quote::quote!(<#first as Trait>#(::#segments)*)
        );

        self.error(node, err_msg);
    }

    fn visit_type_impl_trait(&mut self, node: &syn::TypeImplTrait) {
        self.error(node, "`impl Trait` is not allowed in this position");
    }

    fn visit_impl_item(&mut self, _: &syn::ImplItem) {}
}

pub fn validate_impl_syntax(item_impl: &ItemImpl) -> syn::Result<()> {
    let mut validator = Validator::new(&item_impl.generics);
    validator.visit_item_impl(item_impl);
    validator.finish()
}

pub fn validate_trait_impls<'a, I: IntoIterator<Item = &'a ItemImpl>>(
    trait_: &ItemTrait,
    item_impls: I,
) -> syn::Result<()>
where
    I::IntoIter: Clone,
{
    let item_impls = item_impls.into_iter();
    let nparams = trait_.generics.params.len();

    for item_impl in item_impls.clone() {
        if let Some((trait_path, _)) = &item_impl.trait_ {
            let last_seg = trait_path.segments.last().unwrap();

            match &last_seg.arguments {
                syn::PathArguments::AngleBracketed(bracketed)
                    if bracketed.args.len() == nparams => {}
                syn::PathArguments::None if nparams == 0 => {}
                _ => {
                    let err_msg = "Specify all trait arguments (including default)";
                    return Err(syn::Error::new_spanned(last_seg, err_msg));
                }
            }

            if trait_.ident != last_seg.ident {
                let err_msg = "Doesn't match trait definition";
                return Err(syn::Error::new_spanned(&last_seg.ident, err_msg));
            }
        } else {
            let err_msg = "Expected trait impl, found inherent impl";
            return Err(syn::Error::new_spanned(item_impl, err_msg));
        }

        match (trait_.unsafety, item_impl.unsafety) {
            (Some(_), Some(_)) | (None, None) => {}
            (Some(unsafety), None) => {
                let err_msg = "Missing in one of the impls";
                return Err(syn::Error::new_spanned(unsafety, err_msg));
            }
            (None, Some(unsafety)) => {
                let err_msg = "Doesn't match trait definition";
                return Err(syn::Error::new_spanned(unsafety, err_msg));
            }
        }
    }
    for impl_items in item_impls.map(|item_impl| &item_impl.items) {
        compare_trait_items(&trait_.items, impl_items)?;
    }

    Ok(())
}

pub fn validate_inherent_impls<'a, I: IntoIterator<Item = &'a ItemImpl>>(
    item_impls: I,
) -> syn::Result<()>
where
    I::IntoIter: Clone,
{
    let item_impls = item_impls.into_iter();

    for item_impl in item_impls.clone() {
        if let Some((item_impl_trait, _)) = &item_impl.trait_ {
            let err_msg = "Expected inherent impl but found trait";
            return Err(syn::Error::new_spanned(item_impl_trait, err_msg));
        }
    }

    let mut impl_items = item_impls.map(|item_impl| &item_impl.items);
    if let Some(first_items) = impl_items.next() {
        for second_items in impl_items {
            compare_inherent_items(first_items, second_items)?;
        }
    }

    Ok(())
}

fn compare_trait_items(items: &[syn::TraitItem], second: &[syn::ImplItem]) -> syn::Result<()> {
    let mut second_consts = IndexMap::new();
    let mut second_types = IndexMap::new();
    let mut second_fns = IndexMap::new();

    for item in second {
        match item {
            syn::ImplItem::Const(item) => {
                second_consts.insert(&item.ident, item);
            }
            syn::ImplItem::Type(item) => {
                second_types.insert(&item.ident, item);
            }
            syn::ImplItem::Fn(item) => {
                second_fns.insert(&item.sig.ident, item);
            }
            item => return Err(syn::Error::new_spanned(item, "Not supported")),
        }
    }

    for trait_item in items {
        match trait_item {
            syn::TraitItem::Const(trait_item) => {
                if let Some(second_item) = second_consts.swap_remove(&trait_item.ident) {
                    if trait_item.generics.params.len() != second_item.generics.params.len() {
                        let err_msg = "Doesn't match trait definition";
                        return Err(syn::Error::new_spanned(&trait_item.generics, err_msg));
                    }
                } else if trait_item.default.is_none() {
                    let err_msg = "Missing in one of the impls";
                    return Err(syn::Error::new_spanned(trait_item, err_msg));
                }
            }
            syn::TraitItem::Type(trait_item) => {
                if second_types.swap_remove(&trait_item.ident).is_none()
                    && trait_item.default.is_none()
                {
                    let err_msg = "Missing in one of the impls";
                    return Err(syn::Error::new_spanned(trait_item, err_msg));
                }
            }
            syn::TraitItem::Fn(trait_item) => {
                if second_fns.swap_remove(&trait_item.sig.ident).is_none()
                    && trait_item.default.is_none()
                {
                    let err_msg = "Missing in one of the impls";
                    return Err(syn::Error::new_spanned(trait_item, err_msg));
                }
            }
            item => {
                let err_msg = "Not supported";
                return Err(syn::Error::new_spanned(item, err_msg));
            }
        }
    }

    if let Some((second_item, _)) = second_consts.into_iter().next() {
        let err_msg = "Not found in trait definition";
        return Err(syn::Error::new_spanned(second_item, err_msg));
    }
    if let Some((second_item, _)) = second_types.into_iter().next() {
        let err_msg = "Not found in trait definition";
        return Err(syn::Error::new_spanned(second_item, err_msg));
    }
    if let Some((second_item, _)) = second_fns.into_iter().next() {
        let err_msg = "Not found in trait definition";
        return Err(syn::Error::new_spanned(second_item, err_msg));
    }

    Ok(())
}

fn compare_inherent_items(first: &[syn::ImplItem], second: &[syn::ImplItem]) -> syn::Result<()> {
    let mut second_consts = IndexMap::new();
    let mut second_types = IndexMap::new();
    let mut second_fns = IndexMap::new();

    for item in second {
        match item {
            syn::ImplItem::Const(item) => {
                second_consts.insert(&item.ident, item);
            }
            syn::ImplItem::Type(item) => {
                second_types.insert(&item.ident, item);
            }
            syn::ImplItem::Fn(item) => {
                second_fns.insert(&item.sig.ident, item);
            }
            item => {
                let err_msg = "Not supported";
                return Err(syn::Error::new_spanned(item, err_msg));
            }
        }
    }

    for first_item in first {
        match first_item {
            syn::ImplItem::Const(first_item) => {
                if let Some(second_item) = second_consts.swap_remove(&first_item.ident) {
                    if first_item.generics.params.len() != second_item.generics.params.len() {
                        let err_msg = "Generics don't match between impls";
                        return Err(syn::Error::new_spanned(&first_item.generics, err_msg));
                    }
                } else {
                    let err_msg = "Not found in one of the impls";
                    return Err(syn::Error::new_spanned(first_item, err_msg));
                }
            }
            syn::ImplItem::Type(first_item) => {
                if second_types.swap_remove(&first_item.ident).is_none() {
                    let err_msg = "Not found in one of the impls";
                    return Err(syn::Error::new_spanned(first_item, err_msg));
                }
            }
            syn::ImplItem::Fn(first_item) => {
                if second_fns.swap_remove(&first_item.sig.ident).is_none() {
                    let err_msg = "Not found in one of the impls";
                    return Err(syn::Error::new_spanned(first_item, err_msg));
                }
            }
            item => {
                let err_msg = "Not supported";
                return Err(syn::Error::new_spanned(item, err_msg));
            }
        }
    }

    if let Some((second_item, _)) = second_consts.into_iter().next() {
        let err_msg = "Not found in one of the impls";
        return Err(syn::Error::new_spanned(second_item, err_msg));
    }
    if let Some((second_item, _)) = second_types.into_iter().next() {
        let err_msg = "Not found in one of the impls";
        return Err(syn::Error::new_spanned(second_item, err_msg));
    }
    if let Some((second_item, _)) = second_fns.into_iter().next() {
        let err_msg = "Not found in one of the impls";
        return Err(syn::Error::new_spanned(second_item, err_msg));
    }

    Ok(())
}
