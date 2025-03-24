//! Contains logic related to uniform position based (re-)naming of parameters

use indexmap::IndexMap;
use proc_macro_error2::abort_call_site;
use proc_macro2::Span;

use quote::format_ident;
use syn::{visit::Visit, visit_mut::VisitMut};

/// Indexer for params used in traits, impl trait or self type, but not predicates.
///
/// For `impl<U, T: IntoIterator<Item = V>, V> Trait<T> for U` resolved indices would be:
/// `T` = 0,
/// `U` = 1,
/// `V` = undetermined
pub struct NonPredicateParamIndexer<'a> {
    unindexed_lifetimes: IndexMap<&'a syn::Ident, &'a syn::LifetimeParam>,
    unindexed_type_params: IndexMap<&'a syn::Ident, &'a syn::TypeParam>,
    unindexed_const_params: IndexMap<&'a syn::Ident, &'a syn::ConstParam>,

    pub indexed_lifetimes: IndexMap<&'a syn::Ident, (usize, &'a syn::LifetimeParam)>,
    pub indexed_type_params: IndexMap<&'a syn::Ident, (usize, &'a syn::TypeParam)>,
    pub indexed_const_params: IndexMap<&'a syn::Ident, (usize, &'a syn::ConstParam)>,

    curr_param_pos_idx: usize,
}

pub struct NonPredicateParamResolver<'a> {
    lifetime_replacements: IndexMap<syn::Ident, &'a syn::Lifetime>,
    type_param_replacements: IndexMap<syn::Ident, &'a syn::Type>,
    const_param_replacements: IndexMap<syn::Ident, &'a syn::Expr>,
}

pub fn resolve_non_predicate_params(item_impl: &mut syn::ItemImpl) {
    let mut non_predicate_param_indexer = NonPredicateParamIndexer::new(
        item_impl
            .generics
            .lifetimes()
            .map(|param| (&param.lifetime.ident, param)),
        item_impl
            .generics
            .type_params()
            .map(|param| (&param.ident, param)),
        item_impl
            .generics
            .const_params()
            .map(|param| (&param.ident, param)),
        0,
    );

    non_predicate_param_indexer.visit_item_impl(item_impl);

    let mut prev_unindexed_params_count = usize::MAX;
    let mut curr_unindexed_params_count = non_predicate_param_indexer.len();
    let mut indexed_lifetimes = non_predicate_param_indexer.indexed_lifetimes;
    let mut indexed_type_params = non_predicate_param_indexer.indexed_type_params;
    let mut indexed_const_params = non_predicate_param_indexer.indexed_const_params;

    while prev_unindexed_params_count > 0
            // NOTE: This discards parameters only used in where clause
            && prev_unindexed_params_count != curr_unindexed_params_count
    {
        non_predicate_param_indexer = NonPredicateParamIndexer::new(
            non_predicate_param_indexer.unindexed_lifetimes,
            non_predicate_param_indexer.unindexed_type_params,
            non_predicate_param_indexer.unindexed_const_params,
            non_predicate_param_indexer.curr_param_pos_idx,
        );

        non_predicate_param_indexer.visit_indexed_params(
            indexed_type_params
                .iter()
                .map(|(_, (idx, param))| (*idx, *param)),
            indexed_const_params
                .iter()
                .map(|(_, (idx, param))| (*idx, *param)),
            item_impl.generics.where_clause.as_ref(),
        );

        prev_unindexed_params_count = curr_unindexed_params_count;
        curr_unindexed_params_count = non_predicate_param_indexer.len();
        indexed_lifetimes.extend(non_predicate_param_indexer.indexed_lifetimes);
        indexed_type_params.extend(non_predicate_param_indexer.indexed_type_params);
        indexed_const_params.extend(non_predicate_param_indexer.indexed_const_params);
    }

    let lifetimes = indexed_lifetimes
        .into_iter()
        .map(|(ident, (idx, _))| (ident.clone(), gen_indexed_param_ident(idx)))
        .collect::<IndexMap<_, _>>();
    let type_params = indexed_type_params
        .into_iter()
        .map(|(ident, (idx, _))| (ident.clone(), gen_indexed_param_ident(idx)))
        .collect::<IndexMap<_, _>>();
    let const_params = indexed_const_params
        .into_iter()
        .map(|(ident, (idx, _))| (ident.clone(), gen_indexed_param_ident(idx)))
        .collect::<IndexMap<_, _>>();

    for param in &mut item_impl.generics.params {
        match param {
            syn::GenericParam::Lifetime(syn::LifetimeParam { lifetime, .. }) => {
                if let Some(new_lifetime) = lifetimes.get(&lifetime.ident) {
                    lifetime.ident = new_lifetime.clone();
                }
            }
            syn::GenericParam::Type(syn::TypeParam { ident, .. }) => {
                if let Some(new_ident) = type_params.get(ident) {
                    *ident = new_ident.clone();
                }
            }
            syn::GenericParam::Const(syn::ConstParam { ident, .. }) => {
                if let Some(new_ident) = const_params.get(ident) {
                    *ident = new_ident.clone();
                }
            }
        }
    }

    let lifetimes = lifetimes
        .into_iter()
        .map(|(old_lifetime, new_lifetime)| {
            let new_lifetime = syn::Lifetime {
                apostrophe: Span::call_site(),
                ident: new_lifetime,
            };

            (old_lifetime, new_lifetime)
        })
        .collect::<IndexMap<_, _>>();
    let type_params = type_params
        .into_iter()
        .map(|(old_type_param, new_type_param)| {
            (old_type_param, syn::parse_quote!(#new_type_param))
        })
        .collect::<IndexMap<_, _>>();
    let const_params = const_params
        .into_iter()
        .map(|(old_const_param, new_const_param)| {
            (old_const_param, syn::parse_quote!(#new_const_param))
        })
        .collect::<IndexMap<_, _>>();
    NonPredicateParamResolver::new(
        lifetimes.iter().map(|(old, new)| (old.clone(), new)),
        type_params.iter().map(|(old, new)| (old.clone(), new)),
        const_params.iter().map(|(old, new)| (old.clone(), new)),
    )
    .visit_item_impl_mut(item_impl);

    // TODO: Add unnamed lifetimes (&u32) or elided lifetimes (&'_ u32)
    // TODO: Remove unused lifetimes. Example where 'b is unused:
    // impl<'a: 'b, 'b: 'a, T: 'b > Kara<'a, T> for &'a T {
    //
    //self.generics.params = self
    //    .generics
    //    .params
    //    .into_iter()
    //    .filter(|param| match param {
    //        syn::GenericParam::Lifetime(lifetime)
    //            if param_resolver.0.get(&lifetime.lifetime.ident).1 =>
    //        {
    //            syn::GenericParam::Lifetime(lifetime)
    //        }
    //        param => param,
    //    })
    //    .collect();
}

pub(super) fn gen_indexed_param_ident(idx: usize) -> syn::Ident {
    format_ident!("_ŠČ{idx}")
}

impl<'a> NonPredicateParamIndexer<'a> {
    pub fn new(
        unindexed_lifetimes: impl IntoIterator<Item = (&'a syn::Ident, &'a syn::LifetimeParam)>,
        unindexed_type_params: impl IntoIterator<Item = (&'a syn::Ident, &'a syn::TypeParam)>,
        unindexed_const_params: impl IntoIterator<Item = (&'a syn::Ident, &'a syn::ConstParam)>,
        curr_param_pos_idx: usize,
    ) -> Self {
        Self {
            indexed_lifetimes: IndexMap::new(),
            indexed_type_params: IndexMap::new(),
            indexed_const_params: IndexMap::new(),
            unindexed_lifetimes: unindexed_lifetimes.into_iter().collect(),
            unindexed_type_params: unindexed_type_params.into_iter().collect(),
            unindexed_const_params: unindexed_const_params.into_iter().collect(),
            curr_param_pos_idx,
        }
    }

    fn len(&self) -> usize {
        self.unindexed_lifetimes.len()
            + self.unindexed_type_params.len()
            + self.unindexed_const_params.len()
    }

    pub fn visit_indexed_params(
        &mut self,
        indexed_type_params: impl IntoIterator<Item = (usize, &'a syn::TypeParam)>,
        indexed_const_params: impl IntoIterator<Item = (usize, &'a syn::ConstParam)>,
        where_clause: Option<&'a syn::WhereClause>,
    ) {
        enum GenericParamRef<'a> {
            Type(&'a syn::TypeParam),
            Const(&'a syn::ConstParam),
        }

        let node = indexed_type_params
            .into_iter()
            .map(|(idx, type_param)| (idx, GenericParamRef::Type(type_param)))
            .chain(
                indexed_const_params
                    .into_iter()
                    .map(|(idx, const_param)| (idx, GenericParamRef::Const(const_param))),
            )
            .collect::<IndexMap<_, _>>();

        let mut indexed_params = node.into_iter().collect::<Vec<_>>();
        indexed_params.sort_by_key(|(k, _)| *k);

        for (_, param) in indexed_params {
            match param {
                GenericParamRef::Type(type_param) => self.visit_type_param(type_param),
                GenericParamRef::Const(const_param) => self.visit_const_param(const_param),
            }
        }

        if let Some(where_clause) = where_clause {
            let prev_unindexed_param_count =
                self.unindexed_type_params.len() + self.unindexed_const_params.len();
            syn::visit::visit_where_clause(self, where_clause);
            let curr_unindexed_param_count =
                self.unindexed_type_params.len() + self.unindexed_const_params.len();

            if prev_unindexed_param_count < curr_unindexed_param_count {
                abort_call_site!("Generics bounded only in where clause are not supported");
            }
        }
    }

    fn visit_lifetime_ident(&mut self, lifetime_ident: &'a syn::Ident) -> bool {
        if let Some(removed) = self.unindexed_lifetimes.swap_remove(lifetime_ident) {
            self.indexed_lifetimes
                .insert(lifetime_ident, (self.curr_param_pos_idx, removed));
            self.curr_param_pos_idx = self.curr_param_pos_idx.checked_add(1).unwrap();
        }

        false
    }

    fn visit_type_param_ident(&mut self, param_ident: &'a syn::Ident) -> bool {
        if let Some(removed) = self.unindexed_type_params.swap_remove(param_ident) {
            self.indexed_type_params
                .insert(param_ident, (self.curr_param_pos_idx, removed));
            self.curr_param_pos_idx = self.curr_param_pos_idx.checked_add(1).unwrap();

            return true;
        }

        false
    }

    fn visit_const_param_ident(&mut self, param_ident: &'a syn::Ident) -> bool {
        if let Some(removed) = self.unindexed_const_params.swap_remove(param_ident) {
            self.indexed_const_params
                .insert(param_ident, (self.curr_param_pos_idx, removed));
            self.curr_param_pos_idx = self.curr_param_pos_idx.checked_add(1).unwrap();

            return true;
        }

        false
    }
}

impl<'a> Visit<'a> for NonPredicateParamIndexer<'a> {
    fn visit_generics(&mut self, _: &syn::Generics) {}

    fn visit_lifetime(&mut self, node: &'a syn::Lifetime) {
        self.visit_lifetime_ident(&node.ident);
    }

    fn visit_type(&mut self, node: &'a syn::Type) {
        match node {
            syn::Type::Path(ty) => {
                if let Some(qself) = &ty.qself {
                    self.visit_qself(qself);
                }

                let first_seg = ty.path.segments.first().unwrap();
                self.visit_type_param_ident(&first_seg.ident);
                syn::visit::visit_path(self, &ty.path);
            }
            _ => syn::visit::visit_type(self, node),
        }
    }

    fn visit_expr(&mut self, node: &'a syn::Expr) {
        match node {
            syn::Expr::Path(ty) => {
                if let Some(qself) = &ty.qself {
                    self.visit_qself(qself);
                }

                let first_seg = ty.path.segments.first().unwrap();
                if !self.visit_type_param_ident(&first_seg.ident) {
                    self.visit_const_param_ident(&first_seg.ident);
                }

                syn::visit::visit_path(self, &ty.path);
            }
            _ => syn::visit::visit_expr(self, node),
        }
    }
}

impl<'a> NonPredicateParamResolver<'a> {
    pub fn new(
        lifetime_replacements: impl IntoIterator<Item = (syn::Ident, &'a syn::Lifetime)>,
        type_param_replacements: impl IntoIterator<Item = (syn::Ident, &'a syn::Type)>,
        const_param_replacements: impl IntoIterator<Item = (syn::Ident, &'a syn::Expr)>,
    ) -> Self {
        Self {
            lifetime_replacements: lifetime_replacements.into_iter().collect(),
            type_param_replacements: type_param_replacements.into_iter().collect(),
            const_param_replacements: const_param_replacements.into_iter().collect(),
        }
    }

    fn try_replace_type_path_with_type(&self, path: &syn::Path) -> Option<syn::Type> {
        let mut segments = path.segments.iter();
        let ident = &segments.next().unwrap().ident;

        if let Some(&replacement) = self.type_param_replacements.get(ident) {
            return Some(if path.segments.len() > 1 {
                syn::parse_quote!(<#replacement> #(::#segments)*)
            } else {
                syn::parse_quote!(#replacement)
            });
        }

        None
    }

    fn try_replace_expr_path_with_type(&self, path: &mut syn::Path) {
        let first_seg = path.segments.first_mut().unwrap();

        if let Some(replacement) = self.const_param_replacements.get(&first_seg.ident) {
            *first_seg = syn::parse_quote!(#replacement);
        }
    }
}

impl VisitMut for NonPredicateParamResolver<'_> {
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
                    *ty = syn::parse_quote!(#new_ty);
                } else {
                    self.try_replace_expr_path_with_type(&mut ty.path);
                }
            }
            _ => syn::visit_mut::visit_expr_mut(self, node),
        }
    }
}

// TODO: Lifetimes as type/const params can have the same ident
pub fn get_param_ident(generic_param: &syn::GenericParam) -> &syn::Ident {
    match generic_param {
        syn::GenericParam::Lifetime(syn::LifetimeParam { lifetime, .. }) => &lifetime.ident,
        syn::GenericParam::Type(syn::TypeParam { ident, .. }) => ident,
        syn::GenericParam::Const(syn::ConstParam { ident, .. }) => ident,
    }
}
