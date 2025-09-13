//! Contains logic related to uniform position based (re-)naming of parameters

use proc_macro2::Span;

use quote::format_ident;
use syn::{parse_quote, visit_mut::VisitMut};

struct ElidedLifetimeNamer {
    curr_elided_lifetime_idx: usize,
}

struct SelfReplacer<'a> {
    self_ty: &'a syn::Type,
}

impl VisitMut for ElidedLifetimeNamer {
    fn visit_lifetime_mut(&mut self, node: &mut syn::Lifetime) {
        if node.ident == "_" {
            let idx = self.curr_elided_lifetime_idx;
            node.ident = format_ident!("_lšč{idx}");
            self.curr_elided_lifetime_idx += 1;
        }
    }
    fn visit_type_reference_mut(&mut self, node: &mut syn::TypeReference) {
        if node.lifetime.is_none() {
            node.lifetime = parse_quote!('_);
        }

        syn::visit_mut::visit_type_reference_mut(self, node);
    }
}

impl VisitMut for SelfReplacer<'_> {
    fn visit_type_mut(&mut self, node: &mut syn::Type) {
        if let syn::Type::Path(ty) = node {
            let first_seg = &ty.path.segments.first().unwrap();

            if first_seg.ident == "Self" {
                *node = replace_path(&ty.path, self.self_ty);
            }
        }

        syn::visit_mut::visit_type_mut(self, node);
    }
}

/// Normalizes an impl in the following ways:
///
/// * Assign default values for elided type arguments in trait impls
/// * Replace all occurrences of `Self` with concrete `self_ty`
/// * Uniquely name all elided lifetimes as `_lšč{idx}`
pub fn normalize(mut item_impl: syn::ItemImpl) -> syn::ItemImpl {
    let mut elided_lifetime_namer = ElidedLifetimeNamer {
        curr_elided_lifetime_idx: 0,
    };
    if let Some((_, trait_, _)) = &mut item_impl.trait_ {
        elided_lifetime_namer.visit_path_mut(trait_);
    }
    elided_lifetime_namer.visit_type_mut(&mut item_impl.self_ty);
    elided_lifetime_namer.visit_generics_mut(&mut item_impl.generics);

    let mut self_replacer = SelfReplacer {
        self_ty: &item_impl.self_ty,
    };
    if let Some((_, trait_, _)) = &mut item_impl.trait_ {
        self_replacer.visit_path_mut(trait_);
    }
    self_replacer.visit_generics_mut(&mut item_impl.generics);

    item_impl
        .generics
        .params
        .extend(
            (0..elided_lifetime_namer.curr_elided_lifetime_idx).map(|idx| {
                syn::GenericParam::from(syn::LifetimeParam::new(syn::Lifetime::new(
                    &format!("'_lšč{idx}"),
                    Span::call_site(),
                )))
            }),
        );

    item_impl
}

pub fn replace_path(ty: &syn::Path, replacement: &syn::Type) -> syn::Type {
    let segments = ty.segments.iter().skip(1);

    if segments.len() > 0 {
        return parse_quote!(<#replacement> #(::#segments)*);
    }

    parse_quote!(#replacement)
}
