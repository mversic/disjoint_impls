//! Contains logic for normalizing impls

use indexmap::{IndexMap, IndexSet};
use proc_macro2::Span;

use quote::format_ident;
use syn::{
    parse_quote,
    visit_mut::{VisitMut, visit_trait_bound_mut},
};

type AssocBindingIdent = (syn::Type, syn::TraitBound, syn::Ident);
type AssocBindingDesc = Vec<(Vec<syn::Attribute>, IndexSet<syn::TypeParamBound>)>;

struct ElidedLifetimeNamer {
    curr_elided_lifetime_idx: usize,
}

struct SelfReplacer<'a> {
    self_ty: &'a syn::Type,
}

struct ConstraintReplacer {
    curr_bounded_ty: Option<syn::Type>,
    curr_trait_bound: Option<syn::TraitBound>,
    curr_attrs: Vec<syn::Attribute>,
    curr_type_param_pos: Option<usize>,
    next_type_param_pos: usize,
    bounds: IndexMap<AssocBindingIdent, AssocBindingDesc>,
}

impl ConstraintReplacer {
    fn new() -> Self {
        Self {
            curr_bounded_ty: None,
            curr_trait_bound: None,
            curr_attrs: Vec::new(),
            curr_type_param_pos: None,
            next_type_param_pos: 0,
            bounds: IndexMap::new(),
        }
    }
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

impl VisitMut for ConstraintReplacer {
    fn visit_type_param_mut(&mut self, node: &mut syn::TypeParam) {
        let ident = &node.ident;

        self.curr_type_param_pos = Some(self.next_type_param_pos);
        self.curr_bounded_ty = Some(parse_quote!(#ident));
        self.curr_attrs = node.attrs.clone();
        self.next_type_param_pos += 1;

        syn::visit_mut::visit_type_param_mut(self, node);

        self.curr_bounded_ty = None;
        self.curr_attrs = Vec::new();
        self.curr_type_param_pos = None;
    }

    fn visit_predicate_type_mut(&mut self, node: &mut syn::PredicateType) {
        self.curr_bounded_ty = Some(node.bounded_ty.clone());
        self.curr_type_param_pos = None;
        syn::visit_mut::visit_predicate_type_mut(self, node);

        self.curr_bounded_ty = None;
    }

    fn visit_trait_bound_mut(&mut self, node: &mut syn::TraitBound) {
        self.curr_trait_bound = Some(node.clone());
        visit_trait_bound_mut(self, node);
    }

    fn visit_angle_bracketed_generic_arguments_mut(
        &mut self,
        node: &mut syn::AngleBracketedGenericArguments,
    ) {
        node.args.iter_mut().for_each(|arg| match arg {
            syn::GenericArgument::Constraint(constraint) => {
                let ident = &constraint.ident;
                let generics = core::mem::take(&mut constraint.generics);

                let current_bounded_ty = self.curr_bounded_ty.take().unwrap();
                let current_trait_bound = self.curr_trait_bound.take().unwrap();

                let mut key_trait_bound = current_trait_bound.clone();
                for seg in &mut key_trait_bound.path.segments {
                    if let syn::PathArguments::AngleBracketed(args) = &mut seg.arguments {
                        args.args = args
                            .args
                            .iter()
                            .filter(|arg| !matches!(arg, syn::GenericArgument::Constraint(_)))
                            .cloned()
                            .collect();
                    }
                }
                let key = (current_bounded_ty.clone(), key_trait_bound, ident.clone());

                let entry = self.bounds.entry(key.clone());
                let ty_idx = entry.index();
                entry.or_default();

                let ty = format_ident!("_TŠČ{}", ty_idx);
                let ty: syn::Type = parse_quote! { #ty };
                self.curr_bounded_ty = Some(ty.clone());

                let mut bounds = core::mem::take(&mut constraint.bounds);
                for constraint_bound in &mut bounds {
                    syn::visit_mut::visit_type_param_bound_mut(self, constraint_bound);
                }

                *arg = syn::GenericArgument::AssocType(syn::AssocType {
                    ident: ident.clone(),
                    generics,
                    eq_token: parse_quote!(=),
                    ty,
                });

                self.curr_bounded_ty = Some(current_bounded_ty);
                self.curr_trait_bound = Some(current_trait_bound);

                let entry = self.bounds.get_mut(&key).unwrap();
                if self.curr_type_param_pos.is_none() {
                    if entry.is_empty() {
                        entry.push(Default::default());
                    }

                    for (_, existing_bounds) in entry.iter_mut() {
                        existing_bounds.extend(bounds.clone());
                    }
                } else {
                    entry.push((self.curr_attrs.clone(), bounds.into_iter().collect()));
                }
            }
            _ => syn::visit_mut::visit_generic_argument_mut(self, arg),
        });
    }
}

/// Normalizes an impl in the following ways:
///
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

    let mut constraint_replacer = ConstraintReplacer::new();
    constraint_replacer.visit_item_impl_mut(&mut item_impl);

    item_impl.generics.params.extend(
        constraint_replacer
            .bounds
            .into_values()
            .enumerate()
            .flat_map(|(idx, entries)| {
                let ty = format_ident!("_TŠČ{}", idx);

                entries
                    .into_iter()
                    .map::<syn::GenericParam, _>(move |(attrs, bounds)| {
                        let bounds = bounds.into_iter();
                        parse_quote!(#(#attrs)* #ty: #(#bounds)+*)
                    })
            }),
    );

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nested_assoc_constraints() {
        let item_impl: syn::ItemImpl = parse_quote! {
            impl<T: Dispatch<Group: Dispatch<Group: Dispatch<Group = GroupD>>>> Kita for T {}
        };
        let expected: syn::ItemImpl = parse_quote! {
            impl<
                T: Dispatch<Group = _TŠČ0>,
                _TŠČ0: Dispatch<Group = _TŠČ1>,
                _TŠČ1: Dispatch<Group = GroupD>
            > Kita for T {}
        };

        let res = normalize(item_impl);
        assert_eq!(expected, res);
    }
}
