use proc_macro2::Span;
use quote::ToTokens;
use syn::punctuated::Punctuated;

use super::*;

mod expr;
mod generics;
mod item;
mod pat;
mod path;
mod stmt;
mod token;
mod ty;

/// Collection of type or constant generic parameters
pub type Params = IndexMap<syn::Ident, GenericParam>;

type TypeGeneralization<'a> = (Sizedness, IndexSet<(&'a syn::Lifetime, &'a syn::Lifetime)>);

type ExprGeneralization = (syn::Type, ExprTypeKind);

/// Types that can be generalized to a common supertype
///
/// # Example
///
/// 1. `Vec<u32>` and `Vec<i32>` can be generalized into `Vec<T>`
/// 2. `Vec<T>` and `Vec<T>` can be generalized into `Vec<T>`
/// 3. `[u32; 2]` and `[u32; 2]` can be generalized into `[u32; 2]`
/// 4. `[u32; 2]` and `(i32, u32)` can be generalized into `T`
pub trait Generalize: ToOwned {
    /// Find a generalization of `self` and `other` (if it exists).
    ///
    /// If `self` and `other` are identical but are not concrete types (i.e. they are generic types),
    /// substitutions will contain identity mappings of type parameters (e.g. `T` -> `T`).
    ///
    /// If the substitutions are empty, `self` and `other are identical concrete types.
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self::Owned>;
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Generalizations<'a> {
    /// List of mappings transforming lifetimes such as:
    /// `'a -> `'b in `Vec<&'a [u32; 2]>` and `Vec<&'b i32>`
    lifetime_generalizations: IndexSet<(&'a syn::Ident, &'a syn::Ident)>,

    /// Type mappings that share a common parent such as:
    /// `(T, U)` and `[T; 2]` in `Vec<(T, U)>` and `Vec<[T; 2]>`
    type_generalizations: IndexMap<(&'a syn::Type, &'a syn::Type), TypeGeneralization<'a>>,
    /// Expression mappings that share a common parent such as:
    /// `2 * N` and `N + 2` in `[T; 2 * N]` and `[T; N + 2]`
    expr_generalizations: IndexMap<(Expr<'a>, Expr<'a>), ExprGeneralization>,

    /// Helper that keeps track of the expected type of the constant parameter
    curr_expr_expected_type: Option<syn::Type>,

    /// Helper that keeps track of the lifetime to be attached to type parameters
    curr_ref_lifetime: Vec<(&'a syn::Lifetime, &'a syn::Lifetime)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Disjointness {
    Disjoint,
    SoftOverlap,
    ParamOverlap,
}

/// Tracks whether [`syn::Type`] is sized or unsized
#[derive(Debug, Clone, Copy, Eq, PartialEq, Default)]
pub enum Sizedness {
    Unsized,
    #[default]
    Sized,
}

#[derive(Debug, Clone, Copy, Eq)]
pub enum Expr<'a> {
    Ident(&'a syn::Ident),
    Expr(&'a syn::Expr),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExprTypeKind {
    Inferred,
    Given,
}

#[derive(Debug, Clone)]
pub enum GenericParam {
    Type(Sizedness, IndexSet<syn::LifetimeParam>),
    Const(syn::Type),
}

/// Convert [`Params`] to a list of [`syn::GenericParam`]
pub fn as_generics(params: &Params) -> impl Iterator<Item = syn::GenericParam> {
    let type_params = params.iter().filter_map(|(ident, param)| {
        if let GenericParam::Type(sizedness, lifetimes) = param {
            let type_param: syn::TypeParam = if *sizedness == Sizedness::Unsized {
                let lifetimes = lifetimes.iter();
                parse_quote! { #ident: ?Sized #(+ #lifetimes)*}
            } else if !lifetimes.is_empty() {
                let lifetimes = lifetimes.iter();
                parse_quote! { #ident: #(#lifetimes) + *}
            } else {
                parse_quote! { #ident }
            };

            return Some(syn::GenericParam::Type(type_param));
        }

        None
    });

    let const_params = params.iter().filter_map(|(ident, param)| {
        if let GenericParam::Const(ty) = param {
            return Some(syn::GenericParam::Const(parse_quote! { const #ident: #ty }));
        }

        None
    });

    type_params.chain(const_params)
}

impl ImplItems {
    pub fn generalize(
        &self,
        other: &Self,
        params1: &Params,
        params2: &Params,
        subs: &Generalizations<'_>,
    ) -> Option<Self> {
        let mut new_subs = subs.clone();

        let generalized_fns = self
            .fns
            .iter()
            .map(|(ident, sig)| {
                let other_sig = other.fns.get(ident)?;

                Some((
                    ident.clone(),
                    sig.generalize(other_sig, params1, params2, &mut new_subs)?,
                ))
            })
            .collect::<Option<_>>()?;

        let generalized_assoc_types = self
            .assoc_types
            .iter()
            .all(|ident| other.assoc_types.contains(ident))
            .then_some(self.assoc_types.clone())?;

        let generalized_assoc_consts = self
            .assoc_consts
            .iter()
            .map(|(ident, self_ty)| {
                let other_ty = other.assoc_consts.get(ident)?;

                Some((
                    ident.clone(),
                    self_ty.generalize(other_ty, params1, params2, &mut new_subs)?,
                ))
            })
            .collect::<Option<_>>()?;

        if !new_subs.difference(subs).is_empty() {
            return None;
        }

        Some(ImplItems {
            fns: generalized_fns,
            assoc_types: generalized_assoc_types,
            assoc_consts: generalized_assoc_consts,
        })
    }
}

impl PartialEq for Expr<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Expr::Ident(i1), Expr::Ident(i2)) => i1 == i2,
            (Expr::Expr(syn::Expr::Path(e1)), Expr::Ident(e2))
            | (Expr::Ident(e2), Expr::Expr(syn::Expr::Path(e1))) => {
                e1.path.get_ident().is_some_and(|e1| e1 == *e2)
            }
            (Expr::Expr(e1), Expr::Expr(e2)) => e1 == e2,
            _ => false,
        }
    }
}

impl PartialEq<syn::Expr> for Expr<'_> {
    fn eq(&self, other: &syn::Expr) -> bool {
        match (self, other) {
            (Expr::Ident(i1), syn::Expr::Path(e2)) => {
                e2.path.get_ident().is_some_and(|id| id == *i1)
            }
            (Expr::Expr(e1), e2) => *e1 == e2,
            _ => false,
        }
    }
}

impl core::hash::Hash for Expr<'_> {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        match self {
            Expr::Ident(i) => i.hash(state),
            Expr::Expr(expr) => {
                if let syn::Expr::Path(e) = expr
                    && let Some(ident) = e.path.get_ident()
                {
                    ident.hash(state);
                } else {
                    expr.hash(state);
                }
            }
        }
    }
}

impl<'a> From<&'a syn::Expr> for Expr<'a> {
    fn from(value: &'a syn::Expr) -> Self {
        Self::Expr(value)
    }
}

impl<'a> From<&'a syn::Ident> for Expr<'a> {
    fn from(value: &'a syn::Ident) -> Self {
        Self::Ident(value)
    }
}

impl ToTokens for Expr<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            Expr::Expr(e) => e.to_tokens(tokens),
            Expr::Ident(i) => i.to_tokens(tokens),
        }
    }
}

impl<'a> Generalizations<'a> {
    fn insert_lifetime(&mut self, src: &'a syn::Ident, dst: &'a syn::Ident) -> syn::Lifetime {
        let (idx, _) = self.lifetime_generalizations.insert_full((src, dst));
        syn::Lifetime::new(&format!("'_lšč{}", idx), Span::call_site())
    }

    fn insert_type(
        &mut self,
        src: &'a syn::Type,
        dst: &'a syn::Type,
        params1: &Params,
        params2: &Params,
    ) -> syn::Type {
        fn sizedness(ty: &syn::Type, params: &Params) -> Sizedness {
            match ty {
                syn::Type::Slice(_) => Sizedness::Unsized,
                syn::Type::TraitObject(_) => Sizedness::Unsized,
                syn::Type::Path(p) => {
                    if let Some(ident) = p.path.get_ident() {
                        if *ident == "str" {
                            return Sizedness::Unsized;
                        }

                        return params
                            .get(ident)
                            .map(|param| match param {
                                &GenericParam::Type(sizedness, _) => sizedness,
                                _ => unreachable!(),
                            })
                            .unwrap_or_default();
                    }

                    Sizedness::Sized
                }
                _ => Sizedness::Sized,
            }
        }

        let sizedness = {
            if sizedness(src, params1) == Sizedness::Unsized {
                Sizedness::Unsized
            } else {
                sizedness(dst, params2)
            }
        };

        let entry = self.type_generalizations.entry((src, dst));
        let ident = format_ident!("_TŠČ{}", entry.index());
        let val = entry.or_insert((sizedness, IndexSet::new()));
        val.1.extend(self.curr_ref_lifetime.iter().copied());

        parse_quote! { #ident }
    }

    fn insert_expr(
        &mut self,
        src: impl Into<Expr<'a>>,
        dst: impl Into<Expr<'a>>,
        params1: &Params,
        params2: &Params,
    ) -> Option<syn::Expr> {
        fn infer_expr_type(expr: &syn::Expr, params: &Params) -> Option<(syn::Type, ExprTypeKind)> {
            use syn::Expr;

            match expr {
                Expr::Lit(lit) => match &lit.lit {
                    syn::Lit::Str(_) => Some((parse_quote! { &str }, ExprTypeKind::Inferred)),
                    syn::Lit::ByteStr(_) => Some((parse_quote! { &[u8] }, ExprTypeKind::Inferred)),
                    syn::Lit::Byte(_) => Some((parse_quote! { u8 }, ExprTypeKind::Inferred)),
                    syn::Lit::Char(_) => Some((parse_quote! { char }, ExprTypeKind::Inferred)),

                    syn::Lit::Int(l) => match l.suffix() {
                        "" => Some((parse_quote! { i32 }, ExprTypeKind::Inferred)),

                        "i8" => Some((parse_quote! { i8 }, ExprTypeKind::Given)),
                        "i16" => Some((parse_quote! { i16 }, ExprTypeKind::Given)),
                        "i32" => Some((parse_quote! { i32 }, ExprTypeKind::Given)),
                        "i64" => Some((parse_quote! { i64 }, ExprTypeKind::Given)),
                        "i128" => Some((parse_quote! { i128 }, ExprTypeKind::Given)),
                        "isize" => Some((parse_quote! { isize }, ExprTypeKind::Given)),

                        "u8" => Some((parse_quote! { u8 }, ExprTypeKind::Given)),
                        "u16" => Some((parse_quote! { u16 }, ExprTypeKind::Given)),
                        "u32" => Some((parse_quote! { u32 }, ExprTypeKind::Given)),
                        "u64" => Some((parse_quote! { u64 }, ExprTypeKind::Given)),
                        "u128" => Some((parse_quote! { u128 }, ExprTypeKind::Given)),
                        "usize" => Some((parse_quote! { usize }, ExprTypeKind::Given)),

                        _ => None,
                    },

                    syn::Lit::Float(l) => match l.suffix() {
                        "" => Some((parse_quote! { f64 }, ExprTypeKind::Inferred)),
                        "f32" => Some((parse_quote! { f32 }, ExprTypeKind::Given)),
                        "f64" => Some((parse_quote! { f64 }, ExprTypeKind::Given)),
                        _ => None,
                    },

                    syn::Lit::Bool(_) => Some((parse_quote! { bool }, ExprTypeKind::Given)),

                    _ => None,
                },

                Expr::Path(p) => {
                    if let Some(ident) = p.path.get_ident()
                        && let Some(ty) = params
                            .get(ident)
                            .map(|param| match param {
                                GenericParam::Const(ty) => ty,
                                _ => unreachable!(),
                            })
                            .cloned()
                    {
                        return Some((ty, ExprTypeKind::Given));
                    }

                    None
                }

                Expr::Reference(e) => {
                    let (inner, expr_type) = infer_expr_type(&e.expr, params)?;
                    Some((parse_quote! { & #inner }, expr_type))
                }

                Expr::Tuple(t) => {
                    let elems: Vec<_> = t
                        .elems
                        .iter()
                        .map(|e| infer_expr_type(e, params))
                        .collect::<Option<_>>()?;

                    let expr = elems.iter().map(|(expr, _)| expr);
                    let is_type_given = elems
                        .iter()
                        .all(|(_, expr_type)| *expr_type == ExprTypeKind::Given);

                    Some((
                        parse_quote! { ( #(#expr),* ) },
                        if is_type_given {
                            ExprTypeKind::Given
                        } else {
                            ExprTypeKind::Inferred
                        },
                    ))
                }

                Expr::Array(arr) => {
                    let len = arr.elems.len();
                    let mut inferred = None;

                    for e in &arr.elems {
                        if let Some((ty, et)) = infer_expr_type(e, params) {
                            let ty: syn::Type = parse_quote! { [#ty; #len] };

                            match et {
                                ExprTypeKind::Given => {
                                    return Some((ty, ExprTypeKind::Given));
                                }
                                ExprTypeKind::Inferred if inferred.is_none() => {
                                    inferred = Some((ty, ExprTypeKind::Inferred));
                                }
                                _ => {}
                            }
                        }
                    }

                    inferred
                }

                Expr::Unary(unary) => infer_expr_type(&unary.expr, params),

                Expr::Binary(bin) => {
                    let lhs = infer_expr_type(&bin.left, params);
                    if lhs
                        .as_ref()
                        .is_some_and(|(_, ty_)| *ty_ == ExprTypeKind::Given)
                    {
                        return lhs;
                    }

                    let rhs = infer_expr_type(&bin.right, params);
                    if rhs
                        .as_ref()
                        .is_some_and(|(_, ty_)| *ty_ == ExprTypeKind::Given)
                    {
                        return rhs;
                    }

                    lhs.or(rhs)
                }

                Expr::Paren(p) => infer_expr_type(&p.expr, params),

                Expr::Block(b) => b.block.stmts.last().and_then(|stmt| {
                    if let syn::Stmt::Expr(e, None) = stmt {
                        return infer_expr_type(e, params);
                    }

                    None
                }),

                Expr::If(ifexpr) => {
                    if let Some(then_ty) = ifexpr.then_branch.stmts.last().and_then(|stmt| {
                        if let syn::Stmt::Expr(e, None) = stmt {
                            return infer_expr_type(e, params);
                        }

                        None
                    }) {
                        return Some(then_ty);
                    }

                    if let Some((_, else_expr)) = &ifexpr.else_branch {
                        return infer_expr_type(else_expr, params);
                    }

                    None
                }

                Expr::Match(m) => m
                    .arms
                    .iter()
                    .find_map(|arm| infer_expr_type(&arm.body, params)),

                Expr::Struct(s) => {
                    let path = &s.path;
                    let ty = parse_quote! { #path };
                    Some((ty, ExprTypeKind::Given))
                }

                Expr::Closure(_) => Some((parse_quote! { impl Fn }, ExprTypeKind::Inferred)),

                Expr::Call(_) => None,

                _ => None,
            }
        }

        let src = src.into();
        let dst = dst.into();

        let (ty, ty_kind) = if let Some(expected_ty) = &self.curr_expr_expected_type {
            (expected_ty.clone(), ExprTypeKind::Given)
        } else {
            let src = match src {
                Expr::Ident(ident) => match &params1[ident] {
                    GenericParam::Const(ty) => Some((ty.clone(), ExprTypeKind::Given)),
                    _ => unreachable!(),
                },
                Expr::Expr(expr) => infer_expr_type(expr, params1),
            };

            if let Some((src_ty, ExprTypeKind::Given)) = src {
                (src_ty, ExprTypeKind::Given)
            } else {
                let dst = match dst {
                    Expr::Ident(ident) => match &params2[ident] {
                        GenericParam::Const(ty) => Some((ty.clone(), ExprTypeKind::Given)),
                        _ => unreachable!(),
                    },
                    Expr::Expr(expr) => infer_expr_type(expr, params1),
                };

                match dst {
                    Some(dst_ty) => Some(dst_ty),
                    None => src,
                }?
            }
        };

        let entry = self.expr_generalizations.entry((src, dst));
        let ident = format_ident!("_CŠČ{}", entry.index());
        entry.or_insert((ty, ty_kind));

        Some(parse_quote! { #ident })
    }

    #[must_use]
    // FIXME: References could be returned here?
    pub fn difference(&self, other: &Self) -> Self {
        let type_substitutions = self
            .type_generalizations
            .iter()
            .filter(|(k, _)| !other.type_generalizations.contains_key(*k))
            .map(|(k, v)| (*k, v.clone()))
            .collect();

        let expr_substitutions = self
            .expr_generalizations
            .iter()
            .filter(|(k, _)| !other.expr_generalizations.contains_key(*k))
            .map(|(k, v)| (*k, v.clone()))
            .collect();

        Self {
            lifetime_generalizations: IndexSet::new(),
            type_generalizations: type_substitutions,
            expr_generalizations: expr_substitutions,

            curr_expr_expected_type: None,
            curr_ref_lifetime: vec![],
        }
    }

    pub fn is_empty(&self) -> bool {
        self.lifetime_generalizations.is_empty()
            && self.type_generalizations.is_empty()
            && self.expr_generalizations.is_empty()
    }

    #[must_use]
    pub fn unify(mut self, other: Self) -> Self {
        for (mapping, elem_val) in other.lifetime_generalizations {
            self.lifetime_generalizations.insert((mapping, elem_val));
        }

        for (mapping, elem_val) in other.type_generalizations {
            match self.type_generalizations.entry(mapping) {
                indexmap::map::Entry::Vacant(entry) => {
                    entry.insert(elem_val);
                }
                indexmap::map::Entry::Occupied(mut val) => {
                    let val = val.get_mut();

                    if elem_val.0 == Sizedness::Unsized {
                        val.0 = Sizedness::Unsized;
                    };

                    val.1.extend(elem_val.1);
                }
            }
        }

        for (mapping, elem_val) in other.expr_generalizations {
            match self.expr_generalizations.entry(mapping) {
                indexmap::map::Entry::Vacant(entry) => {
                    entry.insert(elem_val);
                }
                indexmap::map::Entry::Occupied(mut val) => {
                    let val = val.get_mut();

                    match (val.1, elem_val.1) {
                        (ExprTypeKind::Given, ExprTypeKind::Given) => {
                            if elem_val.1 != val.1 {
                                unreachable!();
                            }
                        }
                        (ExprTypeKind::Inferred, ExprTypeKind::Given) => {
                            val.1 = ExprTypeKind::Given;
                            val.0 = elem_val.0;
                        }
                        _ => {}
                    }
                }
            }
        }

        self
    }

    pub fn generic_args(
        &self,
    ) -> impl Iterator<Item = (syn::GenericArgument, syn::GenericArgument)> {
        let lifetimes = self.lifetime_generalizations.iter().map(|&(src, dst)| {
            (
                syn::GenericArgument::Lifetime(syn::Lifetime::new(
                    &format!("'{}", src),
                    Span::call_site(),
                )),
                syn::GenericArgument::Lifetime(syn::Lifetime::new(
                    &format!("'{}", dst),
                    Span::call_site(),
                )),
            )
        });

        let types = self.type_generalizations.keys().map(|&(t1, t2)| {
            (
                syn::GenericArgument::Type(t1.clone()),
                syn::GenericArgument::Type(t2.clone()),
            )
        });

        let consts = self.expr_generalizations.keys().map(|&(e1, e2)| {
            let e1 = match e1 {
                Expr::Expr(expr) => syn::GenericArgument::Const(expr.clone()),
                Expr::Ident(ident) => syn::GenericArgument::Const(parse_quote!(#ident)),
            };

            let e2 = match e2 {
                Expr::Expr(expr) => syn::GenericArgument::Const(expr.clone()),
                Expr::Ident(ident) => syn::GenericArgument::Const(parse_quote!(#ident)),
            };

            (e1, e2)
        });

        lifetimes.chain(types).chain(consts)
    }

    pub fn build_params(self) -> (Vec<syn::LifetimeParam>, Params) {
        let build_lifetime = |index| {
            syn::LifetimeParam::new(syn::Lifetime::new(
                &format!("'_lšč{}", index),
                Span::call_site(),
            ))
        };

        let lifetimes = (0..self.lifetime_generalizations.len())
            .map(build_lifetime)
            .collect::<Vec<_>>();

        let type_params = self.type_generalizations.into_iter().enumerate().map(
            |(i, (_, (is_unsized, lifetimes)))| {
                let ident = format_ident!("_TŠČ{}", i);

                let lifetimes = lifetimes
                    .into_iter()
                    .map(|(src, dst)| {
                        let idx = self
                            .lifetime_generalizations
                            .get_index_of(&(&src.ident, &dst.ident))
                            .unwrap();

                        build_lifetime(idx)
                    })
                    .collect();

                (ident, GenericParam::Type(is_unsized, lifetimes))
            },
        );

        let const_params =
            self.expr_generalizations
                .into_iter()
                .enumerate()
                .map(|(i, (_, (ty, _)))| {
                    let ident = format_ident!("_CŠČ{}", i);
                    (ident, GenericParam::Const(ty))
                });

        (lifetimes, type_params.chain(const_params).collect())
    }

    pub fn is_disjoint(&self, params1: &Params, params2: &Params) -> Option<bool> {
        fn is_param_type(ty: &syn::Type, params: &Params) -> bool {
            if let syn::Type::Path(syn::TypePath { path, qself: None }) = ty
                && let Some(ident) = path.get_ident()
                && params.contains_key(ident)
            {
                return true;
            }

            false
        }

        fn is_param_expr(expr: Expr<'_>, params: &Params) -> bool {
            match expr {
                Expr::Ident(ident) => params
                    .get(ident)
                    .is_some_and(|param| matches!(param, GenericParam::Const(_))),
                Expr::Expr(e) => {
                    if let syn::Expr::Path(syn::ExprPath {
                        path, qself: None, ..
                    }) = e
                        && let Some(ident) = path.get_ident()
                    {
                        return params
                            .get(ident)
                            .is_some_and(|param| matches!(param, GenericParam::Const(_)));
                    }

                    false
                }
            }
        }

        let mut soft_overlap = false;
        let mut type_by_src: IndexMap<_, IndexSet<_>> = IndexMap::new();
        let mut type_by_dst: IndexMap<_, IndexSet<_>> = IndexMap::new();
        for &(src, dst) in self.type_generalizations.keys() {
            let src_is_param = is_param_type(src, params1);
            let dst_is_param = is_param_type(dst, params2);

            if src != dst && !src_is_param && !dst_is_param {
                return Some(true);
            }

            if src_is_param ^ dst_is_param {
                soft_overlap = true;
            }

            type_by_src.entry(src).or_default().insert(dst_is_param);
            type_by_dst.entry(dst).or_default().insert(src_is_param);
        }

        for dsts in type_by_src.values() {
            if dsts.iter().filter(|&is_param| !is_param).count() >= 2 {
                return Some(true);
            }
        }

        for srcs in type_by_dst.values() {
            if srcs.iter().filter(|&is_param| !is_param).count() >= 2 {
                return Some(true);
            }
        }

        let mut expr_by_src: IndexMap<_, IndexSet<_>> = IndexMap::new();
        let mut expr_by_dst: IndexMap<_, IndexSet<_>> = IndexMap::new();
        for &(src, dst) in self.expr_generalizations.keys() {
            let src_is_param = is_param_expr(src, params1);
            let dst_is_param = is_param_expr(dst, params2);

            if src != dst && !src_is_param && !dst_is_param {
                return Some(true);
            }

            if src_is_param ^ dst_is_param {
                soft_overlap = true;
            }

            expr_by_src.entry(src).or_default().insert(dst_is_param);
            expr_by_dst.entry(dst).or_default().insert(src_is_param);
        }

        for dsts in expr_by_src.values() {
            if dsts.iter().filter(|&is_param| !is_param).count() >= 2 {
                return Some(true);
            }
        }

        for srcs in expr_by_dst.values() {
            if srcs.iter().filter(|&is_param| !is_param).count() >= 2 {
                return Some(true);
            }
        }

        if soft_overlap {
            return None;
        }

        Some(false)
    }
}

impl Generalize for Bounded {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        subs: &mut Generalizations<'a>,
    ) -> Option<Self> {
        self.0
            .generalize(&other.0, params1, params2, subs)
            .map(Self)
    }
}

impl Generalize for TraitBound {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        subs: &mut Generalizations<'a>,
    ) -> Option<Self> {
        self.0
            .generalize(&other.0, params1, params2, subs)
            .map(Self)
    }
}

impl Generalize for ImplGroupId {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        subs: &mut Generalizations<'a>,
    ) -> Option<Self> {
        let trait_ = self
            .trait_
            .generalize(&other.trait_, params1, params2, subs)?;
        let self_ty = self
            .self_ty
            .generalize(&other.self_ty, params1, params2, subs)?;

        Some(Self { trait_, self_ty })
    }
}

impl<T: Generalize<Owned = X>, U: Generalize<Owned = Y>, X, Y> Generalize for (T, U)
where
    Self: ToOwned<Owned = (X, Y)>,
{
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        subs: &mut Generalizations<'a>,
    ) -> Option<Self::Owned> {
        Some((
            self.0.generalize(&other.0, params1, params2, subs)?,
            self.1.generalize(&other.1, params1, params2, subs)?,
        ))
    }
}

impl<T: Generalize<Owned = X>, X> Generalize for Box<T>
where
    Self: ToOwned<Owned = Box<X>>,
{
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        subs: &mut Generalizations<'a>,
    ) -> Option<Self::Owned> {
        (**self)
            .generalize(other, params1, params2, subs)
            .map(Box::new)
    }
}

impl<T: Generalize<Owned = X>, X> Generalize for Option<T>
where
    Self: ToOwned<Owned = Option<X>>,
{
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        subs: &mut Generalizations<'a>,
    ) -> Option<Self::Owned> {
        match (self, other) {
            (Some(x1), Some(x2)) => x1.generalize(x2, params1, params2, subs).map(Some),
            (None, None) => Some(None),
            _ => None,
        }
    }
}

impl<T: Generalize<Owned = X>, X: Clone> Generalize for [T]
where
    Self: ToOwned<Owned = Vec<X>>,
{
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        subs: &mut Generalizations<'a>,
    ) -> Option<Self::Owned> {
        if self.len() != other.len() {
            return None;
        }

        self.iter()
            .zip_eq(other.iter())
            .map(|(x1, x2)| x1.generalize(x2, params1, params2, subs))
            .collect()
    }
}

impl<T: Generalize<Owned = X>, P: Default, X> Generalize for Punctuated<T, P>
where
    Self: Clone + ToOwned<Owned = Punctuated<X, P>>,
{
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        subs: &mut Generalizations<'a>,
    ) -> Option<Self::Owned> {
        if self.len() != other.len() {
            return None;
        }

        self.iter()
            .zip_eq(other.iter())
            .map(|(x1, x2)| x1.generalize(x2, params1, params2, subs))
            .collect()
    }
}

impl Generalize for syn::Attribute {
    fn generalize(
        &self,
        _: &Self,
        _: &Params,
        _: &Params,
        _: &mut Generalizations<'_>,
    ) -> Option<Self> {
        Some(self.clone())
    }
}

impl Generalize for syn::Macro {
    fn generalize(
        &self,
        other: &Self,
        _: &Params,
        _: &Params,
        _: &mut Generalizations<'_>,
    ) -> Option<Self> {
        (self == other).then_some(self.clone())
    }
}

impl Generalize for syn::Lifetime {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        _: &Params,
        _: &Params,
        subs: &mut Generalizations<'a>,
    ) -> Option<Self> {
        if self.ident == "static" && other.ident == "static" {
            return Some(self.clone());
        }

        Some(subs.insert_lifetime(&self.ident, &other.ident))
    }
}

impl Generalize for syn::Lit {
    fn generalize(
        &self,
        other: &Self,
        _: &Params,
        _: &Params,
        _: &mut Generalizations<'_>,
    ) -> Option<Self> {
        (self == other).then_some(self.clone())
    }
}

impl Generalize for syn::LitStr {
    fn generalize(
        &self,
        other: &Self,
        _: &Params,
        _: &Params,
        _: &mut Generalizations<'_>,
    ) -> Option<Self> {
        (self == other).then_some(self.clone())
    }
}

#[cfg(test)]
mod tests {
    use indexmap::indexmap;

    use super::*;

    #[test]
    fn combine_ty_and_expr_params() {
        let p = indexmap! {
            format_ident!("T0") => GenericParam::Type(Sizedness::Sized, IndexSet::new()),
            format_ident!("T1") => GenericParam::Type(Sizedness::Sized, IndexSet::new()),
            format_ident!("N") => GenericParam::Const(parse_quote!(usize))
        };

        let l1_ty: syn::Type = parse_quote!(([T0; N], T1));
        let l2_ty: syn::Type = parse_quote!(([T1; N], T0));
        let expected_ty = parse_quote!(([_TŠČ0; _CŠČ0], _TŠČ1));

        let mut subs1 = Generalizations::default();
        let mut subs2 = Generalizations::default();

        let ty1 = l1_ty.generalize(&l2_ty, &p, &p, &mut subs1).unwrap();
        let ty2 = l2_ty.generalize(&l1_ty, &p, &p, &mut subs2).unwrap();

        assert_eq!(ty1, expected_ty);
        assert_eq!(ty2, expected_ty);

        let t0 = parse_quote!(T0);
        let t1 = parse_quote!(T1);
        let n = format_ident!("N");

        assert!(subs1.lifetime_generalizations.is_empty());
        assert!(subs2.lifetime_generalizations.is_empty());

        assert!(subs1.type_generalizations.iter().eq(&indexmap! {
            (&t0, &t1) => (Sizedness::Sized, IndexSet::new()),
            (&t1, &t0) => (Sizedness::Sized, IndexSet::new()),
        }));
        assert!(subs2.type_generalizations.iter().eq(&indexmap! {
            (&t1, &t0) => (Sizedness::Sized, IndexSet::new()),
            (&t0, &t1) => (Sizedness::Sized, IndexSet::new()),
        }));

        assert_eq!(
            subs1.expr_generalizations,
            indexmap! {
                (Expr::Ident(&n), Expr::Ident(&n)) => (parse_quote!(usize), ExprTypeKind::Given)
            }
        );
        assert_eq!(
            subs2.expr_generalizations,
            indexmap! {
                (Expr::Ident(&n), Expr::Ident(&n)) => (parse_quote!(usize), ExprTypeKind::Given)
            },
        );
    }
}
