use itertools::iproduct;

use super::*;

mod expr;
mod generics;
mod pat;
mod path;
mod stmt;
mod ty;

pub trait Superset: Eq {
    /// If `self` is a superset of `other`, returns substitutions that are required to convert
    /// `other` into `self`, otherwise returns `None`.
    ///
    /// If `self` and `other` are identical but are not concrete types (i.e. they are generic types),
    /// returned substitutions contain identity mappings of type parameters (e.g. `T` -> `T`).
    ///
    /// If the substitutions are empty, `self` and `other are identical concrete types.
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions<'a>>;
}

trait Substitute {
    /// Returns [`Vec<Self>`] where each element is a clone of the original whose [`syn::Type`] / [`syn::Expr`]
    /// AST nodes have been substituted with a unique permutation of possible substitutions. Substitutions are
    /// only done for [`syn::Type`] and [`syn::Expr`], other types forward the request down the AST.
    ///
    /// An empty vector is never returned. If there are no valid substitution, returns the original value
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self>
    where
        Self: Sized;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SubstitutionValue<'a> {
    /// `T` -> `Vec<T>`
    Type(&'a syn::Type),
    /// `T` -> `T + 1`
    Expr(&'a syn::Expr),
    /// `T` -> `T`
    Identity,
}

impl<'a> From<&'a syn::Type> for SubstitutionValue<'a> {
    fn from(value: &'a syn::Type) -> Self {
        Self::Type(value)
    }
}

impl<'a> From<&'a syn::Expr> for SubstitutionValue<'a> {
    fn from(value: &'a syn::Expr) -> Self {
        Self::Expr(value)
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Substitutions<'a>(IndexMap<&'a syn::Ident, SubstitutionValue<'a>>);

impl<'a> Substitutions<'a> {
    fn new(source: &'a syn::Ident, dst: impl Into<SubstitutionValue<'a>>) -> Self {
        let mut substitutions = IndexMap::new();
        substitutions.insert(source, dst.into());

        Self(substitutions)
    }

    fn identity(source: &'a syn::Ident) -> Self {
        let mut substitutions = IndexMap::new();
        substitutions.insert(source, SubstitutionValue::Identity);

        Self(substitutions)
    }

    #[must_use]
    fn merge(mut self, other: Self) -> Option<Self> {
        for (ident, dst) in other.0.into_iter() {
            match self.0.entry(ident) {
                indexmap::map::Entry::Occupied(val) => {
                    if dst != *val.get() {
                        return None;
                    }
                }
                indexmap::map::Entry::Vacant(entry) => {
                    entry.insert(dst);
                }
            }
        }

        Some(self)
    }

    /// Return `true` if no substitution is required, i.e. this is an identity mapping
    fn is_eq(&self) -> bool {
        self.0.values().all(|&v| v == SubstitutionValue::Identity)
    }

    /// Return new trait bound where all types from the given trait bound, that are values in [`Self`],
    /// were replaced with corresponding mappings. Since each type can be replaced with multiple types,
    /// this functions returns all possible combinations.
    ///
    /// This function does reverse mapping, if you had mappings:
    ///     (`T` => `Vec<T>`, `U` => `Vec<T>`)
    /// the result of the substitution would be two possible trait bounds:
    /// * 1st where `Vec<T>` was replaced with `T`
    /// * 2nd where `Vec<T>` was replaced with `U`
    pub fn substitute(
        &self,
        (bounded_ty, trait_bound): &TraitBoundIdent,
    ) -> impl Iterator<Item = TraitBoundIdent> {
        let reverse_map =
            self.0
                .iter()
                .fold(IndexMap::<_, Vec<_>>::new(), |mut acc, (&source, &dst)| {
                    acc.entry(dst).or_default().push(source);
                    acc
                });

        // TODO: assert that there are no multiple `T` -> `fn(T)` reverse mappings

        let bounded = bounded_ty
            .0
            .substitute(&reverse_map)
            .into_iter()
            .map(Bounded);
        let trait_ = trait_bound
            .0
            .substitute(&reverse_map)
            .into_iter()
            .map(TraitBound);

        bounded.cartesian_product(trait_)
    }
}

impl Superset for ImplGroupId {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions<'a>> {
        match (&self.trait_, &other.trait_) {
            (Some(x1), Some(x2)) => x1.is_superset(x2)?,
            (None, None) => Substitutions::default(),
            _ => return None,
        }
        .merge(self.self_ty.is_superset(&other.self_ty)?)
    }
}

impl Substitute for ImplGroupId {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.trait_
            .as_ref()
            .map_or_else(
                || vec![None],
                |trait_| {
                    trait_
                        .substitute(substitutions)
                        .into_iter()
                        .map(Some)
                        .collect()
                },
            )
            .into_iter()
            .cartesian_product(self.self_ty.substitute(substitutions))
            .map(|(trait_, self_ty)| ImplGroupId { trait_, self_ty })
            .collect()
    }
}

impl<T: Superset> Superset for Option<T> {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions<'a>> {
        match (self, other) {
            (None, None) => Some(Substitutions::default()),
            (Some(x1), Some(x2)) => x1.is_superset(x2),
            _ => None,
        }
    }
}

impl<T: Substitute> Substitute for Option<T> {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.as_ref().map_or(vec![None], |x1| {
            x1.substitute(substitutions).into_iter().map(Some).collect()
        })
    }
}

impl Superset for Vec<syn::Attribute> {
    fn is_superset(&self, _: &Self) -> Option<Substitutions<'_>> {
        Some(Substitutions::default())
    }
}

impl Substitute for Vec<syn::Attribute> {
    fn substitute(&self, _: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>) -> Vec<Self> {
        vec![self.clone()]
    }
}

impl Superset for syn::Macro {
    fn is_superset(&self, other: &Self) -> Option<Substitutions<'_>> {
        (self == other).then_some(Substitutions::default())
    }
}

impl Substitute for syn::Macro {
    fn substitute(&self, _: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>) -> Vec<Self> {
        vec![self.clone()]
    }
}

impl Superset for syn::Lifetime {
    fn is_superset(&self, other: &Self) -> Option<Substitutions<'_>> {
        const UNDERSCORE: &str = "_";

        if self.ident == UNDERSCORE || other.ident == UNDERSCORE {
            return Some(Substitutions::default());
        }

        (self.ident == other.ident).then_some(Substitutions::default())
    }
}

impl Substitute for syn::Lifetime {
    fn substitute(&self, _: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>) -> Vec<Self> {
        vec![self.clone()]
    }
}

impl Superset for syn::BoundLifetimes {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions<'a>> {
        if self.lifetimes.len() != other.lifetimes.len() {
            return None;
        }

        zip(&self.lifetimes, &other.lifetimes)
            .try_fold(Substitutions::default(), |acc, (x1, x2)| {
                acc.merge(x1.is_superset(x2)?)
            })
    }
}

impl Substitute for syn::BoundLifetimes {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        if self.lifetimes.is_empty() {
            return vec![self.clone()];
        }

        self.lifetimes
            .iter()
            .map(|l| l.substitute(substitutions))
            .multi_cartesian_product()
            .map(|lifetimes| Self {
                lifetimes: lifetimes.into_iter().collect(),
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::Lit {
    fn is_superset(&self, other: &Self) -> Option<Substitutions<'_>> {
        (self == other).then_some(Substitutions::default())
    }
}

impl Substitute for syn::Lit {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        if let Some(subs) = substitutions.get(&SubstitutionValue::Expr(&syn::parse_quote!(#self))) {
            return subs.iter().map(|sub| syn::parse_quote!(#sub)).collect();
        }

        vec![self.clone()]
    }
}

impl Superset for syn::LitStr {
    fn is_superset(&self, other: &Self) -> Option<Substitutions<'_>> {
        (self == other).then_some(Substitutions::default())
    }
}

impl Substitute for syn::LitStr {
    fn substitute(&self, _: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>) -> Vec<Self> {
        vec![self.clone()]
    }
}

fn matches_param_ident(path: &syn::Path) -> Option<&syn::Ident> {
    if let Some(ident) = path.get_ident()
        && ident.to_string().starts_with("_ŠČ")
    {
        return Some(ident);
    }

    None
}
