use super::*;

impl Superset for syn::TraitBound {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.modifier != other.modifier {
            return None;
        }

        self.lifetimes
            .is_superset(&other.lifetimes)?
            .merge(self.path.is_superset(&other.path)?)
    }
}

impl Substitute for syn::TraitBound {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.lifetimes
            .substitute(substitutions)
            .into_iter()
            .cartesian_product(self.path.substitute(substitutions))
            .map(|(lifetimes, path)| Self {
                lifetimes,
                path,
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::GenericParam {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        use syn::GenericParam::*;

        match (self, other) {
            (Lifetime(x1), Lifetime(x2)) => x1.is_superset(x2),
            (Type(x1), Type(x2)) => x1.is_superset(x2),
            (Const(x1), Const(x2)) => x1.is_superset(x2),
            _ => None,
        }
    }
}

impl Substitute for syn::GenericParam {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        use syn::GenericParam::*;

        match self {
            Lifetime(x) => x
                .substitute(substitutions)
                .into_iter()
                .map(Lifetime)
                .collect(),
            Type(x) => x.substitute(substitutions).into_iter().map(Type).collect(),
            Const(x) => x.substitute(substitutions).into_iter().map(Const).collect(),
        }
    }
}

impl Superset for syn::LifetimeParam {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.lifetime.is_superset(&other.lifetime)?)
    }
}

impl Substitute for syn::LifetimeParam {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.lifetime
            .substitute(substitutions)
            .into_iter()
            .map(|lifetime| Self {
                lifetime,
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::TypeParam {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.ident != other.ident || self.bounds.len() != other.bounds.len() {
            return None;
        }

        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.default.is_superset(&other.default)?)?
            .merge(
                // TODO: Order is irrelevant here. Take a look at TypeImplTrait
                zip(&self.bounds, &other.bounds)
                    .try_fold(Substitutions::default(), |acc, (x1, x2)| {
                        acc.merge(x1.is_superset(x2)?)
                    })?,
            )
    }
}

impl Substitute for syn::TypeParam {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.default
            .substitute(substitutions)
            .into_iter()
            .cartesian_product(
                self.bounds
                    .iter()
                    .map(|bound| bound.substitute(substitutions))
                    .multi_cartesian_product(),
            )
            .map(|(default, bounds)| Self {
                default,
                bounds: bounds.into_iter().collect(),
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::ConstParam {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.ident != other.ident {
            return None;
        }

        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.ty.is_superset(&other.ty)?)?
            .merge(match (&self.default, &other.default) {
                (Some(x1), Some(x2)) => x1.is_superset(x2)?,
                (None, None) => Substitutions::default(),
                _ => return None,
            })
    }
}

impl Substitute for syn::ConstParam {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.ty
            .substitute(substitutions)
            .into_iter()
            .cartesian_product(self.default.substitute(substitutions))
            .map(|(ty, default)| Self {
                ty,
                default,
                ..self.clone()
            })
            .collect()
    }
}
