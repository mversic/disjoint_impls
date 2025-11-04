use super::*;

impl Generalize for syn::LifetimeParam {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        let attrs = self
            .attrs
            .generalize(&other.attrs, params1, params2, substitutions)?;
        let lifetime =
            self.lifetime
                .generalize(&other.lifetime, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            lifetime,
            ..self.clone()
        })
    }
}

impl Generalize for syn::TypeParamBound {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        use syn::TypeParamBound::*;

        match (self, other) {
            (Trait(x1), Trait(x2)) => {
                let generalized = x1.generalize(x2, params1, params2, substitutions)?;
                Some(Trait(generalized))
            }
            (Lifetime(x1), Lifetime(x2)) => {
                let generalized = x1.generalize(x2, params1, params2, substitutions)?;
                Some(Lifetime(generalized))
            }
            (PreciseCapture(_), _) | (_, PreciseCapture(_)) => unreachable!(),
            _ => None,
        }
    }
}

impl Generalize for syn::TraitBound {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        if self.paren_token != other.paren_token || self.modifier != other.modifier {
            return None;
        }

        let lifetimes =
            self.lifetimes
                .generalize(&other.lifetimes, params1, params2, substitutions)?;
        let path = self
            .path
            .generalize(&other.path, params1, params2, substitutions)?;

        Some(Self {
            lifetimes,
            path,
            ..self.clone()
        })
    }
}

impl Generalize for syn::BoundLifetimes {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        let lifetimes =
            self.lifetimes
                .generalize(&other.lifetimes, params1, params2, substitutions)?;

        Some(Self {
            lifetimes,
            ..self.clone()
        })
    }
}

impl Generalize for syn::GenericParam {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        use syn::GenericParam::*;

        match (self, other) {
            (Lifetime(x1), Lifetime(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Lifetime),
            (Type(x1), Type(x2)) => x1.generalize(x2, params1, params2, substitutions).map(Type),
            (Const(x1), Const(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Const),
            _ => None,
        }
    }
}

impl Generalize for syn::TypeParam {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        let attrs = self
            .attrs
            .generalize(&other.attrs, params1, params2, substitutions)?;
        let bounds = self
            .bounds
            .generalize(&other.bounds, params1, params2, substitutions)?;

        let default = self
            .default
            .generalize(&other.default, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            bounds,
            default,
            ..self.clone()
        })
    }
}

impl Generalize for syn::ConstParam {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        let attrs = self
            .attrs
            .generalize(&other.attrs, params1, params2, substitutions)?;
        let ty = self
            .ty
            .generalize(&other.ty, params1, params2, substitutions)?;

        let default = self
            .default
            .generalize(&other.default, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            ty,
            default,
            ..self.clone()
        })
    }
}

impl Generalize for syn::WhereClause {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        let predicates =
            self.predicates
                .generalize(&other.predicates, params1, params2, substitutions)?;

        Some(Self {
            predicates,
            ..self.clone()
        })
    }
}

impl Generalize for syn::WherePredicate {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        use syn::WherePredicate::*;

        match (self, other) {
            (Lifetime(x1), Lifetime(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Lifetime),
            (Type(x1), Type(x2)) => x1.generalize(x2, params1, params2, substitutions).map(Type),
            _ => None,
        }
    }
}

impl Generalize for syn::PredicateLifetime {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        let lifetime =
            self.lifetime
                .generalize(&other.lifetime, params1, params2, substitutions)?;
        let bounds = self
            .bounds
            .generalize(&other.bounds, params1, params2, substitutions)?;

        Some(Self {
            lifetime,
            bounds,
            ..self.clone()
        })
    }
}

impl Generalize for syn::PredicateType {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        let lifetimes =
            self.lifetimes
                .generalize(&other.lifetimes, params1, params2, substitutions)?;
        let bounded_ty =
            self.bounded_ty
                .generalize(&other.bounded_ty, params1, params2, substitutions)?;
        let bounds = self
            .bounds
            .generalize(&other.bounds, params1, params2, substitutions)?;

        Some(Self {
            lifetimes,
            bounded_ty,
            bounds,
            ..self.clone()
        })
    }
}

impl Generalize for syn::Generics {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        if self.lt_token.is_some() != other.lt_token.is_some()
            || self.gt_token.is_some() != other.gt_token.is_some()
        {
            return None;
        }

        let params = self
            .params
            .generalize(&other.params, params1, params2, substitutions)?;
        let where_clause =
            self.where_clause
                .generalize(&other.where_clause, params1, params2, substitutions)?;

        Some(Self {
            params,
            where_clause,
            ..self.clone()
        })
    }
}
