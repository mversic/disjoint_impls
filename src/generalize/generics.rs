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
    fn generalize(
        &self,
        _: &Self,
        _: &Params,
        _: &Params,
        _: &mut Generalizations<'_>,
    ) -> Option<Self> {
        unimplemented!("KITA")
    }
}
