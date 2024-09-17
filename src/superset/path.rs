use super::*;

impl Superset for syn::Path {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.segments.len() != other.segments.len() {
            return None;
        }

        if let Some(ident) = matches_param_ident(self) {
            if Some(ident) == matches_param_ident(other) {
                return Some(Substitutions::identity(ident));
            }
        }

        let mut substitutions = Substitutions::default();
        for (x1, x2) in zip(&self.segments, &other.segments) {
            if x1.ident != x2.ident {
                return None;
            }

            use syn::PathArguments;
            match (&x1.arguments, &x2.arguments) {
                (PathArguments::None, PathArguments::None) => {}
                (PathArguments::Parenthesized(x1), PathArguments::Parenthesized(x2)) => {
                    if x1.inputs.len() != x2.inputs.len() {
                        return None;
                    }

                    substitutions = zip(&x1.inputs, &x2.inputs)
                        .try_fold(substitutions, |acc, (x1, x2)| {
                            acc.merge(x1.is_superset(x2)?)
                        })?;

                    use syn::ReturnType::*;
                    match (&x1.output, &x2.output) {
                        (Type(_, x1), Type(_, x2)) => {
                            substitutions = substitutions.merge(x1.is_superset(x2)?)?;
                        }
                        (Default, Default) => {}
                        _ => return None,
                    }
                }
                (PathArguments::AngleBracketed(x1), PathArguments::AngleBracketed(x2)) => {
                    substitutions = substitutions.merge(x1.is_superset(x2)?)?;
                }
                _ => return None,
            }
        }

        Some(substitutions)
    }
}

impl Substitute for syn::Path {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        if self.segments.is_empty() {
            return vec![self.clone()];
        }

        self.segments
            .iter()
            .map(|segment| {
                use syn::PathArguments;

                match &segment.arguments {
                    PathArguments::None => vec![PathArguments::None],
                    PathArguments::Parenthesized(x1) => {
                        let output = if let syn::ReturnType::Type(r_arrow, output) = &x1.output {
                            output
                                .substitute(substitutions)
                                .into_iter()
                                .map(|output| syn::ReturnType::Type(*r_arrow, Box::new(output)))
                                .collect()
                        } else {
                            vec![syn::ReturnType::Default]
                        };

                        if x1.inputs.is_empty() {
                            output
                                .into_iter()
                                .map(|output| syn::ParenthesizedGenericArguments {
                                    paren_token: x1.paren_token,
                                    inputs: x1.inputs.clone(),
                                    output,
                                })
                                .map(PathArguments::Parenthesized)
                                .collect()
                        } else {
                            let inputs = x1
                                .inputs
                                .iter()
                                .map(|input| input.substitute(substitutions))
                                .multi_cartesian_product();

                            inputs
                                .cartesian_product(output)
                                .map(|(inputs, output)| syn::ParenthesizedGenericArguments {
                                    paren_token: x1.paren_token,
                                    inputs: inputs.into_iter().collect(),
                                    output,
                                })
                                .map(PathArguments::Parenthesized)
                                .collect()
                        }
                    }
                    PathArguments::AngleBracketed(x1) => x1
                        .substitute(substitutions)
                        .into_iter()
                        .map(PathArguments::AngleBracketed)
                        .collect(),
                }
                .into_iter()
                .map(|arguments| syn::PathSegment {
                    ident: segment.ident.clone(),
                    arguments,
                })
            })
            .multi_cartesian_product()
            .map(|segments| syn::Path {
                leading_colon: self.leading_colon,
                segments: segments.into_iter().collect(),
            })
            .collect()
    }
}

impl Superset for syn::QSelf {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        let substitutions = self.ty.is_superset(&other.ty)?;

        if self.position != other.position || self.as_token != other.as_token {
            return None;
        }

        if substitutions.is_eq() {
            // NOTE: It's not possible to tell whether 2 associated types overlap or not
            // Is <Option<T> as Deref>::Target a superset of <Vec<T> as Deref>::Target?
            return Some(substitutions);
        }

        None
    }
}

impl Substitute for syn::QSelf {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.ty
            .substitute(substitutions)
            .into_iter()
            .map(|ty| Self {
                ty: Box::new(ty),
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::AngleBracketedGenericArguments {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.args.len() != other.args.len() {
            return None;
        }

        use syn::GenericArgument::*;
        zip(&self.args, &other.args).try_fold(Substitutions::default(), |acc, (x1, x2)| {
            match (x1, x2) {
                (Type(syn::Type::Path(x1)), Const(x2)) => {
                    if let Some(ident) = matches_param_ident(&x1.path) {
                        return acc.merge(Substitutions::new(ident, x2));
                    }

                    None
                }

                (Lifetime(x1), Lifetime(x2)) => x1.is_superset(x2),
                (Type(x1), Type(x2)) => acc.merge(x1.is_superset(x2)?),
                (Const(x1), Const(x2)) => acc.merge(x1.is_superset(x2)?),
                (AssocType(x1), AssocType(x2)) => x1.is_superset(x2),
                (AssocConst(x1), AssocConst(x2)) => x1.is_superset(x2),
                (Constraint(x1), Constraint(x2)) => x1.is_superset(x2),

                (x1, x2) => (x1 == x2).then_some(Substitutions::default()),
            }
        })
    }
}

impl Substitute for syn::AngleBracketedGenericArguments {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        use syn::GenericArgument::*;

        if self.args.is_empty() {
            return vec![self.clone()];
        }

        self.args
            .iter()
            .map(|arg| match arg {
                Lifetime(x1) => x1
                    .substitute(substitutions)
                    .into_iter()
                    .map(Lifetime)
                    .collect(),
                Type(x1) => x1.substitute(substitutions).into_iter().map(Type).collect(),
                Const(x1) => x1
                    .substitute(substitutions)
                    .into_iter()
                    .map(Const)
                    .collect(),
                AssocType(x1) => x1
                    .substitute(substitutions)
                    .into_iter()
                    .map(AssocType)
                    .collect(),
                AssocConst(x1) => x1
                    .substitute(substitutions)
                    .into_iter()
                    .map(AssocConst)
                    .collect(),
                Constraint(x1) => x1
                    .substitute(substitutions)
                    .into_iter()
                    .map(Constraint)
                    .collect(),
                _ => vec![arg.clone()],
            })
            .multi_cartesian_product()
            .map(|args| Self {
                args: args.into_iter().collect(),
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::AssocType {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.ident != other.ident {
            return None;
        }

        match (&self.generics, &other.generics) {
            (Some(x1), Some(x2)) => x1.is_superset(x2)?,
            (None, None) => Substitutions::default(),
            _ => return None,
        }
        .merge(self.ty.is_superset(&other.ty)?)
    }
}

impl Substitute for syn::AssocType {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        let ty = self.ty.substitute(substitutions);

        if let Some(generics) = &self.generics {
            generics
                .substitute(substitutions)
                .into_iter()
                .cartesian_product(ty)
                .map(|(generics, ty)| Self {
                    ident: self.ident.clone(),
                    generics: Some(generics),
                    eq_token: self.eq_token,
                    ty,
                })
                .collect()
        } else {
            ty.into_iter()
                .map(|ty| Self {
                    ident: self.ident.clone(),
                    generics: None,
                    eq_token: self.eq_token,
                    ty,
                })
                .collect()
        }
    }
}

impl Superset for syn::AssocConst {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.ident != other.ident {
            return None;
        }

        match (&self.generics, &other.generics) {
            (Some(x1), Some(x2)) => x1.is_superset(x2)?,
            (None, None) => Substitutions::default(),
            _ => return None,
        }
        .merge(self.value.is_superset(&other.value)?)
    }
}

impl Substitute for syn::AssocConst {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        let value = self.value.substitute(substitutions);

        if let Some(generics) = &self.generics {
            generics
                .substitute(substitutions)
                .into_iter()
                .cartesian_product(value)
                .map(|(generics, value)| Self {
                    ident: self.ident.clone(),
                    generics: Some(generics),
                    eq_token: self.eq_token,
                    value,
                })
                .collect()
        } else {
            value
                .into_iter()
                .map(|value| Self {
                    ident: self.ident.clone(),
                    generics: None,
                    eq_token: self.eq_token,
                    value,
                })
                .collect()
        }
    }
}

impl Superset for syn::Constraint {
    fn is_superset<'a>(&'a self, _: &'a Self) -> Option<Substitutions> {
        unimplemented!()
    }
}

impl Substitute for syn::Constraint {
    fn substitute(&self, _: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>) -> Vec<Self> {
        unimplemented!()
    }
}

#[cfg(test)]
mod tests {
    use syn::parse_quote;

    use super::*;

    #[test]
    fn identity_path_superset() {
        let r: syn::Ident = syn::parse_quote!(_ŠČ);
        let substitutions = Substitutions::identity(&r);

        let p1: syn::Path = syn::parse_quote!(_ŠČ);
        assert_eq!(p1.is_superset(&p1), Some(substitutions.clone()));

        let p2: syn::Path = syn::parse_quote!(Vec<[_ŠČ; 2]>);
        assert_eq!(p2.is_superset(&p2), Some(substitutions.clone()));

        let p3: syn::Path = syn::parse_quote!(Vec<[_ŠČ; 2]>::Target);
        assert_eq!(p3.is_superset(&p3), Some(substitutions.clone()));
    }

    #[test]
    fn concrete_path_superset() {
        let substitutions = Substitutions::default();

        let p1: syn::Path = syn::parse_quote!(i32);
        assert_eq!(p1.is_superset(&p1), Some(substitutions.clone()));

        let p2: syn::Path = syn::parse_quote!(Vec<[i32; 2]>);
        assert_eq!(p2.is_superset(&p2), Some(substitutions.clone()));

        let p3: syn::Path = syn::parse_quote!(Vec<[i32; 2]>::Target);
        assert_eq!(p3.is_superset(&p3), Some(substitutions.clone()));
    }

    #[test]
    fn identity_path_substitute() {
        let substituted = (
            Bounded(parse_quote!(Option<Vec<_ŠČ>>)),
            TraitBound(parse_quote!(Dispatch)),
        );

        let p1: syn::Path = syn::parse_quote!(_ŠČ);
        assert!(p1
            .is_superset(&p1)
            .unwrap()
            .substitute(&substituted)
            .eq([substituted.clone()]));

        let p2: syn::Path = syn::parse_quote!(Vec<[_ŠČ; 2]>);
        assert!(p2
            .is_superset(&p2)
            .unwrap()
            .substitute(&substituted)
            .eq([substituted.clone()]));

        let p3: syn::Path = syn::parse_quote!(Vec<[_ŠČ; 2]>::Target);
        assert!(p3
            .is_superset(&p3)
            .unwrap()
            .substitute(&substituted)
            .eq([substituted.clone()]));
    }

    #[test]
    fn concrete_path_substitute() {
        let substituted = (
            Bounded(parse_quote!(Option<Vec<i32>>)),
            TraitBound(parse_quote!(Dispatch)),
        );

        let p1: syn::Path = syn::parse_quote!(i32);

        assert!(p1
            .is_superset(&p1)
            .unwrap()
            .substitute(&substituted)
            .eq([substituted.clone()]));

        let p2: syn::Path = syn::parse_quote!(Vec<[i32; 2]>);
        assert!(p2
            .is_superset(&p2)
            .unwrap()
            .substitute(&substituted)
            .eq([substituted.clone()]));

        let p3: syn::Path = syn::parse_quote!(Vec<[i32; 2]>::Target);
        assert!(p3
            .is_superset(&p3)
            .unwrap()
            .substitute(&substituted)
            .eq([substituted.clone()]));
    }

    #[test]
    fn identity_path_substitute_trait_with_generic_param() {
        let substituted = (
            Bounded(parse_quote!(_ŠČ1)),
            TraitBound(parse_quote!(Dispatch<_ŠČ1>)),
        );

        let p1: syn::Path = syn::parse_quote!(_ŠČ1);

        assert!(p1
            .is_superset(&p1)
            .unwrap()
            .substitute(&substituted)
            .eq([substituted.clone()]));
    }

    #[test]
    fn identity_path_substitute_trait_with_concrete_param() {
        let substituted = (
            Bounded(parse_quote!(_ŠČ1)),
            TraitBound(parse_quote!(Dispatch<()>)),
        );

        let p1: syn::Path = syn::parse_quote!(_ŠČ1);

        assert!(p1
            .is_superset(&p1)
            .unwrap()
            .substitute(&substituted)
            .eq([substituted.clone()]));
    }
}
