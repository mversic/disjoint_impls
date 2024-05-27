use super::*;

impl Superset for syn::Pat {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        use syn::Pat::*;

        match (self, other) {
            (Const(x1), Const(x2)) => x1.is_superset(x2),
            (Ident(x1), Ident(x2)) => x1.is_superset(x2),
            (Lit(x1), Lit(x2)) => x1.is_superset(x2),
            (Macro(x1), Macro(x2)) => x1.is_superset(x2),
            (Or(x1), Or(x2)) => x1.is_superset(x2),

            (Paren(x1), x2) => x1.pat.is_superset(x2),
            (x1, Paren(x2)) => x1.is_superset(&x2.pat),

            (Path(x1), Path(x2)) => x1.is_superset(x2),
            (Range(x1), Range(x2)) => x1.is_superset(x2),
            (Reference(x1), Reference(x2)) => x1.is_superset(x2),
            (Rest(x1), Rest(x2)) => x1.is_superset(x2),
            (Slice(x1), Slice(x2)) => x1.is_superset(x2),
            (Struct(x1), Struct(x2)) => x1.is_superset(x2),
            (Tuple(x1), Tuple(x2)) => x1.is_superset(x2),
            (TupleStruct(x1), TupleStruct(x2)) => x1.is_superset(x2),
            (Type(x1), Type(x2)) => x1.is_superset(x2),
            (Wild(_), _) | (_, Wild(_)) => Some(Substitutions::default()),

            (x1, x2) => (x1 == x2).then_some(Substitutions::default()),
        }
    }
}

impl Substitute for syn::Pat {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        use syn::Pat::*;

        match self {
            Const(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Const)
                .collect(),
            Ident(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Ident)
                .collect(),
            Lit(x1) => x1.substitute(substitutions).into_iter().map(Lit).collect(),
            Macro(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Macro)
                .collect(),
            Or(x1) => x1.substitute(substitutions).into_iter().map(Or).collect(),
            Paren(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Paren)
                .collect(),
            Path(x1) => x1.substitute(substitutions).into_iter().map(Path).collect(),
            Range(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Range)
                .collect(),
            Reference(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Reference)
                .collect(),
            Rest(x1) => x1.substitute(substitutions).into_iter().map(Rest).collect(),
            Slice(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Slice)
                .collect(),
            Struct(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Struct)
                .collect(),
            Tuple(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Tuple)
                .collect(),
            TupleStruct(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(TupleStruct)
                .collect(),
            Type(x1) => x1.substitute(substitutions).into_iter().map(Type).collect(),
            _ => vec![self.clone()],
        }
    }
}

impl Superset for syn::PatIdent {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.by_ref != other.by_ref
            || self.mutability != other.mutability
            || self.ident != other.ident
        {
            return None;
        }

        self.attrs
            .is_superset(&other.attrs)?
            .merge(match (&self.subpat, &other.subpat) {
                (Some((_, x1)), Some((_, x2))) => x1.is_superset(x2)?,
                (None, None) => Substitutions::default(),
                _ => return None,
            })
    }
}

impl Substitute for syn::PatIdent {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.subpat
            .as_ref()
            .map_or(vec![None], |(at, pat)| {
                pat.substitute(substitutions)
                    .into_iter()
                    .map(Box::new)
                    .map(|pat| (*at, pat))
                    .map(Some)
                    .collect()
            })
            .into_iter()
            .map(|subpat| Self {
                subpat,
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::PatOr {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs.is_superset(&other.attrs)?.merge(
            zip(&self.cases, &other.cases)
                .try_fold(Substitutions::default(), |acc, (x1, x2)| {
                    acc.merge(x1.is_superset(x2)?)
                })?,
        )
    }
}

impl Substitute for syn::PatOr {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.cases
            .iter()
            .map(|case| case.substitute(substitutions))
            .multi_cartesian_product()
            .map(|cases| Self {
                cases: cases.into_iter().collect(),
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::PatParen {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.pat.is_superset(&other.pat)?)
    }
}

impl Substitute for syn::PatParen {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.pat
            .substitute(substitutions)
            .into_iter()
            .map(|pat| Self {
                pat: Box::new(pat),
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::PatReference {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.mutability != other.mutability {
            return None;
        }

        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.pat.is_superset(&other.pat)?)
    }
}

impl Substitute for syn::PatReference {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.pat
            .substitute(substitutions)
            .into_iter()
            .map(|pat| Self {
                pat: Box::new(pat),
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::PatRest {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs.is_superset(&other.attrs)
    }
}

impl Substitute for syn::PatRest {
    fn substitute(&self, _: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>) -> Vec<Self> {
        vec![self.clone()]
    }
}

impl Superset for syn::PatStruct {
    fn is_superset<'a>(&'a self, _: &'a Self) -> Option<Substitutions> {
        unimplemented!()
    }
}

impl Substitute for syn::PatStruct {
    fn substitute(&self, _: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>) -> Vec<Self> {
        unimplemented!()
    }
}

impl Superset for syn::PatSlice {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs.is_superset(&other.attrs)?.merge(
            zip(&self.elems, &other.elems)
                .try_fold(Substitutions::default(), |acc, (x1, x2)| {
                    acc.merge(x1.is_superset(x2)?)
                })?,
        )
    }
}

impl Substitute for syn::PatSlice {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.elems
            .iter()
            .map(|elem| elem.substitute(substitutions))
            .multi_cartesian_product()
            .map(|elems| Self {
                elems: elems.into_iter().collect(),
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::PatTuple {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs.is_superset(&other.attrs)?.merge(
            zip(&self.elems, &other.elems)
                .try_fold(Substitutions::default(), |acc, (x1, x2)| {
                    acc.merge(x1.is_superset(x2)?)
                })?,
        )
    }
}

impl Substitute for syn::PatTuple {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.elems
            .iter()
            .map(|elem| elem.substitute(substitutions))
            .multi_cartesian_product()
            .map(|elems| Self {
                elems: elems.into_iter().collect(),
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::PatTupleStruct {
    fn is_superset<'a>(&'a self, _: &'a Self) -> Option<Substitutions> {
        unimplemented!()
    }
}

impl Substitute for syn::PatTupleStruct {
    fn substitute(&self, _: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>) -> Vec<Self> {
        unimplemented!()
    }
}

impl Superset for syn::PatType {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.pat.is_superset(&other.pat)?)?
            .merge(self.ty.is_superset(&other.ty)?)
    }
}

impl Substitute for syn::PatType {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.pat
            .substitute(substitutions)
            .into_iter()
            .cartesian_product(self.ty.substitute(substitutions))
            .map(|(pat, ty)| Self {
                pat: Box::new(pat),
                ty: Box::new(ty),
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::PatWild {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs.is_superset(&other.attrs)
    }
}

impl Substitute for syn::PatWild {
    fn substitute(&self, _: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>) -> Vec<Self> {
        vec![self.clone()]
    }
}
