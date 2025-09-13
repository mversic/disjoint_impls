use super::*;

impl Generalize for syn::Pat {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        use syn::Pat::*;

        match (self, other) {
            (Const(x1), Const(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Const),
            (Ident(x1), Ident(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Ident),
            (Lit(x1), Lit(x2)) => x1.generalize(x2, params1, params2, substitutions).map(Lit),
            (Macro(x1), Macro(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Macro),
            (Or(x1), Or(x2)) => x1.generalize(x2, params1, params2, substitutions).map(Or),
            (Paren(x1), Paren(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Paren),
            (Path(x1), Path(x2)) => x1.generalize(x2, params1, params2, substitutions).map(Path),
            (Range(x1), Range(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Range),
            (Reference(x1), Reference(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Reference),
            (Rest(x1), Rest(x2)) => x1.generalize(x2, params1, params2, substitutions).map(Rest),
            (Slice(x1), Slice(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Slice),
            (Struct(x1), Struct(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Struct),
            (Tuple(x1), Tuple(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Tuple),
            (TupleStruct(x1), TupleStruct(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(TupleStruct),
            (Type(x1), Type(x2)) => x1.generalize(x2, params1, params2, substitutions).map(Type),
            (x1, Wild(_)) => x1.generalize(x1, params1, params1, substitutions),
            (Wild(_), x2) => x2.generalize(x2, params2, params2, substitutions),
            _ => None,
        }
    }
}

impl Generalize for syn::PatIdent {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        if self.by_ref != other.by_ref
            || self.mutability != other.mutability
            || self.ident != other.ident
        {
            return None;
        }
        let attrs = self
            .attrs
            .generalize(&other.attrs, params1, params2, substitutions)?;
        let subpat = self
            .subpat
            .generalize(&other.subpat, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            subpat,
            ..self.clone()
        })
    }
}

impl Generalize for syn::PatOr {
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
        let cases = self
            .cases
            .generalize(&other.cases, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            cases,
            ..self.clone()
        })
    }
}

impl Generalize for syn::PatParen {
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
        let pat = self
            .pat
            .generalize(&other.pat, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            pat,
            ..self.clone()
        })
    }
}

impl Generalize for syn::PatReference {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        if self.mutability != other.mutability {
            return None;
        }
        let attrs = self
            .attrs
            .generalize(&other.attrs, params1, params2, substitutions)?;
        let pat = self
            .pat
            .generalize(&other.pat, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            pat,
            ..self.clone()
        })
    }
}

impl Generalize for syn::PatRest {
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
        Some(Self {
            attrs,
            ..self.clone()
        })
    }
}

impl Generalize for syn::PatStruct {
    fn generalize(
        &self,
        _: &Self,
        _: &Params,
        _: &Params,
        _: &mut Generalizations<'_>,
    ) -> Option<Self> {
        unimplemented!()
    }
}

impl Generalize for syn::PatSlice {
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
        let elems = self
            .elems
            .generalize(&other.elems, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            elems,
            ..self.clone()
        })
    }
}

impl Generalize for syn::PatTuple {
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
        let elems = self
            .elems
            .generalize(&other.elems, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            elems,
            ..self.clone()
        })
    }
}

impl Generalize for syn::PatTupleStruct {
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
        let path = self
            .path
            .generalize(&other.path, params1, params2, substitutions)?;
        let elems = self
            .elems
            .generalize(&other.elems, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            path,
            elems,
            ..self.clone()
        })
    }
}

impl Generalize for syn::PatType {
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

        Some(Self {
            attrs,
            ty,
            ..self.clone()
        })
    }
}

impl Generalize for syn::PatWild {
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

        Some(Self {
            attrs,
            ..self.clone()
        })
    }
}
