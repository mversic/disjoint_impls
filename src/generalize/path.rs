use super::*;

impl Generalize for syn::Path {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        if self.leading_colon != other.leading_colon {
            return None;
        }

        let segments =
            self.segments
                .generalize(&other.segments, params1, params2, substitutions)?;

        Some(Self {
            segments,
            ..self.clone()
        })
    }
}

impl Generalize for syn::PathSegment {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        if self.ident != other.ident {
            return None;
        }

        use syn::PathArguments;
        let arguments = match (&self.arguments, &other.arguments) {
            (PathArguments::None, PathArguments::None) => PathArguments::None,
            (PathArguments::Parenthesized(x1), PathArguments::Parenthesized(x2)) => {
                let inputs = x1
                    .inputs
                    .generalize(&x2.inputs, params1, params2, substitutions)?;
                let output = x1
                    .output
                    .generalize(&x2.output, params1, params2, substitutions)?;

                PathArguments::Parenthesized(syn::ParenthesizedGenericArguments {
                    inputs,
                    output,
                    ..x1.clone()
                })
            }
            (PathArguments::AngleBracketed(x1), PathArguments::AngleBracketed(x2)) => {
                let args = x1.generalize(x2, params1, params2, substitutions)?;
                PathArguments::AngleBracketed(args)
            }
            _ => return None,
        };

        Some(Self {
            ident: self.ident.clone(),
            arguments,
        })
    }
}

impl Generalize for syn::QSelf {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        if self.position != other.position || self.as_token != other.as_token {
            return None;
        }

        let ty = self
            .ty
            .generalize(&other.ty, params1, params2, substitutions)?;
        Some(Self { ty, ..self.clone() })
    }
}

impl Generalize for syn::AngleBracketedGenericArguments {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        if self.colon2_token != other.colon2_token {
            return None;
        }

        let args = self
            .args
            .generalize(&other.args, params1, params2, substitutions)?;

        Some(Self {
            args,
            ..self.clone()
        })
    }
}

impl Generalize for syn::GenericArgument {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        use syn::GenericArgument::*;

        match (self, other) {
            (Type(syn::Type::Path(p1)), Const(x2)) => {
                if let Some(ident) = p1.path.get_ident() {
                    substitutions
                        .insert_expr(ident, x2, params1, params2)
                        .map(Const)
                } else {
                    None
                }
            }
            (Const(x1), Type(syn::Type::Path(p2))) => {
                if let Some(ident) = p2.path.get_ident() {
                    substitutions
                        .insert_expr(x1, ident, params1, params2)
                        .map(Const)
                } else {
                    None
                }
            }
            (Type(p1), Type(p2)) => {
                if let (syn::Type::Path(p1), syn::Type::Path(p2)) = (p1, p2)
                    && let Some(ident1) = p1.path.get_ident()
                    && let Some(ident2) = p2.path.get_ident()
                    && params1
                        .get(ident1)
                        .is_some_and(|param| matches!(param, GenericParam::Const(_)))
                    && params2
                        .get(ident2)
                        .is_some_and(|param| matches!(param, GenericParam::Const(_)))
                {
                    substitutions
                        .insert_expr(ident1, ident2, params1, params2)
                        .map(Const)
                } else {
                    p1.generalize(p2, params1, params2, substitutions).map(Type)
                }
            }
            (Lifetime(x1), Lifetime(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Lifetime),
            (Const(x1), Const(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Const),
            (AssocType(x1), AssocType(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(AssocType),
            (AssocConst(x1), AssocConst(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(AssocConst),
            (Constraint(_), _) | (_, Constraint(_)) => unreachable!(),
            _ => None,
        }
    }
}

impl Generalize for syn::AssocType {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        if self.ident != other.ident {
            return None;
        }

        let generics =
            self.generics
                .generalize(&other.generics, params1, params2, substitutions)?;
        let ty = self
            .ty
            .generalize(&other.ty, params1, params2, substitutions)?;

        Some(Self {
            generics,
            ty,
            ..self.clone()
        })
    }
}

impl Generalize for syn::AssocConst {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        if self.ident != other.ident {
            return None;
        }

        let generics =
            self.generics
                .generalize(&other.generics, params1, params2, substitutions)?;
        let value = self
            .value
            .generalize(&other.value, params1, params2, substitutions)?;

        Some(Self {
            generics,
            value,
            ..self.clone()
        })
    }
}

#[cfg(test)]
mod tests {
    use indexmap::indexmap;

    use super::*;

    #[test]
    fn substitute_path() {
        let p = indexmap! {
            format_ident!("U") => GenericParam::Type(Sizedness::Sized, IndexSet::new()),
            format_ident!("V") => GenericParam::Type(Sizedness::Sized, IndexSet::new()),
        };

        let l1: syn::Path = parse_quote!(Kita<U, V>);
        let l2: syn::Path = parse_quote!(Kita<U, V>);

        let mut subs1 = Generalizations::default();
        let mut subs2 = Generalizations::default();

        let path1 = l1.generalize(&l2, &p, &p, &mut subs1).unwrap();
        let path2 = l2.generalize(&l1, &p, &p, &mut subs2).unwrap();

        let expected_path = parse_quote!(Kita<_TŠČ0, _TŠČ1>);

        assert_eq!(path1, expected_path);
        assert_eq!(path2, expected_path);

        assert!(subs1.lifetime_generalizations.is_empty(),);
        assert!(subs2.lifetime_generalizations.is_empty(),);

        let r1: syn::Type = parse_quote!(U);
        let r2: syn::Type = parse_quote!(V);

        assert_eq!(
            subs1.type_generalizations,
            indexmap! {
                (&r1, &r1) => (Sizedness::Sized, IndexSet::new()),
                (&r2, &r2) => (Sizedness::Sized, IndexSet::new()),
            }
        );
        assert_eq!(
            subs2.type_generalizations,
            indexmap! {
                (&r2, &r2) => (Sizedness::Sized, IndexSet::new()),
                (&r1, &r1) => (Sizedness::Sized, IndexSet::new()),
            }
        );

        assert!(subs1.expr_generalizations.is_empty());
        assert!(subs2.expr_generalizations.is_empty());
    }
}
