use super::*;

impl Superset for syn::Stmt {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions<'a>> {
        use syn::Stmt::*;

        match (self, other) {
            (Local(x1), Local(x2)) => x1.is_superset(x2),
            (Item(_), _) | (_, Item(_)) => unimplemented!(),
            (Expr(x1, _), Expr(x2, _)) => x1.is_superset(x2),
            (Macro(x1), Macro(x2)) => x1.is_superset(x2),
            _ => None,
        }
    }
}

impl Substitute for syn::Stmt {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        use syn::Stmt::*;

        match self {
            Local(x) => x.substitute(substitutions).into_iter().map(Local).collect(),
            Item(_) => unimplemented!(),
            Expr(x, semicolon) => x
                .substitute(substitutions)
                .into_iter()
                .map(|e| Expr(e, *semicolon))
                .collect(),
            Macro(x) => x.substitute(substitutions).into_iter().map(Macro).collect(),
        }
    }
}

impl Superset for syn::Local {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions<'a>> {
        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.pat.is_superset(&other.pat)?)?
            .merge(self.init.is_superset(&other.init)?)
    }
}

impl Substitute for syn::Local {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.pat
            .substitute(substitutions)
            .into_iter()
            .cartesian_product(self.init.substitute(substitutions))
            .map(|(pat, init)| Self {
                pat,
                init,
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::StmtMacro {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions<'a>> {
        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.mac.is_superset(&other.mac)?)
    }
}

impl Substitute for syn::StmtMacro {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.mac
            .substitute(substitutions)
            .into_iter()
            .map(|mac| Self {
                mac,
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::LocalInit {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions<'a>> {
        self.expr
            .is_superset(&other.expr)?
            .merge(match (&self.diverge, &other.diverge) {
                (Some((_, x1)), Some((_, x2))) => x1.is_superset(x2)?,
                (None, None) => Substitutions::default(),
                _ => return None,
            })
    }
}

impl Substitute for syn::LocalInit {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.expr
            .substitute(substitutions)
            .into_iter()
            .cartesian_product(self.diverge.as_ref().map_or(
                vec![None],
                |(else_token, else_branch)| {
                    else_branch
                        .substitute(substitutions)
                        .into_iter()
                        .map(|else_branch| Some((*else_token, Box::new(else_branch))))
                        .collect()
                },
            ))
            .map(|(expr, diverge)| Self {
                expr: Box::new(expr),
                diverge,
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::Block {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions<'a>> {
        if self.stmts.len() != other.stmts.len() {
            return None;
        }

        zip(&self.stmts, &other.stmts).try_fold(Substitutions::default(), |acc, (x1, x2)| {
            acc.merge(x1.is_superset(x2)?)
        })
    }
}

impl Substitute for syn::Block {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        if self.stmts.is_empty() {
            return vec![self.clone()];
        }

        self.stmts
            .iter()
            .map(|stmt| stmt.substitute(substitutions))
            .multi_cartesian_product()
            .map(|stmts| Self {
                stmts,
                ..self.clone()
            })
            .collect()
    }
}
