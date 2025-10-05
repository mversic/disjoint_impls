use super::*;

impl Generalize for syn::Stmt {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        use syn::Stmt::*;

        match (self, other) {
            (Local(x1), Local(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Local),
            (Item(_), _) | (_, Item(_)) => unimplemented!(),
            (Expr(x1, semi_token1), Expr(x2, semi_token2)) if semi_token1 == semi_token2 => x1
                .generalize(x2, params1, params2, substitutions)
                .map(|generalized| Expr(generalized, *semi_token1)),
            (Macro(x1), Macro(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Macro),
            _ => None,
        }
    }
}

impl Generalize for syn::Local {
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
        let prev_expected_type = match (&self.pat, &other.pat) {
            (syn::Pat::Type(ty), _) | (_, syn::Pat::Type(ty)) => substitutions
                .curr_expr_expected_type
                .replace((*ty.ty).clone()),
            _ => None,
        };
        let init = self
            .init
            .generalize(&other.init, params1, params2, substitutions)?;
        substitutions.curr_expr_expected_type = prev_expected_type;

        Some(Self {
            attrs,
            pat,
            init,
            ..self.clone()
        })
    }
}

impl Generalize for syn::StmtMacro {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        if self.semi_token != other.semi_token {
            return None;
        }

        let attrs = self
            .attrs
            .generalize(&other.attrs, params1, params2, substitutions)?;
        let mac = self
            .mac
            .generalize(&other.mac, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            mac,
            ..self.clone()
        })
    }
}

impl Generalize for syn::LocalInit {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        let expr = self
            .expr
            .generalize(&other.expr, params1, params2, substitutions)?;
        let diverge = self
            .diverge
            .generalize(&other.diverge, params1, params2, substitutions)?;

        Some(Self {
            expr,
            diverge,
            ..self.clone()
        })
    }
}

impl Generalize for syn::Block {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        let stmts = self
            .stmts
            .generalize(&other.stmts, params1, params2, substitutions)?;

        Some(Self {
            stmts,
            ..self.clone()
        })
    }
}
