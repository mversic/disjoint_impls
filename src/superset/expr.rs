use super::*;

impl Superset for syn::Expr {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        use syn::Expr::*;

        match (self, other) {
            (Array(x1), Array(x2)) => x1.is_superset(x2),
            (Assign(x1), Assign(x2)) => x1.is_superset(x2),
            (Async(x1), Async(x2)) => x1.is_superset(x2),
            (Await(x1), Await(x2)) => x1.is_superset(x2),
            (Binary(x1), Binary(x2)) => x1.is_superset(x2),
            (Block(x1), Block(x2)) => x1.is_superset(x2),
            (Break(x1), Break(x2)) => x1.is_superset(x2),
            (Call(x1), Call(x2)) => x1.is_superset(x2),
            (Cast(x1), Cast(x2)) => x1.is_superset(x2),
            (Closure(x1), Closure(x2)) => x1.is_superset(x2),
            (Const(x1), Const(x2)) => x1.is_superset(x2),
            (Continue(x1), Continue(x2)) => x1.is_superset(x2),
            (Field(x1), Field(x2)) => x1.is_superset(x2),
            (ForLoop(x1), ForLoop(x2)) => x1.is_superset(x2),

            (Group(x1), x2) => x1.expr.is_superset(x2),
            (x1, Group(x2)) => x1.is_superset(&x2.expr),

            (If(x1), If(x2)) => x1.is_superset(x2),
            (Index(x1), Index(x2)) => x1.is_superset(x2),
            (Infer(x1), Infer(x2)) => x1.is_superset(x2),
            (Let(x1), Let(x2)) => x1.is_superset(x2),
            (Lit(x1), Lit(x2)) => x1.is_superset(x2),
            (Loop(x1), Loop(x2)) => x1.is_superset(x2),
            (Macro(x1), Macro(x2)) => x1.is_superset(x2),
            (Match(x1), Match(x2)) => x1.is_superset(x2),
            (MethodCall(x1), MethodCall(x2)) => x1.is_superset(x2),

            // NOTE: 2 * (2 + 2) != 2 * 2 + 2
            (Paren(x1), Paren(x2)) => x1.is_superset(x2),

            (Path(x1), x2) => {
                if let Some(ident) = matches_param_ident(&x1.path) {
                    if let syn::Expr::Path(x2) = x2 {
                        if Some(ident) == matches_param_ident(&x2.path) {
                            return Some(Substitutions::identity(ident));
                        }
                    }

                    return Some(Substitutions::new(ident, x2));
                }

                if let syn::Expr::Path(x2) = x2 {
                    return x1.is_superset(x2);
                }

                None
            }
            (Range(x1), Range(x2)) => x1.is_superset(x2),
            (Reference(x1), Reference(x2)) => x1.is_superset(x2),
            (Repeat(x1), Repeat(x2)) => x1.is_superset(x2),
            (Return(x1), Return(x2)) => x1.is_superset(x2),
            (Struct(x1), Struct(x2)) => x1.is_superset(x2),
            (Try(x1), Try(x2)) => x1.is_superset(x2),
            (TryBlock(x1), TryBlock(x2)) => x1.is_superset(x2),
            (Tuple(x1), Tuple(x2)) => x1.is_superset(x2),
            (Unary(x1), Unary(x2)) => x1.is_superset(x2),
            (Unsafe(x1), Unsafe(x2)) => x1.is_superset(x2),
            (While(x1), While(x2)) => x1.is_superset(x2),
            (Yield(x1), Yield(x2)) => x1.is_superset(x2),

            (x1, x2) => (x1 == x2).then_some(Substitutions::default()),
        }
    }
}

impl Substitute for syn::Expr {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        if let Some(subs) = substitutions.get(&SubstitutionValue::Expr(self)) {
            if subs.is_empty() {
                return vec![self.clone()];
            }

            return subs.iter().map(|sub| syn::parse_quote!(#sub)).collect();
        }

        use syn::Expr::*;
        match self {
            Array(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Array)
                .collect(),
            Assign(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Assign)
                .collect(),
            Async(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Async)
                .collect(),
            Await(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Await)
                .collect(),
            Binary(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Binary)
                .collect(),
            Block(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Block)
                .collect(),
            Break(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Break)
                .collect(),
            Call(x1) => x1.substitute(substitutions).into_iter().map(Call).collect(),
            Cast(x1) => x1.substitute(substitutions).into_iter().map(Cast).collect(),
            Closure(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Closure)
                .collect(),
            Const(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Const)
                .collect(),
            Continue(x1) => vec![Continue(x1.clone())],
            Field(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Field)
                .collect(),
            ForLoop(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(ForLoop)
                .collect(),
            Group(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Group)
                .collect(),
            If(x1) => x1.substitute(substitutions).into_iter().map(If).collect(),
            Index(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Index)
                .collect(),
            Infer(x1) => vec![Infer(x1.clone())],
            Let(x1) => x1.substitute(substitutions).into_iter().map(Let).collect(),
            Loop(x1) => x1.substitute(substitutions).into_iter().map(Loop).collect(),
            Macro(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Macro)
                .collect(),
            Match(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Match)
                .collect(),
            MethodCall(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(MethodCall)
                .collect(),
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
            Repeat(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Repeat)
                .collect(),
            Return(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Return)
                .collect(),
            Struct(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Struct)
                .collect(),
            Try(x1) => x1.substitute(substitutions).into_iter().map(Try).collect(),
            TryBlock(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(TryBlock)
                .collect(),
            Tuple(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Tuple)
                .collect(),
            Unary(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Unary)
                .collect(),
            Unsafe(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Unsafe)
                .collect(),
            While(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(While)
                .collect(),
            Yield(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Yield)
                .collect(),
            _ => vec![self.clone()],
        }
    }
}

impl Superset for syn::ExprArray {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.elems.len() != other.elems.len() {
            return None;
        }

        self.attrs.is_superset(&other.attrs)?.merge(
            zip(&self.elems, &other.elems)
                .try_fold(Substitutions::default(), |acc, (x1, x2)| {
                    acc.merge(x1.is_superset(x2)?)
                })?,
        )
    }
}

impl Substitute for syn::ExprArray {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        if self.elems.is_empty() {
            return vec![self.clone()];
        }

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

impl Superset for syn::ExprAssign {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.left.is_superset(&other.left)?)?
            .merge(self.right.is_superset(&other.right)?)
    }
}

impl Substitute for syn::ExprAssign {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.left
            .substitute(substitutions)
            .into_iter()
            .cartesian_product(self.left.substitute(substitutions))
            .map(|(left, right)| Self {
                left: Box::new(left),
                right: Box::new(right),
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::ExprAsync {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.capture != other.capture {
            return None;
        }

        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.block.is_superset(&other.block)?)
    }
}

impl Substitute for syn::ExprAsync {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.block
            .substitute(substitutions)
            .into_iter()
            .map(|block| Self {
                block,
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::ExprAwait {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.base.is_superset(&other.base)?)
    }
}

impl Substitute for syn::ExprAwait {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.base
            .substitute(substitutions)
            .into_iter()
            .map(Box::new)
            .map(|base| Self {
                base,
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::ExprBinary {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.op != other.op {
            return None;
        }

        if let Some(substitutions) = self.left.is_superset(&other.left) {
            substitutions.merge(self.right.is_superset(&other.right)?)?
        } else {
            self.left
                .is_superset(&other.right)?
                .merge(self.right.is_superset(&other.left)?)?
        }
        .merge(self.attrs.is_superset(&other.attrs)?)
    }
}

impl Substitute for syn::ExprBinary {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.left
            .substitute(substitutions)
            .into_iter()
            .cartesian_product(self.left.substitute(substitutions))
            .map(|(left, right)| Self {
                left: Box::new(left),
                right: Box::new(right),
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::ExprBlock {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.label.is_superset(&other.label)?)?
            .merge(self.block.is_superset(&other.block)?)
    }
}

impl Substitute for syn::ExprBlock {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.label
            .substitute(substitutions)
            .into_iter()
            .cartesian_product(self.block.substitute(substitutions))
            .map(|(label, block)| Self {
                label,
                block,
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::ExprBreak {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.label.is_superset(&other.label)?)?
            .merge(match (&self.expr, &other.expr) {
                (None, None) => Some(Substitutions::default()),
                (Some(x1), Some(x2)) => x1.is_superset(x2),
                _ => return None,
            }?)
    }
}

impl Substitute for syn::ExprBreak {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.label
            .substitute(substitutions)
            .into_iter()
            .cartesian_product(self.expr.as_ref().map_or(vec![None], |expr| {
                expr.substitute(substitutions)
                    .into_iter()
                    .map(Box::new)
                    .map(Some)
                    .collect()
            }))
            .map(|(label, expr)| Self {
                label,
                expr,
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::ExprCall {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.args.len() != other.args.len() {
            return None;
        }

        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.func.is_superset(&other.func)?)?
            .merge(
                zip(&self.args, &other.args)
                    .try_fold(Substitutions::default(), |acc, (x1, x2)| {
                        acc.merge(x1.is_superset(x2)?)
                    })?,
            )
    }
}

impl Substitute for syn::ExprCall {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        let func = self.func.substitute(substitutions);

        if self.args.is_empty() {
            func.into_iter()
                .map(|func| Self {
                    func: Box::new(func),
                    ..self.clone()
                })
                .collect()
        } else {
            func.into_iter()
                .cartesian_product(
                    self.args
                        .iter()
                        .map(|args| args.substitute(substitutions))
                        .multi_cartesian_product(),
                )
                .map(|(func, args)| Self {
                    args: args.into_iter().collect(),
                    func: Box::new(func),
                    ..self.clone()
                })
                .collect()
        }
    }
}

impl Superset for syn::ExprCast {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.expr.is_superset(&other.expr)?)?
            .merge(self.ty.is_superset(&other.ty)?)
    }
}

impl Substitute for syn::ExprCast {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.expr
            .substitute(substitutions)
            .into_iter()
            .cartesian_product(self.ty.substitute(substitutions))
            .map(|(expr, ty)| Self {
                expr: Box::new(expr),
                ty: Box::new(ty),
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::ExprClosure {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.constness != other.constness
            || self.movability != other.movability
            || self.asyncness != other.asyncness
            || self.capture != other.capture
            || self.inputs.len() != other.inputs.len()
        {
            return None;
        }

        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.lifetimes.is_superset(&other.lifetimes)?)?
            .merge(
                zip(&self.inputs, &other.inputs)
                    .try_fold(Substitutions::default(), |acc, (x1, x2)| {
                        acc.merge(x1.is_superset(x2)?)
                    })?,
            )?
            .merge(self.output.is_superset(&other.output)?)?
            .merge(self.body.is_superset(&other.body)?)
    }
}

impl Substitute for syn::ExprClosure {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        let lifetimes = self.lifetimes.substitute(substitutions);
        let output = self.output.substitute(substitutions);
        let body = self.body.substitute(substitutions);

        if self.inputs.is_empty() {
            iproduct!(lifetimes, output, body)
                .map(|(lifetimes, output, body)| Self {
                    lifetimes,
                    output,
                    body: Box::new(body),
                    ..self.clone()
                })
                .collect()
        } else {
            let inputs = self
                .inputs
                .iter()
                .map(|input| input.substitute(substitutions))
                .multi_cartesian_product();

            iproduct!(lifetimes, inputs, output, body)
                .map(|(lifetimes, inputs, output, body)| Self {
                    lifetimes,
                    inputs: inputs.into_iter().collect(),
                    output,
                    body: Box::new(body),
                    ..self.clone()
                })
                .collect()
        }
    }
}

impl Superset for syn::ExprConst {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.block.is_superset(&other.block)?)
    }
}

impl Substitute for syn::ExprConst {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.block
            .substitute(substitutions)
            .into_iter()
            .map(|block| Self {
                block,
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::ExprContinue {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.label.is_superset(&other.label)?)
    }
}

impl Substitute for syn::ExprContinue {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.label
            .substitute(substitutions)
            .into_iter()
            .map(|label| Self {
                label,
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::ExprField {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.member != other.member {
            return None;
        }

        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.base.is_superset(&other.base)?)
    }
}

impl Substitute for syn::ExprField {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.base
            .substitute(substitutions)
            .into_iter()
            .map(|base| Self {
                base: Box::new(base),
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::ExprForLoop {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.label.is_superset(&other.label)?)?
            .merge(self.pat.is_superset(&other.pat)?)?
            .merge(self.expr.is_superset(&other.expr)?)?
            .merge(self.body.is_superset(&other.body)?)
    }
}

impl Substitute for syn::ExprForLoop {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        iproduct!(
            self.label.substitute(substitutions),
            self.pat.substitute(substitutions),
            self.expr.substitute(substitutions),
            self.body.substitute(substitutions)
        )
        .map(|(label, pat, expr, body)| Self {
            label,
            pat: Box::new(pat),
            expr: Box::new(expr),
            body,
            ..self.clone()
        })
        .collect()
    }
}

impl Superset for syn::ExprGroup {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.expr.is_superset(&other.expr)?)
    }
}

impl Substitute for syn::ExprGroup {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.expr
            .substitute(substitutions)
            .into_iter()
            .map(Box::new)
            .map(|expr| Self {
                expr,
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::ExprIf {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.cond.is_superset(&other.cond)?)?
            .merge(self.then_branch.is_superset(&other.then_branch)?)?
            .merge(match (&self.else_branch, &other.else_branch) {
                (Some((_, x1)), Some((_, x2))) => x1.is_superset(x2)?,
                (None, None) => Substitutions::default(),
                _ => return None,
            })
    }
}

impl Substitute for syn::ExprIf {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        iproduct!(
            self.cond.substitute(substitutions),
            self.then_branch.substitute(substitutions),
            self.else_branch
                .as_ref()
                .map_or(vec![None], |(else_token, else_branch)| else_branch
                    .substitute(substitutions)
                    .into_iter()
                    .map(|else_branch| Some((*else_token, Box::new(else_branch))))
                    .collect())
        )
        .map(|(cond, then_branch, else_branch)| Self {
            cond: Box::new(cond),
            then_branch,
            else_branch,
            ..self.clone()
        })
        .collect()
    }
}

impl Superset for syn::ExprIndex {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.expr.is_superset(&other.expr)?)?
            .merge(self.index.is_superset(&other.index)?)
    }
}

impl Substitute for syn::ExprIndex {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.expr
            .substitute(substitutions)
            .into_iter()
            .cartesian_product(self.index.substitute(substitutions))
            .map(|(expr, index)| Self {
                expr: Box::new(expr),
                index: Box::new(index),
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::ExprInfer {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs.is_superset(&other.attrs)
    }
}

impl Substitute for syn::ExprInfer {
    fn substitute(&self, _: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>) -> Vec<Self> {
        vec![self.clone()]
    }
}

impl Superset for syn::ExprLet {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.pat.is_superset(&other.pat)?)?
            .merge(self.expr.is_superset(&other.expr)?)
    }
}

impl Substitute for syn::ExprLet {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.pat
            .substitute(substitutions)
            .into_iter()
            .cartesian_product(self.expr.substitute(substitutions))
            .map(|(pat, expr)| Self {
                pat: Box::new(pat),
                expr: Box::new(expr),
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::ExprLit {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.lit.is_superset(&other.lit)?)
    }
}

impl Substitute for syn::ExprLit {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.lit
            .substitute(substitutions)
            .into_iter()
            .map(|lit| Self {
                lit,
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::ExprLoop {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.label.is_superset(&other.label)?)?
            .merge(self.body.is_superset(&other.body)?)
    }
}

impl Substitute for syn::ExprLoop {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.label
            .substitute(substitutions)
            .into_iter()
            .cartesian_product(self.body.substitute(substitutions))
            .map(|(label, body)| Self {
                label,
                body,
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::ExprMacro {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.mac.is_superset(&other.mac)?)
    }
}

impl Substitute for syn::ExprMacro {
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

impl Superset for syn::ExprMatch {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.arms.len() != other.arms.len() {
            return None;
        }

        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.expr.is_superset(&other.expr)?)?
            .merge(
                zip(&self.arms, &other.arms)
                    .try_fold(Substitutions::default(), |acc, (x1, x2)| {
                        acc.merge(x1.is_superset(x2)?)
                    })?,
            )
    }
}

impl Substitute for syn::ExprMatch {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        let expr = self.expr.substitute(substitutions);

        if self.arms.is_empty() {
            expr.into_iter()
                .map(|expr| Self {
                    expr: Box::new(expr),
                    ..self.clone()
                })
                .collect()
        } else {
            expr.into_iter()
                .cartesian_product(
                    self.arms
                        .iter()
                        .map(|arm| arm.substitute(substitutions))
                        .multi_cartesian_product(),
                )
                .map(|(expr, arms)| Self {
                    expr: Box::new(expr),
                    arms,
                    ..self.clone()
                })
                .collect()
        }
    }
}

impl Superset for syn::ExprMethodCall {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.method != other.method || self.args.len() != other.args.len() {
            return None;
        }

        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.receiver.is_superset(&other.receiver)?)?
            .merge(match (&self.turbofish, &other.turbofish) {
                (None, _) | (_, None) => Substitutions::default(),
                (Some(x1), Some(x2)) => x1.is_superset(x2)?,
            })?
            .merge(
                zip(&self.args, &other.args)
                    .try_fold(Substitutions::default(), |acc, (x1, x2)| {
                        acc.merge(x1.is_superset(x2)?)
                    })?,
            )
    }
}

impl Substitute for syn::ExprMethodCall {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        let receiver = self.receiver.substitute(substitutions);
        let turbofish = self.turbofish.substitute(substitutions);

        if self.args.is_empty() {
            receiver
                .into_iter()
                .cartesian_product(turbofish)
                .map(|(receiver, turbofish)| Self {
                    receiver: Box::new(receiver),
                    turbofish,
                    ..self.clone()
                })
                .collect()
        } else {
            let args = self
                .args
                .iter()
                .map(|arg| arg.substitute(substitutions))
                .multi_cartesian_product();

            receiver
                .into_iter()
                .cartesian_product(turbofish)
                .cartesian_product(args)
                .map(|((receiver, turbofish), args)| Self {
                    receiver: Box::new(receiver),
                    turbofish,
                    args: args.into_iter().collect(),
                    ..self.clone()
                })
                .collect()
        }
    }
}

impl Superset for syn::ExprParen {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.expr.is_superset(&other.expr)?)
    }
}

impl Substitute for syn::ExprParen {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.expr
            .substitute(substitutions)
            .into_iter()
            .map(|expr| Self {
                expr: Box::new(expr),
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::ExprPath {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.qself.is_superset(&other.qself)?)?
            .merge(self.path.is_superset(&other.path)?)
    }
}

impl Substitute for syn::ExprPath {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.qself
            .substitute(substitutions)
            .into_iter()
            .cartesian_product(self.path.substitute(substitutions))
            .map(|(qself, path)| Self {
                path,
                qself,
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::ExprRange {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.limits != other.limits {
            return None;
        }

        self.attrs
            .is_superset(&other.attrs)?
            .merge(match (&self.start, &other.start) {
                (Some(x1), Some(x2)) => x1.is_superset(x2)?,
                (None, None) => Substitutions::default(),
                _ => return None,
            })?
            .merge(match (&self.end, &other.end) {
                (Some(x1), Some(x2)) => x1.is_superset(x2)?,
                (None, None) => Substitutions::default(),
                _ => return None,
            })
    }
}

impl Substitute for syn::ExprRange {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.start
            .as_ref()
            .map_or(vec![None], |start| {
                start
                    .substitute(substitutions)
                    .into_iter()
                    .map(Some)
                    .collect()
            })
            .into_iter()
            .cartesian_product(self.end.as_ref().map_or(vec![None], |end| {
                end.substitute(substitutions)
                    .into_iter()
                    .map(Some)
                    .collect()
            }))
            .map(|(start, end)| Self {
                start: start.map(Box::new),
                end: end.map(Box::new),
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::ExprReference {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.mutability != other.mutability {
            return None;
        }

        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.expr.is_superset(&other.expr)?)
    }
}

impl Substitute for syn::ExprReference {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.expr
            .substitute(substitutions)
            .into_iter()
            .map(|expr| Self {
                expr: Box::new(expr),
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::ExprRepeat {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.expr.is_superset(&other.expr)?)?
            .merge(self.len.is_superset(&other.len)?)
    }
}

impl Substitute for syn::ExprRepeat {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.expr
            .substitute(substitutions)
            .into_iter()
            .cartesian_product(self.len.substitute(substitutions))
            .map(|(expr, len)| Self {
                expr: Box::new(expr),
                len: Box::new(len),
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::ExprReturn {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs
            .is_superset(&other.attrs)?
            .merge(match (&self.expr, &other.expr) {
                (None, None) => Some(Substitutions::default()),
                (Some(x1), Some(x2)) => x1.is_superset(x2),
                _ => return None,
            }?)
    }
}

impl Substitute for syn::ExprReturn {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.expr
            .as_ref()
            .map_or(vec![None], |expr| {
                expr.substitute(substitutions)
                    .into_iter()
                    .map(Some)
                    .collect()
            })
            .into_iter()
            .map(|expr| Self {
                expr: expr.map(Box::new),
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::ExprStruct {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.fields.len() != other.fields.len() || self.dot2_token != other.dot2_token {
            return None;
        }

        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.qself.is_superset(&other.qself)?)?
            .merge(self.path.is_superset(&other.path)?)?
            .merge(
                zip(&self.fields, &other.fields)
                    .try_fold(Substitutions::default(), |acc, (x1, x2)| {
                        acc.merge(x1.is_superset(x2)?)
                    })?,
            )?
            .merge(match (&self.rest, &other.rest) {
                (Some(x1), Some(x2)) => x1.is_superset(x2)?,
                (None, None) => Substitutions::default(),
                _ => return None,
            })
    }
}

impl Substitute for syn::ExprStruct {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        let qself = self.qself.substitute(substitutions);
        let path = self.path.substitute(substitutions);
        let rest = self.rest.as_ref().map_or(vec![None], |rest| {
            rest.substitute(substitutions)
                .into_iter()
                .map(Box::new)
                .map(Some)
                .collect()
        });

        if self.fields.is_empty() {
            iproduct!(qself, path, rest)
                .map(|(qself, path, rest)| Self {
                    qself,
                    path,
                    rest,
                    ..self.clone()
                })
                .collect()
        } else {
            let fields = self
                .fields
                .iter()
                .map(|field| field.substitute(substitutions))
                .multi_cartesian_product();
            iproduct!(qself, path, fields, rest)
                .map(|(qself, path, fields, rest)| Self {
                    qself,
                    path,
                    fields: fields.into_iter().collect(),
                    rest,
                    ..self.clone()
                })
                .collect()
        }
    }
}

impl Superset for syn::ExprTry {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.expr.is_superset(&other.expr)?)
    }
}

impl Substitute for syn::ExprTry {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.expr
            .substitute(substitutions)
            .into_iter()
            .map(|expr| Self {
                expr: Box::new(expr),
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::ExprTryBlock {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.block.is_superset(&other.block)?)
    }
}

impl Substitute for syn::ExprTryBlock {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.block
            .substitute(substitutions)
            .into_iter()
            .map(|block| Self {
                block,
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::ExprTuple {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.elems.len() != other.elems.len() {
            return None;
        }

        self.attrs.is_superset(&other.attrs)?.merge(
            zip(&self.elems, &other.elems)
                .try_fold(Substitutions::default(), |acc, (x1, x2)| {
                    acc.merge(x1.is_superset(x2)?)
                })?,
        )
    }
}

impl Substitute for syn::ExprTuple {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        if self.elems.is_empty() {
            return vec![self.clone()];
        }

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

impl Superset for syn::ExprUnary {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.op != other.op {
            return None;
        }

        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.expr.is_superset(&other.expr)?)
    }
}

impl Substitute for syn::ExprUnary {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.expr
            .substitute(substitutions)
            .into_iter()
            .map(|expr| Self {
                expr: Box::new(expr),
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::ExprUnsafe {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.block.is_superset(&other.block)?)
    }
}

impl Substitute for syn::ExprUnsafe {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.block
            .substitute(substitutions)
            .into_iter()
            .map(|block| Self {
                block,
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::ExprWhile {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.label.is_superset(&other.label)?)?
            .merge(self.cond.is_superset(&other.cond)?)?
            .merge(self.body.is_superset(&other.body)?)
    }
}

impl Substitute for syn::ExprWhile {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        iproduct!(
            self.label.substitute(substitutions),
            self.cond.substitute(substitutions),
            self.body.substitute(substitutions)
        )
        .map(|(label, cond, body)| Self {
            label,
            cond: Box::new(cond),
            body,
            ..self.clone()
        })
        .collect()
    }
}

impl Superset for syn::ExprYield {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs
            .is_superset(&other.attrs)?
            .merge(match (&self.expr, &other.expr) {
                (None, None) => Some(Substitutions::default()),
                (Some(x1), Some(x2)) => x1.is_superset(x2),
                _ => return None,
            }?)
    }
}

impl Substitute for syn::ExprYield {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.expr
            .as_ref()
            .map_or(vec![None], |expr| {
                expr.substitute(substitutions)
                    .into_iter()
                    .map(Some)
                    .collect()
            })
            .into_iter()
            .map(|expr| Self {
                expr: expr.map(Box::new),
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::Label {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.name.is_superset(&other.name)
    }
}

impl Substitute for syn::Label {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.name
            .substitute(substitutions)
            .into_iter()
            .map(|name| Self {
                name,
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::Arm {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.pat.is_superset(&other.pat)?)?
            .merge(match (&self.guard, &other.guard) {
                (Some((_, x1)), Some((_, x2))) => x1.is_superset(x2)?,
                (None, None) => Substitutions::default(),
                _ => return None,
            })?
            .merge(self.body.is_superset(&*other.body)?)
    }
}

impl Substitute for syn::Arm {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        iproduct!(
            self.pat.substitute(substitutions),
            self.guard
                .as_ref()
                .map_or(vec![None], |(if_token, guard)| guard
                    .substitute(substitutions)
                    .into_iter()
                    .map(|guard| (*if_token, Box::new(guard)))
                    .map(Some)
                    .collect()),
            self.body.substitute(substitutions)
        )
        .map(|(pat, guard, body)| Self {
            pat,
            guard,
            body: Box::new(body),
            ..self.clone()
        })
        .collect()
    }
}

impl Superset for syn::FieldValue {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.member != other.member {
            return None;
        }

        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.expr.is_superset(&other.expr)?)
    }
}

impl Substitute for syn::FieldValue {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.expr
            .substitute(substitutions)
            .into_iter()
            .map(|expr| Self {
                expr,
                ..self.clone()
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use syn::parse_quote;

    use super::*;

    #[test]
    fn identity_expr_superset() {
        let r: syn::Ident = syn::parse_quote!(_ŠČ);
        let substitutions = Substitutions::identity(&r);

        let e1: syn::Expr = syn::parse_quote!(_ŠČ);
        assert_eq!(e1.is_superset(&e1), Some(substitutions.clone()));

        // FIXME: Macro like `vec![]` doesn't work saddly
        let e2: syn::Expr = syn::parse_quote!([_ŠČ + 1, String::new()]);
        assert_eq!(e2.is_superset(&e2), Some(substitutions.clone()));

        let e3: syn::Expr = syn::parse_quote!(kita((_ŠČ + 1, String::new())));
        assert_eq!(e3.is_superset(&e3), Some(substitutions.clone()));
    }

    #[test]
    fn concrete_expr_superset() {
        let substitutions = Substitutions::default();

        let e1: syn::Expr = syn::parse_quote!(420);
        assert_eq!(e1.is_superset(&e1), Some(substitutions.clone()));

        let e2: syn::Expr = syn::parse_quote!(vec![420 + 1, String::new()]);
        assert_eq!(e2.is_superset(&e2), Some(substitutions.clone()));

        let e3: syn::Expr = syn::parse_quote!(|m| 1 + String::new());
        assert_eq!(e3.is_superset(&e3), Some(substitutions.clone()));
    }

    #[test]
    fn expression_supersets() {
        let a: syn::Expr = syn::parse_quote!({
            let a = [_ŠČ0, _ŠČ1];
            a[0] + _ŠČ3
        });

        let b: syn::Expr = syn::parse_quote!({
            let a = [_ŠČ1 + 1, |m: Vec<u32>| 1 + _ŠČ0];
            // FIXME: It doesn't work without parentheses
            a[0] + (!_ŠČ3 + 1)
        });

        assert!(b.is_superset(&a).is_none());

        let mut substitutions = IndexMap::new();
        let (k1, m1): (_, syn::Expr) = (parse_quote!(_ŠČ0), parse_quote!(_ŠČ1 + 1));
        let (k2, m2): (_, syn::Expr) = (parse_quote!(_ŠČ1), parse_quote!(|m: Vec<u32>| 1 + _ŠČ0));
        let (k3, m3): (_, syn::Expr) = (parse_quote!(_ŠČ3), parse_quote!((!_ŠČ3 + 1)));
        substitutions.insert(&k1, (&m1).into());
        substitutions.insert(&k2, (&m2).into());
        substitutions.insert(&k3, (&m3).into());

        assert_eq!(a.is_superset(&b), Some(Substitutions(substitutions)));
    }
}
