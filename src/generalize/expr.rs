use super::*;

impl Generalize for syn::Expr {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        use syn::Expr::*;
        let res = match (self, other) {
            (Array(x1), Array(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Array),
            (Assign(x1), Assign(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Assign),
            (Async(x1), Async(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Async),
            (Await(x1), Await(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Await),
            (Binary(x1), Binary(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Binary),
            (Block(x1), Block(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Block),
            (Break(x1), Break(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Break),
            (Call(x1), Call(x2)) => x1.generalize(x2, params1, params2, substitutions).map(Call),
            (Cast(x1), Cast(x2)) => x1.generalize(x2, params1, params2, substitutions).map(Cast),
            (Closure(x1), Closure(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Closure),
            (Const(x1), Const(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Const),
            (Continue(x1), Continue(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Continue),
            (Field(x1), Field(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Field),
            (ForLoop(x1), ForLoop(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(ForLoop),
            (Group(x1), Group(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Group),
            (If(x1), If(x2)) => x1.generalize(x2, params1, params2, substitutions).map(If),
            (Index(x1), Index(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Index),
            (Infer(_), Infer(_)) => Some(self.clone()),
            (x1, Infer(_)) => x1.generalize(x1, params1, params2, substitutions),
            (Infer(_), x2) => x2.generalize(x2, params2, params2, substitutions),
            (Lit(x1), Lit(x2)) => x1.generalize(x2, params1, params2, substitutions).map(Lit),
            (Loop(x1), Loop(x2)) => x1.generalize(x2, params1, params2, substitutions).map(Loop),
            (Macro(x1), Macro(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Macro),
            (Match(x1), Match(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Match),
            (MethodCall(x1), MethodCall(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(MethodCall),
            (Paren(x1), Paren(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Paren),
            (Path(x1), _) if x1.path.get_ident().is_some_and(|i| params1.contains_key(i)) => {
                substitutions.insert_expr(self, other, params1, params2)
            }
            (_, Path(x2)) if x2.path.get_ident().is_some_and(|i| params2.contains_key(i)) => {
                substitutions.insert_expr(self, other, params1, params2)
            }
            (Path(x1), Path(x2)) => x1.generalize(x2, params1, params2, substitutions).map(Path),
            (Range(x1), Range(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Range),
            (Reference(x1), Reference(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Reference),
            (Repeat(x1), Repeat(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Repeat),
            (Return(x1), Return(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Return),
            (Struct(x1), Struct(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Struct),
            (Try(x1), Try(x2)) => x1.generalize(x2, params1, params2, substitutions).map(Try),
            (TryBlock(x1), TryBlock(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(TryBlock),
            (Tuple(x1), Tuple(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Tuple),
            (Unary(x1), Unary(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Unary),
            (Unsafe(x1), Unsafe(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Unsafe),
            (While(x1), While(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(While),
            (Yield(x1), Yield(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Yield),
            _ => None,
        };

        res.or_else(|| substitutions.insert_expr(self, other, params1, params2))
    }
}

impl Generalize for syn::ExprArray {
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

impl Generalize for syn::ExprAssign {
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
        let left = self
            .left
            .generalize(&other.left, params1, params2, substitutions)?;
        let right = self
            .right
            .generalize(&other.right, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            left,
            right,
            ..self.clone()
        })
    }
}

impl Generalize for syn::ExprAsync {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        if self.capture != other.capture {
            return None;
        }

        let attrs = self
            .attrs
            .generalize(&other.attrs, params1, params2, substitutions)?;
        let block = self
            .block
            .generalize(&other.block, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            block,
            ..self.clone()
        })
    }
}

impl Generalize for syn::ExprAwait {
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
        let base = self
            .base
            .generalize(&other.base, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            base,
            ..self.clone()
        })
    }
}

impl Generalize for syn::ExprBinary {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        if self.op != other.op {
            return None;
        }

        let attrs = self
            .attrs
            .generalize(&other.attrs, params1, params2, substitutions)?;
        let left = self
            .left
            .generalize(&other.left, params1, params2, substitutions)?;
        let right = self
            .right
            .generalize(&other.right, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            left,
            right,
            ..self.clone()
        })
    }
}

impl Generalize for syn::ExprBlock {
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
        let label = self
            .label
            .generalize(&other.label, params1, params2, substitutions)?;
        let block = self
            .block
            .generalize(&other.block, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            label,
            block,
        })
    }
}

impl Generalize for syn::ExprBreak {
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
        let label = self
            .label
            .generalize(&other.label, params1, params2, substitutions)?;
        let expr = self
            .expr
            .generalize(&other.expr, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            label,
            expr,
            ..self.clone()
        })
    }
}

impl Generalize for syn::ExprCall {
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
        let func = self
            .func
            .generalize(&other.func, params1, params2, substitutions)?;
        let args = self
            .args
            .generalize(&other.args, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            func,
            args,
            ..self.clone()
        })
    }
}

impl Generalize for syn::ExprCast {
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
        let expr = self
            .expr
            .generalize(&other.expr, params1, params2, substitutions)?;
        let ty = self
            .ty
            .generalize(&other.ty, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            expr,
            ty,
            ..self.clone()
        })
    }
}

impl Generalize for syn::ExprClosure {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        if self.constness != other.constness
            || self.movability != other.movability
            || self.asyncness != other.asyncness
            || self.capture != other.capture
        {
            return None;
        }

        let attrs = self
            .attrs
            .generalize(&other.attrs, params1, params2, substitutions)?;
        let inputs = self
            .inputs
            .generalize(&other.inputs, params1, params2, substitutions)?;
        let output = self
            .output
            .generalize(&other.output, params1, params2, substitutions)?;
        let body = self
            .body
            .generalize(&other.body, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            inputs,
            output,
            body,
            ..self.clone()
        })
    }
}

impl Generalize for syn::ExprConst {
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
        let block = self
            .block
            .generalize(&other.block, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            block,
            ..self.clone()
        })
    }
}

impl Generalize for syn::ExprContinue {
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
        let label = self
            .label
            .generalize(&other.label, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            label,
            ..self.clone()
        })
    }
}

impl Generalize for syn::ExprField {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        if self.member != other.member {
            return None;
        }

        let attrs = self
            .attrs
            .generalize(&other.attrs, params1, params2, substitutions)?;
        let base = self
            .base
            .generalize(&other.base, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            base,
            ..self.clone()
        })
    }
}

impl Generalize for syn::ExprForLoop {
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
        let label = self
            .label
            .generalize(&other.label, params1, params2, substitutions)?;
        let pat = self
            .pat
            .generalize(&other.pat, params1, params2, substitutions)?;
        let expr = self
            .expr
            .generalize(&other.expr, params1, params2, substitutions)?;
        let body = self
            .body
            .generalize(&other.body, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            label,
            pat,
            expr,
            body,
            ..self.clone()
        })
    }
}

impl Generalize for syn::ExprGroup {
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
        let expr = self
            .expr
            .generalize(&other.expr, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            expr,
            ..self.clone()
        })
    }
}

impl Generalize for syn::ExprIf {
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
        let cond = self
            .cond
            .generalize(&other.cond, params1, params2, substitutions)?;
        let then_branch =
            self.then_branch
                .generalize(&other.then_branch, params1, params2, substitutions)?;
        let else_branch =
            self.else_branch
                .generalize(&other.else_branch, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            cond,
            then_branch,
            else_branch,
            ..self.clone()
        })
    }
}

impl Generalize for syn::ExprIndex {
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
        let expr = self
            .expr
            .generalize(&other.expr, params1, params2, substitutions)?;
        let index = self
            .index
            .generalize(&other.index, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            expr,
            index,
            ..self.clone()
        })
    }
}

impl Generalize for syn::ExprLet {
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
        let expr = self
            .expr
            .generalize(&other.expr, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            pat,
            expr,
            ..self.clone()
        })
    }
}

impl Generalize for syn::ExprLit {
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
        let lit = self
            .lit
            .generalize(&other.lit, params1, params2, substitutions)?;

        Some(Self { attrs, lit })
    }
}

impl Generalize for syn::ExprLoop {
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
        let label = self
            .label
            .generalize(&other.label, params1, params2, substitutions)?;
        let body = self
            .body
            .generalize(&other.body, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            label,
            body,
            ..self.clone()
        })
    }
}

impl Generalize for syn::ExprMacro {
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
        let mac = self
            .mac
            .generalize(&other.mac, params1, params2, substitutions)?;

        Some(Self { attrs, mac })
    }
}

impl Generalize for syn::ExprMatch {
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
        let expr = self
            .expr
            .generalize(&other.expr, params1, params2, substitutions)?;
        let arms = self
            .arms
            .generalize(&other.arms, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            expr,
            arms,
            ..self.clone()
        })
    }
}

impl Generalize for syn::ExprMethodCall {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        if self.method != other.method {
            return None;
        }

        let attrs = self
            .attrs
            .generalize(&other.attrs, params1, params2, substitutions)?;
        let receiver =
            self.receiver
                .generalize(&other.receiver, params1, params2, substitutions)?;
        let turbofish =
            self.turbofish
                .generalize(&other.turbofish, params1, params2, substitutions)?;
        let args = self
            .args
            .generalize(&other.args, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            receiver,
            turbofish,
            args,
            ..self.clone()
        })
    }
}

impl Generalize for syn::ExprParen {
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
        let expr = self
            .expr
            .generalize(&other.expr, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            expr,
            ..self.clone()
        })
    }
}

impl Generalize for syn::ExprPath {
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
        let qself = self
            .qself
            .generalize(&other.qself, params1, params2, substitutions)?;
        let path = self
            .path
            .generalize(&other.path, params1, params2, substitutions)?;

        Some(Self { attrs, qself, path })
    }
}

impl Generalize for syn::ExprRange {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        if self.limits != other.limits {
            return None;
        }

        let attrs = self
            .attrs
            .generalize(&other.attrs, params1, params2, substitutions)?;
        let start = self
            .start
            .generalize(&other.start, params1, params2, substitutions)?;
        let end = self
            .end
            .generalize(&other.end, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            start,
            end,
            ..self.clone()
        })
    }
}

impl Generalize for syn::ExprReference {
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
        let expr = self
            .expr
            .generalize(&other.expr, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            mutability: self.mutability,
            expr,
            ..self.clone()
        })
    }
}

impl Generalize for syn::ExprRepeat {
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
        let expr = self
            .expr
            .generalize(&other.expr, params1, params2, substitutions)?;
        let len = self
            .len
            .generalize(&other.len, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            expr,
            len,
            ..self.clone()
        })
    }
}

impl Generalize for syn::ExprReturn {
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
        let expr = self
            .expr
            .generalize(&other.expr, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            expr,
            ..self.clone()
        })
    }
}

impl Generalize for syn::ExprStruct {
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
        let fields = self
            .fields
            .generalize(&other.fields, params1, params2, substitutions)?;
        let rest = self
            .rest
            .generalize(&other.rest, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            path,
            fields,
            rest,
            ..self.clone()
        })
    }
}

impl Generalize for syn::ExprTry {
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
        let expr = self
            .expr
            .generalize(&other.expr, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            expr,
            ..self.clone()
        })
    }
}

impl Generalize for syn::ExprTryBlock {
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
        let block = self
            .block
            .generalize(&other.block, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            block,
            ..self.clone()
        })
    }
}

impl Generalize for syn::ExprTuple {
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

impl Generalize for syn::ExprUnary {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        if self.op != other.op {
            return None;
        }

        let attrs = self
            .attrs
            .generalize(&other.attrs, params1, params2, substitutions)?;
        let expr = self
            .expr
            .generalize(&other.expr, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            expr,
            ..self.clone()
        })
    }
}

impl Generalize for syn::ExprUnsafe {
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
        let block = self
            .block
            .generalize(&other.block, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            block,
            ..self.clone()
        })
    }
}

impl Generalize for syn::ExprWhile {
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
        let label = self
            .label
            .generalize(&other.label, params1, params2, substitutions)?;
        let cond = self
            .cond
            .generalize(&other.cond, params1, params2, substitutions)?;
        let body = self
            .body
            .generalize(&other.body, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            label,
            cond,
            body,
            ..self.clone()
        })
    }
}

impl Generalize for syn::ExprYield {
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
        let expr = self
            .expr
            .generalize(&other.expr, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            expr,
            ..self.clone()
        })
    }
}

impl Generalize for syn::Label {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        let name = self
            .name
            .generalize(&other.name, params1, params2, substitutions)?;

        Some(Self {
            name,
            ..self.clone()
        })
    }
}

impl Generalize for syn::Arm {
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
        let body = self
            .body
            .generalize(&other.body, params1, params2, substitutions)?;
        let guard = self
            .guard
            .generalize(&other.guard, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            pat,
            guard,
            body,
            ..self.clone()
        })
    }
}

impl Generalize for syn::FieldValue {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        if self.member != other.member {
            return None;
        }

        let attrs = self
            .attrs
            .generalize(&other.attrs, params1, params2, substitutions)?;
        let expr = self
            .expr
            .generalize(&other.expr, params1, params2, substitutions)?;

        Some(Self {
            attrs,
            member: self.member.clone(),
            expr,
            ..self.clone()
        })
    }
}

#[cfg(test)]
mod tests {
    use indexmap::indexmap;
    use syn::parse_quote;

    use super::*;

    #[test]
    fn identity_expr_generalize() {
        let p1 = indexmap! {
            format_ident!("N") => GenericParam::Const(parse_quote!(usize))
        };

        let e1: syn::Expr = parse_quote!(N);
        let mut subs1 = Generalizations::default();
        let expr = e1.generalize(&e1, &p1, &p1, &mut subs1).unwrap();

        let expected_expr = parse_quote!(_CŠČ0);

        assert_eq!(expr, expected_expr);

        assert!(subs1.lifetime_generalizations.is_empty());
        assert!(subs1.type_generalizations.is_empty());
        assert_eq!(
            subs1.expr_generalizations,
            indexmap! { (Expr::Expr(&e1), Expr::Expr(&e1)) => (GeneralizationKind::Common, parse_quote!(usize), ExprTypeKind::Given) }
        );

        // ---

        let e2: syn::Expr = parse_quote!([N + 1, String::new()]);
        let mut subs2 = Generalizations::default();
        let expr = e2.generalize(&e2, &p1, &p1, &mut subs2).unwrap();

        let expected_expr_2: syn::Expr = parse_quote!([_CŠČ0 + 1, String::new()]);

        assert_eq!(expr, expected_expr_2);
        assert!(subs2.lifetime_generalizations.is_empty());
        assert!(subs2.type_generalizations.is_empty());
        assert_eq!(
            subs2.expr_generalizations,
            indexmap! { (Expr::Expr(&e1), Expr::Expr(&e1)) => (GeneralizationKind::Common, parse_quote!(usize), ExprTypeKind::Given) }
        );

        // ---

        let e3: syn::Expr = parse_quote!(kita([N + 2, String::new()]));
        let mut subs3 = Generalizations::default();
        let expr = e3.generalize(&e3, &p1, &p1, &mut subs3).unwrap();

        let expected_expr_3: syn::Expr = parse_quote!(kita([_CŠČ0 + 2, String::new()]));

        assert_eq!(expr, expected_expr_3);
        assert!(subs3.lifetime_generalizations.is_empty());
        assert!(subs3.type_generalizations.is_empty());
        assert_eq!(
            subs3.expr_generalizations,
            indexmap! { (Expr::Expr(&e1), Expr::Expr(&e1)) => (GeneralizationKind::Common, parse_quote!(usize), ExprTypeKind::Given) }
        );
    }

    #[test]
    fn concrete_expr_generalize() {
        let p1 = indexmap! {};

        let e1: syn::Expr = parse_quote!(420);
        let mut subs1 = Generalizations::default();
        let expr = e1.generalize(&e1, &p1, &p1, &mut subs1).unwrap();

        assert_eq!(expr, e1);
        assert!(subs1.lifetime_generalizations.is_empty());
        assert!(subs1.type_generalizations.is_empty());
        assert!(subs1.expr_generalizations.is_empty());

        // ---

        let e2: syn::Expr = parse_quote!(vec![420 + 1, String::new()]);
        let mut subs2 = Generalizations::default();
        let expr = e2.generalize(&e2, &p1, &p1, &mut subs2).unwrap();

        let expected_expr_2: syn::Expr = parse_quote!(vec![420 + 1, String::new()]);

        assert_eq!(expr, expected_expr_2);

        assert!(subs2.lifetime_generalizations.is_empty());
        assert!(subs2.type_generalizations.is_empty());
        assert!(subs2.expr_generalizations.is_empty());

        // ---

        let e3: syn::Expr = parse_quote!(|m| 1 + String::new());
        let mut subs3 = Generalizations::default();
        let expr = e3.generalize(&e3, &p1, &p1, &mut subs3).unwrap();

        let expected_expr_3: syn::Expr = parse_quote!(|m| 1 + String::new());

        assert_eq!(expr, expected_expr_3);

        assert!(subs3.lifetime_generalizations.is_empty());
        assert!(subs3.type_generalizations.is_empty());
        assert!(subs3.expr_generalizations.is_empty());
    }

    #[test]
    fn expr_full_generalization() {
        let p = indexmap! {
            format_ident!("_ŠČ0") => GenericParam::Const(parse_quote!(usize)),
            format_ident!("_ŠČ1") => GenericParam::Const(parse_quote!(usize)),
            format_ident!("_ŠČ3") => GenericParam::Const(parse_quote!(usize)),
        };

        let a: syn::Expr = parse_quote!({
            let a = [_ŠČ0, _ŠČ1];
            a[0] + _ŠČ3
        });

        let b: syn::Expr = parse_quote!({
            let a = [_ŠČ1 + 1, |m: Vec<u32>| 1 + _ŠČ0];
            // FIXME: It doesn't work without parentheses
            a[0] + (!_ŠČ3 + 1)
        });

        let mut subs1 = Generalizations::default();
        let mut subs2 = Generalizations::default();

        let ty1 = a.generalize(&b, &p, &p, &mut subs1).unwrap();
        let ty2 = b.generalize(&a, &p, &p, &mut subs2).unwrap();

        let expected_ty: syn::Expr = parse_quote!({
            let a = [_CŠČ0, _CŠČ1];
            a[0] + _CŠČ2
        });

        assert_eq!(ty1, expected_ty);
        assert_eq!(ty2, expected_ty);

        let r1: syn::Expr = parse_quote!(_ŠČ0);
        let r2: syn::Expr = parse_quote!(_ŠČ1);
        let r3: syn::Expr = parse_quote!(_ŠČ3);

        let r4: syn::Expr = parse_quote!(|m: Vec<u32>| 1 + _ŠČ0);
        let r5: syn::Expr = parse_quote!(_ŠČ1 + 1);
        let r6: syn::Expr = parse_quote!((!_ŠČ3 + 1));

        assert!(subs1.lifetime_generalizations.is_empty());
        assert!(subs2.lifetime_generalizations.is_empty());

        assert!(subs1.type_generalizations.is_empty());
        assert!(subs2.type_generalizations.is_empty());

        assert_eq!(
            subs1.expr_generalizations,
            indexmap! {
                (Expr::Expr(&r1), Expr::Expr(&r5)) => (GeneralizationKind::Common, parse_quote!(usize), ExprTypeKind::Given),
                (Expr::Expr(&r2), Expr::Expr(&r4)) => (GeneralizationKind::Common, parse_quote!(usize), ExprTypeKind::Given),
                (Expr::Expr(&r3), Expr::Expr(&r6)) => (GeneralizationKind::Common, parse_quote!(usize), ExprTypeKind::Given)
            }
        );

        assert_eq!(
            subs2.expr_generalizations,
            indexmap! {
                (Expr::Expr(&r5), Expr::Ident(p.get_index(0).unwrap().0)) => (GeneralizationKind::Common, parse_quote!(usize), ExprTypeKind::Given),
                (Expr::Expr(&r4), Expr::Ident(p.get_index(1).unwrap().0)) => (GeneralizationKind::Common, parse_quote!(usize), ExprTypeKind::Given),
                (Expr::Expr(&r6), Expr::Ident(p.get_index(2).unwrap().0)) => (GeneralizationKind::Common, parse_quote!(usize), ExprTypeKind::Given)
            }
        );
    }
}
