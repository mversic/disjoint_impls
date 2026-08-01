use super::*;

impl Generalize for syn::Signature {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        subs: &mut Generalizations<'a>,
    ) -> Option<Self> {
        if self.asyncness != other.asyncness
            || self.constness != other.constness
            || self.safety != other.safety
            || self.ident != other.ident
        {
            return None;
        }

        let abi = self.abi.generalize(&other.abi, params1, params2, subs)?;
        let generics = self
            .generics
            .generalize(&other.generics, params1, params2, subs)?;
        let inputs = self
            .inputs
            .generalize(&other.inputs, params1, params2, subs)?;
        let output = self
            .output
            .generalize(&other.output, params1, params2, subs)?;

        Some(syn::Signature {
            abi,
            generics,
            inputs,
            output,
            ..self.clone()
        })
    }
}

impl Generalize for syn::ImplItemConst {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        subs: &mut Generalizations<'a>,
    ) -> Option<Self> {
        if self.vis != other.vis || self.ident != other.ident {
            return None;
        }

        let attrs = self
            .attrs
            .generalize(&other.attrs, params1, params2, subs)?;
        let generics = self
            .generics
            .generalize(&other.generics, params1, params2, subs)?;
        let ty = self.ty.generalize(&other.ty, params1, params2, subs)?;
        let expr = self.expr.generalize(&other.expr, params1, params2, subs)?;

        Some(Self {
            attrs,
            generics,
            ty,
            expr,
            ..self.clone()
        })
    }
}

impl Generalize for syn::ImplItemType {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        subs: &mut Generalizations<'a>,
    ) -> Option<Self> {
        if self.vis != other.vis || self.ident != other.ident {
            return None;
        }

        let attrs = self
            .attrs
            .generalize(&other.attrs, params1, params2, subs)?;
        let generics = self
            .generics
            .generalize(&other.generics, params1, params2, subs)?;
        let ty = self.ty.generalize(&other.ty, params1, params2, subs)?;

        Some(Self {
            attrs,
            generics,
            ty,
            ..self.clone()
        })
    }
}

impl Generalize for syn::FnArg {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        subs: &mut Generalizations<'a>,
    ) -> Option<Self> {
        match (self, other) {
            (syn::FnArg::Receiver(x1), syn::FnArg::Receiver(x2)) => x1
                .generalize(x2, params1, params2, subs)
                .map(syn::FnArg::Receiver),
            (syn::FnArg::Typed(pat1), syn::FnArg::Typed(pat2)) => pat1
                .generalize(pat2, params1, params2, subs)
                .map(syn::FnArg::Typed),
            _ => None,
        }
    }
}

impl Generalize for syn::Receiver {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        subs: &mut Generalizations<'a>,
    ) -> Option<Self> {
        let attrs = self
            .attrs
            .generalize(&other.attrs, params1, params2, subs)?;
        let kind = match (&self.kind, &other.kind) {
            (syn::ReceiverKind::Value, syn::ReceiverKind::Value) => self.kind.clone(),
            (
                syn::ReceiverKind::Reference(and_token, lifetime1, mutability1),
                syn::ReceiverKind::Reference(_, lifetime2, mutability2),
            ) if mutability1 == mutability2 => {
                let lifetime = lifetime1.generalize(lifetime2, params1, params2, subs)?;
                syn::ReceiverKind::Reference(*and_token, lifetime, *mutability1)
            }
            (syn::ReceiverKind::Typed(colon_token, ty1), syn::ReceiverKind::Typed(_, ty2)) => {
                let ty = ty1.generalize(ty2, params1, params2, subs)?;
                syn::ReceiverKind::Typed(*colon_token, ty)
            }
            _ => return None,
        };

        Some(Self {
            attrs,
            kind,
            ..self.clone()
        })
    }
}
