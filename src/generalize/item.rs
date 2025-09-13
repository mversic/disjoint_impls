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
            || self.unsafety != other.unsafety
            || self.ident != other.ident
        {
            return None;
        }

        let abi = self.abi.generalize(&other.abi, params1, params2, subs)?;
        let inputs = self
            .inputs
            .generalize(&other.inputs, params1, params2, subs)?;
        let output = self
            .output
            .generalize(&other.output, params1, params2, subs)?;

        Some(syn::Signature {
            abi,
            inputs,
            output,
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
        if self.mutability != other.mutability || self.colon_token != other.colon_token {
            return None;
        }

        let attrs = self
            .attrs
            .generalize(&other.attrs, params1, params2, subs)?;
        let reference = self
            .reference
            .generalize(&other.reference, params1, params2, subs)?;
        let ty = self.ty.generalize(&other.ty, params1, params2, subs)?;

        Some(Self {
            attrs,
            reference,
            ty,
            ..self.clone()
        })
    }
}
