use super::*;

impl Superset for syn::Type {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions<'a>> {
        use syn::Type::*;

        match (self, other) {
            (Array(x1), Array(x2)) => x1.is_superset(x2),
            (BareFn(x1), BareFn(x2)) => x1.is_superset(x2),
            (Group(x1), x2) => x1.elem.is_superset(x2),
            (x1, Group(x2)) => x1.is_superset(&x2.elem),
            (ImplTrait(x1), ImplTrait(x2)) => x1.is_superset(x2),
            (Infer(x1), Infer(x2)) => x1.is_superset(x2),
            (Macro(x1), Macro(x2)) => x1.is_superset(x2),
            (Never(x1), Never(x2)) => x1.is_superset(x2),

            (Paren(x1), x2) => x1.elem.is_superset(x2),
            (x1, Paren(x2)) => x1.is_superset(&x2.elem),

            (Path(x1), x2) => {
                if let Some(ident) = matches_param_ident(&x1.path) {
                    if let syn::Type::Path(other) = other
                        && Some(ident) == matches_param_ident(&other.path)
                    {
                        return Some(Substitutions::identity(ident));
                    }

                    return Some(Substitutions::new(ident, other));
                }

                if let syn::Type::Path(x2) = x2 {
                    return x1.is_superset(x2);
                }

                None
            }
            (Ptr(x1), Ptr(x2)) => x1.is_superset(x2),
            (Reference(x1), Reference(x2)) => x1.is_superset(x2),
            (Slice(x1), Slice(x2)) => x1.is_superset(x2),
            (TraitObject(x1), TraitObject(x2)) => x1.is_superset(x2),
            (Tuple(x1), Tuple(x2)) => x1.is_superset(x2),

            (x1, x2) => (x1 == x2).then_some(Substitutions::default()),
        }
    }
}

impl Substitute for syn::Type {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        if let Some(subs) = substitutions.get(&SubstitutionValue::Type(self)) {
            if subs.is_empty() {
                return vec![self.clone()];
            }

            return subs.iter().map(|sub| syn::parse_quote!(#sub)).collect();
        }

        use syn::Type::*;
        match self {
            Array(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Array)
                .collect(),
            BareFn(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(BareFn)
                .collect(),
            Group(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Group)
                .collect(),
            ImplTrait(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(ImplTrait)
                .collect(),
            Infer(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Infer)
                .collect(),
            Macro(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Macro)
                .collect(),
            Never(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Never)
                .collect(),
            Paren(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Paren)
                .collect(),
            Path(x1) => x1.substitute(substitutions).into_iter().map(Path).collect(),
            Ptr(x1) => x1.substitute(substitutions).into_iter().map(Ptr).collect(),
            Reference(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Reference)
                .collect(),
            Slice(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Slice)
                .collect(),
            TraitObject(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(TraitObject)
                .collect(),
            Tuple(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Tuple)
                .collect(),
            _ => vec![self.clone()],
        }
    }
}

impl Superset for syn::TypeArray {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions<'a>> {
        self.elem
            .is_superset(&other.elem)?
            .merge(self.len.is_superset(&other.len)?)
    }
}

impl Substitute for syn::TypeArray {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.elem
            .substitute(substitutions)
            .into_iter()
            .cartesian_product(self.len.substitute(substitutions))
            .map(|(elem, len)| Self {
                elem: Box::new(elem),
                len,
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::TypeBareFn {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions<'a>> {
        if self.unsafety != other.unsafety
            || self.variadic != other.variadic
            || self.inputs.len() != other.inputs.len()
        {
            return None;
        }

        if let (Some(abi1), Some(abi2)) = (&self.abi, &other.abi) {
            let c_abi = syn::parse_quote!("C");

            match (&abi1.name, &abi2.name) {
                (Some(x1), None) if *x1 == c_abi => {}
                (None, Some(x2)) if *x2 == c_abi => {}
                (Some(x1), Some(x2)) if x1 == x2 => {}
                (None, None) => {}
                _ => return None,
            }
        } else {
            return None;
        }

        self.lifetimes
            .is_superset(&other.lifetimes)?
            .merge(
                zip(&self.inputs, &other.inputs)
                    .try_fold(Substitutions::default(), |acc, (x1, x2)| {
                        acc.merge(x1.is_superset(x2)?)
                    })?,
            )?
            .merge(self.output.is_superset(&other.output)?)
    }
}

impl Substitute for syn::TypeBareFn {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        let lifetimes = self.lifetimes.substitute(substitutions);
        let output = self.output.substitute(substitutions);

        if self.inputs.is_empty() {
            iproduct!(lifetimes, output)
                .map(|(lifetimes, output)| Self {
                    lifetimes,
                    output,
                    ..self.clone()
                })
                .collect()
        } else {
            let inputs = self
                .inputs
                .iter()
                .map(|input| input.substitute(substitutions))
                .multi_cartesian_product();

            iproduct!(lifetimes, inputs, output)
                .map(|(lifetimes, inputs, output)| Self {
                    lifetimes,
                    inputs: inputs.into_iter().collect(),
                    output,
                    ..self.clone()
                })
                .collect()
        }
    }
}

impl Superset for syn::TypeGroup {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions<'a>> {
        self.elem.is_superset(&other.elem)
    }
}

impl Substitute for syn::TypeGroup {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.elem
            .substitute(substitutions)
            .into_iter()
            .map(Box::new)
            .map(|elem| Self {
                elem,
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::TypeInfer {
    fn is_superset(&self, _: &Self) -> Option<Substitutions<'_>> {
        Some(Substitutions::default())
    }
}

impl Substitute for syn::TypeInfer {
    fn substitute(&self, _: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>) -> Vec<Self> {
        vec![self.clone()]
    }
}

impl Superset for syn::TypeImplTrait {
    fn is_superset<'a>(&'a self, _: &'a Self) -> Option<Substitutions<'a>> {
        unreachable!()
    }
}

impl Substitute for syn::TypeImplTrait {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        if self.bounds.is_empty() {
            return vec![self.clone()];
        }

        self.bounds
            .iter()
            .map(|x| x.substitute(substitutions))
            .multi_cartesian_product()
            .map(|bounds| Self {
                bounds: bounds.into_iter().collect(),
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::TypeMacro {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions<'a>> {
        self.mac.is_superset(&other.mac)
    }
}

impl Substitute for syn::TypeMacro {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.mac
            .substitute(substitutions)
            .into_iter()
            .map(|mac| Self { mac })
            .collect()
    }
}

impl Superset for syn::TypeNever {
    fn is_superset(&self, _: &Self) -> Option<Substitutions<'_>> {
        Some(Substitutions::default())
    }
}

impl Substitute for syn::TypeNever {
    fn substitute(&self, _: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>) -> Vec<Self> {
        vec![self.clone()]
    }
}

impl Superset for syn::TypeParen {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions<'a>> {
        self.elem.is_superset(&other.elem)
    }
}

impl Substitute for syn::TypeParen {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.elem
            .substitute(substitutions)
            .into_iter()
            .map(Box::new)
            .map(|elem| Self {
                elem,
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::TypePath {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions<'a>> {
        self.qself
            .is_superset(&other.qself)?
            .merge(self.path.is_superset(&other.path)?)
    }
}

impl Substitute for syn::TypePath {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.qself
            .substitute(substitutions)
            .into_iter()
            .cartesian_product(self.path.substitute(substitutions))
            .map(|(qself, path)| syn::TypePath { path, qself })
            .collect()
    }
}

impl Superset for syn::TypePtr {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions<'a>> {
        if self.const_token != other.const_token || self.mutability != other.mutability {
            return None;
        }

        self.elem.is_superset(&other.elem)
    }
}

impl Substitute for syn::TypePtr {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.elem
            .substitute(substitutions)
            .into_iter()
            .map(Box::new)
            .map(|elem| Self {
                elem,
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::TypeReference {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions<'a>> {
        if self.mutability != other.mutability {
            return None;
        }

        self.lifetime
            .is_superset(&other.lifetime)?
            .merge(self.elem.is_superset(&other.elem)?)
    }
}

impl Substitute for syn::TypeReference {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.lifetime
            .substitute(substitutions)
            .into_iter()
            .cartesian_product(self.elem.substitute(substitutions))
            .map(|(lifetime, elem)| Self {
                lifetime,
                elem: Box::new(elem),
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::TypeParamBound {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions<'a>> {
        use syn::TypeParamBound::*;

        match (self, other) {
            (Trait(x1), Trait(x2)) => x1.is_superset(x2),
            (Lifetime(x1), Lifetime(x2)) => x1.is_superset(x2),
            (x1, x2) => (x1 == x2).then_some(Substitutions::default()),
        }
    }
}

impl Substitute for syn::TypeParamBound {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        use syn::TypeParamBound::*;

        match self {
            Trait(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Trait)
                .collect(),
            Lifetime(x1) => x1
                .substitute(substitutions)
                .into_iter()
                .map(Lifetime)
                .collect(),
            _ => vec![self.clone()],
        }
    }
}

impl Superset for syn::TypeSlice {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions<'a>> {
        self.elem.is_superset(&other.elem)
    }
}

impl Substitute for syn::TypeSlice {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.elem
            .substitute(substitutions)
            .into_iter()
            .map(Box::new)
            .map(|elem| Self {
                elem,
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::TypeTraitObject {
    fn is_superset<'a>(&'a self, _: &'a Self) -> Option<Substitutions<'a>> {
        unreachable!()
    }
}

impl Substitute for syn::TypeTraitObject {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        if self.bounds.is_empty() {
            return vec![self.clone()];
        }

        self.bounds
            .iter()
            .map(|bounds| bounds.substitute(substitutions))
            .multi_cartesian_product()
            .map(|bounds| Self {
                bounds: bounds.into_iter().collect(),
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::TypeTuple {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions<'a>> {
        if self.elems.len() != other.elems.len() {
            return None;
        }

        zip(&self.elems, &other.elems).try_fold(Substitutions::default(), |acc, (x1, x2)| {
            acc.merge(x1.is_superset(x2)?)
        })
    }
}

impl Substitute for syn::TypeTuple {
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

impl Superset for syn::ReturnType {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions<'a>> {
        use syn::ReturnType::*;

        match (&self, &other) {
            (Default, Default) => Some(Substitutions::default()),
            (Type(_, x1), Type(_, x2)) => x1.is_superset(x2),
            _ => None,
        }
    }
}

impl Substitute for syn::ReturnType {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        use syn::ReturnType::*;

        if let Type(r_arrow, x1) = &self {
            return x1
                .substitute(substitutions)
                .into_iter()
                .map(|sub| Type(*r_arrow, Box::new(sub)))
                .collect();
        }

        vec![self.clone()]
    }
}

impl Superset for syn::BareFnArg {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions<'a>> {
        if self.name != other.name {
            return None;
        }

        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.ty.is_superset(&other.ty)?)
    }
}

impl Substitute for syn::BareFnArg {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.ty
            .substitute(substitutions)
            .into_iter()
            .map(|ty| Self { ty, ..self.clone() })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use syn::parse_quote;

    use super::*;

    #[test]
    fn identity_type_superset() {
        let r: syn::Ident = syn::parse_quote!(_ŠČ);
        let substitutions = Substitutions::identity(&r);

        let t1: syn::Type = syn::parse_quote!(_ŠČ);
        assert_eq!(t1.is_superset(&t1), Some(substitutions.clone()));

        let t2: syn::Type = syn::parse_quote!(Vec<[_ŠČ; 2]>);
        assert_eq!(t2.is_superset(&t2), Some(substitutions.clone()));

        let t3: syn::Type = syn::parse_quote!(<Vec<[_ŠČ; 2]> as Deref>::Target);
        assert_eq!(t3.is_superset(&t3), Some(substitutions.clone()));
    }

    #[test]
    fn concrete_type_superset() {
        let substitutions = Substitutions::default();

        let t1: syn::Type = syn::parse_quote!(i32);
        assert_eq!(t1.is_superset(&t1), Some(substitutions.clone()));

        let t2: syn::Type = syn::parse_quote!(Vec<[i32; 2]>);
        assert_eq!(t2.is_superset(&t2), Some(substitutions.clone()));

        let t3: syn::Type = syn::parse_quote!(<Vec<[i32; 2]> as Deref>::Target);
        assert_eq!(t3.is_superset(&t3), Some(substitutions.clone()));
    }

    #[test]
    fn different_qself() {
        let a: syn::Type = syn::parse_quote!(<Option<_ŠČ> as Deref>::Target);
        let b: syn::Type = syn::parse_quote!(<Vec<_ŠČ> as Deref>::Target);

        assert!(a.is_superset(&b).is_none());
    }

    #[test]
    fn t_is_superset_of_vec_t() {
        let a: syn::Type = syn::parse_quote!(_ŠČ);
        let b: syn::Type = syn::parse_quote!(Vec<_ŠČ>);

        assert!(b.is_superset(&a).is_none());

        let k = parse_quote!(_ŠČ);
        let mut substitutions = IndexMap::new();
        substitutions.insert(&k, (&b).into());

        assert_eq!(a.is_superset(&b), Some(Substitutions(substitutions)));
    }

    #[test]
    fn superset_cannot_be_substituted() {
        // NOTE: Even though it looks 1st type is a supersets of the 2nd,
        // there is no valid substitution that converts one into the other
        let a: syn::Type = syn::parse_quote!((_ŠČ, Vec<_ŠČ>));
        let b: syn::Type = syn::parse_quote!((Vec<_ŠČ>, Vec<_ŠČ>));

        assert!(a.is_superset(&b).is_none());
        assert!(b.is_superset(&a).is_none());
    }

    #[test]
    fn identity_type_substitute() {
        let substituted = (parse_quote!(Option<Vec<_ŠČ>>), parse_quote!(Dispatch));

        let t1: syn::Type = syn::parse_quote!(_ŠČ);
        assert!(
            t1.is_superset(&t1)
                .unwrap()
                .substitute(&substituted)
                .eq([substituted.clone()])
        );

        let t2: syn::Type = syn::parse_quote!(Vec<[_ŠČ; 2]>);
        assert!(
            t2.is_superset(&t2)
                .unwrap()
                .substitute(&substituted)
                .eq([substituted.clone()])
        );

        let t3: syn::Type = syn::parse_quote!(<Vec<[_ŠČ; 2]> as Deref>::Target);
        assert!(
            t3.is_superset(&t3)
                .unwrap()
                .substitute(&substituted)
                .eq([substituted.clone()])
        );
    }

    #[test]
    fn concrete_type_substitute() {
        let substituted = (parse_quote!(Option<Vec<i32>>), parse_quote!(Dispatch));

        let t1: syn::Type = syn::parse_quote!(i32);
        assert!(
            t1.is_superset(&t1)
                .unwrap()
                .substitute(&substituted)
                .eq([substituted.clone()])
        );

        let t2: syn::Type = syn::parse_quote!(Vec<[i32; 2]>);
        assert!(
            t2.is_superset(&t2)
                .unwrap()
                .substitute(&substituted)
                .eq([substituted.clone()])
        );

        let t3: syn::Type = syn::parse_quote!(<Vec<[i32; 2]> as Deref>::Target);
        assert!(
            t3.is_superset(&t3)
                .unwrap()
                .substitute(&substituted)
                .eq([substituted.clone()])
        );
    }

    #[test]
    fn substitute_vec_t_with_t() {
        let a: syn::Type = syn::parse_quote!(_ŠČ);
        let b = syn::parse_quote!(Vec<_ŠČ>);

        assert!(
            a.is_superset(&b)
                .unwrap()
                .substitute(&(parse_quote!(Option<Vec<_ŠČ>>), parse_quote!(Dispatch)))
                .eq([(parse_quote!(Option<_ŠČ>), parse_quote!(Dispatch))])
        );
    }

    #[test]
    fn multiple_valid_substitutions() {
        let a: syn::Type = syn::parse_quote!((_ŠČ0, _ŠČ1));
        let b = syn::parse_quote!((Vec<_ŠČ>, Vec<_ŠČ>));

        assert!(
            a.is_superset(&b)
                .unwrap()
                .substitute(&(
                    parse_quote!(Option<Vec<_ŠČ>>),
                    parse_quote!(Dispatch<Group = Vec<_ŠČ>>),
                ))
                .eq([
                    (parse_quote!(Option<_ŠČ0>), parse_quote!(Dispatch),),
                    (parse_quote!(Option<_ŠČ1>), parse_quote!(Dispatch),),
                ])
        );
    }
}
