use super::*;

impl Generalize for syn::Type {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        use syn::Type::*;

        let res = match (self, other) {
            (Array(x1), Array(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Array),
            (BareFn(x1), BareFn(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(BareFn),
            (Group(x1), Group(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Group),
            (ImplTrait(x1), ImplTrait(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(ImplTrait),
            (Infer(_), Infer(_)) => Some(self.clone()),
            (x1, Infer(_)) => x1.generalize(x1, params1, params1, substitutions),
            (Infer(_), x2) => x2.generalize(x2, params2, params2, substitutions),
            (Macro(x1), Macro(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Macro),
            (Never(x1), Never(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Never),
            (x1, Paren(x2)) => x1.generalize(&*x2.elem, params1, params2, substitutions),
            (Paren(x1), x2) => (*x1.elem).generalize(x2, params1, params2, substitutions),
            (Path(x1), _) if x1.path.get_ident().is_some_and(|i| params1.contains_key(i)) => {
                Some(substitutions.insert_type(self, other, params1, params2))
            }
            (_, Path(x2)) if x2.path.get_ident().is_some_and(|i| params2.contains_key(i)) => {
                Some(substitutions.insert_type(self, other, params1, params2))
            }
            (Path(x1), Path(x2)) => x1.generalize(x2, params1, params2, substitutions).map(Path),
            (Ptr(x1), Ptr(x2)) => x1.generalize(x2, params1, params2, substitutions).map(Ptr),
            (Reference(x1), Reference(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Reference),
            (Slice(x1), Slice(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Slice),
            (TraitObject(x1), TraitObject(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(TraitObject),
            (Tuple(x1), Tuple(x2)) => x1
                .generalize(x2, params1, params2, substitutions)
                .map(Tuple),
            _ => None,
        };

        res.or_else(|| Some(substitutions.insert_type(self, other, params1, params2)))
    }
}

impl Generalize for syn::TypeArray {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        let elem = self
            .elem
            .generalize(&other.elem, params1, params2, substitutions)?;

        let prev_expected_type = substitutions
            .curr_expr_expected_type
            .replace(parse_quote!(usize));
        let len = self
            .len
            .generalize(&other.len, params1, params2, substitutions)?;
        substitutions.curr_expr_expected_type = prev_expected_type;

        Some(Self {
            elem,
            len,
            ..self.clone()
        })
    }
}

impl Generalize for syn::TypeBareFn {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        if self.unsafety != other.unsafety || self.variadic != other.variadic {
            return None;
        }

        let abi = self
            .abi
            .generalize(&other.abi, params1, params2, substitutions)?;
        let inputs = self
            .inputs
            .generalize(&other.inputs, params1, params2, substitutions)?;
        let output = self
            .output
            .generalize(&other.output, params1, params2, substitutions)?;

        Some(Self {
            abi,
            inputs,
            output,
            ..self.clone()
        })
    }
}

impl Generalize for syn::TypeGroup {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        let elem = self
            .elem
            .generalize(&other.elem, params1, params2, substitutions)?;

        Some(Self {
            elem,
            ..self.clone()
        })
    }
}

impl Generalize for syn::TypeImplTrait {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        let bounds = self
            .bounds
            .generalize(&other.bounds, params1, params2, substitutions)?;

        Some(Self {
            bounds,
            ..self.clone()
        })
    }
}

impl Generalize for syn::TypeInfer {
    fn generalize(
        &self,
        _: &Self,
        _: &Params,
        _: &Params,
        _: &mut Generalizations<'_>,
    ) -> Option<Self> {
        Some(self.clone())
    }
}

impl Generalize for syn::TypeMacro {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        let mac = self
            .mac
            .generalize(&other.mac, params1, params2, substitutions)?;
        Some(Self { mac })
    }
}

impl Generalize for syn::TypeNever {
    fn generalize(
        &self,
        _other: &Self,
        _: &Params,
        _: &Params,
        _substitutions: &mut Generalizations<'_>,
    ) -> Option<Self> {
        Some(self.clone())
    }
}

impl Generalize for syn::TypePath {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        let qself = self
            .qself
            .generalize(&other.qself, params1, params2, substitutions)?;
        let path = self
            .path
            .generalize(&other.path, params1, params2, substitutions)?;

        Some(syn::TypePath { path, qself })
    }
}

impl Generalize for syn::TypePtr {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        if self.const_token != other.const_token || self.mutability != other.mutability {
            return None;
        }

        let elem = self
            .elem
            .generalize(&other.elem, params1, params2, substitutions)?;

        Some(Self {
            elem,
            ..self.clone()
        })
    }
}

impl Generalize for syn::TypeReference {
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
        let lifetime =
            self.lifetime
                .generalize(&other.lifetime, params1, params2, substitutions)?;

        if lifetime.is_some() {
            substitutions.curr_ref_lifetime.push((
                self.lifetime.as_ref().unwrap(),
                other.lifetime.as_ref().unwrap(),
            ));
        }

        let elem = self
            .elem
            .generalize(&other.elem, params1, params2, substitutions)?;

        if lifetime.is_some() {
            substitutions.curr_ref_lifetime.pop();
        }

        Some(Self {
            lifetime,
            elem,
            ..self.clone()
        })
    }
}

impl Generalize for syn::TypeSlice {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        let elem = self
            .elem
            .generalize(&other.elem, params1, params2, substitutions)?;

        Some(Self {
            elem,
            ..self.clone()
        })
    }
}

impl Generalize for syn::TypeTraitObject {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        let mut bounds = Punctuated::new();

        let self_bounds = self
            .bounds
            .iter()
            .filter_map(|b| {
                if let syn::TypeParamBound::Trait(bound) = b {
                    return Some(bound);
                }

                None
            })
            .collect::<Vec<_>>();

        let other_bounds = other
            .bounds
            .iter()
            .filter_map(|b| {
                if let syn::TypeParamBound::Trait(bound) = b {
                    return Some(bound);
                }

                None
            })
            .collect::<Vec<_>>();

        if self_bounds.len() != other_bounds.len() {
            return None;
        }

        for self_bound in self_bounds {
            let other_bound = other_bounds
                .iter()
                .find(|&&other_bound| other_bound == self_bound)?;

            let bound = self_bound
                .generalize(other_bound, params1, params2, substitutions)
                .unwrap();

            bounds.push(syn::TypeParamBound::Trait(bound));
        }

        Some(Self {
            bounds,
            ..self.clone()
        })
    }
}

impl Generalize for syn::TypeTuple {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        let elems = self
            .elems
            .generalize(&other.elems, params1, params2, substitutions)?;

        Some(Self {
            elems,
            ..self.clone()
        })
    }
}

impl Generalize for syn::ReturnType {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        use syn::ReturnType::*;

        match (self, other) {
            (Default, Default) => Some(self.clone()),
            (Type(arrow_token, x1), Type(_, x2)) => {
                let ty = x1.generalize(x2, params1, params2, substitutions)?;
                Some(Type(*arrow_token, ty))
            }
            _ => None,
        }
    }
}

impl Generalize for syn::BareFnArg {
    fn generalize<'a>(
        &'a self,
        other: &'a Self,
        params1: &Params,
        params2: &Params,
        substitutions: &mut Generalizations<'a>,
    ) -> Option<Self> {
        if self.name != other.name {
            return None;
        }

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

impl Generalize for syn::Abi {
    fn generalize(
        &self,
        other: &Self,
        _: &Params,
        _: &Params,
        _: &mut Generalizations<'_>,
    ) -> Option<Self> {
        let c_abi = syn::parse_quote!("C");

        match (&self.name, &other.name) {
            (Some(x1), Some(x2)) if x1 == x2 => Some(self.clone()),
            (Some(x1), None) if *x1 == c_abi => Some(self.clone()),
            (None, Some(x2)) if *x2 == c_abi => Some(other.clone()),
            (None, None) => None,
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use indexmap::indexmap;
    use syn::parse_quote;

    use super::*;

    #[test]
    fn identity_type_generalize() {
        let p1 = indexmap! {
            format_ident!("T") => GenericParam::Type(Sizedness::Sized, IndexSet::new())
        };

        let t1: syn::Type = parse_quote!(T);
        let mut subs1 = Generalizations::default();
        let ty = t1.generalize(&t1, &p1, &p1, &mut subs1).unwrap();

        let expected_ty = parse_quote!(_TŠČ0);

        assert_eq!(ty, expected_ty);
        assert!(subs1.lifetime_generalizations.is_empty());
        assert_eq!(
            subs1.type_generalizations,
            indexmap! {
                (&t1, &t1) => (Sizedness::Sized, IndexSet::new())
            }
        );
        assert!(subs1.expr_generalizations.is_empty());

        // ---

        let t2: syn::Type = parse_quote!(Vec<[T; 2]>);
        let mut subs2 = Generalizations::default();
        let ty = t2.generalize(&t2, &p1, &p1, &mut subs2).unwrap();

        let expected_ty_2: syn::Type = parse_quote!(Vec<[_TŠČ0; 2]>);

        assert_eq!(ty, expected_ty_2);
        assert!(subs2.lifetime_generalizations.is_empty());
        assert_eq!(
            subs2.type_generalizations,
            indexmap! {
                (&t1, &t1) => (Sizedness::Sized, IndexSet::new())
            }
        );
        assert!(subs2.expr_generalizations.is_empty());

        // ---

        let t3: syn::Type = parse_quote!(<Vec<[T; 2]> as Deref>::Target);
        let mut subs3 = Generalizations::default();
        let ty = t3.generalize(&t3, &p1, &p1, &mut subs3).unwrap();

        let expected_ty_3: syn::Type = parse_quote!(<Vec<[_TŠČ0; 2]> as Deref>::Target);

        assert_eq!(ty, expected_ty_3);
        assert!(subs3.lifetime_generalizations.is_empty());
        assert_eq!(
            subs3.type_generalizations,
            indexmap! {
                (&t1, &t1) => (Sizedness::Sized, IndexSet::new())
            }
        );
        assert!(subs3.expr_generalizations.is_empty());
    }

    #[test]
    fn concrete_type_generalize() {
        let p1 = indexmap! {};

        let t1: syn::Type = parse_quote!(i32);
        let mut subs1 = Generalizations::default();
        let ty = t1.generalize(&t1, &p1, &p1, &mut subs1).unwrap();

        assert_eq!(ty, t1);

        assert!(subs1.lifetime_generalizations.is_empty());
        assert!(subs1.type_generalizations.is_empty());
        assert!(subs1.expr_generalizations.is_empty());

        // ---

        let t2: syn::Type = parse_quote!(Vec<[i32; 2]>);
        let mut subs2 = Generalizations::default();
        let ty = t2.generalize(&t2, &p1, &p1, &mut subs2).unwrap();

        let expected_ty_2: syn::Type = parse_quote!(Vec<[i32; 2]>);

        assert_eq!(ty, expected_ty_2);

        assert!(subs2.lifetime_generalizations.is_empty());
        assert!(subs2.type_generalizations.is_empty());
        assert!(subs2.expr_generalizations.is_empty());

        // ---

        let t3: syn::Type = parse_quote!(<Vec<[i32; 2]> as Deref>::Target);
        let mut subs3 = Generalizations::default();
        let ty = t3.generalize(&t3, &p1, &p1, &mut subs3).unwrap();

        let expected_ty_3: syn::Type = parse_quote!(<Vec<[i32; 2]> as Deref>::Target);

        assert_eq!(ty, expected_ty_3);

        assert!(subs3.lifetime_generalizations.is_empty());
        assert!(subs3.type_generalizations.is_empty());
        assert!(subs3.expr_generalizations.is_empty());
    }

    #[test]
    fn different_qself() {
        let p = indexmap! {
            format_ident!("_ŠČ") => GenericParam::Type(Sizedness::Sized, IndexSet::new())
        };

        let a: syn::Type = parse_quote!(<Option<_ŠČ> as Deref>::Target);
        let b: syn::Type = parse_quote!(<Vec<_ŠČ> as Deref>::Target);

        let mut subs1 = Generalizations::default();
        let mut subs2 = Generalizations::default();

        let ty1 = a.generalize(&b, &p, &p, &mut subs1).unwrap();
        let ty2 = b.generalize(&a, &p, &p, &mut subs2).unwrap();

        let r1: syn::Type = parse_quote!(Option<_ŠČ>);
        let r2: syn::Type = parse_quote!(Vec<_ŠČ>);

        let expected_ty = parse_quote!(<_TŠČ0 as Deref>::Target);

        assert_eq!(ty1, expected_ty);
        assert_eq!(ty2, expected_ty);

        assert!(subs1.lifetime_generalizations.is_empty());
        assert!(subs2.lifetime_generalizations.is_empty());

        assert_eq!(
            subs1.type_generalizations,
            indexmap! { (&r1, &r2) => (Sizedness::Sized, IndexSet::new()) }
        );
        assert_eq!(
            subs2.type_generalizations,
            indexmap! { (&r2, &r1) => (Sizedness::Sized, IndexSet::new()) }
        );

        assert!(subs1.expr_generalizations.is_empty());
        assert!(subs2.expr_generalizations.is_empty());
    }

    #[test]
    fn t_and_vec_t() {
        let p = indexmap! {
            format_ident!("_ŠČ") => GenericParam::Type(Sizedness::Sized, IndexSet::new())
        };

        let a: syn::Type = parse_quote!(_ŠČ);
        let b: syn::Type = parse_quote!(Vec<_ŠČ>);

        let mut subs1 = Generalizations::default();
        let mut subs2 = Generalizations::default();

        let ty1 = a.generalize(&b, &p, &p, &mut subs1).unwrap();
        let ty2 = b.generalize(&a, &p, &p, &mut subs2).unwrap();

        let expected_ty = parse_quote!(_TŠČ0);

        assert_eq!(ty1, expected_ty);
        assert_eq!(ty2, expected_ty);

        assert!(subs1.lifetime_generalizations.is_empty());
        assert!(subs2.lifetime_generalizations.is_empty());

        assert_eq!(
            subs1.type_generalizations,
            indexmap! { (&a, &b) => (Sizedness::Sized, IndexSet::new()) }
        );
        assert_eq!(
            subs2.type_generalizations,
            indexmap! { (&b, &a) => (Sizedness::Sized, IndexSet::new()) }
        );

        assert!(subs1.expr_generalizations.is_empty());
        assert!(subs2.expr_generalizations.is_empty());
    }

    #[test]
    fn common_ident_generalize() {
        let p = indexmap! {
            format_ident!("_ŠČ") => GenericParam::Type(Sizedness::Sized, IndexSet::new())
        };

        let a: syn::Type = parse_quote!((_ŠČ, Vec<_ŠČ>));
        let b: syn::Type = parse_quote!((Vec<_ŠČ>, Vec<_ŠČ>));

        let mut subs1 = Generalizations::default();
        let mut subs2 = Generalizations::default();

        let ty1 = a.generalize(&b, &p, &p, &mut subs1).unwrap();
        let ty2 = b.generalize(&a, &p, &p, &mut subs2).unwrap();

        let r1: syn::Type = parse_quote!(_ŠČ);
        let r2: syn::Type = parse_quote!(Vec<_ŠČ>);

        let expected_ty = parse_quote!((_TŠČ0, Vec<_TŠČ1>));

        assert_eq!(ty1, expected_ty);
        assert_eq!(ty2, expected_ty);

        assert!(subs1.lifetime_generalizations.is_empty());
        assert!(subs2.lifetime_generalizations.is_empty());

        assert_eq!(
            subs1.type_generalizations,
            indexmap! {
                (&r1, &r2) => (Sizedness::Sized, IndexSet::new()),
                (&r1, &r1) => (Sizedness::Sized, IndexSet::new()),
            }
        );
        assert_eq!(
            subs2.type_generalizations,
            indexmap! {
                (&r2, &r1) => (Sizedness::Sized, IndexSet::new()),
                (&r1, &r1) => (Sizedness::Sized, IndexSet::new()),
            }
        );

        assert!(subs1.expr_generalizations.is_empty());
        assert!(subs2.expr_generalizations.is_empty());
    }

    #[test]
    fn type_full_generalization() {
        let p = indexmap! {
            format_ident!("_ŠČ0") => GenericParam::Type(Sizedness::Sized, IndexSet::new()),
            format_ident!("_ŠČ1") => GenericParam::Type(Sizedness::Sized, IndexSet::new()),
        };

        let l1: syn::Type = parse_quote!((_ŠČ0, _ŠČ1));
        let l2: syn::Type = parse_quote!((Vec<_ŠČ0>, Vec<_ŠČ1>));

        let mut subs1 = Generalizations::default();
        let mut subs2 = Generalizations::default();

        let ty1 = l1.generalize(&l2, &p, &p, &mut subs1).unwrap();
        let ty2 = l2.generalize(&l1, &p, &p, &mut subs2).unwrap();

        let r1: syn::Type = parse_quote!(_ŠČ0);
        let r2: syn::Type = parse_quote!(_ŠČ1);
        let r3: syn::Type = parse_quote!(Vec<_ŠČ0>);
        let r4: syn::Type = parse_quote!(Vec<_ŠČ1>);

        let expected_ty = parse_quote!((_TŠČ0, _TŠČ1));

        assert_eq!(ty1, expected_ty);
        assert_eq!(ty2, expected_ty);

        assert!(subs1.lifetime_generalizations.is_empty());
        assert!(subs2.lifetime_generalizations.is_empty());

        assert_eq!(
            subs1.type_generalizations,
            indexmap! {
                (&r1, &r3) => (Sizedness::Sized, IndexSet::new()),
                (&r2, &r4) => (Sizedness::Sized, IndexSet::new()),
            }
        );
        assert_eq!(
            subs2.type_generalizations,
            indexmap! {
                (&r3, &r1) => (Sizedness::Sized, IndexSet::new()),
                (&r4, &r2) => (Sizedness::Sized, IndexSet::new()),
            }
        );

        assert!(subs1.expr_generalizations.is_empty());
        assert!(subs2.expr_generalizations.is_empty());
    }
}
