use itertools::iproduct;

use super::*;

pub trait Superset: Eq {
    /// If `self` is a superset of `other`, returns substitutions that are required to convert
    /// `other` into `self`, otherwise returns `None`.
    ///
    /// If `self` and `other` are identical but are not concrete types (i.e. they are generic types)
    /// then returned substitutions contain identity mappings of type parameters (e.g. `T` -> `T`).
    ///
    /// If the substitutions are empty, then `self` and `other are identical concrete types.
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions>;
}

pub trait Substitute {
    /// Returns [`Vec<Self>`] where each element is a clone of the original but with a unique
    /// permutation of possible substitutions.
    ///
    /// An empty vector is never returned. If there is no valid substitution, returns the original value
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self>
    where
        Self: Sized;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SubstitutionValue<'a> {
    /// `T` -> `Vec<T>`
    Type(&'a syn::Type),
    /// `T` -> `T + 1`
    Expr(&'a syn::Expr),
    /// `T` -> `T`
    Identity,
}

impl<'a> From<&'a syn::Type> for SubstitutionValue<'a> {
    fn from(value: &'a syn::Type) -> Self {
        Self::Type(value)
    }
}

impl<'a> From<&'a syn::Expr> for SubstitutionValue<'a> {
    fn from(value: &'a syn::Expr) -> Self {
        Self::Expr(value)
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Substitutions<'a>(IndexMap<&'a syn::Ident, SubstitutionValue<'a>>);

impl<'a> Substitutions<'a> {
    fn new(source: &'a syn::Ident, dst: impl Into<SubstitutionValue<'a>>) -> Self {
        let mut substitutions = IndexMap::new();
        substitutions.insert(source, dst.into());

        Self(substitutions)
    }

    fn identity(source: &'a syn::Ident) -> Self {
        let mut substitutions = IndexMap::new();
        substitutions.insert(source, SubstitutionValue::Identity);

        Self(substitutions)
    }

    #[must_use]
    fn merge(mut self, other: Self) -> Option<Self> {
        for (ident, dst) in other.0.into_iter() {
            match self.0.entry(ident) {
                indexmap::map::Entry::Occupied(val) => {
                    if dst != *val.get() {
                        return None;
                    }
                }
                indexmap::map::Entry::Vacant(entry) => {
                    entry.insert(dst);
                }
            }
        }

        Some(self)
    }

    /// Return `true` if no substitution is required, i.e. this is an identity mapping
    pub fn is_eq(&self) -> bool {
        self.0.values().all(|&v| v == SubstitutionValue::Identity)
    }

    /// Return new trait bound where all types from the given trait bound, that are values in [`Self`],
    /// were replaced with corresponding mappings. Since each type can be replaced with multiple types,
    /// this functions returns all possible combinations.
    ///
    /// This function does reverse mapping, if you had mappings:
    ///     (`T` => `Vec<T>`, `U` => `Vec<T>`)
    /// the result of the substitution would be two possible trait bounds:
    /// * 1st where `Vec<T>` was replaced with `T`
    /// * 2nd where `Vec<T>` was replaced with `U`
    pub fn substitute(
        &self,
        trait_bound: &TraitBoundIdent,
    ) -> impl Iterator<Item = TraitBoundIdent> {
        let reverse_map =
            self.0
                .iter()
                .fold(IndexMap::<_, Vec<_>>::new(), |mut acc, (&source, &dst)| {
                    acc.entry(dst).or_default().push(source);
                    acc
                });

        // TODO: assert that there are no multiple `T` -> `fn(T)` reverse mappings

        let bounded = trait_bound
            .0
             .0
            .substitute(&reverse_map)
            .into_iter()
            .map(Bounded);
        let trait_ = trait_bound
            .1
             .0
            .substitute(&reverse_map)
            .into_iter()
            .map(TraitBound);

        bounded.cartesian_product(trait_)
    }
}

impl Superset for ImplGroupId {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        match (&self.0, &other.0) {
            (Some(x1), Some(x2)) => x1.is_superset(x2)?,
            (None, None) => Substitutions::default(),
            _ => return None,
        }
        .merge(self.1.is_superset(&other.1)?)
    }
}

impl Substitute for ImplGroupId {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.0
            .as_ref()
            .map_or_else(
                || vec![None],
                |trait_| {
                    trait_
                        .substitute(substitutions)
                        .into_iter()
                        .map(Some)
                        .collect()
                },
            )
            .into_iter()
            .cartesian_product(self.1.substitute(substitutions))
            .map(|(trait_, self_ty)| ImplGroupId(trait_, self_ty))
            .collect()
    }
}

impl<T: Superset> Superset for Option<T> {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        match (self, other) {
            (None, None) => Some(Substitutions::default()),
            (Some(x1), Some(x2)) => x1.is_superset(x2),
            _ => None,
        }
    }
}

impl<T: Substitute> Substitute for Option<T> {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.as_ref().map_or(vec![None], |x1| {
            x1.substitute(substitutions).into_iter().map(Some).collect()
        })
    }
}

impl Superset for Vec<syn::Attribute> {
    fn is_superset<'a>(&'a self, _: &'a Self) -> Option<Substitutions> {
        Some(Substitutions::default())
    }
}

impl Substitute for Vec<syn::Attribute> {
    fn substitute(&self, _: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>) -> Vec<Self> {
        vec![self.clone()]
    }
}

impl Superset for syn::Macro {
    fn is_superset(&self, other: &Self) -> Option<Substitutions> {
        (self == other).then_some(Substitutions::default())
    }
}

impl Substitute for syn::Macro {
    fn substitute(&self, _: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>) -> Vec<Self> {
        vec![self.clone()]
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

impl Superset for syn::Lifetime {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        const UNDERSCORE: &str = "_";

        if self.ident == UNDERSCORE || other.ident == UNDERSCORE {
            return Some(Substitutions::default());
        }

        (self.ident == other.ident).then_some(Substitutions::default())
    }
}

impl Substitute for syn::Lifetime {
    fn substitute(&self, _: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>) -> Vec<Self> {
        vec![self.clone()]
    }
}

impl Substitute for syn::Path {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.segments
            .iter()
            .map(|segment| {
                use syn::PathArguments;

                match &segment.arguments {
                    PathArguments::None => vec![PathArguments::None],
                    PathArguments::Parenthesized(x1) => {
                        let inputs = x1
                            .inputs
                            .iter()
                            .map(|input| input.substitute(substitutions))
                            .multi_cartesian_product();

                        let output = if let syn::ReturnType::Type(r_arrow, output) = &x1.output {
                            output
                                .substitute(substitutions)
                                .into_iter()
                                .map(|output| syn::ReturnType::Type(*r_arrow, Box::new(output)))
                                .collect::<Vec<_>>()
                        } else {
                            vec![syn::ReturnType::Default]
                        };

                        inputs
                            .cartesian_product(output)
                            .map(|(inputs, output)| syn::ParenthesizedGenericArguments {
                                paren_token: x1.paren_token,
                                inputs: inputs.into_iter().collect(),
                                output,
                            })
                            .map(PathArguments::Parenthesized)
                            .collect::<Vec<_>>()
                    }
                    PathArguments::AngleBracketed(x1) => x1
                        .substitute(substitutions)
                        .into_iter()
                        .map(PathArguments::AngleBracketed)
                        .collect::<Vec<_>>(),
                }
                .into_iter()
                .map(|arguments| syn::PathSegment {
                    ident: segment.ident.clone(),
                    arguments,
                })
            })
            .multi_cartesian_product()
            .map(|segments| syn::Path {
                leading_colon: self.leading_colon,
                segments: segments.into_iter().collect(),
            })
            .collect()
    }
}

impl Superset for syn::QSelf {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        let substitutions = self.ty.is_superset(&other.ty)?;

        if self.position != other.position || self.as_token != other.as_token {
            return None;
        }

        if substitutions.is_eq() {
            // NOTE: It's not possible to tell whether 2 associated types overlap or not
            // Is <Option<T> as Deref>::Target a superset of <Vec<T> as Deref>::Target?
            return Some(substitutions);
        }

        None
    }
}

impl Substitute for syn::QSelf {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.ty
            .substitute(substitutions)
            .into_iter()
            .map(|ty| Self {
                ty: Box::new(ty),
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::Type {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
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
                    if let syn::Type::Path(other) = other {
                        if Some(ident) == matches_param_ident(&other.path) {
                            return Some(Substitutions::identity(ident));
                        }
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
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
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
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
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
        iproduct!(
            self.lifetimes.substitute(substitutions),
            self.inputs
                .iter()
                .map(|input| input.substitute(substitutions))
                .multi_cartesian_product(),
            self.output.substitute(substitutions)
        )
        .map(|(lifetimes, inputs, output)| Self {
            lifetimes,
            inputs: inputs.into_iter().collect(),
            output,
            ..self.clone()
        })
        .collect()
    }
}

impl Superset for syn::TypeGroup {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
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
    fn is_superset<'a>(&'a self, _: &'a Self) -> Option<Substitutions> {
        Some(Substitutions::default())
    }
}

impl Substitute for syn::TypeInfer {
    fn substitute(&self, _: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>) -> Vec<Self> {
        vec![self.clone()]
    }
}

impl Superset for syn::TypeImplTrait {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.bounds.len() != other.bounds.len() {
            return None;
        }

        // TODO: Order of traits doesn't matter here
        // They don't even have to have the same size or associated types
        // trait Kita {
        //      fn kita(a: impl Clone + Kara<Bound = Self> + Kara);
        // }
        // impl Kita for i32 {
        //     fn kita(a: impl Kara + Clone) {}
        // }
        zip(&self.bounds, &other.bounds).try_fold(Substitutions::default(), |acc, (x1, x2)| {
            acc.merge(x1.is_superset(x2)?)
        })
    }
}

impl Substitute for syn::TypeImplTrait {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
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
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
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
    fn is_superset<'a>(&'a self, _: &'a Self) -> Option<Substitutions> {
        Some(Substitutions::default())
    }
}

impl Substitute for syn::TypeNever {
    fn substitute(&self, _: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>) -> Vec<Self> {
        vec![self.clone()]
    }
}

impl Superset for syn::TypeParen {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
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
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
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
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
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
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
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
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
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
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
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
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.bounds.len() != other.bounds.len() {
            return None;
        }

        // TODO: Does order of trait bounds matter here? Take a look at TypeImplTrait
        zip(&self.bounds, &other.bounds).try_fold(Substitutions::default(), |acc, (x1, x2)| {
            acc.merge(x1.is_superset(x2)?)
        })
    }
}

impl Substitute for syn::TypeTraitObject {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
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
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
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

impl Superset for syn::BareFnArg {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
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

impl Superset for syn::TraitBound {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.modifier != other.modifier {
            return None;
        }

        self.lifetimes
            .is_superset(&other.lifetimes)?
            .merge(self.path.is_superset(&other.path)?)
    }
}

impl Substitute for syn::TraitBound {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.lifetimes
            .substitute(substitutions)
            .into_iter()
            .cartesian_product(self.path.substitute(substitutions))
            .map(|(lifetimes, path)| Self {
                lifetimes,
                path,
                ..self.clone()
            })
            .collect()
    }
}

impl Superset for syn::ReturnType {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
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

impl Superset for syn::BoundLifetimes {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.lifetimes.len() != other.lifetimes.len() {
            return None;
        }

        zip(&self.lifetimes, &other.lifetimes)
            .try_fold(Substitutions::default(), |acc, (x1, x2)| {
                acc.merge(x1.is_superset(x2)?)
            })
    }
}

impl Substitute for syn::BoundLifetimes {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        self.lifetimes
            .iter()
            .map(|l| l.substitute(substitutions))
            .multi_cartesian_product()
            .map(|lifetimes| Self {
                lifetimes: lifetimes.into_iter().collect(),
                ..self.clone()
            })
            .collect()
    }
}

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
        self.func
            .substitute(substitutions)
            .into_iter()
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
        iproduct!(
            self.lifetimes.substitute(substitutions),
            self.inputs
                .iter()
                .map(|input| input.substitute(substitutions))
                .multi_cartesian_product(),
            self.output.substitute(substitutions),
            self.body.substitute(substitutions)
        )
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
        self.expr
            .substitute(substitutions)
            .into_iter()
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
        self.receiver
            .substitute(substitutions)
            .into_iter()
            .cartesian_product(self.turbofish.substitute(substitutions))
            .cartesian_product(
                self.args
                    .iter()
                    .map(|arg| arg.substitute(substitutions))
                    .multi_cartesian_product(),
            )
            .map(|((receiver, turbofish), args)| Self {
                receiver: Box::new(receiver),
                turbofish,
                args: args.into_iter().collect(),
                ..self.clone()
            })
            .collect()
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
        iproduct!(
            self.qself.substitute(substitutions),
            self.path.substitute(substitutions),
            self.fields
                .iter()
                .map(|field| field.substitute(substitutions))
                .multi_cartesian_product(),
            self.rest.as_ref().map_or(vec![None], |rest| rest
                .substitute(substitutions)
                .into_iter()
                .map(Box::new)
                .map(Some)
                .collect())
        )
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

impl Superset for syn::ExprLit {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.lit.is_superset(&other.lit)?)
    }
}

impl Superset for syn::LocalInit {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.expr
            .is_superset(&other.expr)?
            .merge(match (&self.diverge, &other.diverge) {
                (Some((_, x1)), Some((_, x2))) => x1.is_superset(x2)?,
                (None, None) => Substitutions::default(),
                _ => return None,
            })
    }
}

impl Superset for syn::Local {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.pat.is_superset(&other.pat)?)?
            .merge(self.init.is_superset(&other.init)?)
    }
}

impl Superset for syn::StmtMacro {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.mac.is_superset(&other.mac)?)
    }
}

impl Superset for syn::Stmt {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        use syn::Stmt::*;
        match (self, other) {
            (Item(_), _) | (_, Item(_)) => unimplemented!(),
            (Local(x1), Local(x2)) => x1.is_superset(x2),
            (Expr(x1, _), Expr(x2, _)) => x1.is_superset(x2),
            (Macro(x1), Macro(x2)) => x1.is_superset(x2),
            _ => None,
        }
    }
}

impl Superset for syn::Block {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.stmts.len() != other.stmts.len() {
            return None;
        }

        zip(&self.stmts, &other.stmts).try_fold(Substitutions::default(), |acc, (x1, x2)| {
            acc.merge(x1.is_superset(x2)?)
        })
    }
}

impl Substitute for syn::Block {
    fn substitute(&self, _: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>) -> Vec<Self> {
        unimplemented!()
    }
}

impl Superset for syn::Pat {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        use syn::Pat::*;
        match (self, other) {
            (Const(x1), Const(x2)) => x1.is_superset(x2),
            (Ident(x1), Ident(x2)) => x1.is_superset(x2),
            (Lit(x1), Lit(x2)) => x1.is_superset(x2),
            (Macro(x1), Macro(x2)) => x1.is_superset(x2),
            (Or(x1), Or(x2)) => x1.is_superset(x2),

            (Paren(x1), x2) => x1.pat.is_superset(x2),
            (x1, Paren(x2)) => x1.is_superset(&x2.pat),

            (Path(x1), Path(x2)) => x1.is_superset(x2),
            (Range(x1), Range(x2)) => x1.is_superset(x2),
            (Reference(x1), Reference(x2)) => x1.is_superset(x2),
            (Rest(x1), Rest(x2)) => x1.is_superset(x2),
            (Slice(x1), Slice(x2)) => x1.is_superset(x2),
            (Struct(x1), Struct(x2)) => x1.is_superset(x2),
            (Tuple(x1), Tuple(x2)) => x1.is_superset(x2),
            (TupleStruct(x1), TupleStruct(x2)) => x1.is_superset(x2),
            (Type(x1), Type(x2)) => x1.is_superset(x2),
            (Wild(_), _) | (_, Wild(_)) => Some(Substitutions::default()),

            (x1, x2) => (x1 == x2).then_some(Substitutions::default()),
        }
    }
}

impl Substitute for syn::Pat {
    fn substitute(&self, _: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>) -> Vec<Self> {
        unimplemented!()
    }
}

impl Superset for syn::PatIdent {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.by_ref != other.by_ref
            || self.mutability != other.mutability
            || self.ident != other.ident
        {
            return None;
        }

        self.attrs
            .is_superset(&other.attrs)?
            .merge(match (&self.subpat, &other.subpat) {
                (Some((_, x1)), Some((_, x2))) => x1.is_superset(x2)?,
                (None, None) => Substitutions::default(),
                _ => return None,
            })
    }
}

impl Superset for syn::PatOr {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs.is_superset(&other.attrs)?.merge(
            zip(&self.cases, &other.cases)
                .try_fold(Substitutions::default(), |acc, (x1, x2)| {
                    acc.merge(x1.is_superset(x2)?)
                })?,
        )
    }
}

impl Superset for syn::PatParen {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.pat.is_superset(&other.pat)?)
    }
}

impl Superset for syn::PatReference {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.mutability != other.mutability {
            return None;
        }

        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.pat.is_superset(&other.pat)?)
    }
}

impl Superset for syn::PatRest {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs.is_superset(&other.attrs)
    }
}

impl Superset for syn::PatStruct {
    fn is_superset<'a>(&'a self, _: &'a Self) -> Option<Substitutions> {
        unimplemented!()
    }
}

impl Superset for syn::PatSlice {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs.is_superset(&other.attrs)?.merge(
            zip(&self.elems, &other.elems)
                .try_fold(Substitutions::default(), |acc, (x1, x2)| {
                    acc.merge(x1.is_superset(x2)?)
                })?,
        )
    }
}

impl Superset for syn::PatTuple {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs.is_superset(&other.attrs)?.merge(
            zip(&self.elems, &other.elems)
                .try_fold(Substitutions::default(), |acc, (x1, x2)| {
                    acc.merge(x1.is_superset(x2)?)
                })?,
        )
    }
}

impl Superset for syn::PatTupleStruct {
    fn is_superset<'a>(&'a self, _: &'a Self) -> Option<Substitutions> {
        unimplemented!()
    }
}

impl Superset for syn::PatType {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.pat.is_superset(&other.pat)?)?
            .merge(self.ty.is_superset(&other.ty)?)
    }
}

impl Superset for syn::PatWild {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs.is_superset(&other.attrs)
    }
}

impl Superset for syn::Lit {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        (self == other).then_some(Substitutions::default())
    }
}

impl Substitute for syn::Lit {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        if let Some(subs) = substitutions.get(&SubstitutionValue::Expr(&syn::parse_quote!(#self))) {
            return subs.iter().map(|sub| syn::parse_quote!(#sub)).collect();
        }

        vec![self.clone()]
    }
}

impl Superset for syn::LitStr {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        (self == other).then_some(Substitutions::default())
    }
}

impl Substitute for syn::LitStr {
    fn substitute(&self, _: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>) -> Vec<Self> {
        vec![self.clone()]
    }
}

impl Superset for syn::Path {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.segments.len() != other.segments.len() {
            return None;
        }

        if let Some(ident) = matches_param_ident(self) {
            if Some(ident) == matches_param_ident(other) {
                return Some(Substitutions::identity(ident));
            }
        }

        let mut substitutions = Substitutions::default();
        for (x1, x2) in zip(&self.segments, &other.segments) {
            if x1.ident != x2.ident {
                return None;
            }

            use syn::PathArguments;
            match (&x1.arguments, &x2.arguments) {
                (PathArguments::None, PathArguments::None) => {}
                (PathArguments::Parenthesized(x1), PathArguments::Parenthesized(x2)) => {
                    if x1.inputs.len() != x2.inputs.len() {
                        return None;
                    }

                    substitutions = zip(&x1.inputs, &x2.inputs)
                        .try_fold(substitutions, |acc, (x1, x2)| {
                            acc.merge(x1.is_superset(x2)?)
                        })?;

                    use syn::ReturnType::*;
                    match (&x1.output, &x2.output) {
                        (Type(_, x1), Type(_, x2)) => {
                            substitutions = substitutions.merge(x1.is_superset(x2)?)?;
                        }
                        (Default, Default) => {}
                        _ => return None,
                    }
                }
                (PathArguments::AngleBracketed(x1), PathArguments::AngleBracketed(x2)) => {
                    substitutions = substitutions.merge(x1.is_superset(x2)?)?;
                }
                _ => return None,
            }
        }

        Some(substitutions)
    }
}

impl Superset for syn::AssocType {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.ident != other.ident {
            return None;
        }

        match (&self.generics, &other.generics) {
            (Some(x1), Some(x2)) => x1.is_superset(x2)?,
            (None, None) => Substitutions::default(),
            _ => return None,
        }
        .merge(self.ty.is_superset(&other.ty)?)
    }
}

impl Superset for syn::AssocConst {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.ident != other.ident {
            return None;
        }

        match (&self.generics, &other.generics) {
            (Some(x1), Some(x2)) => x1.is_superset(x2)?,
            (None, None) => Substitutions::default(),
            _ => return None,
        }
        .merge(self.value.is_superset(&other.value)?)
    }
}

impl Superset for syn::GenericParam {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        use syn::GenericParam::*;

        match (self, other) {
            (Lifetime(x1), Lifetime(x2)) => x1.is_superset(x2),
            (Type(x1), Type(x2)) => x1.is_superset(x2),
            (Const(x1), Const(x2)) => x1.is_superset(x2),
            _ => None,
        }
    }
}

impl Substitute for syn::GenericParam {
    fn substitute(&self, _: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>) -> Vec<Self> {
        unimplemented!()
    }
}

impl Superset for syn::LifetimeParam {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.lifetime.is_superset(&other.lifetime)?)
    }
}

impl Superset for syn::TypeParam {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.ident != other.ident || self.bounds.len() != other.bounds.len() {
            return None;
        }

        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.default.is_superset(&other.default)?)?
            .merge(
                // TODO: Order is irrelevant here. Take a look at TypeImplTrait
                zip(&self.bounds, &other.bounds)
                    .try_fold(Substitutions::default(), |acc, (x1, x2)| {
                        acc.merge(x1.is_superset(x2)?)
                    })?,
            )
    }
}

impl Superset for syn::ConstParam {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.ident != other.ident {
            return None;
        }

        self.attrs
            .is_superset(&other.attrs)?
            .merge(self.ty.is_superset(&other.ty)?)?
            .merge(match (&self.default, &other.default) {
                (Some(x1), Some(x2)) => x1.is_superset(x2)?,
                (None, None) => Substitutions::default(),
                _ => return None,
            })
    }
}

impl Superset for syn::AngleBracketedGenericArguments {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.args.len() != other.args.len() {
            return None;
        }

        use syn::GenericArgument::*;
        zip(&self.args, &other.args).try_fold(Substitutions::default(), |acc, (x1, x2)| {
            match (x1, x2) {
                (Type(syn::Type::Path(x1)), Const(x2)) => {
                    if let Some(ident) = matches_param_ident(&x1.path) {
                        return acc.merge(Substitutions::new(ident, x2));
                    }

                    None
                }

                (Lifetime(x1), Lifetime(x2)) => x1.is_superset(x2),
                (Type(x1), Type(x2)) => acc.merge(x1.is_superset(x2)?),
                (Const(x1), Const(x2)) => acc.merge(x1.is_superset(x2)?),
                (AssocType(x1), AssocType(x2)) => x1.is_superset(x2),
                (AssocConst(x1), AssocConst(x2)) => x1.is_superset(x2),

                (x1, x2) => (x1 == x2).then_some(Substitutions::default()),
            }
        })
    }
}

impl Substitute for syn::AngleBracketedGenericArguments {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        use syn::GenericArgument::*;

        self.args
            .iter()
            .map(|arg| match arg {
                Lifetime(x1) => vec![Lifetime(x1.clone())],
                Type(x1) => x1.substitute(substitutions).into_iter().map(Type).collect(),
                Const(x1) => x1
                    .substitute(substitutions)
                    .into_iter()
                    .map(Const)
                    .collect(),
                AssocType(x1) => {
                    let ty = x1.ty.substitute(substitutions);

                    if let Some(generics) = &x1.generics {
                        generics
                            .substitute(substitutions)
                            .into_iter()
                            .cartesian_product(ty)
                            .map(|(generics, ty)| syn::AssocType {
                                ident: x1.ident.clone(),
                                generics: Some(generics),
                                eq_token: x1.eq_token,
                                ty,
                            })
                            .map(AssocType)
                            .collect::<Vec<_>>()
                    } else {
                        ty.into_iter()
                            .map(|ty| syn::AssocType {
                                ident: x1.ident.clone(),
                                generics: None,
                                eq_token: x1.eq_token,
                                ty,
                            })
                            .map(AssocType)
                            .collect::<Vec<_>>()
                    }
                }
                AssocConst(x1) => {
                    let value = x1.value.substitute(substitutions);

                    if let Some(generics) = &x1.generics {
                        generics
                            .substitute(substitutions)
                            .into_iter()
                            .cartesian_product(value)
                            .map(|(generics, value)| syn::AssocConst {
                                ident: x1.ident.clone(),
                                generics: Some(generics),
                                eq_token: x1.eq_token,
                                value,
                            })
                            .map(AssocConst)
                            .collect::<Vec<_>>()
                    } else {
                        value
                            .into_iter()
                            .map(|value| syn::AssocConst {
                                ident: x1.ident.clone(),
                                generics: None,
                                eq_token: x1.eq_token,
                                value,
                            })
                            .map(AssocConst)
                            .collect::<Vec<_>>()
                    }
                }
                _ => unimplemented!(),
            })
            .multi_cartesian_product()
            .map(|args| Self {
                args: args.into_iter().collect(),
                ..self.clone()
            })
            .collect::<Vec<_>>()
    }
}

fn matches_param_ident(path: &syn::Path) -> Option<&syn::Ident> {
    if let Some(ident) = path.get_ident() {
        if ident.to_string().starts_with("_ŠČ") {
            return Some(ident);
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use syn::parse_quote;

    use super::*;

    #[test]
    fn param_identity_superset() {
        let r: syn::Ident = syn::parse_quote!(_ŠČ);
        let substitutions = Substitutions::identity(&r);

        let p1: syn::Path = syn::parse_quote!(_ŠČ);
        assert_eq!(p1.is_superset(&p1), Some(substitutions.clone()));

        let p2: syn::Path = syn::parse_quote!(Vec<[_ŠČ; 2]>);
        assert_eq!(p2.is_superset(&p2), Some(substitutions.clone()));

        let p3: syn::Path = syn::parse_quote!(Vec<[_ŠČ; 2]>::Target);
        assert_eq!(p3.is_superset(&p3), Some(substitutions.clone()));

        let t1: syn::Type = syn::parse_quote!(_ŠČ);
        assert_eq!(t1.is_superset(&t1), Some(substitutions.clone()));

        let t2: syn::Type = syn::parse_quote!(Vec<[_ŠČ; 2]>);
        assert_eq!(t2.is_superset(&t2), Some(substitutions.clone()));

        let t3: syn::Type = syn::parse_quote!(<Vec<[_ŠČ; 2]> as Deref>::Target);
        assert_eq!(t3.is_superset(&t3), Some(substitutions.clone()));

        let e1: syn::Expr = syn::parse_quote!(_ŠČ);
        assert_eq!(e1.is_superset(&e1), Some(substitutions.clone()));

        // FIXME: Macro like `vec![]` doesn't work saddly
        let e2: syn::Expr = syn::parse_quote!([_ŠČ + 1, String::new()]);
        assert_eq!(e2.is_superset(&e2), Some(substitutions.clone()));

        let e3: syn::Expr = syn::parse_quote!(kita((_ŠČ + 1, String::new())));
        assert_eq!(e3.is_superset(&e3), Some(substitutions.clone()));
    }

    #[test]
    fn concrete_identity_superset() {
        let substitutions = Substitutions::default();

        let p1: syn::Path = syn::parse_quote!(i32);
        assert_eq!(p1.is_superset(&p1), Some(substitutions.clone()));

        let p2: syn::Path = syn::parse_quote!(Vec<[i32; 2]>);
        assert_eq!(p2.is_superset(&p2), Some(substitutions.clone()));

        let p3: syn::Path = syn::parse_quote!(Vec<[i32; 2]>::Target);
        assert_eq!(p3.is_superset(&p3), Some(substitutions.clone()));

        let t1: syn::Type = syn::parse_quote!(i32);
        assert_eq!(t1.is_superset(&t1), Some(substitutions.clone()));

        let t2: syn::Type = syn::parse_quote!(Vec<[i32; 2]>);
        assert_eq!(t2.is_superset(&t2), Some(substitutions.clone()));

        let t3: syn::Type = syn::parse_quote!(<Vec<[i32; 2]> as Deref>::Target);
        assert_eq!(t3.is_superset(&t3), Some(substitutions.clone()));

        let e1: syn::Expr = syn::parse_quote!(420);
        assert_eq!(e1.is_superset(&e1), Some(substitutions.clone()));

        let e2: syn::Expr = syn::parse_quote!(vec![420 + 1, String::new()]);
        assert_eq!(e2.is_superset(&e2), Some(substitutions.clone()));

        let e3: syn::Expr = syn::parse_quote!(|m| 1 + String::new());
        assert_eq!(e3.is_superset(&e3), Some(substitutions.clone()));
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
    fn param_identity_substitute() {
        let substituted = (
            Bounded(parse_quote!(Option<Vec<_ŠČ>>)),
            TraitBound(parse_quote!(Dispatch)),
        );

        let p1: syn::Path = syn::parse_quote!(_ŠČ);
        let p2: syn::Path = syn::parse_quote!(Vec<[_ŠČ; 2]>);
        let p3: syn::Path = syn::parse_quote!(Vec<[_ŠČ; 2]>::Target);

        let t1: syn::Type = syn::parse_quote!(_ŠČ);
        let t2: syn::Type = syn::parse_quote!(Vec<[_ŠČ; 2]>);
        let t3: syn::Type = syn::parse_quote!(<Vec<[_ŠČ; 2]> as Deref>::Target);

        assert!(p1
            .is_superset(&p1)
            .unwrap()
            .substitute(&substituted)
            .eq([substituted.clone()]));
        assert!(p2
            .is_superset(&p2)
            .unwrap()
            .substitute(&substituted)
            .eq([substituted.clone()]));
        assert!(p3
            .is_superset(&p3)
            .unwrap()
            .substitute(&substituted)
            .eq([substituted.clone()]));

        assert!(t1
            .is_superset(&t1)
            .unwrap()
            .substitute(&substituted)
            .eq([substituted.clone()]));
        assert!(t2
            .is_superset(&t2)
            .unwrap()
            .substitute(&substituted)
            .eq([substituted.clone()]));
        assert!(t3
            .is_superset(&t3)
            .unwrap()
            .substitute(&substituted)
            .eq([substituted.clone()]));
    }

    #[test]
    fn concrete_identity_substitute() {
        let substituted = (
            Bounded(parse_quote!(Option<Vec<i32>>)),
            TraitBound(parse_quote!(Dispatch)),
        );

        let p1: syn::Path = syn::parse_quote!(i32);
        let p2: syn::Path = syn::parse_quote!(Vec<[i32; 2]>);
        let p3: syn::Path = syn::parse_quote!(Vec<[i32; 2]>::Target);

        let t1: syn::Type = syn::parse_quote!(i32);
        let t2: syn::Type = syn::parse_quote!(Vec<[i32; 2]>);
        let t3: syn::Type = syn::parse_quote!(<Vec<[i32; 2]> as Deref>::Target);

        assert!(p1
            .is_superset(&p1)
            .unwrap()
            .substitute(&substituted)
            .eq([substituted.clone()]));
        assert!(p2
            .is_superset(&p2)
            .unwrap()
            .substitute(&substituted)
            .eq([substituted.clone()]));
        assert!(p3
            .is_superset(&p3)
            .unwrap()
            .substitute(&substituted)
            .eq([substituted.clone()]));

        assert!(t1
            .is_superset(&t1)
            .unwrap()
            .substitute(&substituted)
            .eq([substituted.clone()]));
        assert!(t2
            .is_superset(&t2)
            .unwrap()
            .substitute(&substituted)
            .eq([substituted.clone()]));
        assert!(t3
            .is_superset(&t3)
            .unwrap()
            .substitute(&substituted)
            .eq([substituted.clone()]));
    }

    #[test]
    fn substitute_vec_t_with_t() {
        let a: syn::Type = syn::parse_quote!(_ŠČ);
        let b = syn::parse_quote!(Vec<_ŠČ>);

        assert!(a
            .is_superset(&b)
            .unwrap()
            .substitute(&(
                Bounded(parse_quote!(Option<Vec<_ŠČ>>)),
                TraitBound(parse_quote!(Dispatch)),
            ))
            .eq([(
                Bounded(parse_quote!(Option<_ŠČ>)),
                TraitBound(parse_quote!(Dispatch)),
            )]));
    }

    #[test]
    fn multiple_valid_substitutions() {
        let a: syn::Type = syn::parse_quote!((_ŠČ0, _ŠČ1));
        let b = syn::parse_quote!((Vec<_ŠČ>, Vec<_ŠČ>));

        assert!(a
            .is_superset(&b)
            .unwrap()
            .substitute(&(
                Bounded(parse_quote!(Option<Vec<_ŠČ>>)),
                TraitBound(parse_quote!(Dispatch<Group = Vec<_ŠČ>>)),
            ))
            .eq([
                (
                    Bounded(parse_quote!(Option<_ŠČ0>)),
                    TraitBound(parse_quote!(Dispatch<Group = _ŠČ0>)),
                ),
                (
                    Bounded(parse_quote!(Option<_ŠČ0>)),
                    TraitBound(parse_quote!(Dispatch<Group = _ŠČ1>)),
                ),
                (
                    Bounded(parse_quote!(Option<_ŠČ1>)),
                    TraitBound(parse_quote!(Dispatch<Group = _ŠČ0>)),
                ),
                (
                    Bounded(parse_quote!(Option<_ŠČ1>)),
                    TraitBound(parse_quote!(Dispatch<Group = _ŠČ1>)),
                )
            ]));
    }
}
