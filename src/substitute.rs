use super::*;

pub trait IsSuperset {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum SubstitutionValue<'a> {
    Type(&'a syn::Type),
    Expr(&'a syn::Expr),
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

#[derive(Debug, Default)]
pub struct Substitutions<'a>(IndexMap<&'a syn::Ident, SubstitutionValue<'a>>);

impl<'a> Substitutions<'a> {
    fn new(source: &'a syn::Ident, dst: impl Into<SubstitutionValue<'a>>) -> Self {
        let mut substitutions = IndexMap::new();
        substitutions.insert(source, dst.into());

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

    // TODO:
    //fn substitute(&self, trait_bound_ident: &TraitBoundIdent) -> impl Iterator<Item = TraitBoundIdent> {
    #[must_use]
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

trait Substitute: Sized {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self>;
}

impl Substitute for syn::Type {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        use syn::Type::*;

        let mut res = match self {
            Ptr(x1) => x1
                .elem
                .substitute(substitutions)
                .into_iter()
                .map(Box::new)
                .map(|elem| syn::TypePtr { elem, ..x1.clone() })
                .map(Ptr)
                .collect::<Vec<_>>(),
            Reference(x1) => x1
                .elem
                .substitute(substitutions)
                .into_iter()
                .map(Box::new)
                .map(|elem| syn::TypeReference { elem, ..x1.clone() })
                .map(Reference)
                .collect::<Vec<_>>(),
            Tuple(x1) => x1
                .elems
                .iter()
                .map(|elem| elem.substitute(substitutions))
                .multi_cartesian_product()
                .map(|elems| syn::TypeTuple {
                    elems: elems.into_iter().collect(),
                    ..x1.clone()
                })
                .map(Tuple)
                .collect::<Vec<_>>(),
            Array(x1) => x1
                .elem
                .substitute(substitutions)
                .into_iter()
                .map(Box::new)
                .map(|elem| syn::TypeArray { elem, ..x1.clone() })
                .map(Array)
                .collect::<Vec<_>>(),
            Path(x1) => x1
                .path
                .substitute(substitutions)
                .into_iter()
                .map(|path| syn::TypePath { path, ..x1.clone() })
                .map(Path)
                .collect::<Vec<_>>(),
            BareFn(_x1) => {
                // TODO: compare properly
                vec![]
            }
            TraitObject(_x1) => {
                // TODO: How to compare trait objects?
                vec![]
            }
            Slice(x1) => x1
                .elem
                .substitute(substitutions)
                .into_iter()
                .map(Box::new)
                .map(|elem| syn::TypeSlice { elem, ..x1.clone() })
                .map(Slice)
                .collect::<Vec<_>>(),
            Paren(x1) => x1
                .elem
                .substitute(substitutions)
                .into_iter()
                .map(Box::new)
                .map(|elem| syn::TypeParen { elem, ..x1.clone() })
                .map(Paren)
                .collect::<Vec<_>>(),
            Group(x1) => x1
                .elem
                .substitute(substitutions)
                .into_iter()
                .map(Box::new)
                .map(|elem| syn::TypeGroup { elem, ..x1.clone() })
                .map(Group)
                .collect::<Vec<_>>(),
            ImplTrait(_) | Infer(_) => {
                // NOTE: Not possible in impl declaration
                unreachable!()
            }
            _ => vec![],
        };

        substitutions
            .get(&SubstitutionValue::Type(self))
            .map(|subs| res.extend(subs.iter().map(|sub| syn::parse_quote!(#sub))));

        res
    }
}

impl IsSuperset for syn::Type {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        use syn::Type::*;

        match (self, other) {
            (Ptr(x1), Ptr(x2)) => {
                if x1.const_token != x2.const_token || x1.mutability != x2.mutability {
                    // TODO: Can *mut T be considered a superset of *const T? Make a test
                    return None;
                }

                x1.elem.is_superset(&x2.elem)
            }
            (Reference(x1), Reference(x2)) => {
                if x1.lifetime != x2.lifetime {
                    // TODO: Can &mut T be considered a superset of &T? Make a test
                    // TODO: How do lifetimes come into picture for supersets? Make a test
                    return None;
                }
                if x1.mutability != x2.mutability {
                    return None;
                }

                x1.elem.is_superset(&x2.elem)
            }
            (Tuple(x1), Tuple(x2)) => {
                if x1.elems.len() != x2.elems.len() {
                    return None;
                }

                zip(&x1.elems, &x2.elems).try_fold(Substitutions::default(), |acc, (x1, x2)| {
                    acc.merge(x1.is_superset(x2)?)
                })
            }
            (Array(x1), Array(x2)) => {
                if x1.len != x2.len {
                    return None;
                }

                x1.elem.is_superset(&x2.elem)
            }
            (Path(x1), x2) => {
                if let Some(ident) = matches_param_ident(&x1.path) {
                    return Some(Substitutions::new(ident, x2));
                }

                if let syn::Type::Path(x2) = x2 {
                    // TODO: Equality is not well defined (for example `syn::TypeParen` or `syn::TypeGroup`)
                    if x1.qself != x2.qself {
                        // NOTE: It's not possible to tell whether 2 associated types overlap or not
                        // Is <Vec<T> as Deref>::Target a superset of <Option<T> as Deref>::Target?
                        return None;
                    }

                    return x1.path.is_superset(&x2.path);
                }

                None
            }
            (BareFn(x1), BareFn(x2)) => {
                // TODO: compare properly
                if x1 == x2 {
                    return Some(Substitutions::default());
                }

                None
            }
            (TraitObject(x1), TraitObject(x2)) => {
                // TODO: How to compare trait objects?
                if x1 == x2 {
                    return Some(Substitutions::default());
                }

                None
            }
            (Slice(x1), Slice(x2)) => x1.elem.is_superset(&x2.elem),
            (Paren(x1), x2) => x1.elem.is_superset(x2),
            (x1, Paren(x2)) => x1.is_superset(&x2.elem),
            (Group(x1), x2) => x1.elem.is_superset(x2),
            (x1, Group(x2)) => x1.is_superset(&x2.elem),
            (ImplTrait(_), ImplTrait(_)) | (Infer(_), Infer(_)) => {
                // NOTE: Not possible in impl declaration
                unreachable!()
            }
            (x1, x2) => (x1 == x2).then_some(Substitutions::default()),
        }
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

impl Substitute for syn::Expr {
    fn substitute(
        &self,
        substitutions: &IndexMap<SubstitutionValue, Vec<&syn::Ident>>,
    ) -> Vec<Self> {
        unimplemented!()
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
                leading_colon: self.leading_colon.clone(),
                segments: segments.into_iter().collect(),
            })
            .collect()
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
                    let generics = x1
                        .generics
                        .as_ref()
                        .map(|generics| generics.substitute(substitutions));
                    let ty = x1.ty.substitute(substitutions);

                    if let Some(generics) = generics {
                        generics
                            .into_iter()
                            .cartesian_product(ty)
                            .map(|(generics, ty)| syn::AssocType {
                                ident: x1.ident.clone(),
                                generics: Some(generics),
                                eq_token: x1.eq_token.clone(),
                                ty,
                            })
                            .map(AssocType)
                            .collect::<Vec<_>>()
                    } else {
                        ty.into_iter()
                            .map(|ty| syn::AssocType {
                                ident: x1.ident.clone(),
                                generics: None,
                                eq_token: x1.eq_token.clone(),
                                ty,
                            })
                            .map(AssocType)
                            .collect::<Vec<_>>()
                    }
                }
                AssocConst(_) | Constraint(_) => unimplemented!(),
                _ => unimplemented!(),
            })
            .multi_cartesian_product()
            .map(|args| syn::AngleBracketedGenericArguments {
                args: args.into_iter().collect(),
                ..self.clone()
            })
            .collect::<Vec<_>>()
    }
}

impl IsSuperset for syn::Path {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        if self.segments.len() != other.segments.len() {
            return None;
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

                    match (&x1.output, &x2.output) {
                        (syn::ReturnType::Default, syn::ReturnType::Default) => {}
                        (syn::ReturnType::Type(_, x1), syn::ReturnType::Type(_, x2)) => {
                            substitutions = substitutions.merge(x1.is_superset(x2)?)?;
                        }
                        _ => return None,
                    }
                }
                (PathArguments::AngleBracketed(x1), PathArguments::AngleBracketed(x2)) => {
                    if x1.args.len() != x2.args.len() {
                        return None;
                    }

                    use syn::GenericArgument::*;
                    substitutions = zip(&x1.args, &x2.args).try_fold(
                        substitutions,
                        |acc, (x1, x2)| match (x1, x2) {
                            (Lifetime(x1), Lifetime(x2)) => (x1 == x2).then_some(acc),
                            (Type(x1), Type(x2)) => acc.merge(x1.is_superset(x2)?),
                            (Const(x1), Const(x2)) => acc.merge(x1.is_superset(x2)?),
                            (Type(syn::Type::Path(x1)), Const(x2)) => {
                                if let Some(ident) = matches_param_ident(&x1.path) {
                                    return acc.merge(Substitutions::new(ident, x2));
                                }

                                None
                            }
                            (AssocType(_), _)
                            | (_, AssocType(_))
                            | (AssocConst(_), _)
                            | (_, AssocConst(_))
                            | (Constraint(_), _)
                            | (_, Constraint(_)) => unreachable!(),
                            _ => return None,
                        },
                    )?;
                }
                _ => return None,
            }
        }

        Some(substitutions)
    }
}

impl IsSuperset for syn::Expr {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        use syn::Expr::*;

        match (self, other) {
            (Array(x1), Array(x2)) => {
                if x1.elems.len() != x2.elems.len() {
                    return None;
                }

                zip(&x1.elems, &x2.elems).try_fold(Substitutions::default(), |acc, (x1, x2)| {
                    acc.merge(x1.is_superset(x2)?)
                })
            }
            (Assign(x1), Assign(x2)) => x1
                .left
                .is_superset(&x2.left)?
                .merge(x1.right.is_superset(&x2.right)?),
            (Binary(x1), Binary(x2)) => {
                if x1.op != x2.op {
                    return None;
                }

                x1.left
                    .is_superset(&x2.left)?
                    .merge(x1.right.is_superset(&x2.right)?)
            }
            (Break(x1), Break(x2)) => match (&x1.expr, &x2.expr) {
                (Some(x1), Some(x2)) => x1.is_superset(x2),
                (None, None) => Some(Substitutions::default()),
                _ => None,
            },
            (Call(x1), Call(x2)) => {
                if x1.args.len() != x2.args.len() {
                    return None;
                }

                zip(&x1.args, &x2.args).try_fold(x1.func.is_superset(&x2.func)?, |acc, (x1, x2)| {
                    acc.merge(x1.is_superset(x2)?)
                })
            }
            (Cast(x1), Cast(x2)) => x1
                .expr
                .is_superset(&x2.expr)?
                .merge(x1.ty.is_superset(&x2.ty)?),
            (Path(x1), x2) => {
                if let Some(ident) = matches_param_ident(&x1.path) {
                    return Some(Substitutions::new(ident, x2));
                }

                if let syn::Expr::Path(x2) = x2 {
                    // TODO: Equality is not well defined (for example `syn::TypeParen` or `syn::TypeGroup`)
                    if x1.qself != x2.qself {
                        // NOTE: It's not possible to tell whether 2 associated types overlap or not
                        // Is <Vec<T> as Deref>::Target a superset of <Option<T> as Deref>::Target?
                        return None;
                    }

                    return x1.path.is_superset(&x2.path);
                }

                None
            }
            (Return(x1), Return(x2)) => match (&x1.expr, &x2.expr) {
                (None, None) => Some(Substitutions::default()),
                (Some(x1), Some(x2)) => x1.is_superset(x2),
                _ => None,
            },
            (Tuple(x1), Tuple(x2)) => {
                if x1.elems.len() != x2.elems.len() {
                    return None;
                }

                zip(&x1.elems, &x2.elems).try_fold(Substitutions::default(), |acc, (x1, x2)| {
                    acc.merge(x1.is_superset(x2)?)
                })
            }
            (Continue(x1), Continue(x2)) => {
                if x1.label == x2.label {
                    return Some(Substitutions::default());
                }

                None
            }
            (Group(x1), x2) => x1.expr.is_superset(x2),
            (x1, Group(x2)) => x1.is_superset(&x2.expr),
            (Index(x1), Index(x2)) => x1
                .expr
                .is_superset(&x2.expr)?
                .merge(x1.index.is_superset(&x2.index)?),
            (Infer(x1), Infer(x2)) => Some(Substitutions::default()),
            (Field(x1), Field(x2)) => unimplemented!(),
            (ForLoop(x1), ForLoop(x2)) => unimplemented!(),
            (If(x1), If(x2)) => unimplemented!(),
            (Let(x1), Let(x2)) => unimplemented!(),
            (Loop(x1), Loop(x2)) => unimplemented!(),
            (Macro(x1), Macro(x2)) => unimplemented!(),
            (Match(x1), Match(x2)) => unimplemented!(),
            (MethodCall(x1), MethodCall(x2)) => unimplemented!(),
            (Paren(x1), Paren(x2)) => unimplemented!(),
            (Range(x1), Range(x2)) => unimplemented!(),
            (Reference(x1), Reference(x2)) => unimplemented!(),
            (Repeat(x1), Repeat(x2)) => unimplemented!(),
            (Struct(x1), Struct(x2)) => unimplemented!(),
            (Try(x1), Try(x2)) => unimplemented!(),
            (TryBlock(x1), TryBlock(x2)) => unimplemented!(),
            (Unary(x1), Unary(x2)) => unimplemented!(),
            (Unsafe(x1), Unsafe(x2)) => unimplemented!(),
            (Verbatim(x1), Verbatim(x2)) => unimplemented!(),
            (While(x1), While(x2)) => unimplemented!(),
            (Closure(x1), Closure(x2)) => unimplemented!(),
            (Const(x1), Const(x2)) => unimplemented!(),
            (Block(x1), Block(x2)) => unimplemented!(),
            (Yield(x1), Yield(x2)) => unimplemented!(),
            (Async(x1), Async(x2)) => unimplemented!(),
            (Await(x1), Await(x2)) => unimplemented!(),
            (x1, x2) => (x1 == x2).then_some(Substitutions::default()),
        }
    }
}
impl IsSuperset for ImplGroupId {
    fn is_superset<'a>(&'a self, other: &'a Self) -> Option<Substitutions> {
        match (&self.0, &other.0) {
            (Some(x1), Some(x2)) => x1.is_superset(x2)?,
            (None, None) => Substitutions::default(),
            _ => return None,
        }
        .merge(self.1.is_superset(&other.1)?)
    }
}
