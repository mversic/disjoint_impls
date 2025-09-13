use syn::Token;

use super::*;

impl Generalize for Token![&] {
    fn generalize(
        &self,
        _: &Self,
        _: &Params,
        _: &Params,
        _: &mut Generalizations<'_>,
    ) -> Option<Self> {
        Some(*self)
    }
}

impl Generalize for Token![if] {
    fn generalize(
        &self,
        _: &Self,
        _: &Params,
        _: &Params,
        _: &mut Generalizations<'_>,
    ) -> Option<Self> {
        Some(*self)
    }
}

impl Generalize for Token![else] {
    fn generalize(
        &self,
        _: &Self,
        _: &Params,
        _: &Params,
        _: &mut Generalizations<'_>,
    ) -> Option<Self> {
        Some(*self)
    }
}

impl Generalize for Token![@] {
    fn generalize(
        &self,
        _: &Self,
        _: &Params,
        _: &Params,
        _: &mut Generalizations<'_>,
    ) -> Option<Self> {
        Some(*self)
    }
}
