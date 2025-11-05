use disjoint_impls::disjoint_impls;

pub trait LocalTrait {}

pub trait Dispatch {
    type Group;
}

pub enum LocalType {}
impl Dispatch for String {
    type Group = LocalType;
}
impl<T> Dispatch for Vec<T> {
    type Group = u32;
}

impl Dispatch for u32 {
    type Group = i32;
}

impl LocalTrait for i32 {}

// NOTE: Makes impls overlap
//impl LocalTrait for u32 {}

disjoint_impls! {
    pub trait Kita {
        const NAME: &str;

        type Kita;
    }

    impl<T: Dispatch<Group = LocalType>> Kita for T {
        const NAME: &'static str = "Blanket A";

        type Kita = u32;
    }
    impl<T: Dispatch<Group = u32>> Kita for T {
        const NAME: &'static str = "Blanket B";

        type Kita = u32;
    }
    impl<U: Dispatch<Group = T>, T: LocalTrait> Kita for U {
        const NAME: &'static str = "Blanket C";

        type Kita = U::Group;
    }
}

/*
pub trait Kita {
    const NAME: &str;
    type Kita;
}

const _: () = {
    pub trait Kita0<_TŠČ0: ?Sized> {
        const NAME: &str;
        type Kita;
    }
    impl<T: Dispatch<Group = LocalType>> Kita0<LocalType> for T {
        const NAME: &'static str = "Blanket A";
        type Kita = u32;
    }
    impl<T: Dispatch<Group = u32>> Kita0<u32> for T {
        const NAME: &'static str = "Blanket B";
        type Kita = u32;
    }
    impl<U: Dispatch<Group = T>, T: LocalTrait> Kita0<T> for U {
        const NAME: &'static str = "Blanket C";
        type Kita = U::Group;
    }

    impl<_TŠČ0> Kita for _TŠČ0
    where
        _TŠČ0: Dispatch,
        Self: Kita0<<_TŠČ0 as Dispatch>::Group>,
    {
        const NAME: &str = <Self as Kita0<<_TŠČ0 as Dispatch>::Group>>::NAME;
        type Kita = <Self as Kita0<<_TŠČ0 as Dispatch>::Group>>::Kita;
    }
};
*/

#[test]
fn dispatch_on_local_trait() {
    assert_eq!("Blanket A", String::NAME);
    assert_eq!("Blanket B", Vec::<u32>::NAME);
    assert_eq!("Blanket C", u32::NAME);
}
