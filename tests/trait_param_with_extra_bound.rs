use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group;
}

trait A {}

pub enum GroupA {}
impl A for u16 {}
impl Dispatch for u16 {
    type Group = GroupA;
}
impl A for i16 {}
impl Dispatch for i16 {
    type Group = GroupA;
}

pub enum GroupB {}
impl Dispatch for i32 {
    type Group = GroupB;
}
impl Dispatch for u32 {
    type Group = GroupB;
}

disjoint_impls! {
    pub trait Kita<U: From<u8>> {
        const NAME: &'static str;
    }

    impl<U: Dispatch<Group = GroupA> + A, T> Kita<U> for T where U: From<u8> {
        const NAME: &'static str = "Blanket A";
    }
    impl<U: Dispatch<Group = GroupB> + From<u8>, T> Kita<U> for T {
        const NAME: &'static str = "Blanket B";
    }
}

/*
pub trait Kita<T0: From<u8>> {
    const NAME: &'static str;
}

const _: () = {
    trait _Kita0<T0: From<u8>, T1: ?Sized> {
        const NAME: &'static str;
    }

    impl<T0: Dispatch<Group = GroupA> + A, T1> _Kita0<T0, GroupA> for T1 where From<u8> {
        const NAME: &'static str = "Blanket A";
    }
    impl<T0: From<u8> + Dispatch<Group = GroupB>, T1> _Kita0<T0, GroupB> for T1 {
        const NAME: &'static str = "Blanket B";
    }

    impl<T0: From<u8>, T1> Kita<T0> for T1 where T0: Dispatch, Self: _Kita0<T0, <T0 as Dispatch>::Group> {
        const NAME: &'static str = <Self as _Kita0<T0, <T0 as Dispatch>::Group>>::NAME;
    }
};
*/

#[test]
fn trait_param_with_extra_bound() {
    assert_eq!("Blanket A", <u32 as Kita<u16>>::NAME);
    assert_eq!("Blanket A", <u32 as Kita<i16>>::NAME);
    assert_eq!("Blanket B", <u32 as Kita<u32>>::NAME);
    assert_eq!("Blanket B", <u32 as Kita<i32>>::NAME);
}
