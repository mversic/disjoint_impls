use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group;
}

pub enum GroupA {}
impl Dispatch for String {
    type Group = GroupA;
}
impl<T> Dispatch for Vec<T> {
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
    pub trait Kita<U> where U: From<u8> + From<bool> {
        const NAME: &'static str;
    }

    impl<U, T: Dispatch<Group = GroupA>> Kita<U> for T where U: From<u8> + From<bool> {
        const NAME: &'static str = "Blanket A";
    }
    impl<U, T: Dispatch<Group = GroupB>> Kita<U> for T where U: From<u8> + From<bool> {
        const NAME: &'static str = "Blanket B";
    }
}

/*
pub trait Kita<T0> where T0: From<u8> + From<bool> {
    const NAME: &'static str;
}

const _: () = {
    trait _Kita0<T0, T1: ?Sized> where T0: From<u8> + From<bool> {
        const NAME: &'static str;
    }

    impl<T0, T1: Dispatch<Group = GroupA>> _Kita0<T0, GroupA> for T1 where T0: From<u8> + From<bool> {
        const NAME: &'static str = "Blanket A";
    }
    impl<T0, T1: Dispatch<Group = GroupB>> _Kita0<T0, GroupB> for T1 where T0: From<u8> + From<bool> {
        const NAME: &'static str = "Blanket B";
    }

    impl<T0, T1> Kita<T0> for T1 where T0: From<u8> + From<bool>, T1: Dispatch, Self: _Kita0<T0, <T1 as Dispatch>::Group> {
        const NAME: &'static str = <Self as _Kita0<T0, <T1 as Dispatch>::Group>>::NAME;
    }
};
*/

#[test]
fn extra_parameter() {
    assert_eq!("Blanket A", <String as Kita<u8>>::NAME);
    assert_eq!("Blanket A", <Vec::<u32> as Kita<u16>>::NAME);
    assert_eq!("Blanket B", <u32 as Kita<u32>>::NAME);
    assert_eq!("Blanket B", <i32 as Kita<u64>>::NAME);
}
