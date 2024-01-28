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
    pub trait U<U> where U: From<u8> {
        const NAME: &'static str;
    }

    impl<U, T: Dispatch<Group = GroupA>> U<U> for T where U: From<u8> {
        const NAME: &'static str = "Blanket A";
    }
    impl<U, T: Dispatch<Group = GroupB>> U<U> for T where U: From<u8> {
        const NAME: &'static str = "Blanket B";
    }
}

/*
pub trait U<U> where U: From<u8> {
    const NAME: &'static str;
}

const _: () = {
    pub trait _U0<_1: ?Sized, U> where U: From<u8> {
        const NAME: &'static str;
    }

    impl<_0, _1: Dispatch<Group = GroupA>> _U0<GroupA, _0> for _1 where _0: From<u8> {
        const NAME: &'static str = "Blanket A";
    }
    impl<_0, _1: Dispatch<Group = GroupB>> _U0<GroupB, _0> for _1 where _0: From<u8> {
        const NAME: &'static str = "Blanket B";
    }

    impl<_0, _1> U<_0> for _1 where _0: From<u8>, _1: Dispatch, Self: _U0<<_1 as Dispatch>::Group, _0> {
        const NAME: &'static str = <Self as _U0<<_1 as Dispatch>::Group, _0>>::NAME;
    }
};
*/

#[test]
fn overlapping_trait_and_param_idents() {
    assert_eq!("Blanket A", <String as U<u8>>::NAME);
    assert_eq!("Blanket A", <Vec::<u32> as U<u16>>::NAME);
    assert_eq!("Blanket B", <u32 as U<u32>>::NAME);
    assert_eq!("Blanket B", <i32 as U<u64>>::NAME);
}
