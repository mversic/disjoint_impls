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
    pub trait U<U>
    where
        U: From<u8>,
    {
        type U: From<U>;
        const U: &'static str;
    }

    impl<U, T: Dispatch<Group = GroupA>> U<U> for T
    where
        U: From<u8>,
    {
        type U = U;
        const U: &'static str = "Blanket A";
    }
    impl<U, T: Dispatch<Group = GroupB>> U<U> for T
    where
        U: From<u8>,
    {
        type U = U;
        const U: &'static str = "Blanket B";
    }
}

/*
pub trait U<U>
where
    U: From<u8>,
{
    type U: From<U>;
    const U: &'static str;
}

const _: () = {
    pub trait U0<_1: ?Sized, U>
    where
        U: From<u8>,
    {
        type U: From<U>;
        const U: &'static str;
    }

    impl<_0, _1: Dispatch<Group = GroupA>> U0<GroupA, _0> for _1
    where
        _0: From<u8>,
    {
        type U = _0;
        const U: &'static str = "Blanket A";
    }
    impl<_0, _1: Dispatch<Group = GroupB>> U0<GroupB, _0> for _1
    where
        _0: From<u8>,
    {
        type U = _0;
        const U: &'static str = "Blanket B";
    }

    impl<_0, _1> U<_0> for _1
    where
        _0: From<u8>,
        _1: Dispatch,
        Self: U0<<_1 as Dispatch>::Group, _0>,
    {
        type U = <Self as U0<<_1 as Dispatch>::Group, _0>>::U;
        const U: &'static str = <Self as U0<<_1 as Dispatch>::Group, _0>>::U;
    }
};
*/

#[test]
fn overlapping_trait_and_param_idents() {
    assert_eq!("Blanket A", <String as U<u8>>::U);
    assert_eq!("Blanket A", <Vec::<u32> as U<u16>>::U);
    assert_eq!("Blanket B", <u32 as U<u32>>::U);
    assert_eq!("Blanket B", <i32 as U<u64>>::U);
}
