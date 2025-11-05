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

    impl<K, T: Dispatch<Group = GroupA>> U<K> for T
    where
        K: From<u8>,
    {
        type U = K;
        const U: &'static str = "Blanket A";
    }
    impl<K, T: Dispatch<Group = GroupB>> U<K> for T
    where
        K: From<u8>,
    {
        type U = K;
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
    pub trait U0<_TŠČ1: ?Sized, U>
    where
        U: From<u8>,
    {
        type U: From<U>;
        const U: &'static str;
    }
    impl<K, T: Dispatch<Group = GroupA>> U0<GroupA, K> for T
    where
        K: From<u8>,
    {
        type U = K;
        const U: &'static str = "Blanket A";
    }
    impl<K, T: Dispatch<Group = GroupB>> U0<GroupB, K> for T
    where
        K: From<u8>,
    {
        type U = K;
        const U: &'static str = "Blanket B";
    }

    impl<_TŠČ0, _TŠČ1> U<_TŠČ0> for _TŠČ1
    where
        _TŠČ0: From<u8>,
        _TŠČ0: From<u8>,
        _TŠČ1: Dispatch,
        Self: U0<<_TŠČ1 as Dispatch>::Group, _TŠČ0>,
    {
        type U = <Self as U0<<_TŠČ1 as Dispatch>::Group, _TŠČ0>>::U;
        const U: &'static str = <Self as U0<<_TŠČ1 as Dispatch>::Group, _TŠČ0>>::U;
    }
};
*/

#[test]
fn overlapping_idents() {
    assert_eq!("Blanket A", <String as U<u8>>::U);
    assert_eq!("Blanket A", <Vec::<u32> as U<u16>>::U);
    assert_eq!("Blanket B", <u32 as U<u32>>::U);
    assert_eq!("Blanket B", <i32 as U<u64>>::U);
}
