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
    pub trait Kita<U>: Dispatch
    where
        U: From<u8> + From<bool>,
    {
        const NAME: &'static str;
    }

    impl<U, T: Dispatch<Group = GroupA>> Kita<U> for T
    where
        U: From<u8> + From<bool>,
    {
        const NAME: &'static str = "Blanket A";
    }
    impl<U, T: Dispatch<Group = GroupB>> Kita<U> for T
    where
        U: From<u8> + From<bool>,
    {
        const NAME: &'static str = "Blanket B";
    }
}

/*
pub trait Kita<U>: Dispatch
where
    U: From<u8> + From<bool>,
{
    const NAME: &'static str;
}

const _: () = {
    pub trait Kita0<_TŠČ1: ?Sized, U>: Dispatch
    where
        U: From<u8> + From<bool>,
    {
        const NAME: &'static str;
    }
    impl<U, T: Dispatch<Group = GroupA>> Kita0<GroupA, U> for T
    where
        U: From<u8> + From<bool>,
    {
        const NAME: &'static str = "Blanket A";
    }
    impl<U, T: Dispatch<Group = GroupB>> Kita0<GroupB, U> for T
    where
        U: From<u8> + From<bool>,
    {
        const NAME: &'static str = "Blanket B";
    }
    impl<U, T> Kita<U> for T
    where
        U: From<u8> + From<bool>,
        Self: Dispatch,
        Self: Kita0<<T as Dispatch>::Group, U>,
        T: Dispatch,
        U: From<u8>,
        U: From<bool>,
    {
        const NAME: &'static str = <Self as Kita0<<T as Dispatch>::Group, U>>::NAME;
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
