use disjoint_impls::disjoint_impls;

pub trait Dispatch<U> {
    type Group;
}

pub enum GroupA {}
impl Dispatch<GroupB> for String {
    type Group = GroupA;
}
impl<T> Dispatch<GroupB> for Vec<T> {
    type Group = GroupA;
}

pub enum GroupB {}
impl Dispatch<GroupB> for i32 {
    type Group = GroupB;
}
impl Dispatch<GroupA> for u32 {
    type Group = GroupB;
}
impl Dispatch<GroupB> for u32 {
    type Group = GroupB;
}

disjoint_impls! {
    pub trait Kita {
        const NAME: &'static str;
    }

    impl<T: Dispatch<U, Group = GroupA>, U> Kita for T
    where
        u32: Dispatch<GroupA, Group = U>,
    {
        const NAME: &'static str = "Blanket A";
    }
    impl<T: Dispatch<U, Group = GroupB>, U> Kita for T
    where
        u32: Dispatch<GroupA, Group = U>,
    {
        const NAME: &'static str = "Blanket B";
    }
}

/*
pub trait Kita {
    const NAME: &'static str;
}

const _: () = {
    pub trait Kita0<_TŠČ0: ?Sized, _TŠČ1: ?Sized> {
        const NAME: &'static str;
    }

    impl<T: Dispatch<U, Group = GroupA>, U> Kita0<U, GroupA> for T
    where
        u32: Dispatch<GroupA, Group = U>,
    {
        const NAME: &'static str = "Blanket A";
    }
    impl<T: Dispatch<U, Group = GroupB>, U> Kita0<U, GroupB> for T
    where
        u32: Dispatch<GroupA, Group = U>,
    {
        const NAME: &'static str = "Blanket B";
    }

    impl<T, _TŠČ1, _TŠČ2> Kita for T
    where
        Self: Kita0<_TŠČ1, _TŠČ2>,
        u32: Dispatch<GroupA, Group = _TŠČ1>,
        T: Dispatch<_TŠČ1, Group = _TŠČ2>,
    {
        const NAME: &'static str = <Self as Kita0<_TŠČ1, _TŠČ2>>::NAME;
    }
};
*/

fn main() {
    assert_eq!("Blanket A", <String as Kita>::NAME);
    assert_eq!("Blanket A", <Vec::<u32> as Kita>::NAME);

    assert_eq!("Blanket B", <u32 as Kita>::NAME);
    assert_eq!("Blanket B", <i32 as Kita>::NAME);
}
