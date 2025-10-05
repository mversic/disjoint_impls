use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group;
}

pub trait Dispatch2 {
    type Group;
}

pub enum GroupA {}
impl Dispatch for String {
    type Group = GroupA;
}
impl Dispatch2 for String {
    type Group = GroupA;
}
impl<T> Dispatch for Vec<T> {
    type Group = GroupA;
}
impl<T> Dispatch2 for Vec<T> {
    type Group = GroupA;
}

pub enum GroupB {}
impl Dispatch for i32 {
    type Group = GroupB;
}
impl Dispatch2 for i32 {
    type Group = GroupA;
}
impl Dispatch for u32 {
    type Group = GroupB;
}
impl Dispatch2 for u32 {
    type Group = GroupA;
}
impl Dispatch2 for (u32,) {
    type Group = GroupB;
}

disjoint_impls! {
    pub trait Kita {
        const NAME: &'static str;
    }

    impl<T: Dispatch<Group = GroupA>> Kita for T
    where
        T: Dispatch2<Group = GroupA>,
    {
        const NAME: &'static str = "Blanket A";
    }
    impl<U: Dispatch<Group = GroupB>> Kita for U
    where
        U: Dispatch2<Group = GroupA>,
    {
        const NAME: &'static str = "Blanket B";
    }
    impl<U> Kita for (U,)
    where
        (U,): Dispatch2<Group = GroupB>,
    {
        const NAME: &'static str = "Blanket C";
    }
}

/*
const _: () = {
    pub trait Kita {
        const NAME: &'static str;
    }

    pub trait Kita0<_TŠČ0: ?Sized, _TŠČ1: ?Sized> {
        const NAME: &'static str;
    }

    impl<T: Dispatch<Group = GroupA>> Kita0<GroupA, GroupA> for T
    where
        T: Dispatch2<Group = GroupA>,
    {
        const NAME: &'static str = "Blanket A";
    }
    impl<U: Dispatch<Group = GroupB>> Kita0<GroupB, GroupA> for U
    where
        U: Dispatch2<Group = GroupA>,
    {
        const NAME: &'static str = "Blanket B";
    }

    impl<T, _TŠČ1> Kita for T
    where
        Self: Kita0<_TŠČ1, GroupA>,
        T: Dispatch<Group = _TŠČ1>,
        T: Dispatch2<Group = GroupA>,
    {
        const NAME: &'static str = <Self as Kita0<_TŠČ1, GroupA>>::NAME;
    }
    impl<U> Kita for (U,)
    where
        (U,): Dispatch2<Group = GroupB>,
    {
        const NAME: &'static str = "Blanket B";
    }
};
*/

#[test]
fn overlapping_common_group() {
    assert_eq!("Blanket A", String::NAME);
    assert_eq!("Blanket A", Vec::<u32>::NAME);

    assert_eq!("Blanket B", i32::NAME);
    assert_eq!("Blanket B", u32::NAME);

    assert_eq!("Blanket C", <(u32,)>::NAME);
}
