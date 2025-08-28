use disjoint_impls::disjoint_impls;

pub trait Dispatch1 {
    type Group;
}
pub trait Dispatch2 {
    type Group;
}

pub enum GroupA {}
pub enum GroupB {}

impl Dispatch1 for String {
    type Group = GroupA;
}
impl Dispatch2 for String {
    type Group = GroupA;
}
impl<T> Dispatch1 for Vec<T> {
    type Group = GroupA;
}
impl<T> Dispatch2 for Vec<T> {
    type Group = GroupB;
}

impl Dispatch1 for i32 {
    type Group = GroupB;
}
impl Dispatch2 for i32 {
    type Group = GroupB;
}
impl Dispatch1 for u32 {
    type Group = GroupB;
}
impl Dispatch2 for u32 {
    type Group = GroupB;
}

impl core::fmt::Display for GroupB {
    fn fmt(&self, _: &mut core::fmt::Formatter) -> core::fmt::Result {
        unimplemented!()
    }
}

disjoint_impls! {
    pub trait Kita {
        const NAME: &str;
    }

    impl<T: Dispatch1<Group = GroupA> + Dispatch2<Group = GroupA>> Kita for T {
        const NAME: &str = "Blanket A";
    }
    impl<T: Dispatch1<Group = GroupB> + Dispatch2<Group: ToString>> Kita for T {
        const NAME: &'static str = "Blanket B";
    }
}

/*
pub trait Kita {
    const NAME: &'static str;
}

const _: () = {
    pub trait Kita0<_ŠČ0: ?Sized, _ŠČ1: ?Sized> {
        const NAME: &'static str;
    }

    impl<
        _ŠČ0: Dispatch1<Group = GroupA> + Dispatch2<Group = GroupA>,
    > Kita0<GroupA, GroupA> for _ŠČ0 {
        const NAME: &'static str = "Blanket A";
    }
    impl<
        _ŠČ0: Dispatch1<Group = GroupB> + Dispatch2<Group: ToString>,
    > Kita0<GroupB, <_ŠČ0 as Dispatch2>::Group> for _ŠČ0 {
        const NAME: &'static str = "Blanket B";
    }

    impl<_ŠČ0> Kita for _ŠČ0
    where
        _ŠČ0: Dispatch1 + Dispatch2,
        Self: Kita0<<_ŠČ0 as Dispatch1>::Group, <_ŠČ0 as Dispatch2>::Group>,
    {
        const NAME: &'static str = <Self as Kita0<
            <_ŠČ0 as Dispatch1>::Group,
            <_ŠČ0 as Dispatch2>::Group,
        >>::NAME;
    }
};
*/

#[test]
fn ignored_assoc_bound() {
    assert_eq!("Blanket A", String::NAME);
    assert_eq!("Blanket B", u32::NAME);
    assert_eq!("Blanket B", i32::NAME);
}
