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
    type Group = GroupA;
}
impl Dispatch1 for u32 {
    type Group = GroupB;
}
impl Dispatch2 for u32 {
    type Group = GroupB;
}

disjoint_impls! {
    pub trait Kita {
        const NAME: &'static str;
    }

    impl<T: Dispatch1<Group = GroupA> + Dispatch2<Group = GroupA>> Kita for T {
        const NAME: &'static str = "Blanket AA";
    }
    impl<T: Dispatch1<Group = GroupA> + Dispatch2<Group = GroupB>> Kita for T {
        const NAME: &'static str = "Blanket AB";
    }
    impl<T: Dispatch1<Group = GroupB> + Dispatch2> Kita for T {
        const NAME: &'static str = "Blanket B*";
    }
}

/*
pub trait Kita {
    const NAME: &'static str;
}

const _: () = {
    pub trait _Kita0<_0: ?Sized, _1: ?Sized> {
        const NAME: &'static str;
    }

    impl<_0: Dispatch1<Group = GroupA> + Dispatch2<Group = GroupA>> _Kita0<GroupA, GroupA> for _0 {
        const NAME: &'static str = "Blanket AA";
    }
    impl<_0: Dispatch1<Group = GroupA> + Dispatch2<Group = GroupB>> _Kita0<GroupB, GroupA> for _0 {
        const NAME: &'static str = "Blanket AB";
    }
    impl<_0: Dispatch1<Group = GroupB>, _1: Dispatch2> _Kita0<<_1 as Dispatch>::Group, GroupB> for _0 {
        const NAME: &'static str = "Blanket B*";
    }

    impl<_0> Kita for _0 where _0: Dispatch2 + Dispatch1, Self: _Kita0<<_0 as Dispatch2>::Group, <_0 as Dispatch1>::Group> {
        const NAME: &'static str = <Self as _Kita0<<_0 as Dispatch2>::Group, <_0 as Dispatch1>::Group>>::NAME;
    }
};
*/

#[test]
fn multiple_dispatch_traits() {
    assert_eq!("Blanket AA", String::NAME);
    assert_eq!("Blanket AB", Vec::<u32>::NAME);
    assert_eq!("Blanket B*", u32::NAME);
    assert_eq!("Blanket B*", i32::NAME);
}
