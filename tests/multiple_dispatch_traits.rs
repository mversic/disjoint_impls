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
    pub trait Kita0<_TŠČ0: ?Sized, _TŠČ1: ?Sized> {
        const NAME: &'static str;
    }

    impl<T: Dispatch1<Group = GroupA> + Dispatch2<Group = GroupA>> Kita0<GroupA, GroupA>
    for T {
        const NAME: &'static str = "Blanket AA";
    }

    impl<T: Dispatch1<Group = GroupA> + Dispatch2<Group = GroupB>> Kita0<GroupA, GroupB>
    for T {
        const NAME: &'static str = "Blanket AB";
    }

    impl<T: Dispatch1<Group = GroupB> + Dispatch2> Kita0<GroupB, <T as Dispatch2>::Group>
    for T {
        const NAME: &'static str = "Blanket B*";
    }

    impl<T> Kita for T
    where
        Self: Kita0<<T as Dispatch1>::Group, <T as Dispatch2>::Group>,
        T: Dispatch1,
        T: Dispatch2,
    {
        const NAME: &'static str = <Self as Kita0<
            <T as Dispatch1>::Group,
            <T as Dispatch2>::Group,
        >>::NAME;
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
