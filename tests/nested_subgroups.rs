use disjoint_impls::disjoint_impls;

pub trait Dispatch1 {
    type Group;
}
pub trait Dispatch2 {
    type Group;
}
pub trait Dispatch3 {
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
impl Dispatch3 for String {
    type Group = GroupA;
}

impl Dispatch1 for Vec<u32> {
    type Group = GroupA;
}
impl Dispatch2 for Vec<u32> {
    type Group = GroupA;
}
impl Dispatch3 for Vec<u32> {
    type Group = GroupB;
}

impl Dispatch1 for Vec<i32> {
    type Group = GroupA;
}
impl Dispatch2 for Vec<i32> {
    type Group = GroupB;
}
impl Dispatch3 for Vec<i32> {
    type Group = GroupA;
}

impl Dispatch1 for i32 {
    type Group = GroupB;
}

disjoint_impls! {
    pub trait Kita<A> {
        const NAME: &'static str;
    }

    impl<T: Dispatch1<Group = GroupA> + Dispatch2<Group = GroupA> + Dispatch3<Group = GroupA>> Kita<u32> for T {
        const NAME: &'static str = "Blanket AAA";
    }
    impl<T: Dispatch1<Group = GroupA> + Dispatch2<Group = GroupA> + Dispatch3<Group = GroupB>> Kita<u32> for T {
        const NAME: &'static str = "Blanket AAB";
    }
    impl<T: Dispatch1<Group = GroupA> + Dispatch2<Group = GroupB>> Kita<u32> for T {
        const NAME: &'static str = "Blanket AB*";
    }
    impl<T: Dispatch1<Group = GroupB>> Kita<u32> for T {
        const NAME: &'static str = "Blanket B**";
    }
}

/*
pub trait Kita<A> {
    const NAME: &'static str;
}

const _: () = {
    pub trait Kita0<_TŠČ1: ?Sized, A> {
        const NAME: &'static str;
    }
    impl<T: Dispatch1<Group = GroupB>> Kita0<GroupB, u32> for T {
        const NAME: &'static str = "Blanket B**";
    }

    pub trait Kita00<_TŠČ1: ?Sized, A> {
        const NAME: &'static str;
    }
    impl<T: Dispatch1<Group = GroupA> + Dispatch2<Group = GroupB>> Kita00<GroupB, u32>
    for T {
        const NAME: &'static str = "Blanket AB*";
    }

    pub trait Kita000<_TŠČ1: ?Sized, A> {
        const NAME: &'static str;
    }
    impl<
        T: Dispatch1<Group = GroupA> + Dispatch2<Group = GroupA>
            + Dispatch3<Group = GroupA>,
    > Kita000<GroupA, u32> for T {
        const NAME: &'static str = "Blanket AAA";
    }
    impl<
        T: Dispatch1<Group = GroupA> + Dispatch2<Group = GroupA>
            + Dispatch3<Group = GroupB>,
    > Kita000<GroupB, u32> for T {
        const NAME: &'static str = "Blanket AAB";
    }

    impl<_TŠČ0> Kita00<GroupA, u32> for _TŠČ0
    where
        _TŠČ0: Dispatch3,
        Self: Kita000<<_TŠČ0 as Dispatch3>::Group, u32>,
    {
        const NAME: &'static str = <Self as Kita000<
            <_TŠČ0 as Dispatch3>::Group,
            u32,
        >>::NAME;
    }
    impl<_TŠČ0> Kita0<GroupA, u32> for _TŠČ0
    where
        _TŠČ0: Dispatch2,
        Self: Kita00<<_TŠČ0 as Dispatch2>::Group, u32>,
    {
        const NAME: &'static str = <Self as Kita00<
            <_TŠČ0 as Dispatch2>::Group,
            u32,
        >>::NAME;
    }
    impl<_TŠČ0> Kita<u32> for _TŠČ0
    where
        _TŠČ0: Dispatch1,
        Self: Kita0<<_TŠČ0 as Dispatch1>::Group, u32>,
    {
        const NAME: &'static str = <Self as Kita0<
            <_TŠČ0 as Dispatch1>::Group,
            u32,
        >>::NAME;
    }
};
*/

#[test]
fn nested_subgroups() {
    assert_eq!("Blanket AAA", String::NAME);
    assert_eq!("Blanket AAB", Vec::<u32>::NAME);
    assert_eq!("Blanket AB*", Vec::<i32>::NAME);
    assert_eq!("Blanket B**", i32::NAME);
}
