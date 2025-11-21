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

impl Dispatch1 for u32 {
    type Group = GroupA;
}
impl Dispatch2 for u32 {
    type Group = GroupB;
}
impl Dispatch3 for u32 {
    type Group = GroupB;
}

impl Dispatch1 for i32 {
    type Group = GroupB;
}

struct Wrapper<T>(T);

disjoint_impls! {
    impl<T: Dispatch1<Group = GroupA> + Dispatch2<Group = GroupA> + Dispatch3<Group = GroupA>> Wrapper<T> {
        const NAME: &'static str = "Blanket AAA";
    }
    impl<T: Dispatch1<Group = GroupA> + Dispatch2<Group = GroupA> + Dispatch3<Group = GroupB>> Wrapper<T> {
        const NAME: &'static str = "Blanket AAB";
    }
    impl<T: Dispatch1<Group = GroupA> + Dispatch2<Group = GroupB>> Wrapper<T> {
        const NAME: &'static str = "Blanket AB*";
    }
    impl<T: Dispatch1<Group = GroupB>> Wrapper<T> {
        const NAME: &'static str = "Blanket B**";
    }
}

/*
const _: () = {
    pub trait Wrapper0<_TŠČ2: ?Sized, _TŠČ0, _TŠČ1> {
        const NAME: &'static str;
    }
    impl<T: Dispatch1<Group = GroupB>> Wrapper0<GroupB, T, GroupB> for Wrapper<T> {
        const NAME: &'static str = "Blanket B**";
    }

    pub trait Wrapper00<_TŠČ2: ?Sized, _TŠČ0, _TŠČ1> {
        const NAME: &'static str;
    }
    impl<
        T: Dispatch1<Group = GroupA> + Dispatch2<Group = GroupB>,
    > Wrapper00<GroupB, T, GroupB> for Wrapper<T> {
        const NAME: &'static str = "Blanket AB*";
    }

    pub trait Wrapper000<_TŠČ2: ?Sized, _TŠČ0, _TŠČ1> {
        const NAME: &'static str;
    }
    impl<
        T: Dispatch1<Group = GroupA> + Dispatch2<Group = GroupA>
            + Dispatch3<Group = GroupA>,
    > Wrapper000<GroupA, T, GroupA> for Wrapper<T> {
        const NAME: &'static str = "Blanket AAA";
    }
    impl<
        T: Dispatch1<Group = GroupA> + Dispatch2<Group = GroupA>
            + Dispatch3<Group = GroupB>,
    > Wrapper000<GroupB, T, GroupB> for Wrapper<T> {
        const NAME: &'static str = "Blanket AAB";
    }

    impl<_TŠČ0, _TŠČ1> Wrapper00<GroupA, _TŠČ0, GroupA> for Wrapper<_TŠČ0>
    where
        _TŠČ0: Dispatch3<Group = _TŠČ1>,
        Self: Wrapper000<_TŠČ1, _TŠČ0, _TŠČ1>,
    {
        const NAME: &'static str = <Self as Wrapper000<_TŠČ1, _TŠČ0, _TŠČ1>>::NAME;
    }
    impl<_TŠČ0, _TŠČ1> Wrapper0<GroupA, _TŠČ0, GroupA> for Wrapper<_TŠČ0>
    where
        _TŠČ0: Dispatch2<Group = _TŠČ1>,
        Self: Wrapper00<_TŠČ1, _TŠČ0, _TŠČ1>,
    {
        const NAME: &'static str = <Self as Wrapper00<_TŠČ1, _TŠČ0, _TŠČ1>>::NAME;
    }
    impl<_TŠČ0, _TŠČ1> Wrapper<_TŠČ0>
    where
        _TŠČ0: Dispatch1<Group = _TŠČ1>,
        Self: Wrapper0<_TŠČ1, _TŠČ0, _TŠČ1>,
    {
        const NAME: &'static str = <Self as Wrapper0<_TŠČ1, _TŠČ0, _TŠČ1>>::NAME;
    }
};
*/

#[test]
fn inherent_nested_subgroups() {
    assert_eq!("Blanket AAA", Wrapper::<String>::NAME);
    assert_eq!("Blanket AAB", Wrapper::<Vec::<u32>>::NAME);
    assert_eq!("Blanket AB*", Wrapper::<Vec::<i32>>::NAME);
    assert_eq!("Blanket B**", Wrapper::<i32>::NAME);
}
