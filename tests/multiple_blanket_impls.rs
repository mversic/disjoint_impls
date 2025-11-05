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
    pub trait Kita {
        const NAME: &'static str;
    }

    impl<T: Dispatch<Group = GroupA>> Kita for T {
        const NAME: &'static str = "Blanket A";
    }
    impl<U: Dispatch<Group = GroupB>> Kita for U {
        const NAME: &'static str = "Blanket B";
    }

    impl<T: Dispatch<Group = GroupA>, U: Dispatch<Group = GroupA>> Kita for (T, U) {
        const NAME: &'static str = "Blanket AA";
    }
    impl<U, T> Kita for (U, T)
    where
        U: Dispatch<Group = GroupA>,
        T: Dispatch<Group = GroupB>,
    {
        const NAME: &'static str = "Blanket AB";
    }
    impl<T: Dispatch<Group = GroupB>, U: Dispatch> Kita for (T, U) {
        const NAME: &'static str = "Blanket B*";
    }
}

/*
pub trait Kita {
    const NAME: &'static str;
}

const _: () = {
    pub trait Kita0<_TŠČ0: ?Sized> {
        const NAME: &'static str;
    }
    impl<T: Dispatch<Group = GroupA>> Kita0<GroupA> for T {
        const NAME: &'static str = "Blanket A";
    }
    impl<U: Dispatch<Group = GroupB>> Kita0<GroupB> for U {
        const NAME: &'static str = "Blanket B";
    }

    pub trait Kita1<_TŠČ0: ?Sized, _TŠČ1: ?Sized> {
        const NAME: &'static str;
    }
    impl<T: Dispatch<Group = GroupA>, U: Dispatch<Group = GroupA>> Kita1<GroupA, GroupA>
    for (T, U) {
        const NAME: &'static str = "Blanket AA";
    }
    impl<U, T> Kita1<GroupA, GroupB> for (U, T)
    where
        U: Dispatch<Group = GroupA>,
        T: Dispatch<Group = GroupB>,
    {
        const NAME: &'static str = "Blanket AB";
    }
    impl<T: Dispatch<Group = GroupB>, U: Dispatch> Kita1<GroupB, <U as Dispatch>::Group>
    for (T, U) {
        const NAME: &'static str = "Blanket B*";
    }

    impl<_TŠČ0> Kita for _TŠČ0
    where
        _TŠČ0: Dispatch,
        Self: Kita0<<_TŠČ0 as Dispatch>::Group>,
    {
        const NAME: &'static str = <Self as Kita0<<_TŠČ0 as Dispatch>::Group>>::NAME;
    }
    impl<_TŠČ0, _TŠČ1> Kita for (_TŠČ0, _TŠČ1)
    where
        _TŠČ0: Dispatch,
        _TŠČ1: Dispatch,
        Self: Kita1<<_TŠČ0 as Dispatch>::Group, <_TŠČ1 as Dispatch>::Group>,
    {
        const NAME: &'static str = <Self as Kita1<
            <_TŠČ0 as Dispatch>::Group,
            <_TŠČ1 as Dispatch>::Group,
        >>::NAME;
    }
};
*/

#[test]
fn multiple_blanket_impls() {
    assert_eq!("Blanket A", String::NAME);
    assert_eq!("Blanket A", Vec::<u32>::NAME);
    assert_eq!("Blanket B", u32::NAME);
    assert_eq!("Blanket B", i32::NAME);

    assert_eq!("Blanket AA", <(String, String)>::NAME);
    assert_eq!("Blanket AB", <(String, u32)>::NAME);
    assert_eq!("Blanket B*", <(u32, i32)>::NAME);
}
