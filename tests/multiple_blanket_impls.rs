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
    pub trait Kita0<_0: ?Sized> {
        const NAME: &'static str;
    }
    pub trait Kita1<_0: ?Sized, _1: ?Sized> {
        const NAME: &'static str;
    }

    impl<_0: Dispatch<Group = GroupA>> Kita0<GroupA> for _0 {
        const NAME: &'static str = "Blanket A";
    }
    impl<_0: Dispatch<Group = GroupB>> Kita0<GroupB> for _0 {
        const NAME: &'static str = "Blanket B";
    }

    impl<_0: Dispatch<Group = GroupA>, _1: Dispatch<Group = GroupA>> Kita1<GroupA, GroupA>
        for (_0, _1)
    {
        const NAME: &'static str = "Blanket AA";
    }
    impl<_0, _1> Kita1<GroupA, GroupB> for (_0, _1)
    where
        _0: Dispatch<Group = GroupA>,
        _1: Dispatch<Group = GroupB>,
    {
        const NAME: &'static str = "Blanket AB";
    }
    impl<_0: Dispatch<Group = GroupB>, _1: Dispatch> Kita1<GroupB, <_1 as Dispatch>::Group>
        for (_0, _1)
    {
        const NAME: &'static str = "Blanket B*";
    }

    impl<_0> Kita for _0
    where
        _0: Dispatch,
        Self: Kita0<<_0 as Dispatch>::Group>,
    {
        const NAME: &'static str = <Self as Kita0<<_0 as Dispatch>::Group>>::NAME;
    }
    impl<_0, _1> Kita for (_0, _1)
    where
        _0: Dispatch,
        _1: Dispatch,
        Self: Kita1<<_0 as Dispatch>::Group, <_1 as Dispatch>::Group>,
    {
        const NAME: &'static str =
            <Self as Kita1<<_0 as Dispatch>::Group, <_1 as Dispatch>::Group>>::NAME;
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
