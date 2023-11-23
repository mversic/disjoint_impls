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

    impl<T: Dispatch<Group = GroupA>, U: Dispatch<Group = GroupA>> Kita for (T, U) {
        const NAME: &'static str = "Blanket AA";
    }
    impl<U, T> Kita for (U, T)
    where
        U: Dispatch<Group = GroupA>,
        T: Dispatch<Group = GroupB>
    {
        const NAME: &'static str = "Blanket AB";
    }
    impl<T: Dispatch<Group = GroupB>, U> Kita for (T, U) {
        const NAME: &'static str = "Blanket B*";
    }

    impl<T: Dispatch<Group = GroupA>> Kita for T {
        const NAME: &'static str = "Blanket A";
    }
    impl<U: Dispatch<Group = GroupB>> Kita for U {
        const NAME: &'static str = "Blanket B";
    }
}

/*
pub trait Kita {
    const NAME: &'static str;
}

const _: () = {
    pub trait _Kita0<T0: ?Sized, T1: ?Sized> {
        const NAME: &'static str;
    }
    pub trait _Kita1<T0: ?Sized> {
        const NAME: &'static str;
    }

    impl<T0: Dispatch<Group = GroupA>, T1: Dispatch<Group = GroupA>> _Kita0<GroupA, GroupA> for (T0, T1) {
        const NAME: &'static str = "Blanket AA";
    }
    impl<T0: Dispatch<Group = GroupA>, T1: Dispatch<Group = GroupB>> _Kita0<GroupA, GroupB> for (T0, T1) {
        const NAME: &'static str = "Blanket AB";
    }
    impl<T0: Dispatch<Group = GroupB>, T1, M1> _Kita0<GroupB, M1> for (T0, T1) {
        const NAME: &'static str = "Blanket B*";
    }
    impl<T0: Dispatch<Group = GroupA>> _Kita1<GroupA> for T0 {
        const NAME: &'static str = "Blanket A";
    }
    impl<T0: Dispatch<Group = GroupB>> _Kita1<GroupB> for T0 {
        const NAME: &'static str = "Blanket B";
    }

    impl<T0, T1> Kita for (T0, T1) where T1: Dispatch, T0: Dispatch, Self: _Kita0<<T0 as Dispatch>::Group, <T1 as Dispatch>::Group> {
        const NAME: &'static str = <Self as _Kita0<<T0 as Dispatch>::Group, <T1 as Dispatch>::Group>>::NAME;
    }
    impl<T0> Kita for T0 where T0: Dispatch, Self: _Kita1<<T0 as Dispatch>::Group> {
        const NAME: &'static str = <Self as _Kita1<<T0 as Dispatch>::Group>>::NAME;
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
