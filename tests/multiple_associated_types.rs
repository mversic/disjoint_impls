use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group1;
    type Group2;
}

pub enum GroupA {}
pub enum GroupB {}

impl Dispatch for String {
    type Group1 = GroupA;
    type Group2 = GroupA;
}
impl<T> Dispatch for Vec<T> {
    type Group1 = GroupA;
    type Group2 = GroupB;
}

impl Dispatch for i32 {
    type Group1 = GroupB;
    type Group2 = GroupA;
}
impl Dispatch for u32 {
    type Group1 = GroupB;
    type Group2 = GroupB;
}

disjoint_impls! {
    pub trait Kita {
        const NAME: &'static str;
    }

    impl<T: Dispatch<Group1 = GroupA, Group2 = GroupA>> Kita for T {
        const NAME: &'static str = "Blanket AA";
    }
    impl<T: Dispatch<Group1 = GroupA, Group2 = GroupB>> Kita for T {
        const NAME: &'static str = "Blanket AB";
    }
    impl<T: Dispatch<Group1 = GroupB>> Kita for T {
        const NAME: &'static str = "Blanket B*";
    }
}

/*
pub trait Kita {
    const NAME: &'static str;
}

const _: () = {
    trait _Kita0<T0: ?Sized, T1: ?Sized> {
        const NAME: &'static str;
    }

    impl<T0: Dispatch<Group1 = GroupA, Group2 = GroupA>> _Kita0<GroupA, GroupA> for T0 {
        const NAME: &'static str = "Blanket AA";
    }
    impl<T0: Dispatch<Group1 = GroupA, Group2 = GroupB>> _Kita0<GroupA, GroupB> for T0 {
        const NAME: &'static str = "Blanket AB";
    }
    impl<T0: Dispatch<Group1 = GroupB>, T1> _Kita0<GroupB, T1> for T0 {
        const NAME: &'static str = "Blanket B*";
    }

    impl<T0> Kita for T0 where T0: Dispatch, Self: _Kita0<<T0 as Dispatch>::Group1, <T0 as Dispatch>::Group2> {
        const NAME: &'static str = <Self as _Kita0<<T0 as Dispatch>::Group1, <T0 as Dispatch>::Group2>>::NAME;
    }
};
*/

#[test]
fn multiple_associated_types() {
    assert_eq!("Blanket AA", String::NAME);
    assert_eq!("Blanket AB", Vec::<u32>::NAME);
    assert_eq!("Blanket B*", u32::NAME);
    assert_eq!("Blanket B*", i32::NAME);
}
