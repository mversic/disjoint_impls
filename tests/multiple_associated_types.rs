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

    impl<T: Dispatch<Group1 = GroupB>> Kita for T {
        const NAME: &'static str = "Blanket B*";
    }
    impl<T: Dispatch<Group1 = GroupA, Group2 = GroupA>> Kita for T {
        const NAME: &'static str = "Blanket AA";
    }
    impl<T: Dispatch<Group1 = GroupA, Group2 = GroupB>> Kita for T {
        const NAME: &'static str = "Blanket AB";
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
    impl<T: Dispatch<Group1 = GroupB>> Kita0<GroupB, <T as Dispatch>::Group2> for T {
        const NAME: &'static str = "Blanket B*";
    }
    impl<T: Dispatch<Group1 = GroupA, Group2 = GroupA>> Kita0<GroupA, GroupA> for T {
        const NAME: &'static str = "Blanket AA";
    }
    impl<T: Dispatch<Group1 = GroupA, Group2 = GroupB>> Kita0<GroupA, GroupB> for T {
        const NAME: &'static str = "Blanket AB";
    }

    impl<_TŠČ0> Kita for _TŠČ0
    where
        _TŠČ0: Dispatch,
        Self: Kita0<<_TŠČ0 as Dispatch>::Group1, <_TŠČ0 as Dispatch>::Group2>,
    {
        const NAME: &'static str = <Self as Kita0<
            <_TŠČ0 as Dispatch>::Group1,
            <_TŠČ0 as Dispatch>::Group2,
        >>::NAME;
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
