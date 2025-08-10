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

disjoint_impls! {
    pub trait Kita {
        const NAME: &'static str;
    }

    impl<T: Dispatch1<Group = GroupA> + Dispatch2<Group = GroupA>> Kita for T {
        const NAME: &'static str = "Blanket A";
    }
    impl<T: Dispatch1<Group = GroupB>> Kita for T {
        const NAME: &'static str = "Blanket B";
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

    impl<_0: Dispatch1<Group = GroupA> + Dispatch2<Group = GroupA>> Kita0<GroupA> for _0 {
        const NAME: &'static str = "Blanket A";
    }
    impl<_0: Dispatch1<Group = GroupB>> Kita0<GroupB> for _0 {
        const NAME: &'static str = "Blanket B";
    }

    impl<_0> Kita for _0
    where
        _0: Dispatch1,
        Self: Kita0<<_0 as Dispatch1>::Group>,
    {
        const NAME: &'static str = <Self as Kita0<<_0 as Dispatch1>::Group>>::NAME;
    }
};
*/

#[test]
fn ignored_assoc_bound() {
    assert_eq!("Blanket A", String::NAME);
    assert_eq!("Blanket B", u32::NAME);
    assert_eq!("Blanket B", i32::NAME);

    // TODO: Make it a compile test
    // NOTE: Fails to compile as it should
    //assert_eq!("Blanket AB", Vec::<u32>::NAME);
}
