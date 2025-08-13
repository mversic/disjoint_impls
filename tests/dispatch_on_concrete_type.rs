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

    impl<T: Dispatch<Group = GroupA>> Kita for T
    where
        u32: Dispatch<Group = GroupB>,
    {
        const NAME: &'static str = "Blanket A";
    }
    impl<T: Dispatch<Group = GroupB>> Kita for T
    where
        u32: Dispatch<Group = GroupB>,
    {
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

    impl<_0: Dispatch<Group = GroupA>> Kita0<GroupA> for _0
    where
        u32: Dispatch<Group = GroupB>,
    {
        const NAME: &'static str = "Blanket A";
    }
    impl<_0: Dispatch<Group = GroupB>> Kita0<GroupB> for _0
    where
        u32: Dispatch<Group = GroupB>,
    {
        const NAME: &'static str = "Blanket B";
    }

    impl<_0> Kita for _0
    where
        _0: Dispatch,
        Self: Kita0<<_0 as Dispatch>::Group>,
    {
        const NAME: &'static str = <Self as Kita0<<_0 as Dispatch>::Group>>::NAME;
    }
};
 */

#[test]
fn dispatch_on_concrete_type() {
    assert_eq!("Blanket A", <String as Kita>::NAME);
    assert_eq!("Blanket A", <Vec::<u32> as Kita>::NAME);

    assert_eq!("Blanket B", <u32 as Kita>::NAME);
    assert_eq!("Blanket B", <i32 as Kita>::NAME);
}
