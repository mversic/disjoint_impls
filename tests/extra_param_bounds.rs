use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group;
}

pub trait A {}
pub trait B {}

pub enum GroupA {}
impl A for String {}
impl Dispatch for String {
    type Group = GroupA;
}
impl<T> A for Vec<T> {}
impl<T> Dispatch for Vec<T> {
    type Group = GroupA;
}

pub enum GroupB {}
impl B for i32 {}
impl Dispatch for i32 {
    type Group = GroupB;
}
impl B for u32 {}
impl Dispatch for u32 {
    type Group = GroupB;
}

disjoint_impls! {
    pub trait Kita<U> {
        const NAME: &'static str;
    }

    impl<T: Dispatch<Group = GroupA> + Dispatch + A> Kita<u32> for T {
        const NAME: &'static str = "Blanket A";
    }
    impl<T: Dispatch<Group = GroupB>> Kita<u32> for T
    where
        T: B + Dispatch<Group = GroupB>,
    {
        const NAME: &'static str = "Blanket B";
    }
}

/*
pub trait Kita<U> {
    const NAME: &'static str;
}

const _: () = {
    pub trait Kita0<_TŠČ1: ?Sized, U> {
        const NAME: &'static str;
    }

    impl<T: Dispatch<Group = GroupA> + Dispatch + A> Kita0<GroupA, u32> for T {
        const NAME: &'static str = "Blanket A";
    }

    impl<T: Dispatch<Group = GroupB>> Kita0<GroupB, u32> for T
    where
        T: B + Dispatch<Group = GroupB>,
    {
        const NAME: &'static str = "Blanket B";
    }

    impl<T> Kita<u32> for T
    where
        Self: Kita0<<T as Dispatch>::Group, u32>,
        T: Dispatch,
    {
        const NAME: &'static str = <Self as Kita0<<T as Dispatch>::Group, u32>>::NAME;
    }
};
*/

#[test]
fn extra_param_bounds() {
    assert_eq!("Blanket A", String::NAME);
    assert_eq!("Blanket A", Vec::<u32>::NAME);
    assert_eq!("Blanket B", u32::NAME);
    assert_eq!("Blanket B", i32::NAME);
}
