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
    pub trait Kita<U> {
        const NAME: &'static str;
    }

    impl<T, U> Kita<U> for T
    where
        U: Dispatch<Group = GroupA>,
    {
        const NAME: &'static str = "Blanket A";
    }
    impl<T, U: Dispatch<Group = GroupB>> Kita<U> for T {
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

    impl<T, U> Kita0<GroupA, U> for T
    where
        U: Dispatch<Group = GroupA>,
    {
        const NAME: &'static str = "Blanket A";
    }
    impl<T, U: Dispatch<Group = GroupB>> Kita0<GroupB, U> for T {
        const NAME: &'static str = "Blanket B";
    }

    impl<U, T, _TŠČ2> Kita<U> for T
    where
        Self: Kita0<_TŠČ2, U>,
        U: Dispatch<Group = _TŠČ2>,
    {
        const NAME: &'static str = <Self as Kita0<_TŠČ2, U>>::NAME;
    }
};
*/

#[test]
fn dispatch_on_different_parameters() {
    assert_eq!("Blanket A", <String as Kita<String>>::NAME);
    assert_eq!("Blanket B", <Vec::<u32> as Kita<u32>>::NAME);
    assert_eq!("Blanket B", <u32 as Kita<u32>>::NAME);
    assert_eq!("Blanket B", <i32 as Kita<u32>>::NAME);
}
