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

    impl<T, U> Kita<U> for T where U: Dispatch<Group = GroupA> {
        const NAME: &'static str = "Blanket A";
    }
    impl<T, U: Dispatch<Group = GroupB>> Kita<U> for T {
        const NAME: &'static str = "Blanket B";
    }
}

/*
pub trait Kita<T0> {
    const NAME: &'static str;
}
const _: () = {
    pub trait _Kita0<T0, T1: ?Sized> {
        const NAME: &'static str;
    }

    impl<T1, T0> _Kita0<T0, GroupA> for T1 where T0: Dispatch<Group = GroupA> {
        const NAME: &'static str = "Blanket A";
    }
    impl<T1, T0: Dispatch<Group = GroupB>> _Kita0<T0, GroupB> for T1 {
        const NAME: &'static str = "Blanket B";
    }

    impl<T1, T0> Kita<T0> for T1 where T0: Dispatch, Self: _Kita0<T0, <T0 as Dispatch>::Group> {
        const NAME: &'static str = <Self as _Kita0<T0, <T0 as Dispatch>::Group>>::NAME;
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
