pub trait Dispatch {
    type Group;
}

trait A {}

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
impl Dispatch for i32 {
    type Group = GroupB;
}
impl Dispatch for u32 {
    type Group = GroupB;
}

disjoint::impls! {
    pub trait Kita<U> {
        const NAME: &'static str;
    }

    impl<U: Dispatch<Group = GroupA> + A, T> Kita<U> for T {
        const NAME: &'static str = "Blanket A";
    }
    impl<U: Dispatch<Group = GroupB>, T> Kita<U> for T {
        const NAME: &'static str = "Blanket B";
    }
}

/*
pub trait Kita<T0> {
    const NAME: &'static str;
}

const _: () = {
    trait _Kita<T0, T1> {
        const NAME: &'static str;
    }

    impl<T0: Dispatch<Group = GroupA> + A, T1> Kita<T0, GroupA> for T1 {
        const NAME: &'static str = "Blanket A";
    }
    impl<T0: Dispatch<Group = GroupB>, T1> Kita<GroupB> for T1 {
        const NAME: &'static str = "Blanket B";
    }

    impl<T0, T1> Kita<T0> for T1 where T0: Dispatch, Self: _Kita<T0, <T0 as Dispatch>::Group> {
        const NAME: &'static str = <Self as _Kita<T0, <T0 as Dispatch>::Group>>::NAME;
    }
};
*/

#[test]
fn main() {
    assert_eq!("Blanket A", <u32 as Kita<String>>::NAME);
    assert_eq!("Blanket A", <u32 as Kita<Vec<String>>>::NAME);
    assert_eq!("Blanket B", <u32 as Kita<u32>>::NAME);
    assert_eq!("Blanket B", <u32 as Kita<i32>>::NAME);
}
