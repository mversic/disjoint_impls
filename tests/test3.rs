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

disjoint::impls! {
    pub trait Kita<U> {
        const NAME: &'static str;
    }

    impl<U: Dispatch<Group = GroupA>, T> Kita<U> for T {
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

    impl<T1, M1> _Kita<GroupA, M1> for T1 {
        const NAME: &'static str = "Blanket A";
    }
    impl<T1, M1> _Kita<GroupB, M1> for T1 {
        const NAME: &'static str = "Blanket B";
    }

    impl<T0: Dispatch, T1> Kita<T0> for T1 where Self: _Kita<<T0 as Dispatch>::Group, T1> {
        const NAME: &'static str = <Self as _Kita<<T0 as Dispatch>::Group, T1>>::NAME;
    }
};
*/

#[test]
fn main() {
    assert_eq!("Blanket A", <u32 as Kita<String>>::NAME);
    assert_eq!("Blanket B", <u32 as Kita<u32>>::NAME);
}
