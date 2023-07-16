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

// TODO: T1 can be removed because it doesn't have any associated bound
const _: () = {
    trait _Kita<T0, T1> {
        const _NAME: &'static str;
    }

    impl<T1> _Kita<GroupA, T1> for T1 {
        const _NAME: &'static str = "Blanket A";
    }
    impl<T1> _Kita<GroupB, T1> for T1 {
        const _NAME: &'static str = "Blanket B";
    }

    impl<T1, T0> Kita<T0> for T1 where T0: Dispatch, Self: _Kita<<T0 as Dispatch>::Group, T1> {
        const NAME: &'static str = <Self as _Kita<<T0 as Dispatch>::Group, T1>>::_NAME;
    }
};
*/

fn main() {
    assert_eq!("Blanket A", <String as Kita<String>>::NAME);
    assert_eq!("Blanket B", <Vec::<u32> as Kita<u32>>::NAME);
    assert_eq!("Blanket B", <u32 as Kita<u32>>::NAME);
    assert_eq!("Blanket B", <i32 as Kita<u32>>::NAME);
}
