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

    impl<U, T: Dispatch<Group = GroupA>> Kita<U> for T {
        const NAME: &'static str = "Blanket A";
    }
    impl<U, T: Dispatch<Group = GroupB>> Kita<U> for T {
        const NAME: &'static str = "Blanket B";
    }
}

/*
pub trait Kita<T0> {
    const NAME: &'static str;
}

const _: () = {
    trait _Kita<T0, T1> {
        const _NAME: &'static str;
    }

    impl<T0, T1: Dispatch<Group = GroupA>> _Kita<T0, GroupA> for T1 {
        const _NAME: &'static str = "Blanket A";
    }
    impl<T0, T1: Dispatch<Group = GroupB>> _Kita<T0, GroupB> for T1 {
        const _NAME: &'static str = "Blanket B";
    }

    impl<T0, T1: Dispatch> Kita<T0> for T1 where Self: _Kita<T0, <T1 as Dispatch>::Group> {
        const NAME: &'static str = <Self as _Kita<T0, <T1 as Dispatch>::Group>>::_NAME;
    }
};
*/

#[test]
fn main() {
    assert_eq!("Blanket A", <String as Kita<GroupB>>::NAME);
    assert_eq!("Blanket A", <Vec::<u32> as Kita<GroupA>>::NAME);
    assert_eq!("Blanket B", <u32 as Kita<String>>::NAME);
    assert_eq!("Blanket B", <i32 as Kita<i32>>::NAME);
}
