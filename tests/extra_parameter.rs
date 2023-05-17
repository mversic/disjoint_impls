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
pub trait Kita<U> {
    const NAME: &'static str;
}

const _: () = {
    trait _Kita<T> {
        const _NAME: &'static str;
    }

    impl<U, T: Dispatch + _Kita<T::Group>> Kita<U> for T {
        const NAME: &'static str = <T as _Kita<T::Group>>::_NAME;
    }

    impl<U, T: Dispatch<Group = GroupA>> _Kita<U, GroupA> for T {
        const _NAME: &'static str = "Blanket A";
    }
    impl<U, T: Dispatch<Group = GroupB>> Kita<U, GroupB> for T {
        const _NAME: &'static str = "Blanket B";
    }
};
*/

fn main() {
//    assert_eq!("Blanket A", <String as Kita<GroupB>>::NAME);
//    assert_eq!("Blanket A", <Vec::<u32> as Kita<GroupA>>::NAME);
//    assert_eq!("Blanket B", <u32 as Kita<String>>::NAME);
//    assert_eq!("Blanket B", <i32 as Kita<i32>>::NAME);
}
