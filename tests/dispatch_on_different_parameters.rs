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
pub trait Kita<T> {
    const NAME: &'static str;
}

const _: () = {
    trait _Kita<T> {
        const _NAME: &'static str;
    }

    impl<T, U> Kita<U> for T where U: Dispatch + _Kita<<U as Dispatch>::Group> {
        const NAME: &'static str = <U as _Kita<<U as Dispatch>::Group>>::_NAME;
    }

    impl<T, U> _Kita<U, GroupA> for T where U: Dispatch<Group = GroupA> {
        const _NAME: &'static str = "Blanket A";
    }
    impl<T, U: Dispatch<Group = GroupB>> _Kita<U, GroupB> for T {
        const _NAME: &'static str = "Blanket B";
    }
};
*/

fn main() {
//    assert_eq!("Blanket A", <String as Kita<String>>::NAME);
//    assert_eq!("Blanket B", <Vec::<u32> as Kita<u32>>::NAME);
//    assert_eq!("Blanket B", <u32 as Kita<u32>>::NAME);
//    assert_eq!("Blanket B", <i32 as Kita<u32>>::NAME);
}
