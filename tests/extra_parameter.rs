pub trait Kita<U> {
    const NAME: &'static str;
}
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
    impl<T: Dispatch<Group = GroupA>, U> Kita<U> for T {
        const NAME: &'static str = "Blanket A";
    }
    impl<T: Dispatch<Group = GroupB>, U> Kita<U> for T {
        const NAME: &'static str = "Blanket B";
    }
}

//trait _Kita<T> {
//    const _NAME: &'static str;
//}
//
//impl<T: Dispatch + _Kita<T::Group>, U> Kita<U> for T {
//    const NAME: &'static str = <T as _Kita<T::Group>>::_NAME;
//}
//
//impl<T> _Kita<GroupA> for T {
//    const _NAME: &'static str = "Blanket A";
//}
//impl<T> _Kita<GroupB> for T {
//    const _NAME: &'static str = "Blanket B";
//}

fn main() {
    assert_eq!("Blanket A", <String as Kita<GroupB>>::NAME);
    assert_eq!("Blanket A", <Vec::<u32> as Kita<GroupA>>::NAME);
    assert_eq!("Blanket B", <u32 as Kita<String>>::NAME);
    assert_eq!("Blanket B", <i32 as Kita<i32>>::NAME);
}
