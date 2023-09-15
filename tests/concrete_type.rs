// TODO
//pub trait Dispatch {
//    type Group;
//}
//
//pub enum GroupA {}
//impl Dispatch for String {
//    type Group = GroupA;
//}
//impl<T> Dispatch for Vec<T> {
//    type Group = GroupA;
//}
//
//pub enum GroupB {}
//impl Dispatch for i32 {
//    type Group = GroupB;
//}
//impl Dispatch for u32 {
//    type Group = GroupB;
//}
//
//// NOTE: This is the same as multiple_associated_types test
//disjoint::impls! {
//    pub trait Kita<U> {
//        const NAME: &'static str;
//    }
//
//    impl<T: Dispatch<Group = GroupA>> Kita<()> for T {
//        const NAME: &'static str = "Blanket A";
//    }
//    impl<U, T: Dispatch<Group = GroupB>> Kita<U> for T {
//        const NAME: &'static str = "Blanket B";
//    }
//}
//
//
//pub trait Kita<T0> {
//    const NAME: &'static str;
//}
//
//const _: () = {
//    trait _Kita<T0, T1> {
//        const _NAME: &'static str;
//    }
//
//    impl<T1: Dispatch<Group = GroupA>> _Kita<(), GroupA> for T1 {
//        const _NAME: &'static str = "Blanket A";
//    }
//    impl<T0, T1: Dispatch<Group = GroupB>> _Kita<T0, GroupB> for T1 {
//        const _NAME: &'static str = "Blanket B";
//    }
//
//    impl<T0, T1: Dispatch> Kita<T0> for T1 where Self: _Kita<T0, <T1 as Dispatch>::Group> {
//        const NAME: &'static str = <Self as _Kita<T0, <T1 as Dispatch>::Group>>::_NAME;
//    }
//};
//
//
//#[test]
//fn main() {
//    assert_eq!("Blanket A", String::NAME);
//    assert_eq!("Blanket A", Vec::<u32>::NAME);
//    assert_eq!("Blanket B", <u32 as Kita<String>>::NAME);
//    assert_eq!("Blanket B", <i32 as Kita<i32>>::NAME);
//}


// TODO: Consider this
//pub trait Dispatch {
//    type Group;
//}
//
//pub enum GroupA {}
//impl Dispatch for String {
//    type Group = GroupA;
//}
//impl<T> Dispatch for Vec<T> {
//    type Group = GroupA;
//}
//
//pub enum GroupB {}
//impl Dispatch for i32 {
//    type Group = GroupB;
//}
//impl Dispatch for u32 {
//    type Group = GroupB;
//}
//
//
//disjoint::impls! {
//    pub trait Kita<U> {
//        const NAME: &'static str;
//    }
//
//    impl<U: Dispatch<Group = GroupA>, T> Kita<U> for T {
//        const NAME: &'static str = "Blanket A";
//    }
//    impl<T> Kita<i32> for T {
//        const NAME: &'static str = "Blanket B";
//    }
//}
//*/
//
//pub trait Kita<T0> {
//    const NAME: &'static str;
//}
//
//const _: () = {
//    pub trait _Kita<T0> {
//        const NAME: &'static str;
//    }
//
//    pub struct Concrete;
//    impl Dispatch for i32 {
//        type Group = Concrete;
//    }
//
//    impl<T1> _Kita<GroupA> for T1 {
//        const NAME: &'static str = "Blanket A";
//    }
//    impl<T1> _Kita<Concrete> for T1 {
//        const NAME: &'static str = "Blanket B";
//    }
//
//    impl<T0: Dispatch, T1> Kita<T0> for T1 where Self: _Kita<<T0 as Dispatch>::Group> {
//        const NAME: &'static str = <Self as _Kita<<T0 as Dispatch>::Group>>::NAME;
//    }
//};
//
//fn main() {
//    assert_eq!("Blanket A", <u32 as Kita<String>>::NAME);
//    assert_eq!("Blanket B", <i32 as Kita<i32>>::NAME);
//}
