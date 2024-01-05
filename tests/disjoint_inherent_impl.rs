//use disjoint_impls::disjoint_impls;
//
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
//pub struct Wrapper<'a, T, const N: usize>(&'a T);
//
//disjoint_impls! {
//    impl<T> Wrapper<'_, T, 12> where T: Dispatch<Group = GroupA> + Dispatch<Group = GroupA> {
//        pub const NAME: &'static str = "1st Blanket A";
//    }
//    impl<T> Wrapper<'_, T, 12> where T: Dispatch<Group = GroupB> + Dispatch<Group = GroupB> {
//        pub const NAME: &'static str = "1st Blanket B";
//    }
//
//    impl<T: Dispatch<Group = GroupA> + Dispatch<Group = GroupA>> Wrapper<'_, T, 14> {
//        pub const NAME: &'static str = "2nd Blanket A";
//    }
//    impl<T: Dispatch<Group = GroupB> + Dispatch<Group = GroupB>> Wrapper<'_, T, 14> {
//        pub const NAME: &'static str = "2nd Blanket B";
//    }
//}
//
//
//const _: () = {
//    pub trait _Wrapper0<_0: ?Sized> {
//        const NAME: &'static str;
//    }
//
//    impl<_2> _Wrapper0<GroupA> for Wrapper<'_, _2, 12> where _2: Dispatch<Group = GroupA> + Dispatch<Group = GroupA> {
//        const NAME: &'static str = "Blanket A";
//    }
//    impl<_2> _Wrapper0<GroupB> for Wrapper<'_, _2, 12> where _2: Dispatch<Group = GroupB> {
//        const NAME: &'static str = "Blanket B";
//    }
//
//    impl<_2> Wrapper<'_, _2, 12> where _2: Dispatch, Self: _Wrapper0<<_2 as Dispatch>::Group> {
//        pub const NAME: &'static str = <Self as _Wrapper0<<_2 as Dispatch>::Group>>::NAME;
//    }
//};
//
//
//#[test]
//fn disjoint_inherent_impl() {
//    assert_eq!("Blanket A", <Wrapper<String, 12>>::NAME);
//    assert_eq!("Blanket A", <Wrapper<Vec::<u32>, 12>>::NAME);
//    assert_eq!("Blanket B", <Wrapper<u32, 14>>::NAME);
//    assert_eq!("Blanket B", <Wrapper<i32, 14>>::NAME);
//}
