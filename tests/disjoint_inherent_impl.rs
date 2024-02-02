use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group;
}

pub trait A {}
pub trait B {}

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
impl B for i32 {}
impl Dispatch for i32 {
    type Group = GroupB;
}
impl B for u32 {}
impl Dispatch for u32 {
    type Group = GroupB;
}

pub struct Wrapper<'a, T, const N: usize>(&'a T);

disjoint_impls! {
    impl<'a, T: A, U> Wrapper<'a, (T, U), 12> where T: Dispatch<Group = GroupA> + Dispatch<Group = GroupA> {
        pub const NAME: &'static str = "1st Blanket A";

        fn kita(_a: T, _b: U) -> &'static str where T: 'a, U: 'a {
            Self::NAME
        }
    }
    impl<'b, T, U> Wrapper<'b, (T, U), 12> where T: Dispatch<Group = GroupB> + Dispatch<Group = GroupB> + B {
        pub const NAME: &'static str = "1st Blanket B";

        fn kita(_a: T, _b: U) -> &'static str where T: 'b, U: 'b {
            Self::NAME
        }
    }

    impl<'c, T: Dispatch<Group = GroupA> + Dispatch<Group = GroupA>> Wrapper<'c, T, 14> {
        pub const NAME: &'static str = "2nd Blanket A";
    }
    impl<'c, T: Dispatch<Group = GroupB> + Dispatch<Group = GroupB>> Wrapper<'c, T, 14> {
        pub const NAME: &'static str = "2nd Blanket B";
    }
}

/*
const _: () = {
    pub trait _Wrapper0<'_0, _3: ?Sized, _1, _2> {
        const NAME: &'static str;

        fn kita(_a: _1, _b: _2) -> &'static str where _1: '_0, _2: '_0;
    }
    pub trait _Wrapper1<'_0, _2: ?Sized, _1> {
        const NAME: &'static str;
    }

    impl<'_0, _1: A, _2> _Wrapper0<'_0, GroupA, _1, _2> for Wrapper<'_0, (_1, _2), 12> where _1: Dispatch<Group = GroupA> + Dispatch<Group = GroupA> {
        const NAME: &'static str = "1st Blanket A";

        fn kita(_a: _1, _b: _2) -> &'static str where _1: '_0, _2: '_0 {
            Self::NAME
        }
    }
    impl<'_0, _1, _2> _Wrapper0<'_0, GroupB, _1, _2> for Wrapper<'_0, (_1, _2), 12> where _1: Dispatch<Group = GroupB> + Dispatch<Group = GroupB> + B {
        const NAME: &'static str = "1st Blanket B";

        fn kita(_a: _1, _b: _2) -> &'static str where _1: '_0, _2: '_0 {
            Self::NAME
        }
    }

    impl<'_0, _1: Dispatch<Group = GroupA> + Dispatch<Group = GroupA>> _Wrapper1<'_0, GroupA, _1> for Wrapper<'_0, _1, 14> {
        const NAME: &'static str = "2nd Blanket A";
    }
    impl<'_0, _1: Dispatch<Group = GroupB> + Dispatch<Group = GroupB>> _Wrapper1<'_0, GroupB, _1> for Wrapper<'_0, _1, 14> {
        const NAME: &'static str = "2nd Blanket B";
    }

    impl<'_0, _1, _2> Wrapper<'_0, (_1, _2), 12> where _1: Dispatch, Self: _Wrapper0<'_0, <_1 as Dispatch>::Group, _1, _2> {
        pub const NAME: &'static str = <Self as _Wrapper0<'_0, <_1 as Dispatch>::Group, _1, _2>>::NAME;

        fn kita(_a: _1, _b: _2) -> &'static str where _1: '_0, _2: '_0 {
            <Self as _Wrapper0<'_0, <_1 as Dispatch>::Group, _1, _2>>::kita(_a, _b)
        }
    }
    impl<'_0, _1> Wrapper<'_0, _1, 14> where _1: Dispatch, Self: _Wrapper1<'_0, <_1 as Dispatch>::Group, _1> {
        pub const NAME: &'static str = <Self as _Wrapper1<'_0, <_1 as Dispatch>::Group, _1>>::NAME;
    }
};
*/

#[test]
fn disjoint_inherent_impl() {
    assert_eq!("1st Blanket A", <Wrapper<(String, u32), 12>>::NAME);
    assert_eq!("1st Blanket A", <Wrapper<(Vec::<u32>, u32), 12>>::NAME);
    assert_eq!("1st Blanket B", <Wrapper<(u32, u32), 12>>::NAME);
    assert_eq!("1st Blanket B", <Wrapper<(i32, u32), 12>>::NAME);

    assert_eq!("2nd Blanket A", <Wrapper<String, 14>>::NAME);
    assert_eq!("2nd Blanket A", <Wrapper<Vec::<u32>, 14>>::NAME);
    assert_eq!("2nd Blanket B", <Wrapper<u32, 14>>::NAME);
    assert_eq!("2nd Blanket B", <Wrapper<i32, 14>>::NAME);

    assert_eq!("1st Blanket A", <Wrapper<(String, u32), 12>>::kita(String::new(), 12));
    assert_eq!("1st Blanket A", <Wrapper<(Vec::<u32>, u32), 12>>::kita(vec![], 12));
    assert_eq!("1st Blanket B", <Wrapper<(u32, u32), 12>>::kita(12, 12));
    assert_eq!("1st Blanket B", <Wrapper<(i32, u32), 12>>::kita(12, 12));
}
