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
    impl<T: A> Wrapper<'_, T, 12> where T: Dispatch<Group = GroupA> + Dispatch<Group = GroupA> {
        pub const NAME: &'static str = "1st Blanket A";
    }
    impl<T> Wrapper<'_, T, 12> where T: Dispatch<Group = GroupB> + Dispatch<Group = GroupB> + B {
        pub const NAME: &'static str = "1st Blanket B";
    }

    impl<T: Dispatch<Group = GroupA> + Dispatch<Group = GroupA>> Wrapper<'_, T, 14> {
        pub const NAME: &'static str = "2nd Blanket A";
    }
    impl<T: Dispatch<Group = GroupB> + Dispatch<Group = GroupB>> Wrapper<'_, T, 14> {
        pub const NAME: &'static str = "2nd Blanket B";
    }
}

/*
const _: () = {
    pub trait _Wrapper0<_0: ?Sized> {
        const NAME: &'static str;
    }
    pub trait _Wrapper1<_0: ?Sized> {
        const NAME: &'static str;
    }

    impl<_0: A> _Wrapper0<GroupA> for Wrapper<'_, _0, 12> where _0: Dispatch<Group = GroupA> + Dispatch<Group = GroupA> {
        const NAME: &'static str = "1st Blanket A";
    }
    impl<_0> _Wrapper0<GroupB> for Wrapper<'_, _0, 12> where _0: Dispatch<Group = GroupB> + Dispatch<Group = GroupB> + B {
        const NAME: &'static str = "1st Blanket B";
    }

    impl<_0: Dispatch<Group = GroupA> + Dispatch<Group = GroupA>> _Wrapper1<GroupA> for Wrapper<'_, _0, 14> {
        const NAME: &'static str = "2nd Blanket A";
    }
    impl<_0: Dispatch<Group = GroupB> + Dispatch<Group = GroupB>> _Wrapper1<GroupB> for Wrapper<'_, _0, 14> {
        const NAME: &'static str = "2nd Blanket B";
    }

    impl<_0> Wrapper<'_, _0, 12> where _0: Dispatch, Self: _Wrapper0<<_0 as Dispatch>::Group> {
        pub const NAME: &'static str = <Self as _Wrapper0<<_0 as Dispatch>::Group>>::NAME;
    }
    impl<_0> Wrapper<'_, _0, 14> where _0: Dispatch, Self: _Wrapper1<<_0 as Dispatch>::Group> {
        pub const NAME: &'static str = <Self as _Wrapper1<<_0 as Dispatch>::Group>>::NAME;
    }
};
*/

#[test]
fn disjoint_inherent_impl() {
    assert_eq!("1st Blanket A", <Wrapper<String, 12>>::NAME);
    assert_eq!("1st Blanket A", <Wrapper<Vec::<u32>, 12>>::NAME);
    assert_eq!("1st Blanket B", <Wrapper<u32, 12>>::NAME);
    assert_eq!("1st Blanket B", <Wrapper<i32, 12>>::NAME);

    assert_eq!("2nd Blanket A", <Wrapper<String, 14>>::NAME);
    assert_eq!("2nd Blanket A", <Wrapper<Vec::<u32>, 14>>::NAME);
    assert_eq!("2nd Blanket B", <Wrapper<u32, 14>>::NAME);
    assert_eq!("2nd Blanket B", <Wrapper<i32, 14>>::NAME);
}
