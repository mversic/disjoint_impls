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

pub struct Wrapper<'a, T, const N: usize>(pub &'a T);

disjoint_impls! {
    impl<'a, T: A, U> Wrapper<'a, (T, U), 12>
    where
        T: Dispatch<Group = GroupA> + Dispatch<Group = GroupA>,
    {
        pub const NAME: &'static str = "1st Blanket A";

        fn kita(_a: T, _b: U) -> &'static str
        where
            T: 'a,
            U: 'a,
        {
            Self::NAME
        }
    }
    impl<'b, T, U> Wrapper<'b, (T, U), 12>
    where
        T: Dispatch<Group = GroupB> + Dispatch<Group = GroupB> + B,
    {
        pub const NAME: &'static str = "1st Blanket B";

        fn kita(_a: T, _b: U) -> &'static str
        where
            T: 'b,
            U: 'b,
        {
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
    pub trait Wrapper0<'_lšč0, _TŠČ4: ?Sized, _TŠČ0, _TŠČ1, _TŠČ2> {
        const NAME: &'static str;
        fn kita(_a: _TŠČ0, _b: _TŠČ1) -> &'static str
        where
            _TŠČ0: '_lšč0,
            _TŠČ1: '_lšč0;
    }
    impl<'a, T: A, U> Wrapper0<'a, GroupA, T, U, GroupA> for Wrapper<'a, (T, U), 12>
    where
        T: Dispatch<Group = GroupA> + Dispatch<Group = GroupA>,
    {
        const NAME: &'static str = "1st Blanket A";
        fn kita(_a: T, _b: U) -> &'static str
        where
            T: 'a,
            U: 'a,
        {
            Self::NAME
        }
    }
    impl<'b, T, U> Wrapper0<'b, GroupB, T, U, GroupB> for Wrapper<'b, (T, U), 12>
    where
        T: Dispatch<Group = GroupB> + Dispatch<Group = GroupB> + B,
    {
        const NAME: &'static str = "1st Blanket B";
        fn kita(_a: T, _b: U) -> &'static str
        where
            T: 'b,
            U: 'b,
        {
            Self::NAME
        }
    }

    pub trait Wrapper1<'_lšč0, _TŠČ3: ?Sized, _TŠČ0, _TŠČ1> {
        const NAME: &'static str;
    }
    impl<
        'c,
        T: Dispatch<Group = GroupA> + Dispatch<Group = GroupA>,
    > Wrapper1<'c, GroupA, T, GroupA> for Wrapper<'c, T, 14> {
        const NAME: &'static str = "2nd Blanket A";
    }
    impl<
        'c,
        T: Dispatch<Group = GroupB> + Dispatch<Group = GroupB>,
    > Wrapper1<'c, GroupB, T, GroupB> for Wrapper<'c, T, 14> {
        const NAME: &'static str = "2nd Blanket B";
    }

    impl<'_lšč0, _TŠČ0, _TŠČ1, _TŠČ2> Wrapper<'_lšč0, (_TŠČ0, _TŠČ1), 12>
    where
        _TŠČ0: Dispatch<Group = _TŠČ2>,
        Self: Wrapper0<'_lšč0, _TŠČ2, _TŠČ0, _TŠČ1, _TŠČ2>,
    {
        fn kita(_a: _TŠČ0, _b: _TŠČ1) -> &'static str
        where
            _TŠČ0: '_lšč0,
            _TŠČ1: '_lšč0,
        {
            <Self as Wrapper0<
                '_lšč0,
                _TŠČ2,
                _TŠČ0,
                _TŠČ1,
                _TŠČ2,
            >>::kita(_a, _b)
        }
        const NAME: &'static str = <Self as Wrapper0<
            '_lšč0,
            _TŠČ2,
            _TŠČ0,
            _TŠČ1,
            _TŠČ2,
        >>::NAME;
    }
    impl<'_lšč0, _TŠČ0, _TŠČ1> Wrapper<'_lšč0, _TŠČ0, 14>
    where
        _TŠČ0: Dispatch<Group = _TŠČ1>,
        Self: Wrapper1<'_lšč0, _TŠČ1, _TŠČ0, _TŠČ1>,
    {
        const NAME: &'static str = <Self as Wrapper1<
            '_lšč0,
            _TŠČ1,
            _TŠČ0,
            _TŠČ1,
        >>::NAME;
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

    assert_eq!(
        "1st Blanket A",
        <Wrapper<(String, u32), 12>>::kita(String::new(), 12)
    );
    assert_eq!(
        "1st Blanket A",
        <Wrapper<(Vec::<u32>, u32), 12>>::kita(vec![], 12)
    );
    assert_eq!("1st Blanket B", <Wrapper<(u32, u32), 12>>::kita(12, 12));
    assert_eq!("1st Blanket B", <Wrapper<(i32, u32), 12>>::kita(12, 12));
}
