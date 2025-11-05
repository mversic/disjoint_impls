use disjoint_impls::disjoint_impls;

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
impl Dispatch for &i32 {
    type Group = GroupB;
}
impl Dispatch for &u32 {
    type Group = GroupB;
}

disjoint_impls! {
    pub trait Kita {
        type Item<'a> where Self: 'a;
        type Other<'a, K> where Self: 'a;

        fn kita<'a>(&'a mut self) -> Self::Item<'a>;

        fn pita<'a, K: 'a>() -> &'static str {
            "Default Blanket"
        }
    }

    impl<T: Dispatch<Group = GroupA>> Kita for T {
        type Item<'a> = &'a u32 where Self: 'a;
        type Other<'a, K> = K where Self: 'a;

        fn kita<'a>(&'a mut self) -> &'a u32 {
            &1
        }
    }
    impl<'a, U> Kita for &'a U where Self: Dispatch<Group = GroupB> {
        type Item<'u> = &'u U where Self: 'u;
        type Other<'u, K> = K where Self: 'u;

        fn kita<'u>(&'u mut self) -> Self::Item<'u> {
            self
        }
    }
}

/*
pub trait Kita {
    type Item<'a> where Self: 'a;
    type Other<'a, K> where Self: 'a;

    fn kita<'a>(&'a mut self) -> Self::Item<'a>;
    fn pita<'a, K: 'a>() -> &'static str {
        "Default Blanket"
    }
}

const _: () = {
    pub trait Kita0<_TŠČ0: ?Sized> {
        type Item<'a> where Self: 'a;
        type Other<'a, K> where Self: 'a;

        fn kita<'a>(&'a mut self) -> Self::Item<'a>;
        fn pita<'a, K: 'a>() -> &'static str {
            "Default Blanket"
        }
    }
    impl<T: Dispatch<Group = GroupA>> Kita0<GroupA> for T {
        type Item<'a> = &'a u32 where Self: 'a;
        type Other<'a, K> = K where Self: 'a;

        fn kita<'a>(&'a mut self) -> &'a u32 {
            &1
        }
    }
    impl<'a, U> Kita0<GroupB> for &'a U
    where
        &'a U: Dispatch<Group = GroupB>,
    {
        type Item<'u> = &'u U where Self: 'u;
        type Other<'u, K> = K where Self: 'u;

        fn kita<'u>(&'u mut self) -> Self::Item<'u> {
            self
        }
    }

    impl<_TŠČ0> Kita for _TŠČ0
    where
        _TŠČ0: Dispatch,
        Self: Kita0<<_TŠČ0 as Dispatch>::Group>,
    {
        type Item<'a> = <Self as Kita0<<_TŠČ0 as Dispatch>::Group>>::Item<'a>
        where
            Self: 'a;
        type Other<'a, K> = <Self as Kita0<<_TŠČ0 as Dispatch>::Group>>::Other<'a, K>
        where
            Self: 'a;

        fn kita<'a>(&'a mut self) -> Self::Item<'a> {
            <Self as Kita0<<_TŠČ0 as Dispatch>::Group>>::kita(self)
        }
        fn pita<'a, K: 'a>() -> &'static str {
            <Self as Kita0<<_TŠČ0 as Dispatch>::Group>>::pita::<K>()
        }
    }
};
*/

#[test]
fn gats() {
    assert_eq!(&1, String::new().kita());
    assert_eq!(&1, Vec::<u32>::new().kita());
    assert_eq!(&12, (&12_u32).kita());
    assert_eq!(&12, (&12_i32).kita());
}
