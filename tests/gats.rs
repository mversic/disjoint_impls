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
    impl<U> Kita for &U where Self: Dispatch<Group = GroupB> {
        type Item<'a> = &'a U where Self: 'a;
        type Other<'a, K> = K where Self: 'a;

        fn kita<'a>(&'a mut self) -> Self::Item<'a> {
            self
        }
    }
}

/*
pub trait Kita {
    type Item<'a> where Self: 'a;
    type Other<'a, K> where Self: 'a;

    fn kita<'a>(&'a mut self) -> Self::Item<'a>;
}

const _: () = {
    pub trait Kita0<_TŠČ0: ?Sized> {
        type Item<'a> where Self: 'a;
        fn kita<'a>(&'a mut self) -> Self::Item<'a>;
    }

    impl<T: Dispatch<Group = GroupA>> Kita0<GroupA> for T {
        type Item<'a> = &'a u32 where Self: 'a;
        type Other<'a, K> = K where Self: 'a;

        fn kita<'a>(&'a mut self) -> &'a u32 {
            &1
        }
    }
    impl<'_lšč0, U> Kita0<GroupB> for &'_lšč0 U
    where
        &'_lšč0 U: Dispatch<Group = GroupB>,
    {
        type Item<'a> = &'a U where Self: 'a;
        type Other<'a, K> = K where Self: 'a;

        fn kita<'a>(&'a mut self) -> Self::Item<'a> {
            self
        }
    }

    impl<T> Kita for T
    where
        Self: Kita0<<T as Dispatch>::Group>,
        T: Dispatch,
    {
        type Item<'a> = <Self as Kita0<<T as Dispatch>::Group>>::Item<'a> where Self: 'a;
        type Other<'a, K> = <Self as Kita0<<T as Dispatch>::Group>>::Other<'a, K> where Self: 'a;

        fn kita<'a>(&'a mut self) -> Self::Item<'a> {
            <Self as Kita0<<T as Dispatch>::Group>>::kita::<>(self)
        }

        fn pita<'a, K: 'a>() -> &'static str {
            <Self as Kita0<<T as Dispatch>::Group>>::pita::<K>()
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
