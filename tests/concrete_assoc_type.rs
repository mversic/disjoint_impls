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
impl Dispatch for i32 {
    type Group = GroupB;
}
impl Dispatch for u32 {
    type Group = GroupB;
}

disjoint_impls! {
    pub trait Kita {
        type Item;

        fn kita() -> Self::Item;
    }

    impl<T: Dispatch<Group = GroupA>> Kita for T {
        type Item = u32;

        fn kita() -> u32 {
            1
        }
    }
    impl<U: Dispatch<Group = GroupB> + Default> Kita for U {
        type Item = U;

        fn kita() -> Self::Item {
            <U as Default>::default()
        }
    }
}

/*
pub trait Kita {
    type Item;
    fn kita() -> Self::Item;
}

const _: () = {
    pub trait Kita0<_TŠČ0: ?Sized> {
        type Item;
        fn kita() -> Self::Item;
    }

    impl<T: Dispatch<Group = GroupA>> Kita0<GroupA> for T {
        type Item = u32;
        fn kita() -> u32 {
            1
        }
    }
    impl<U: Dispatch<Group = GroupB> + Default> Kita0<GroupB> for U {
        type Item = U;
        fn kita() -> Self::Item {
            <U as Default>::default()
        }
    }

    impl<T, _TŠČ1> Kita for T
    where
        Self: Kita0<_TŠČ1>,
        T: Dispatch<Group = _TŠČ1>,
    {
        type Item = <Self as Kita0<_TŠČ1>>::Item;
        fn kita() -> Self::Item {
            { <Self as Kita0<_TŠČ1>>::kita() }
        }
    }
};
*/

#[test]
fn concrete_assoc_type() {
    assert_eq!(1, String::kita());
    assert_eq!(1, Vec::<u32>::kita());
    assert_eq!(0, u32::kita());
    assert_eq!(0, i32::kita());
}
