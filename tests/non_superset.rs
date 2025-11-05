use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group;
}

pub enum GroupA {}
impl<'a> Dispatch for &'a String {
    type Group = &'a GroupA;
}
impl<'a, T> Dispatch for &'a Vec<T> {
    type Group = &'a GroupA;
}

pub enum GroupB {}
impl Dispatch for u32 {
    type Group = GroupA;
}
impl<T> Dispatch for Vec<T> {
    type Group = GroupB;
}

disjoint_impls! {
    trait Kita {
        fn get_name() -> &'static str;
    }

    impl<T> Kita for &(T, Vec<T>)
    where
        T: Dispatch<Group = GroupA>,
    {
        fn get_name() -> &'static str {
            "Blanket A"
        }
    }
    impl<X> Kita for &(Vec<X>, Vec<X>)
    where
        Vec<X>: Dispatch<Group = GroupB>,
    {
        fn get_name() -> &'static str {
            "Blanket B"
        }
    }
}

/*
trait Kita {
    fn get_name() -> &'static str;
}

const _: () = {
    pub trait Kita0<_TŠČ0: ?Sized> {
        fn get_name() -> &'static str;
    }
    impl<'_lšč0, T> Kita0<GroupA> for &'_lšč0 (T, Vec<T>)
    where
        T: Dispatch<Group = GroupA>,
    {
        fn get_name() -> &'static str {
            "Blanket A"
        }
    }
    impl<'_lšč0, X> Kita0<GroupB> for &'_lšč0 (Vec<X>, Vec<X>)
    where
        Vec<X>: Dispatch<Group = GroupB>,
    {
        fn get_name() -> &'static str {
            "Blanket B"
        }
    }

    impl<'_lšč0, _TŠČ0: '_lšč0, _TŠČ1: '_lšč0> Kita
    for &'_lšč0 (_TŠČ0, Vec<_TŠČ1>)
    where
        _TŠČ0: Dispatch,
        Self: Kita0<<_TŠČ0 as Dispatch>::Group>,
    {
        fn get_name() -> &'static str {
            <Self as Kita0<<_TŠČ0 as Dispatch>::Group>>::get_name()
        }
    }
};
*/

#[test]
fn non_superset() {
    assert_eq!("Blanket A", <&(u32, Vec<u32>) as Kita>::get_name());
    assert_eq!("Blanket B", <&(Vec<u32>, Vec<u32>) as Kita>::get_name());
}
