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
impl<'a> Dispatch for &'a i32 {
    type Group = &'a GroupB;
}
impl<'a> Dispatch for &'a u32 {
    type Group = &'a GroupB;
}

disjoint_impls! {
    pub trait Kita<'a, 'b, U> {
        fn get_name(&self) -> &str;
    }

    impl<T, U> Kita<'_, '_, U> for &T
    where
        Self: Dispatch<Group = &GroupA>,
    {
        fn get_name(&self) -> &str {
            "Blanket A"
        }
    }
    impl<'a, 'b, 'c, T, U> Kita<'a, 'b, U> for &'c T
    where
        Self: Dispatch<Group = &GroupB>,
    {
        fn get_name(&self) -> &str {
            "Blanket B"
        }
    }
}

/*
pub trait Kita<'a, 'b, U> {
    fn get_name(&self) -> &str;
}
const _: () = {
    /**Helper trait with arguments corresponding to the following associated bindings:
0. < & '_3 _4 as Dispatch > :: Group
*/
    pub trait Kita0<'a, 'b, _3: ?Sized, U> {
        fn get_name(&self) -> &str;
    }
    impl<
        '_0,
        '_1,
        '_3,
        '_5,
        _4,
        _2,
    > Kita0<'_0, '_1, &'_5 GroupA, _2> for &'_3 _4
    where
        &'_3 _4: Dispatch<Group = &'_5 GroupA>,
    {
        fn get_name(&self) -> &str {
            "Blanket A"
        }
    }
    impl<
        '_0,
        '_1,
        '_3,
        _4,
        _2,
    > Kita0<'_0, '_1, GroupB, _2> for &'_3 _4
    where
        &'_3 _4: Dispatch<Group = GroupB>,
    {
        fn get_name(&self) -> &str {
            "Blanket B"
        }
    }
    impl<
        '_0,
        '_1,
        '_3,
        '_5,
        _2,
        _4,
    > Kita<'_0, '_1, _2> for &'_3 _4
    where
        '_0:,
        '_1:,
        &'_3 _4: Dispatch,
        Self: Kita0<'_0, '_1, <&'_3 _4 as Dispatch>::Group, _2>,
    {
        fn get_name(&self) -> &str {
            <Self as Kita0<
                '_0,
                '_1,
                <&'_3 _4 as Dispatch>::Group,
                _2,
            >>::get_name(self)
        }
    }
};
*/

#[test]
fn elided_lifetime() {
    assert_eq!("Blanket A", <&String as Kita<u32>>::get_name(&&"".into()));
    assert_eq!("Blanket A", <&Vec::<u32> as Kita<u32>>::get_name(&&vec![]));
    assert_eq!("Blanket B", <&u32 as Kita<u32>>::get_name(&&0));
    assert_eq!("Blanket B", <&i32 as Kita<u32>>::get_name(&&0));
}
