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
    pub trait Kita0<'a, 'b, _TŠČ3: ?Sized, U> {
        fn get_name(&self) -> &str;
    }

    impl<
        '_lšč0,
        '_lšč1,
        '_lšč2,
        '_lšč3,
        T,
        U,
    > Kita0<'_lšč0, '_lšč1, &'_lšč3 GroupA, U> for &'_lšč2 T
    where
        &'_lšč2 T: Dispatch<Group = &'_lšč3 GroupA>,
    {
        fn get_name(&self) -> &str {
            "Blanket A"
        }
    }

    impl<'a, 'b, 'c, '_lšč0, T, U> Kita0<'a, 'b, &'_lšč0 GroupB, U> for &'c T
    where
        &'c T: Dispatch<Group = &'_lšč0 GroupB>,
    {
        fn get_name(&self) -> &str {
            "Blanket B"
        }
    }

    impl<
        '_lšč0,
        '_lšč1,
        '_lšč2,
        '_lšč3,
        U,
        T: '_lšč2,
    > Kita<'_lšč0, '_lšč1, U> for &'_lšč2 T
    where
        '_lšč0:,
        '_lšč1:,
        Self: Kita0<'_lšč0, '_lšč1, <&'_lšč2 T as Dispatch>::Group, U>,
        &'_lšč2 T: Dispatch,
    {
        fn get_name(&self) -> &str {
            <Self as Kita0<
                '_lšč0,
                '_lšč1,
                <&'_lšč2 T as Dispatch>::Group,
                U,
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
