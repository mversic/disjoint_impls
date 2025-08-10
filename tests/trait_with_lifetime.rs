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
    pub trait Kita<'a, 'b: 'a, U> {
        fn get_name(&'b self) -> &'a str;
    }

    impl<'a, 'x: 'a, T: Dispatch<Group = GroupA>> Kita<'a, 'x, u32> for T {
        fn get_name(&'x self) -> &'a str {
            "Blanket A"
        }
    }
    impl<'a, 'b: 'a, T: Dispatch<Group = GroupB>> Kita<'a, 'b, u32> for T {
        fn get_name(&'b self) -> &'a str {
            "Blanket B"
        }
    }
}

/*
pub trait Kita<'a, 'b: 'a, U> {
    fn get_name(&'b self) -> &'a str;
}

const _: () = {
    pub trait Kita0<'a, 'b: 'a, _3: ?Sized, U> {
        fn get_name(&'b self) -> &'a str;
    }

    impl<'_0, '_1: '_0, _2: Dispatch<Group = GroupA>> Kita0<'_0, '_1, GroupA, u32> for _2 {
        fn get_name(&'_1 self) -> &'_0 str {
            "Blanket A"
        }
    }
    impl<'_0, '_1: '_0, _2: Dispatch<Group = GroupB>> Kita0<'_0, '_1, GroupB, u32> for _2 {
        fn get_name(&'_1 self) -> &'_0 str {
            "Blanket B"
        }
    }

    impl<'_0, '_1, _2> Kita<'_0, '_1, u32> for _2
    where
        '_1: '_0,
        _2: Dispatch,
        Self: Kita0<'_0, '_1, <_2 as Dispatch>::Group, u32>,
    {
        fn get_name(&'_1 self) -> &'_0 str {
            <Self as Kita0<'_0, '_1, <_2 as Dispatch>::Group, u32>>::get_name(self)
        }
    }
};
*/

#[test]
fn trait_with_lifetime() {
    assert_eq!("Blanket A", <String as Kita<u32>>::get_name(&"".into()));
    assert_eq!("Blanket A", <Vec::<u32> as Kita<u32>>::get_name(&vec![]));
    assert_eq!("Blanket B", <u32 as Kita<u32>>::get_name(&0));
    assert_eq!("Blanket B", <i32 as Kita<u32>>::get_name(&0));
}
