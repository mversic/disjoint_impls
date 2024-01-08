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
    pub trait Kita<'a, 'b: 'a> {
        fn get_name(&'b self) -> &'a str;
    }

    impl<'a, 'x: 'a, T: Dispatch<Group = GroupA>> Kita<'a, 'x> for T {
        fn get_name(&'x self) -> &'a str {
            "Blanket A"
        }
    }
    impl<'a, 'b: 'a, T: Dispatch<Group = GroupB>> Kita<'a, 'b> for T {
        fn get_name(&'b self) -> &'a str {
            "Blanket B"
        }
    }
}

/*
pub trait Kita<'a, 'b: 'a> {
    fn get_name(&'b self) -> &'a str;
}

const _: () = {
    pub trait _Kita0<'a, 'b: 'a, _0: ?Sized> {
        fn get_name(&'b self) -> &'a str;
    }

    impl<'_0, '_1: '_0, _2: Dispatch<Group = GroupA>> _Kita0<'_0, '_1, GroupA> for _2 {
        fn get_name(&'_1 self) -> &'_0 str {
            "Blanket A"
        }
    }
    impl<'_0, '_1: '_0, _2: Dispatch<Group = GroupB>> _Kita0<'_0, '_1, GroupB> for _2 {
        fn get_name(&'_1 self) -> &'_0 str {
            "Blanket B"
        }
    }

    impl<'_0, '_1, _2> Kita<'_0, '_1> for _2 where '_1: '_0, _2: Dispatch, Self: _Kita0<'_0, '_1, <_2 as Dispatch>::Group> {
        fn get_name(&'_1 self) -> &'_0 str {
            <Self as _Kita0<'_0, '_1, <_2 as Dispatch>::Group>>::get_name(self)
        }
    }
};
*/

#[test]
fn trait_with_lifetime() {
    assert_eq!("Blanket A", <String as Kita>::get_name(&"".into()));
    assert_eq!("Blanket A", <Vec::<u32> as Kita>::get_name(&vec![]));
    assert_eq!("Blanket B", <u32 as Kita>::get_name(&0));
    assert_eq!("Blanket B", <i32 as Kita>::get_name(&0));
}
