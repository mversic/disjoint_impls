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
pub trait Kita<'L0, 'L1: 'L0> {
    fn get_name(&'L1 self) -> &'L0 str;
}
const _: () = {
    pub trait _Kita<'L0, 'L1: 'L0, T0: ?Sized> {
        fn get_name(&'L1 self) -> &'L0 str;
    }

    impl<'L0, 'L1: 'L0, T0: Dispatch<Group = GroupA>> _Kita<'L0, 'L1, GroupA> for T0 {
        fn get_name(&'L1 self) -> &'L0 str {
            "Blanket A"
        }
    }
    impl<'L0, 'L1: 'L0, T0: Dispatch<Group = GroupB>> _Kita<'L0, 'L1, GroupB> for T0 {
        fn get_name(&'L1 self) -> &'L0 str {
            "Blanket B"
        }
    }

    impl<'L0, 'L1: 'L0, T0> Kita<'L0, 'L1> for T0 where T0: Dispatch, Self: _Kita<'L0, 'L1, <T0 as Dispatch>::Group> {
        fn get_name(&'L1 self) -> &'L0 str {
            <Self as _Kita<<T0 as Dispatch>::Group>>::get_name(self)
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
