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
    pub trait Kita<'a> {
        fn get_name(&self) -> &'a str;
    }

    impl<'a, T: Dispatch<Group = GroupA>> Kita<'a> for T {
        fn get_name(&self) -> &'a str {
            "Blanket A"
        }
    }
    impl<'a, T: Dispatch<Group = GroupB>> Kita<'a> for T {
        fn get_name(&self) -> &'a str {
            "Blanket B"
        }
    }
}

/*
pub trait Kita<'a> {
    fn get_name(&self) -> &'a str;
}
const _: () = {
    pub trait _Kita<'a, T: ?Sized> {
        fn get_name(&self) -> &'a str;
    }
    impl<'a, T: Dispatch<Group = GroupA>> _Kita<'a, GroupA> for T {
        fn get_name(&self) -> &'a str {
            "Blanket A"
        }
    }
    impl<'a, T: Dispatch<Group = GroupB>> _Kita<'a, GroupB> for T {
        fn get_name(&self) -> &'a str {
            "Blanket B"
        }
    }
    impl<'a, T> Kita<'a> for T
    where
        T: Dispatch,
        Self: _Kita<'a, <T as Dispatch>::Group>,
    {
        fn get_name(&self) -> &'a str {
            <Self as _Kita<<T as Dispatch>::Group>>::get_name(self)
        }
    }
};
*/

#[test]
fn main() {
    assert_eq!("Blanket A", <String as Kita>::get_name(&"".into()));
    assert_eq!("Blanket A", <Vec::<u32> as Kita>::get_name(&vec![]));
    assert_eq!("Blanket B", <u32 as Kita>::get_name(&0));
    assert_eq!("Blanket B", <i32 as Kita>::get_name(&0));
}
