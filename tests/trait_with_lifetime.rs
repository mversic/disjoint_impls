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
    pub trait Kita<'a, 'b: 'a, U: 'b>
    where
        U: 'a,
    {
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
pub trait Kita<'a, 'b: 'a, U: 'b>
where
    U: 'a,
{
    fn get_name(&'b self) -> &'a str;
}

const _: () = {
    pub trait Kita0<'a, 'b: 'a, _TŠČ3: ?Sized, U: 'b>
    where
        U: 'a,
    {
        fn get_name(&'b self) -> &'a str;
    }

    impl<'a, 'x: 'a, T: Dispatch<Group = GroupA>> Kita0<'a, 'x, GroupA, u32> for T {
        fn get_name(&'x self) -> &'a str {
            "Blanket A"
        }
    }
    impl<'a, 'b: 'a, T: Dispatch<Group = GroupB>> Kita0<'a, 'b, GroupB, u32> for T {
        fn get_name(&'b self) -> &'a str {
            "Blanket B"
        }
    }

    impl<'a, 'x, T, _TŠČ1> Kita<'a, 'x, u32> for T
    where
        u32: 'a,
        'a:,
        'x: 'a,
        u32: 'x,
        Self: Kita0<'a, 'x, _TŠČ1, u32>,
        T: Dispatch<Group = _TŠČ1>,
    {
        fn get_name(&'x self) -> &'a str {
            { <Self as Kita0<'a, 'x, _TŠČ1, u32>>::get_name(self) }
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
