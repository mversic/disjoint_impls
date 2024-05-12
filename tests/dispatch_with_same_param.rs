use disjoint_impls::disjoint_impls;

pub trait Dispatch<'a, T> {
    type Group;
}

trait T<'a> {}
impl T<'_> for String {}
impl T<'_> for Vec<u32> {}

pub enum GroupA {}
impl Dispatch<'_, ()> for String {
    type Group = GroupA;
}
impl<T> Dispatch<'_, ()> for Vec<T> {
    type Group = GroupA;
}

pub enum GroupB {}
impl Dispatch<'_, ()> for i32 {
    type Group = GroupB;
}
impl Dispatch<'_, ()> for u32 {
    type Group = GroupB;
}

//disjoint_impls! {
//    pub trait Kita {
//        const NAME: &'static str;
//    }
//
//    // NOTE: Dispatch trait parameters must be the same
//    impl<'b, 'k, 'a, T: Dispatch<'b, (), Group = GroupA>> Kita for &'a T where T: T<'k> {
//        const NAME: &'static str = "Blanket A";
//    }
//    impl<'a, 'c, T: Dispatch<'a, (), Group = GroupB>> Kita for &'c T {
//        const NAME: &'static str = "Blanket B";
//    }
//}

/*
pub trait Kita {
    const NAME: &'static str;
}

const _: () = {
    pub trait _Kita0<_0: ?Sized> {
        const NAME: &'static str;
    }

    impl<'_2, '_3, '_0, _1: Dispatch<'_2, (), Group = GroupA>> _Kita0<GroupA> for &'_0 _1 where _1: T<'_3> {
        const NAME: &'static str = "Blanket A";
    }
    impl<'_2, '_0, _1: Dispatch<'_2, (), Group = GroupB>> _Kita0<GroupB> for &'_0 _1 {
        const NAME: &'static str = "Blanket B";
    }

    impl<'_0, '_2, _1> Kita for &'_0 _1 where _1: Dispatch<'_2, ()>, Self: _Kita0<<_1 as Dispatch<'_2, ()>>::Group> {
        const NAME: &'static str = <Self as _Kita0<<_1 as Dispatch<'_2, ()>>::Group>>::NAME;
    }
};
*/

#[test]
fn dispatch_with_same_param() {
    //assert_eq!("Blanket A", <&String>::NAME);
    //assert_eq!("Blanket A", <&Vec::<u32>>::NAME);
    //assert_eq!("Blanket B", <&u32>::NAME);
    //assert_eq!("Blanket B", <&i32>::NAME);
}
