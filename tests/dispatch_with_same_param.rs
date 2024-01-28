use disjoint_impls::disjoint_impls;

pub trait Dispatch<'a, T> {
    type Group;
}

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

disjoint_impls! {
    pub trait Kita {
        const NAME: &'static str;
    }

    // NOTE: Dispatch trait parameters must be the same
    impl<'b, 'a, T: Dispatch<'b, (), Group = GroupA>> Kita for &'a T {
        const NAME: &'static str = "Blanket A";
    }
    impl<'a, 'c, T: Dispatch<'a, (), Group = GroupB>> Kita for &'c T {
        const NAME: &'static str = "Blanket B";
    }
}

/*
pub trait Kita {
    const NAME: &'static str;
}

const _: () = {
    pub trait _Kita0<_0: ?Sized> {
        const NAME: &'static str;
    }

    impl<'_0, '_1, _2: Dispatch<'_0, (), Group = GroupA>> _Kita0<GroupA> for &'_1 _2 {
        const NAME: &'static str = "Blanket A";
    }
    impl<'_0, '_1, _2: Dispatch<'_0, (), Group = GroupB>> _Kita0<GroupB> for &'_1 _2 {
        const NAME: &'static str = "Blanket B";
    }

    impl<'_0, '_1, _2> Kita for &'_1 _2 where _2: Dispatch<'_0, ()>, Self: _Kita0<<_2 as Dispatch<'_0, ()>>::Group> {
        const NAME: &'static str = <Self as _Kita0<<_2 as Dispatch<'_0, ()>>::Group>>::NAME;
    }
};
*/

fn main() {
    assert_eq!("Blanket A", <&String>::NAME);
    assert_eq!("Blanket A", <&Vec::<u32>>::NAME);
    assert_eq!("Blanket B", <&u32>::NAME);
    assert_eq!("Blanket B", <&i32>::NAME);
}
