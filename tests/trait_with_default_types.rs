use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group;
}

pub trait Trait {}

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

impl Trait for u32 {}
impl<T> Trait for [T; 1] {}

disjoint_impls! {
    pub trait Kita<T: Trait = u32, const N: usize = 12> {
        const NAME: &'static str;
    }

    impl<T: Dispatch<Group = GroupA>> Kita for T {
        const NAME: &'static str = "1st Blanket A";
    }
    impl<U: Dispatch<Group = GroupB>> Kita for U {
        const NAME: &'static str = "1st Blanket B";
    }

    impl<T: Dispatch<Group = GroupA>> Kita<[T; 1], 42> for T {
        const NAME: &'static str = "2nd Blanket A";
    }
    impl<U: Dispatch<Group = GroupB>> Kita<[U; 1], 42> for U {
        const NAME: &'static str = "2nd Blanket B";
    }
}

/*
pub trait Kita<T: Trait = u32, const N: usize = 12> {
    const NAME: &'static str;
}

const _: () = {
    pub trait _Kita0<_ŠČ2: ?Sized, T: Trait = u32, const N: usize = 12> {
        const NAME: &'static str;
    }
    pub trait _Kita1<_ŠČ2: ?Sized, T: Trait = u32, const N: usize = 12> {
        const NAME: &'static str;
    }

    impl<_ŠČ0: Dispatch<Group = GroupA>> _Kita0<GroupA> for _ŠČ0 {
        const NAME: &'static str = "1st Blanket A";
    }
    impl<_ŠČ0: Dispatch<Group = GroupB>> _Kita0<GroupB> for _ŠČ0 {
        const NAME: &'static str = "1st Blanket B";
    }
    impl<_ŠČ0: Dispatch<Group = GroupA>> _Kita1<GroupA, [_ŠČ0; 1], 42> for _ŠČ0 {
        const NAME: &'static str = "2nd Blanket A";
    }
    impl<_ŠČ0: Dispatch<Group = GroupB>> _Kita1<GroupB, [_ŠČ0; 1], 42> for _ŠČ0 {
        const NAME: &'static str = "2nd Blanket B";
    }

    impl<_ŠČ0> Kita for _ŠČ0 where _ŠČ0: Dispatch, Self: _Kita0<<_ŠČ0 as Dispatch>::Group> {
        const NAME: &'static str = <Self as _Kita0<<_ŠČ0 as Dispatch>::Group>>::NAME;
    }
    impl<_ŠČ0> Kita<[_ŠČ0; 1], 42> for _ŠČ0 where [_ŠČ0; 1]: Trait, _ŠČ0: Dispatch, Self: _Kita1<<_ŠČ0 as Dispatch>::Group, [_ŠČ0; 1], 42> {
        const NAME: &'static str = <Self as _Kita1<<_ŠČ0 as Dispatch>::Group, [_ŠČ0; 1], 42>>::NAME;
    }
};
*/

#[test]
fn trait_with_default_types() {
    assert_eq!("1st Blanket A", <String as Kita>::NAME);
    assert_eq!("2nd Blanket A", <Vec::<u32> as Kita<[Vec<u32>; 1], 42>>::NAME);
    assert_eq!("1st Blanket B", <u32 as Kita>::NAME);
    assert_eq!("2nd Blanket B", <i32 as Kita<[i32; 1], 42>>::NAME);
}
