use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group;
}

pub enum GroupA {}
impl Dispatch for (u16, u32) {
    type Group = GroupA;
}
impl Dispatch for (i16, i32) {
    type Group = GroupA;
}

pub enum GroupB {}
impl Dispatch for (i32, String) {
    type Group = GroupB;
}
impl Dispatch for (u32, String) {
    type Group = GroupB;
}

disjoint_impls! {
    pub trait Kita<U> {
        const NAME: &'static str;
    }

    impl<T, U, C> Kita<(U, C)> for T where (U, C): Dispatch<Group = GroupA> {
        const NAME: &'static str = "1st Blanket A";
    }
    impl<T, U, C> Kita<(U, C)> for T where (U, C): Dispatch<Group = GroupB> {
        const NAME: &'static str = "1st Blanket B";
    }

    impl<T: Dispatch<Group = GroupA>> Kita<(i32,)> for T {
        const NAME: &'static str = "2nd Blanket A";
    }
    impl<T: Dispatch<Group = GroupB>> Kita<(i32,)> for T {
        const NAME: &'static str = "2nd Blanket B";
    }
}

/*
pub trait Kita<U> {
    const NAME: &'static str;
}

const _: () = {
    pub trait _Kita0<_1: ?Sized, U> {
        const NAME: &'static str;
    }
    pub trait _Kita1<_1: ?Sized, U> {
        const NAME: &'static str;
    }

    impl<_2, U, _1> _Kita0<GroupA, (U, _1)> for _2 where (U, _1): Dispatch<Group = GroupA> {
        const NAME: &'static str = "1st Blanket A";
    }
    impl<_2, U, _1> _Kita0<GroupB, (U, _1)> for _2 where (U, _1): Dispatch<Group = GroupB> {
        const NAME: &'static str = "1st Blanket B";
    }

    impl<_1: Dispatch<Group = GroupA>> _Kita1<GroupA, (i32,)> for _1 {
        const NAME: &'static str = "2nd Blanket A";
    }
    impl<_1: Dispatch<Group = GroupB>> _Kita1<GroupB, (i32,)> for _1 {
        const NAME: &'static str = "2nd Blanket B";
    }

    impl<_2, U, _1> Kita<(U, _1)> for _2 where (U, _1): Dispatch, Self: _Kita0<<(U, _1) as Dispatch>::Group, (U, _1)> {
        const NAME: &'static str = <Self as _Kita0<<(U, _1) as Dispatch>::Group, (U, _1)>>::NAME;
    }
    impl<_1> Kita<(i32,)> for _1 where _1: Dispatch, Self: _Kita1<<_1 as Dispatch>::Group, (i32,)> {
        const NAME: &'static str = <Self as _Kita1<<_1 as Dispatch>::Group, (i32,)>>::NAME;
    }
};
*/

#[test]
fn trait_param_with_extra_bound() {
    assert_eq!("1st Blanket A", <u32 as Kita<(u16, u32)>>::NAME);
    assert_eq!("1st Blanket A", <i32 as Kita<(i16, i32)>>::NAME);
    assert_eq!("1st Blanket B", <String as Kita<(i32, String)>>::NAME);
    assert_eq!("1st Blanket B", <Vec<u32> as Kita<(u32, String)>>::NAME);

    assert_eq!("2nd Blanket A", <(u16, u32) as Kita<(i32,)>>::NAME);
    assert_eq!("2nd Blanket A", <(i16, i32) as Kita<(i32,)>>::NAME);
    assert_eq!("2nd Blanket B", <(i32, String) as Kita<(i32,)>>::NAME);
    assert_eq!("2nd Blanket B", <(u32, String) as Kita<(i32,)>>::NAME);
}
