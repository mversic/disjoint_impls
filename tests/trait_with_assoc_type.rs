use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group;
}

trait A {
    type B;
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

impl A for (u16, u32) {
    type B = u32;
}

impl A for (i32, String) {
    type B = u32;
}

impl A for (i32,) {
    type B = u32;
}

disjoint_impls! {
    trait Kita<U: A<B = u32>> {
        const NAME: &'static str;
    }

    impl<T, U, C> Kita<(U, C)> for T where (U, C): Dispatch<Group = GroupA> + A<B = u32> {
        const NAME: &'static str = "1st blanket A";
    }
    impl<T, U, C> Kita<(U, C)> for T where (U, C): Dispatch<Group = GroupB> + A<B = u32> {
        const NAME: &'static str = "1st blanket B";
    }

    impl<T: Dispatch<Group = GroupA>> Kita<(i32,)> for T {
        const NAME: &'static str = "2nd blanket A";
    }
    impl<T: Dispatch<Group = GroupB>> Kita<(i32,)> for T {
        const NAME: &'static str = "2nd blanket B";
    }
}

/*
pub trait Kita<U: A<B = u32>> {
    const NAME: &'static str;
}

const _: () = {
    pub trait _Kita0<_ŠČ1: ?Sized, _ŠČ2: ?Sized, U: A<B = u32>> {
        const NAME: &'static str;
    }
    pub trait _Kita1<_ŠČ1: ?Sized, U: A<B = u32>> {
        const NAME: &'static str;
    }

    impl<_ŠČ2, _ŠČ0, _ŠČ1> _Kita0<GroupA, u32, (_ŠČ0, _ŠČ1)> for _ŠČ2 where (_ŠČ0, _ŠČ1): Dispatch<Group = GroupA> + A<B = u32> {
        const NAME: &'static str = "1st blanket A";
    }
    impl<_ŠČ2, _ŠČ0, _ŠČ1> _Kita0<GroupB, u32, (_ŠČ0, _ŠČ1)> for _ŠČ2 where (_ŠČ0, _ŠČ1): Dispatch<Group = GroupB> + A<B = u32> {
        const NAME: &'static str = "1st blanket B";
    }

    impl<_ŠČ0: Dispatch<Group = GroupA>> _Kita1<GroupA, (i32,)> for _ŠČ0 {
        const NAME: &'static str = "2nd blanket A";
    }
    impl<_ŠČ0: Dispatch<Group = GroupB>> _Kita1<GroupB, (i32,)> for _ŠČ0 {
        const NAME: &'static str = "2nd blanket B";
    }

    impl<_ŠČ0, _ŠČ1, _ŠČ2> Kita<(_ŠČ0, _ŠČ1)> for _ŠČ2 where (_ŠČ0, _ŠČ1): A<B = u32>, (_ŠČ0, _ŠČ1): Dispatch + A, Self: _Kita0<<(_ŠČ0, _ŠČ1) as Dispatch>::Group, <(_ŠČ0, _ŠČ1) as A>::B, (_ŠČ0, _ŠČ1)> {
        const NAME: &'static str = <Self as _Kita0<<(_ŠČ0, _ŠČ1) as Dispatch>::Group, <(_ŠČ0, _ŠČ1) as A>::B, (_ŠČ0, _ŠČ1)>>::NAME;
    }
    impl<_ŠČ0> Kita<(i32,)> for _ŠČ0 where (i32,): A<B = u32>, _ŠČ0: Dispatch, Self: _Kita1<<_ŠČ0 as Dispatch>::Group, (i32,)> {
        const NAME: &'static str = <Self as _Kita1<<_ŠČ0 as Dispatch>::Group, (i32,)>>::NAME;
    }
};
*/

fn main() {
    assert_eq!("1st Blanket A", <u32 as Kita<(u16, u32)>>::NAME);
    assert_eq!("1st Blanket B", <u32 as Kita<(i32, String)>>::NAME);
    assert_eq!("2nd Blanket A", <(u16, u32) as Kita<(i32,)>>::NAME);
    assert_eq!("2nd Blanket B", <(i16, i32) as Kita<(i32,)>>::NAME);
}
