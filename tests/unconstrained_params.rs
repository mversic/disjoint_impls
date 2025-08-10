use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group;
}

pub trait A {
    type A;
}

pub enum GroupA {}
impl Dispatch for String {
    type Group = GroupA;
}
impl A for String {
    type A = [(Self,); 1];
}
impl Dispatch for (String,) {
    type Group = [GroupA; 1];
}

impl<T> Dispatch for Vec<T> {
    type Group = GroupA;
}
impl<T> A for Vec<T> {
    type A = [(Self,); 1];
}
impl<T> Dispatch for (Vec<T>,) {
    type Group = [u32; 2];
}

pub enum GroupB {}
impl A for i32 {
    type A = [i16; 2];
}
impl Dispatch for i32 {
    type Group = GroupB;
}
impl A for u32 {
    type A = [i16; 2];
}
impl Dispatch for u32 {
    type Group = GroupB;
}
impl Dispatch for i16 {
    type Group = GroupB;
}

disjoint_impls! {
    pub trait Kita<const N: u32 = 12> {
        const NAME: &'static str;
    }

    impl<T: Dispatch<Group = GroupA> + A<A = [A; 1]>, A: Dispatch<Group = [Z; 1]>, Z> Kita for T {
        const NAME: &'static str = "1st Blanket AA";
    }
    impl<T: Dispatch<Group = GroupA> + A<A = [A; 1]>, A: Dispatch<Group = [Z; 2]>, Z> Kita for T {
        const NAME: &'static str = "1st Blanket AB";
    }
    impl<T: Dispatch<Group = GroupB> + A<A = [A; 2]>, A: Dispatch> Kita for T {
        const NAME: &'static str = "1st Blanket B*";
    }

    impl<T: Dispatch<Group = GroupA> + A<A = [A; 1]>, A: Dispatch<Group = [u16; 1]>> Kita for [T; 1] {
        const NAME: &'static str = "2nd Blanket AA";
    }
    impl<T: Dispatch<Group = GroupA> + A<A = [A; 1]>, A: Dispatch<Group = [u16; 2]>> Kita for [T; 1] {
        const NAME: &'static str = "2nd Blanket AB";
    }
    impl<T: Dispatch<Group = GroupB> + A<A = [A; 2]>, A: Dispatch> Kita for [T; 1] {
        const NAME: &'static str = "2nd Blanket B*";
    }
}

/*
pub trait Kita<const N: u32 = 12> {
    const NAME: &'static str;
}

const _: () = {
    pub trait Kita0<_1: ?Sized, _2: ?Sized, _3: ?Sized, const N: u32 = 12> {
        const NAME: &'static str;
    }
    pub trait Kita1<_1: ?Sized, _2: ?Sized, _3: ?Sized, const N: u32 = 12> {
        const NAME: &'static str;
    }

    impl<
        _0: Dispatch<Group = GroupA> + A<A = [_1; 1]>,
        _1: Dispatch<Group = [_2; 1]>,
        _2,
    > Kita0<GroupA, [_1; 1], [_2; 1]> for _0 {
        const NAME: &'static str = "1st Blanket AA";
    }
    impl<
        _0: Dispatch<Group = GroupA> + A<A = [_1; 1]>,
        _1: Dispatch<Group = [_2; 2]>,
        _2,
    > Kita0<GroupA, [_1; 1], [_2; 2]> for _0 {
        const NAME: &'static str = "1st Blanket AB";
    }
    impl<
        _0: Dispatch<Group = GroupB> + A<A = [_1; 2]>,
        _1: Dispatch,
    > Kita0<GroupB, [_1; 2], <_1 as Dispatch>::Group> for _0 {
        const NAME: &'static str = "1st Blanket B*";
    }

    impl<
        _0: Dispatch<Group = GroupA> + A<A = [_1; 1]>,
        _1: Dispatch<Group = [u16; 1]>,
    > Kita1<GroupA, [_1; 1], [u16; 1]> for [_0; 1] {
        const NAME: &'static str = "2nd Blanket AA";
    }
    impl<
        _0: Dispatch<Group = GroupA> + A<A = [_1; 1]>,
        _1: Dispatch<Group = [u16; 2]>,
    > Kita1<GroupA, [_1; 1], [u16; 2]> for [_0; 1] {
        const NAME: &'static str = "2nd Blanket AB";
    }
    impl<
        _0: Dispatch<Group = GroupB> + A<A = [_1; 2]>,
        _1: Dispatch,
    > Kita1<GroupB, [_1; 2], <_1 as Dispatch>::Group> for [_0; 1] {
        const NAME: &'static str = "2nd Blanket B*";
    }

    impl<_0, _1> Kita for _0
    where
        _0: Dispatch + A,
        _1: Dispatch,
        Self: Kita0<
            <_0 as Dispatch>::Group,
            <_0 as A>::A,
            <_1 as Dispatch>::Group,
        >,
        Self: Kita0Constraint<Bound = _1>,
    {
        const NAME: &'static str = <Self as Kita0<
            <_0 as Dispatch>::Group,
            <_0 as A>::A,
            <_1 as Dispatch>::Group,
        >>::NAME;
    }
    impl<_0, _1> Kita for [_0; 1]
    where
        _0: Dispatch + A,
        _1: Dispatch,
        Self: Kita1<
            <_0 as Dispatch>::Group,
            <_0 as A>::A,
            <_1 as Dispatch>::Group,
        >,
        Self: Kita1Constraint<Bound = _1>,
    {
        const NAME: &'static str = <Self as Kita1<
            <_0 as Dispatch>::Group,
            <_0 as A>::A,
            <_1 as Dispatch>::Group,
        >>::NAME;
    }

    disjoint_impls::disjoint_impls! {
        trait Kita0Constraint {
            type Bound: ?Sized;
        }

        impl<_0: Dispatch<Group = GroupA> + A<A = [_1; 1]>, _1: Dispatch> Kita0Constraint for _0 {
            type Bound = _1;
        }
        impl<_0: Dispatch<Group = GroupB> + A<A = [_1; 2]>, _1: Dispatch> Kita0Constraint for _0 {
            type Bound = _1;
        }
    }

    disjoint_impls::disjoint_impls! {
        trait Kita1Constraint {
            type Bound: ?Sized;
        }

        impl<_0: Dispatch<Group = GroupA> + A<A = [_1; 1]>, _1: Dispatch> Kita1Constraint for [_0; 1] {
            type Bound = _1;
        }
        impl<_0: Dispatch<Group = GroupB> + A<A = [_1; 2]>, _1: Dispatch> Kita1Constraint for [_0; 1] {
            type Bound = _1;
        }
    }
};
*/

#[test]
fn unconstrained_params() {
    assert_eq!("1st Blanket AA", String::NAME);
    assert_eq!("1st Blanket AB", Vec::<u32>::NAME);
    assert_eq!("1st Blanket B*", u32::NAME);
    assert_eq!("1st Blanket B*", i32::NAME);

    assert_eq!("2nd Blanket B*", <[u32; 1]>::NAME);
    assert_eq!("2nd Blanket B*", <[i32; 1]>::NAME);
}
