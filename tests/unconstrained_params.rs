use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group;
}

pub trait Tr {
    type A;
}

pub enum GroupA {}
impl Dispatch for String {
    type Group = GroupA;
}
impl Tr for String {
    type A = [(Self,); 1];
}
impl Dispatch for (String,) {
    type Group = [GroupA; 1];
}

impl<T> Dispatch for Vec<T> {
    type Group = GroupA;
}
impl<T> Tr for Vec<T> {
    type A = [(Self,); 1];
}
impl<T> Dispatch for (Vec<T>,) {
    type Group = [u32; 2];
}

pub enum GroupB {}
impl Tr for i32 {
    type A = [i16; 2];
}
impl Dispatch for i32 {
    type Group = GroupB;
}
impl Tr for u32 {
    type A = [i16; 2];
}
impl Dispatch for u32 {
    type Group = GroupB;
}
impl Dispatch for i16 {
    type Group = GroupB;
}

disjoint_impls! {
    pub trait Kita {
        const NAME: &'static str;
    }

    impl<T: Dispatch<Group = GroupA>, A: Dispatch<Group = [Z; 1]>, Z> Kita for T
    where
        Self: Tr<A = [A; 1]>,
    {
        const NAME: &'static str = "1st Blanket AA";
    }
    impl<T: Dispatch<Group = GroupA> + Tr<A = [A; 1]>, A: Dispatch<Group = [Z; 2]>, Z> Kita for T {
        const NAME: &'static str = "1st Blanket AB";
    }
    impl<T: Dispatch<Group = GroupB> + Tr<A = [A; 2]>, A: Dispatch> Kita for T {
        const NAME: &'static str = "1st Blanket B*";
    }

    impl<T: Dispatch<Group = GroupA> + Tr<A = [A; 1]>, A: Dispatch<Group = [u16; 1]>> Kita for [T; 1] {
        const NAME: &'static str = "2nd Blanket AA";
    }
    impl<T: Dispatch<Group = GroupA> + Tr<A = [A; 1]>, A: Dispatch<Group = [u16; 2]>> Kita for [T; 1] {
        const NAME: &'static str = "2nd Blanket AB";
    }
    impl<T: Dispatch<Group = GroupB> + Tr<A = [A; 2]>, A: Dispatch> Kita for [T; 1] {
        const NAME: &'static str = "2nd Blanket B*";
    }
}

/*
pub trait Kita {
    const NAME: &'static str;
}
#[allow(clippy::needless_lifetimes)]
const _: () = {
    pub trait Kita0<_TŠČ0: ?Sized, _TŠČ1: ?Sized, _TŠČ2: ?Sized> {
        const NAME: &'static str;
    }
    impl<
        T: Dispatch<Group = GroupA>,
        A: Dispatch<Group = [Z; 1]>,
        Z,
    > Kita0<GroupA, [A; 1], [Z; 1]> for T
    where
        T: Tr<A = [A; 1]>,
    {
        const NAME: &'static str = "1st Blanket AA";
    }
    impl<
        T: Dispatch<Group = GroupA> + Tr<A = [A; 1]>,
        A: Dispatch<Group = [Z; 2]>,
        Z,
    > Kita0<GroupA, [A; 1], [Z; 2]> for T {
        const NAME: &'static str = "1st Blanket AB";
    }
    impl<
        T: Dispatch<Group = GroupB> + Tr<A = [A; 2]>,
        A: Dispatch,
    > Kita0<GroupB, [A; 2], <A as Dispatch>::Group> for T {
        const NAME: &'static str = "1st Blanket B*";
    }

    pub trait Kita1<_TŠČ0: ?Sized, _TŠČ1: ?Sized, _TŠČ2: ?Sized> {
        const NAME: &'static str;
    }
    impl<
        T: Dispatch<Group = GroupA> + Tr<A = [A; 1]>,
        A: Dispatch<Group = [u16; 1]>,
    > Kita1<GroupA, [A; 1], [u16; 1]> for [T; 1] {
        const NAME: &'static str = "2nd Blanket AA";
    }
    impl<
        T: Dispatch<Group = GroupA> + Tr<A = [A; 1]>,
        A: Dispatch<Group = [u16; 2]>,
    > Kita1<GroupA, [A; 1], [u16; 2]> for [T; 1] {
        const NAME: &'static str = "2nd Blanket AB";
    }
    impl<
        T: Dispatch<Group = GroupB> + Tr<A = [A; 2]>,
        A: Dispatch,
    > Kita1<GroupB, [A; 2], <A as Dispatch>::Group> for [T; 1] {
        const NAME: &'static str = "2nd Blanket B*";
    }

    impl<_TŠČ0, _TŠČ2, const _CŠČ0: usize> Kita for _TŠČ0
    where
        _TŠČ0: Dispatch,
        _TŠČ0: Tr<A = [_TŠČ2; _CŠČ0]>,
        _TŠČ2: Dispatch,
        Self: Kita0<
            <_TŠČ0 as Dispatch>::Group,
            [_TŠČ2; _CŠČ0],
            <_TŠČ2 as Dispatch>::Group,
        >,
    {
        const NAME: &'static str = <Self as Kita0<
            <_TŠČ0 as Dispatch>::Group,
            [_TŠČ2; _CŠČ0],
            <_TŠČ2 as Dispatch>::Group,
        >>::NAME;
    }
    impl<_TŠČ0, _TŠČ2, const _CŠČ0: usize> Kita for [_TŠČ0; 1]
    where
        _TŠČ0: Dispatch,
        _TŠČ0: Tr<A = [_TŠČ2; _CŠČ0]>,
        _TŠČ2: Dispatch,
        Self: Kita1<
            <_TŠČ0 as Dispatch>::Group,
            [_TŠČ2; _CŠČ0],
            <_TŠČ2 as Dispatch>::Group,
        >,
    {
        const NAME: &'static str = <Self as Kita1<
            <_TŠČ0 as Dispatch>::Group,
            [_TŠČ2; _CŠČ0],
            <_TŠČ2 as Dispatch>::Group,
        >>::NAME;
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
