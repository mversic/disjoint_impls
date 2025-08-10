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
    type Group = GroupB;
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

impl<T> A for (u32, T) {
    type B = i32;
}

disjoint_impls! {
    trait Kita<U: A<B = u32>, T = u32>
    where
        (T, U): A<B = i32>,
    {
        const NAME: &'static str;
    }

    impl<T, U, C> Kita<(U, C)> for T
    where
        (U, C): Dispatch<Group = GroupA> + A<B = u32>,
        (u32, (U, C)): A<B = i32>,
    {
        const NAME: &'static str = "1st Blanket A";
    }
    impl<T, U, C> Kita<(U, C)> for T
    where
        (U, C): Dispatch<Group = GroupB> + A<B = u32>,
        (u32, (U, C)): A<B = i32>,
    {
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
trait Kita<U: A<B = u32>, T = u32>
where
    (T, U): A<B = i32>,
{
    const NAME: &'static str;
}

const _: () = {
    pub trait Kita0<_2: ?Sized, _3: ?Sized, U: A<B = u32>, T = u32>
    where
        (T, U): A<B = i32>,
    {
        const NAME: &'static str;
    }
    pub trait Kita1<_2: ?Sized, U: A<B = u32>, T = u32>
    where
        (T, U): A<B = i32>,
    {
        const NAME: &'static str;
    }

    impl<_2, _0, _1> Kita0<GroupA, i32, (_0, _1)> for _2
    where
        (_0, _1): Dispatch<Group = GroupA> + A<B = u32>,
        (u32, (_0, _1)): A<B = i32>,
    {
        const NAME: &'static str = "1st Blanket A";
    }
    impl<_2, _0, _1> Kita0<GroupB, i32, (_0, _1)> for _2
    where
        (_0, _1): Dispatch<Group = GroupB> + A<B = u32>,
        (u32, (_0, _1)): A<B = i32>,
    {
        const NAME: &'static str = "1st Blanket B";
    }

    impl<_0: Dispatch<Group = GroupA>> Kita1<GroupA, (i32,)> for _0 {
        const NAME: &'static str = "2nd Blanket A";
    }
    impl<_0: Dispatch<Group = GroupB>> Kita1<GroupB, (i32,)> for _0 {
        const NAME: &'static str = "2nd Blanket B";
    }

    impl<_0, _1, _2> Kita<(_0, _1)> for _2
    where
        (u32, (_0, _1)): A<B = i32>,
        (_0, _1): A<B = u32>,
        (_0, _1): Dispatch,
        (u32, (_0, _1)): A,
        Self: Kita0<<(_0, _1) as Dispatch>::Group, <(u32, (_0, _1)) as A>::B, (_0, _1)>,
    {
        const NAME: &'static str = <Self as Kita0<
            <(_0, _1) as Dispatch>::Group,
            <(u32, (_0, _1)) as A>::B,
            (_0, _1),
        >>::NAME;
    }
    impl<_0> Kita<(i32,)> for _0
    where
        (u32, (i32,)): A<B = i32>,
        (i32,): A<B = u32>,
        _0: Dispatch,
        Self: Kita1<<_0 as Dispatch>::Group, (i32,)>,
    {
        const NAME: &'static str = <Self as Kita1<<_0 as Dispatch>::Group, (i32,)>>::NAME;
    }
};
*/

#[test]
fn trait_with_assoc_type() {
    assert_eq!("1st Blanket A", <u32 as Kita<(u16, u32)>>::NAME);
    assert_eq!("1st Blanket B", <u32 as Kita<(i32, String)>>::NAME);
    assert_eq!("2nd Blanket A", <(u16, u32) as Kita<(i32,)>>::NAME);
    assert_eq!("2nd Blanket B", <(i16, i32) as Kita<(i32,)>>::NAME);
}
