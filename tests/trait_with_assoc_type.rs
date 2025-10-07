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

    impl<T, U, C> Kita<(U, C), u32> for T
    where
        (U, C): Dispatch<Group = GroupA> + A<B = u32>,
        (u32, (U, C)): A<B = i32>,
    {
        const NAME: &'static str = "1st Blanket A";
    }
    impl<T, U, C> Kita<(U, C), u32> for T
    where
        (U, C): Dispatch<Group = GroupB> + A<B = u32>,
        (u32, (U, C)): A<B = i32>,
    {
        const NAME: &'static str = "1st Blanket B";
    }

    impl<T: Dispatch<Group = GroupA>> Kita<(i32,), u32> for T {
        const NAME: &'static str = "2nd Blanket A";
    }
    impl<T: Dispatch<Group = GroupB>> Kita<(i32,), u32> for T {
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
    pub trait Kita0<
        _TŠČ2: ?Sized,
        _TŠČ3: ?Sized,
        _TŠČ4: ?Sized,
        U: A<B = u32>,
        T = u32,
    >
    where
        (T, U): A<B = i32>,
    {
        const NAME: &'static str;
    }

    pub trait Kita1<_TŠČ2: ?Sized, U: A<B = u32>, T = u32>
    where
        (T, U): A<B = i32>,
    {
        const NAME: &'static str;
    }

    impl<T, U, C> Kita0<GroupA, u32, i32, (U, C), u32> for T
    where
        (U, C): Dispatch<Group = GroupA> + A<B = u32>,
        (u32, (U, C)): A<B = i32>,
    {
        const NAME: &'static str = "1st Blanket A";
    }

    impl<T, U, C> Kita0<GroupB, u32, i32, (U, C), u32> for T
    where
        (U, C): Dispatch<Group = GroupB> + A<B = u32>,
        (u32, (U, C)): A<B = i32>,
    {
        const NAME: &'static str = "1st Blanket B";
    }

    impl<T: Dispatch<Group = GroupA>> Kita1<GroupA, (i32,), u32> for T {
        const NAME: &'static str = "2nd Blanket A";
    }

    impl<T: Dispatch<Group = GroupB>> Kita1<GroupB, (i32,), u32> for T {
        const NAME: &'static str = "2nd Blanket B";
    }

    impl<U, C, T> Kita<(U, C), u32> for T
    where
        (u32, (U, C)): A<B = i32>,
        (U, C): A<B = u32>,
        Self: Kita0<
            <(U, C) as Dispatch>::Group,
            <(U, C) as A>::B,
            <(u32, (U, C)) as A>::B,
            (U, C),
            u32,
        >,
        (U, C): Dispatch,
        (U, C): A,
        (u32, (U, C)): A,
    {
        const NAME: &'static str = <Self as Kita0<
            <(U, C) as Dispatch>::Group,
            <(U, C) as A>::B,
            <(u32, (U, C)) as A>::B,
            (U, C),
            u32,
        >>::NAME;
    }

    impl<T> Kita<(i32,), u32> for T
    where
        (u32, (i32,)): A<B = i32>,
        (i32,): A<B = u32>,
        Self: Kita1<<T as Dispatch>::Group, (i32,), u32>,
        T: Dispatch,
    {
        const NAME: &'static str = <Self as Kita1<
            <T as Dispatch>::Group,
            (i32,),
            u32,
        >>::NAME;
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
