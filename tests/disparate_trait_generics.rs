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

    impl<T, U, C> Kita<(U, C)> for T
    where
        (U, C): Dispatch<Group = GroupA>,
    {
        const NAME: &'static str = "1st Blanket A";
    }
    impl<T, U, C> Kita<(U, C)> for T
    where
        (U, C): Dispatch<Group = GroupB>,
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
pub trait Kita<U> {
    const NAME: &'static str;
}

const _: () = {
    pub trait Kita0<_TŠČ1: ?Sized, U> {
        const NAME: &'static str;
    }

    pub trait Kita1<_TŠČ1: ?Sized, U> {
        const NAME: &'static str;
    }

    impl<T, U, C> Kita0<GroupA, (U, C)> for T
    where
        (U, C): Dispatch<Group = GroupA>,
    {
        const NAME: &'static str = "1st Blanket A";
    }

    impl<T, U, C> Kita0<GroupB, (U, C)> for T
    where
        (U, C): Dispatch<Group = GroupB>,
    {
        const NAME: &'static str = "1st Blanket B";
    }

    impl<T: Dispatch<Group = GroupA>> Kita1<GroupA, (i32,)> for T {
        const NAME: &'static str = "2nd Blanket A";
    }

    impl<T: Dispatch<Group = GroupB>> Kita1<GroupB, (i32,)> for T {
        const NAME: &'static str = "2nd Blanket B";
    }

    impl<U, C, T> Kita<(U, C)> for T
    where
        Self: Kita0<<(U, C) as Dispatch>::Group, (U, C)>,
        (U, C): Dispatch,
    {
        const NAME: &'static str = <Self as Kita0<
            <(U, C) as Dispatch>::Group,
            (U, C),
        >>::NAME;
    }

    impl<T> Kita<(i32,)> for T
    where
        Self: Kita1<<T as Dispatch>::Group, (i32,)>,
        T: Dispatch,
    {
        const NAME: &'static str = <Self as Kita1<<T as Dispatch>::Group, (i32,)>>::NAME;
    }
};
*/

#[test]
fn disparate_trait_generics() {
    assert_eq!("1st Blanket A", <u32 as Kita<(u16, u32)>>::NAME);
    assert_eq!("1st Blanket A", <i32 as Kita<(i16, i32)>>::NAME);
    assert_eq!("1st Blanket B", <String as Kita<(i32, String)>>::NAME);
    assert_eq!("1st Blanket B", <Vec<u32> as Kita<(u32, String)>>::NAME);

    assert_eq!("2nd Blanket A", <(u16, u32) as Kita<(i32,)>>::NAME);
    assert_eq!("2nd Blanket A", <(i16, i32) as Kita<(i32,)>>::NAME);
    assert_eq!("2nd Blanket B", <(i32, String) as Kita<(i32,)>>::NAME);
    assert_eq!("2nd Blanket B", <(u32, String) as Kita<(i32,)>>::NAME);
}
