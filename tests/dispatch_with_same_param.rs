use disjoint_impls::disjoint_impls;

pub trait Dispatch<'a, T> {
    type Group;
}

trait Tr<'a> {}
impl Tr<'_> for String {}
impl Tr<'_> for Vec<u32> {}

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

pub enum GroupC {}
impl Dispatch<'_, ()> for Option<i32> {
    type Group = GroupC;
}
impl Dispatch<'_, ()> for (i32, u32) {
    type Group = GroupC;
}

disjoint_impls! {
    pub trait Kita {
        const NAME: &'static str;
    }

    impl<'a, T: Dispatch<'a, (), Group = GroupC>> Kita for &T {
        const NAME: &'static str = "Blanket C";
    }
    impl<'a, T: Dispatch<'a, (), Group = GroupB>> Kita for &T {
        const NAME: &'static str = "Blanket B";
    }
    impl<'b, 'k, T: Dispatch<'b, (), Group = GroupA>> Kita for &T
    where
        T: Tr<'k>,
    {
        const NAME: &'static str = "Blanket A";
    }
}

/*
pub trait Kita {
    const NAME: &'static str;
}

const _: () = {
    pub trait Kita0<_TŠČ0: ?Sized> {
        const NAME: &'static str;
    }

    impl<'a, '_lšč0, T: Dispatch<'a, (), Group = GroupC>> Kita0<GroupC>
    for &'_lšč0 T {
        const NAME: &'static str = "Blanket C";
    }
    impl<'a, '_lšč0, T: Dispatch<'a, (), Group = GroupB>> Kita0<GroupB>
    for &'_lšč0 T {
        const NAME: &'static str = "Blanket B";
    }
    impl<'b, 'k, '_lšč0, T: Dispatch<'b, (), Group = GroupA>> Kita0<GroupA>
    for &'_lšč0 T
    where
        T: Tr<'k>,
    {
        const NAME: &'static str = "Blanket A";
    }

    impl<'_lšč0, '_lšč1, T: '_lšč0> Kita for &'_lšč0 T
    where
        Self: Kita0<<T as Dispatch<'_lšč1, ()>>::Group>,
        T: Dispatch<'_lšč1, ()>,
    {
        const NAME: &'static str = <Self as Kita0<
            <T as Dispatch<'_lšč1, ()>>::Group,
        >>::NAME;
    }
};
*/

#[test]
fn dispatch_with_same_param() {
    assert_eq!("Blanket A", <&String>::NAME);
    assert_eq!("Blanket A", <&Vec::<u32>>::NAME);
    assert_eq!("Blanket B", <&u32>::NAME);
    assert_eq!("Blanket B", <&i32>::NAME);
    assert_eq!("Blanket C", <&Option::<i32>>::NAME);
    assert_eq!("Blanket C", <&(i32, u32)>::NAME);
}
