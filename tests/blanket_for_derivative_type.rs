use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group;
}

pub enum GroupA {}
pub enum GroupB {}

impl Dispatch for String {
    type Group = GroupA;
}
impl Dispatch for Option<String> {
    type Group = GroupA;
}

impl<T> Dispatch for Vec<T> {
    type Group = GroupB;
}
impl<T> Dispatch for Option<Vec<T>> {
    type Group = GroupA;
}

impl Dispatch for i32 {
    type Group = GroupA;
}
impl Dispatch for Option<i32> {
    type Group = GroupB;
}

impl Dispatch for u32 {
    type Group = GroupB;
}
impl Dispatch for Option<u32> {
    type Group = GroupB;
}

disjoint_impls! {
    pub trait Kita {
        const NAME: &'static str;
    }

    impl<T: Dispatch<Group = GroupA>> Kita for Option<T> where Self: Dispatch<Group = GroupA> {
        const NAME: &'static str = "Blanket AA";
    }
    impl<T: Dispatch<Group = GroupB>> Kita for Option<T> where Option<T>: Dispatch<Group = GroupA> {
        const NAME: &'static str = "Blanket BA";
    }
    impl<T: Dispatch> Kita for Option<T> where Option<T>: Dispatch<Group = GroupB> {
        const NAME: &'static str = "Blanket *B";
    }
}

/*
pub trait Kita {
    const NAME: &'static str;
}

const _: () = {
    /**Helper trait with arguments: < _0 as Dispatch > :: Group,
< Option < _0 > as Dispatch > :: Group*/
    pub trait Kita0<_0: ?Sized, _1: ?Sized> {
        const NAME: &'static str;
    }

    impl<_0: Dispatch<Group = GroupA>> Kita0<GroupA, GroupA> for Option<_0>
    where
        Self: Dispatch<Group = GroupA>,
    {
        const NAME: &'static str = "Blanket AA";
    }
    impl<_0: Dispatch<Group = GroupB>> Kita0<GroupB, GroupA> for Option<_0>
    where
        Option<_0>: Dispatch<Group = GroupA>,
    {
        const NAME: &'static str = "Blanket BA";
    }
    impl<_0: Dispatch> Kita0<<_0 as Dispatch>::Group, GroupB> for Option<_0>
    where
        Option<_0>: Dispatch<Group = GroupB>,
    {
        const NAME: &'static str = "Blanket *B";
    }

    impl<_0> Kita for Option<_0>
    where
        _0: Dispatch,
        Option<_0>: Dispatch,
        Self: Kita0<<_0 as Dispatch>::Group, <Option<_0> as Dispatch>::Group>,
    {
        const NAME: &'static str = <Self as Kita0<
            <_0 as Dispatch>::Group,
            <Option<_0> as Dispatch>::Group,
        >>::NAME;
    }
};
*/

#[test]
fn blanket_for_derivative_type() {
    assert_eq!("Blanket AA", Option::<String>::NAME);
    assert_eq!("Blanket BA", Option::<Vec::<u32>>::NAME);
    assert_eq!("Blanket *B", Option::<u32>::NAME);
    assert_eq!("Blanket *B", Option::<i32>::NAME);
}
