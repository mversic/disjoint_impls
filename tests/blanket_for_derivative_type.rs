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

    impl<T: Dispatch<Group = GroupA>> Kita for Option<T>
    where
        Self: Dispatch<Group = GroupA>,
    {
        const NAME: &str = "Blanket AA";
    }
    impl<T: Dispatch<Group = GroupB>> Kita for Option<T>
    where
        Option<T>: Dispatch<Group = GroupA>,
    {
        const NAME: &'static str = "Blanket BA";
    }
    impl<T: Dispatch> Kita for Option<T>
    where
        Option<T>: Dispatch<Group = GroupB>,
    {
        const NAME: &'static str = "Blanket *B";
    }
}

/*
pub trait Kita {
    const NAME: &'static str;
}

const _: () = {
    pub trait Kita0<_TŠČ0: ?Sized, _TŠČ1: ?Sized> {
        const NAME: &'static str;
    }

    impl<T: Dispatch<Group = GroupA>> Kita0<GroupA, GroupA> for Option<T>
    where
        Option<T>: Dispatch<Group = GroupA>,
    {
        const NAME: &str = "Blanket AA";
    }

    impl<T: Dispatch<Group = GroupB>> Kita0<GroupB, GroupA> for Option<T>
    where
        Option<T>: Dispatch<Group = GroupA>,
    {
        const NAME: &'static str = "Blanket BA";
    }

    impl<T: Dispatch> Kita0<<T as Dispatch>::Group, GroupB> for Option<T>
    where
        Option<T>: Dispatch<Group = GroupB>,
    {
        const NAME: &'static str = "Blanket *B";
    }

    impl<T> Kita for Option<T>
    where
        Self: Kita0<<T as Dispatch>::Group, <Option<T> as Dispatch>::Group>,
        T: Dispatch,
        Option<T>: Dispatch,
    {
        const NAME: &'static str = <Self as Kita0<
            <T as Dispatch>::Group,
            <Option<T> as Dispatch>::Group,
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
