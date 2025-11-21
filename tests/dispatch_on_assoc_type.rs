use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group;
}

pub enum GroupA {}
pub enum GroupB {}
pub enum GroupC {}
pub enum GroupD {}

impl Dispatch for String {
    type Group = i32;
}
impl<T> Dispatch for Vec<T> {
    type Group = u32;
}
impl Dispatch for &str {
    type Group = i16;
}
impl<T> Dispatch for &[T] {
    type Group = u16;
}

impl Dispatch for i32 {
    type Group = GroupA;
}
impl Dispatch for u32 {
    type Group = GroupB;
}

impl Dispatch for i16 {
    type Group = GroupC;
}
impl Dispatch for u16 {
    type Group = GroupD;
}

disjoint_impls! {
    pub trait Kita {
        const NAME: &'static str;
    }

    impl<T> Kita for T
    where
        T: Dispatch,
        <T as Dispatch>::Group: Dispatch<Group = GroupA>,
    {
        const NAME: &'static str = "Blanket A";
    }

    impl<T: Dispatch> Kita for T
    where
        <Self as Dispatch>::Group: Dispatch<Group = GroupB>,
    {
        const NAME: &'static str = "Blanket B";
    }

    impl<T: Dispatch> Kita for T
    where
        <T as Dispatch>::Group: Dispatch<Group = GroupC>,
    {
        const NAME: &'static str = "Blanket C";
    }

    impl<T: Dispatch<Group: Dispatch<Group = GroupD>>> Kita for T {
        const NAME: &'static str = "Blanket D";
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
    impl<T> Kita0<<T as Dispatch>::Group, GroupA> for T
    where
        T: Dispatch,
        <T as Dispatch>::Group: Dispatch<Group = GroupA>,
    {
        const NAME: &'static str = "Blanket A";
    }
    impl<T: Dispatch> Kita0<<T as Dispatch>::Group, GroupB> for T
    where
        <T as Dispatch>::Group: Dispatch<Group = GroupB>,
    {
        const NAME: &'static str = "Blanket B";
    }
    impl<T: Dispatch> Kita0<<T as Dispatch>::Group, GroupC> for T
    where
        <T as Dispatch>::Group: Dispatch<Group = GroupC>,
    {
        const NAME: &'static str = "Blanket C";
    }
    impl<
        T: Dispatch<Group = _TŠČ0>,
        _TŠČ0: Dispatch<Group = GroupD> + ?Sized,
    > Kita0<_TŠČ0, GroupD> for T {
        const NAME: &'static str = "Blanket D";
    }

    impl<_TŠČ0, _TŠČ1: ?Sized> Kita for _TŠČ0
    where
        _TŠČ0: Dispatch<Group = _TŠČ1>,
        _TŠČ1: Dispatch,
        Self: Kita0<_TŠČ1, <_TŠČ1 as Dispatch>::Group>,
    {
        const NAME: &'static str = <Self as Kita0<
            _TŠČ1,
            <_TŠČ1 as Dispatch>::Group,
        >>::NAME;
    }
};
*/

#[test]
fn dispatch_on_assoc_type() {
    assert_eq!(String::NAME, "Blanket A");
    assert_eq!(Vec::<i32>::NAME, "Blanket B");
    assert_eq!(<&str>::NAME, "Blanket C");
    assert_eq!(<&[i32]>::NAME, "Blanket D");
}
