use disjoint_impls::disjoint_impls;

pub trait Dispatch1 {
    type Group;
}
pub trait Dispatch2 {
    type Group;
}

pub enum GroupA {}
pub enum GroupB {}

impl Dispatch1 for String {
    type Group = GroupA;
}
impl Dispatch2 for String {
    type Group = GroupA;
}
impl<T> Dispatch1 for Vec<T> {
    type Group = GroupA;
}
impl<T> Dispatch2 for Vec<T> {
    type Group = GroupB;
}

impl Dispatch1 for i32 {
    type Group = GroupB;
}
impl Dispatch2 for i32 {
    type Group = GroupB;
}
impl Dispatch1 for u32 {
    type Group = GroupB;
}
impl Dispatch2 for u32 {
    type Group = GroupB;
}

impl core::fmt::Display for GroupB {
    fn fmt(&self, _: &mut core::fmt::Formatter) -> core::fmt::Result {
        unimplemented!()
    }
}

disjoint_impls! {
    pub trait Kita {
        const NAME: &str;
    }

    impl<T: Dispatch1<Group = GroupA> + Dispatch2<Group = GroupA>> Kita for T {
        const NAME: &str = "Blanket A";
    }
    impl<T: Dispatch1<Group = GroupB> + Dispatch2<Group: ToString>> Kita for T {
        const NAME: &'static str = "Blanket B";
    }
}

/*
pub trait Kita {
    const NAME: &str;
}

const _: () = {
    pub trait Kita0<_TŠČ0: ?Sized, _TŠČ1: ?Sized> {
        const NAME: &str;
    }
    impl<T: Dispatch1<Group = GroupA> + Dispatch2<Group = GroupA>> Kita0<GroupA, GroupA>
    for T {
        const NAME: &str = "Blanket A";
    }
    impl<
        T: Dispatch1<Group = GroupB> + Dispatch2<Group: ToString>,
    > Kita0<<T as Dispatch2>::Group, GroupB> for T {
        const NAME: &'static str = "Blanket B";
    }

    impl<_TŠČ0> Kita for _TŠČ0
    where
        _TŠČ0: Dispatch2,
        _TŠČ0: Dispatch1,
        Self: Kita0<<_TŠČ0 as Dispatch2>::Group, <_TŠČ0 as Dispatch1>::Group>,
    {
        const NAME: &str = <Self as Kita0<
            <_TŠČ0 as Dispatch2>::Group,
            <_TŠČ0 as Dispatch1>::Group,
        >>::NAME;
    }
};
*/

#[test]
fn dispatch_on_undefined_assoc_binding() {
    assert_eq!("Blanket A", String::NAME);
    assert_eq!("Blanket B", u32::NAME);
    assert_eq!("Blanket B", i32::NAME);
}
