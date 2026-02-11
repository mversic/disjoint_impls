use core::any::type_name;
use core::mem::ManuallyDrop;

pub trait Dispatch {
    type Group;
}

pub enum GroupA {}
impl Dispatch for String {
    type Group = GroupA;
}

pub enum GroupB {}
impl Dispatch for i32 {
    type Group = GroupB;
}

impl Dispatch for Option<String> {
    type Group = Option<GroupA>;
}
impl Dispatch for Option<i32> {
    type Group = Option<GroupA>;
}
impl<R: Dispatch, T> Dispatch for Option<(R, T)> {
    type Group = Option<GroupB>;
}

disjoint_impls::disjoint_impls! {
    pub trait Kita<'d, T> {
        type Wrapped;
    }
    impl<'d> Kita<'d, u32> for i32 {
        type Wrapped = GroupB;
    }
    impl<'d, R: Dispatch<Group = GroupA> + 'd> Kita<'d, u32> for Option<R>
    where
        Self: Dispatch<Group = Option<GroupA>>,
    {
        type Wrapped = Self;
    }
    impl<'d, R: Dispatch<Group = GroupB> + 'd> Kita<'d, u32> for Option<R>
    where
        Self: Dispatch<Group = Option<GroupA>>,
    {
        type Wrapped = Option<ManuallyDrop<R>>;
    }
    impl<'d, R: Kita<'d, T>, T> Kita<'d, T> for Option<(R, T)>
    where
        Self: Dispatch<Group = Option<GroupB>>,
    {
        type Wrapped = Option<R::Wrapped>;
    }
}

/*
pub trait Kita<'d, T> {
    type Wrapped;
}
#[allow(clippy::needless_lifetimes)]
const _: () = {
    pub trait Kita0<'d, _TŠČ2: ?Sized, _TŠČ0>: Kita<'d, _TŠČ0> {
        type Wrapped_šč;
    }
    impl<'d, R: Kita<'d, T>, T> Kita0<'d, Option<GroupB>, T> for Option<(R, T)>
    where
        Option<(R, T)>: Dispatch<Group = Option<GroupB>>,
    {
        type Wrapped_šč = Option<R::Wrapped>;
    }
    pub trait Kita00<'d, _TŠČ2: ?Sized, _TŠČ0>: Kita<'d, _TŠČ0> {
        type Wrapped_šč;
    }
    impl<'d, R: Dispatch<Group = GroupA> + 'd> Kita00<'d, GroupA, u32> for Option<R>
    where
        Option<R>: Dispatch<Group = Option<GroupA>>,
    {
        type Wrapped_šč = Self;
    }
    impl<'d, R: Dispatch<Group = GroupB> + 'd> Kita00<'d, GroupB, u32> for Option<R>
    where
        Option<R>: Dispatch<Group = Option<GroupA>>,
    {
        type Wrapped_šč = Option<ManuallyDrop<R>>;
    }
    impl<'_lšč0, _TŠČ0> Kita0<'_lšč0, Option<GroupA>, u32> for Option<_TŠČ0>
    where
        '_lšč0,
        _TŠČ0: Dispatch,
        Self: Kita00<'_lšč0, <_TŠČ0 as Dispatch>::Group, u32>,
    {
        type Wrapped_šč =
            <Self as Kita00<'_lšč0, <_TŠČ0 as Dispatch>::Group, u32>>::Wrapped_šč;
    }
    impl<'_lšč0, _TŠČ0, _TŠČ1> Kita<'_lšč0, _TŠČ0> for Option<_TŠČ1>
    where
        '_lšč0,
        Option<_TŠČ1>: Dispatch,
        Self: Kita0<'_lšč0, <Option<_TŠČ1> as Dispatch>::Group, _TŠČ0>,
    {
        type Wrapped =
            <Self as Kita0<'_lšč0, <Option<_TŠČ1> as Dispatch>::Group, _TŠČ0>>::Wrapped_šč;
    }
};
*/

fn main() {
    type W1 = <Option<String> as Kita<'static, u32>>::Wrapped;
    assert_eq!(type_name::<W1>(), type_name::<Option<String>>());

    type W2 = <Option<i32> as Kita<'static, u32>>::Wrapped;
    assert_eq!(type_name::<W2>(), type_name::<Option<ManuallyDrop<i32>>>());

    type W3 = <Option<(i32, u32)> as Kita<'static, u32>>::Wrapped;
    assert_eq!(type_name::<W3>(), type_name::<Option<GroupB>>());
}
