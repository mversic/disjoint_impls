use disjoint_impls::disjoint_impls;

trait Dispatch {
    type Group;
}

struct GroupA;
struct GroupB;

struct SelfGroupA;
struct InnerGroupA;
struct InnerGroupB;

impl Dispatch for u8 {
    type Group = GroupA;
}

impl<T> Dispatch for Vec<T> {
    type Group = GroupB;
}

impl<T: Dispatch> Dispatch for [T] {
    type Group = T::Group;
}

impl Dispatch for &mut u8 {
    type Group = GroupA;
}

impl<T> Dispatch for &mut [T] {
    type Group = GroupB;
}

disjoint_impls! {
    trait Decode {
        type Output;
    }

    impl<R: ?Sized> Decode for &mut R
    where
        Self: Dispatch<Group = GroupA>,
    {
        type Output = SelfGroupA;
    }

    impl<R: ?Sized> Decode for &mut R
    where
        Self: Dispatch<Group = GroupB>,
        R: Dispatch<Group = GroupA>,
    {
        type Output = InnerGroupA;
    }
    impl<R> Decode for &mut [R]
    where
        Self: Dispatch<Group = GroupB>,
        [R]: Dispatch<Group = GroupB>,
    {
        type Output = InnerGroupB;
    }
}

/*
trait Decode {
    type Output;
}
const _: () = {
    trait Decode0<_TŠČ0: ?core::marker::Sized>: Decode {
        type Output_šč;
    }
    impl<'_lšč0, R: ?Sized> Decode0<GroupA> for &'_lšč0 mut R
    where
        &'_lšč0 mut R: Dispatch<Group = GroupA>,
    {
        type Output_šč = SelfGroupA;
    }
    trait Decode00<_TŠČ0: ?core::marker::Sized>: Decode {
        type Output_šč;
    }
    impl<'_lšč0, R: ?Sized> Decode00<GroupA> for &'_lšč0 mut R
    where
        &'_lšč0 mut R: Dispatch<Group = GroupB>,
        R: Dispatch<Group = GroupA>,
    {
        type Output_šč = InnerGroupA;
    }
    impl<'_lšč0, R> Decode00<GroupB> for &'_lšč0 mut [R]
    where
        &'_lšč0 mut [R]: Dispatch<Group = GroupB>,
        [R]: Dispatch<Group = GroupB>,
    {
        type Output_šč = InnerGroupB;
    }
    impl<'_lšč0, _TŠČ0: ?core::marker::Sized + '_lšč0> Decode0<GroupB>
    for &'_lšč0 mut _TŠČ0
    where
        _TŠČ0: Dispatch,
        Self: for<'_dšč> Decode00<<_TŠČ0 as Dispatch>::Group>,
    {
        type Output_šč = <Self as Decode00<<_TŠČ0 as Dispatch>::Group>>::Output_šč;
    }
    impl<'_lšč0, _TŠČ0: ?core::marker::Sized + '_lšč0> Decode
    for &'_lšč0 mut _TŠČ0
    where
        &'_lšč0 mut _TŠČ0: Dispatch,
        Self: for<'_dšč> Decode0<<&'_lšč0 mut _TŠČ0 as Dispatch>::Group>,
    {
        type Output = <Self as Decode0<
            <&'_lšč0 mut _TŠČ0 as Dispatch>::Group,
        >>::Output_šč;
    }
};
*/

#[test]
fn soft_overlap() {
    fn assert_decode<T: Decode<Output = SelfGroupA>>() {}
    assert_decode::<&mut u8>();

    fn assert_inner_decode<T: Decode<Output = InnerGroupA>>() {}
    assert_inner_decode::<&mut [u8]>();

    fn assert_nested_decode<T: Decode<Output = InnerGroupB>>() {}
    assert_nested_decode::<&mut [Vec<u8>]>();
}
