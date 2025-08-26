use disjoint_impls::disjoint_impls;

trait Dispatch<const N: usize> {
    type Group;
}

enum GroupA {}
enum GroupB {}

impl Dispatch<1> for [i32; 1] {
    type Group = GroupA;
}
impl Dispatch<2> for [i32; 2] {
    type Group = GroupB;
}

disjoint_impls! {
    trait Kita<const SIZE: usize> {
        const NAME: &'static str;
    }

    impl<const N: usize> Kita<N> for [i32; N]
    where
        [i32; N]: Dispatch<N, Group = GroupA>,
    {
        const NAME: &'static str = "Blanket A";
    }

    impl<const N: usize> Kita<N> for [i32; N]
    where
        [i32; N]: Dispatch<N, Group = GroupB>,
    {
        const NAME: &'static str = "Blanket B";
    }
}

/*
trait Kita<const SIZE: usize> {
    const NAME: &'static str;
}

const _: () = {
    pub trait Kita0<_1: ?Sized, const SIZE: usize> {
        const NAME: &'static str;
    }

    impl<const _0: usize> Kita0<GroupA, _0> for [i32; _0]
    where
        [i32; _0]: Dispatch<_0, Group = GroupA>,
    {
        const NAME: &'static str = "Blanket A";
    }
    impl<const _0: usize> Kita0<GroupB, _0> for [i32; _0]
    where
        [i32; _0]: Dispatch<_0, Group = GroupB>,
    {
        const NAME: &'static str = "Blanket B";
    }

    impl<const _0: usize> Kita<_0> for [i32; _0]
    where
        [i32; _0]: Dispatch<_0>,
        Self: Kita0<<[i32; _0] as Dispatch<_0>>::Group, _0>,
    {
        const NAME: &'static str = <Self as Kita0<
            <[i32; _0] as Dispatch<_0>>::Group,
            _0,
        >>::NAME;
    }
};
 */

#[test]
fn trait_with_const_param() {
    assert_eq!("Blanket A", <[i32; 1]>::NAME);
    assert_eq!("Blanket B", <[i32; 2]>::NAME);
}
