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
    pub trait Kita0<_TŠČ1: ?Sized, const SIZE: usize> {
        const NAME: &'static str;
    }

    impl<const N: usize> Kita0<GroupA, N> for [i32; N]
    where
        [i32; N]: Dispatch<N, Group = GroupA>,
    {
        const NAME: &'static str = "Blanket A";
    }

    impl<const N: usize> Kita0<GroupB, N> for [i32; N]
    where
        [i32; N]: Dispatch<N, Group = GroupB>,
    {
        const NAME: &'static str = "Blanket B";
    }

    impl<const N: usize> Kita<N> for [i32; N]
    where
        Self: Kita0<<[i32; N] as Dispatch<N>>::Group, N>,
        [i32; N]: Dispatch<N>,
    {
        const NAME: &'static str = <Self as Kita0<
            <[i32; N] as Dispatch<N>>::Group,
            N,
        >>::NAME;
    }
};
*/

#[test]
fn trait_with_const_param() {
    assert_eq!("Blanket A", <[i32; 1]>::NAME);
    assert_eq!("Blanket B", <[i32; 2]>::NAME);
}
