use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group;
}

pub enum GroupA {}
impl Dispatch for String {
    type Group = GroupA;
}
impl Dispatch for [u32; 0] {
    type Group = GroupA;
}

pub enum GroupB {}
impl Dispatch for u32 {
    type Group = GroupB;
}
impl Dispatch for [i32; 1] {
    type Group = GroupB;
}

disjoint_impls! {
    pub trait Kita<U>: Dispatch {
        const NAME: &'static str;
    }

    impl<T: Dispatch<Group = GroupA>> Kita<T> for T {
        const NAME: &'static str = "Blanket A";
    }
    impl<R, const N: usize> Kita<*mut Self> for [R; N]
    where
        Self: Dispatch<Group = GroupA>,
    {
        const NAME: &'static str = "Blanket A";
    }
    impl<R, C> Kita<C> for R
    where
        Self: Dispatch<Group = GroupB>,
    {
        const NAME: &'static str = "Blanket B";
    }
}

/*
pub trait Kita<U>: Dispatch {
    const NAME: &'static str;
}

const _: () = {
    pub trait Kita0<_TŠČ1: ?Sized, U>: Dispatch {
        const NAME: &'static str;
    }
    impl<T: Dispatch<Group = GroupA>> Kita0<GroupA, T> for T {
        const NAME: &'static str = "Blanket A";
    }
    impl<R, const N: usize> Kita0<GroupA, *mut [R; N]> for [R; N]
    where
        [R; N]: Dispatch<Group = GroupA>,
    {
        const NAME: &'static str = "Blanket A";
    }
    impl<R, C> Kita0<GroupB, C> for R
    where
        R: Dispatch<Group = GroupB>,
    {
        const NAME: &'static str = "Blanket B";
    }

    impl<_TŠČ0, _TŠČ1> Kita<_TŠČ0> for _TŠČ1
    where
        Self: Dispatch,
        _TŠČ1: Dispatch,
        Self: Kita0<<_TŠČ1 as Dispatch>::Group, _TŠČ0>,
    {
        const NAME: &'static str = <Self as Kita0<
            <_TŠČ1 as Dispatch>::Group,
            _TŠČ0,
        >>::NAME;
    }
};
*/

#[test]
fn shared_helper() {
    assert_eq!("Blanket A", String::NAME);
    assert_eq!("Blanket B", <u32 as Kita<i32>>::NAME);

    assert_eq!("Blanket A", <[u32; 0] as Kita<*mut [u32; 0]>>::NAME);
    assert_eq!("Blanket B", <[i32; 1] as Kita<*mut [i32; 1]>>::NAME);
}
