use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group;
}

pub enum GroupA {}
impl Dispatch for u32 {
    type Group = GroupA;
}

pub enum GroupB {}
impl Dispatch for i32 {
    type Group = GroupB;
}

disjoint_impls! {
    trait Kita {
        const NAME: &'static str;
    }

    impl<T> Kita for (T,) {
        const NAME: &'static str = "Blanket 1";
    }

    impl Kita for Option<u32> {
        const NAME: &'static str = "Concrete Option<u32>";
    }

    impl<_0, _1> Kita for (_0, _1)
    where
        _0: Dispatch<Group = GroupA>,
    {
        const NAME: &'static str = "Blanket A";
    }
    impl<_0> Kita for (Vec<_0>, Vec<_0>)
    where
        _0: Dispatch<Group = GroupB>,
    {
        const NAME: &'static str = "Blanket B";
    }
}

/*
trait Kita {
    const NAME: &'static str;
}

const _: () = {
    trait Kita0: Kita {
        const NAME_šč: &'static str;
    }
    impl<T> Kita0 for (T,) {
        const NAME_šč: &'static str = "Blanket 1";
    }
    trait Kita1: Kita {
        const NAME_šč: &'static str;
    }
    impl Kita1 for Option<u32> {
        const NAME_šč: &'static str = "Concrete Option<u32>";
    }
    trait Kita2<_TŠČ0: ?Sized>: Kita {
        const NAME_šč: &'static str;
    }
    impl<_0, _1> Kita2<GroupA> for (_0, _1)
    where
        _0: Dispatch<Group = GroupA>,
    {
        const NAME_šč: &'static str = "Blanket A";
    }
    trait Kita3<_TŠČ0: ?Sized>: Kita {
        const NAME_šč: &'static str;
    }
    impl<_0> Kita3<GroupB> for (Vec<_0>, Vec<_0>)
    where
        _0: Dispatch<Group = GroupB>,
    {
        const NAME_šč: &'static str = "Blanket B";
    }
    impl<_TŠČ0> Kita for (_TŠČ0,)
    where
        Self: for<'_dšč> Kita0,
    {
        const NAME: &'static str = <Self as Kita0>::NAME_šč;
    }
    impl Kita for Option<u32>
    where
        Self: Kita1,
    {
        const NAME: &'static str = <Self as Kita1>::NAME_šč;
    }
    impl<_TŠČ0, _TŠČ1> Kita for (_TŠČ0, _TŠČ1)
    where
        _TŠČ0: Dispatch,
        Self: Kita2<<_TŠČ0 as Dispatch>::Group>,
    {
        const NAME: &'static str = <Self as Kita2<<_TŠČ0 as Dispatch>::Group>>::NAME_šč;
    }
    impl<_TŠČ0> Kita for (Vec<_TŠČ0>, Vec<_TŠČ0>)
    where
        _TŠČ0: Dispatch,
        Self: Kita3<<_TŠČ0 as Dispatch>::Group>,
    {
        const NAME: &'static str = <Self as Kita3<<_TŠČ0 as Dispatch>::Group>>::NAME_šč;
    }
};
*/

#[test]
fn non_overlapping_blanket_impls() {
    assert_eq!("Blanket 1", <(String,)>::NAME);
    assert_eq!("Concrete Option<u32>", Option::<u32>::NAME);

    assert_eq!(<(u32, Vec<u32>)>::NAME, "Blanket A");
    assert_eq!(<(Vec<i32>, Vec<i32>)>::NAME, "Blanket B");
}
