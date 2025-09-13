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
};
*/

#[test]
fn non_overlapping_blanket_impls() {
    assert_eq!("Blanket 1", <(String,)>::NAME);
    assert_eq!("Concrete Option<u32>", Option::<u32>::NAME);

    assert_eq!(<(u32, Vec<u32>)>::NAME, "Blanket A");
    assert_eq!(<(Vec<i32>, Vec<i32>)>::NAME, "Blanket B");
}
