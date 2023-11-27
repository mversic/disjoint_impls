use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group;
}

pub trait A {}
pub trait B {}

pub enum GroupA {}
impl A for String {}
impl Dispatch for String {
    type Group = GroupA;
}
impl<T> A for Vec<T> {}
impl<T> Dispatch for Vec<T> {
    type Group = GroupA;
}

pub enum GroupB {}
impl B for i32 {}
impl Dispatch for i32 {
    type Group = GroupB;
}
impl B for u32 {}
impl Dispatch for u32 {
    type Group = GroupB;
}

disjoint_impls! {
    pub trait Kita {
        const NAME: &'static str;
    }

    impl<T: Dispatch<Group = GroupA> + Dispatch + A> Kita for T {
        const NAME: &'static str = "Blanket A";
    }
    impl<T: Dispatch<Group = GroupB>> Kita for T where T: B + Dispatch<Group = GroupB> {
        const NAME: &'static str = "Blanket B";
    }
}

/*
pub trait Kita {
    const NAME: &'static str;
}

const _: () = {
    pub trait _Kita0<_0: ?Sized> {
        const NAME: &'static str;
    }

    impl<_0: Dispatch<Group = GroupA> + Dispatch + A> _Kita0<GroupA> for _0 {
        const NAME: &'static str = "Blanket A";
    }
    impl<_0: Dispatch<Group = GroupB>> _Kita0<GroupB> for _0 where _0: B + Dispatch<Group = GroupB> {
        const NAME: &'static str = "Blanket B";
    }

    impl<_0> Kita for _0 where _0: Dispatch, Self: _Kita0<<_0 as Dispatch>::Group> {
        const NAME: &'static str = <Self as _Kita0<<_0 as Dispatch>::Group>>::NAME;
    }
};
*/

#[test]
fn extra_param_bounds() {
    assert_eq!("Blanket A", String::NAME);
    assert_eq!("Blanket A", Vec::<u32>::NAME);
    assert_eq!("Blanket B", u32::NAME);
    assert_eq!("Blanket B", i32::NAME);
}
