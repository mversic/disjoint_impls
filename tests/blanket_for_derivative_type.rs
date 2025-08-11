use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group;
}

pub enum GroupA {}
impl Dispatch for String {
    type Group = GroupA;
}
impl<T> Dispatch for Vec<T> {
    type Group = GroupA;
}

pub enum GroupB {}
impl Dispatch for i32 {
    type Group = GroupB;
}
impl Dispatch for u32 {
    type Group = GroupB;
}

disjoint_impls! {
    pub trait Kita {
        const NAME: &'static str;
    }

    impl<T: Dispatch<Group = GroupA>> Kita for Option<T> {
        const NAME: &'static str = "Blanket A";
    }
    impl<T: Dispatch<Group = GroupB>> Kita for Option<T> {
        const NAME: &'static str = "Blanket B";
    }
}

/*
pub trait Kita {
    const NAME: &'static str;
}

const _: () = {
    pub trait Kita0<_0: ?Sized> {
        const NAME: &'static str;
    }

    impl<_0: Dispatch<Group = GroupA>> Kita0<GroupA> for Option<_0> {
        const NAME: &'static str = "Blanket A";
    }
    impl<_0: Dispatch<Group = GroupB>> Kita0<GroupB> for Option<_0> {
        const NAME: &'static str = "Blanket B";
    }

    impl<_0> Kita for Option<_0>
    where
        _0: Dispatch,
        Self: Kita0<<_0 as Dispatch>::Group>,
    {
        const NAME: &'static str = <Self as Kita0<<_0 as Dispatch>::Group>>::NAME;
    }
};
*/

#[test]
fn blanket_for_derivative_type() {
    assert_eq!("Blanket A", Option::<String>::NAME);
    assert_eq!("Blanket A", Option::<Vec::<u32>>::NAME);
    assert_eq!("Blanket B", Option::<u32>::NAME);
    assert_eq!("Blanket B", Option::<i32>::NAME);
}
