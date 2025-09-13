use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group;
}

pub trait Dispatch2 {
    type Group;
    type Group2;
}

pub enum GroupA {}
impl Dispatch for String {
    type Group = GroupA;
}

pub enum GroupB {}
impl Dispatch for Option<i32> {
    type Group = GroupB;
}

disjoint_impls! {
    pub trait Kita {
        const NAME: &'static str;
    }

    impl<T> Kita for T where Self: Dispatch<Group = GroupA> {
        const NAME: &'static str = "Blanket A";
    }
    impl<T: Dispatch<Group = GroupB>> Kita for T {
        const NAME: &'static str = "Blanket B";
    }
}

/*
pub trait Kita {
    const NAME: &'static str;
}

const _: () = {
    pub trait Kita0<_TŠČ0: ?Sized> {
        const NAME: &'static str;
    }

    impl<T> Kita0<GroupA> for T
    where
        T: Dispatch<Group = GroupA>,
    {
        const NAME: &'static str = "Blanket A";
    }
    impl<T: Dispatch<Group = GroupB>> Kita0<GroupB> for T {
        const NAME: &'static str = "Blanket B";
    }

    impl<T, _TŠČ1> Kita for T
    where
        Self: Kita0<_TŠČ1>,
        T: Dispatch<Group = _TŠČ1>,
    {
        const NAME: &'static str = <Self as Kita0<_TŠČ1>>::NAME;
    }
};
*/

#[test]
fn dispatch_on_self() {
    assert_eq!("Blanket A", String::NAME);
    assert_eq!("Blanket B", Option::<i32>::NAME);
}
