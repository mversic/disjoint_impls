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

        fn name() -> &'static str {
            "Default blanket"
        }
    }

    impl<T: Dispatch<Group = GroupA>> Kita for T {
        const NAME: &'static str = "Blanket A";
    }
    impl<U: Dispatch<Group = GroupB>> Kita for U {
        const NAME: &'static str = "Blanket B";

        fn name() -> &'static str {
            "Blanket B"
        }
    }
}

/*
pub trait Kita {
    const NAME: &'static str;
    fn name() -> &'static str {
        "Default blanket"
    }
}

const _: () = {
    pub trait Kita0<_TŠČ0: ?Sized> {
        const NAME: &'static str;
        fn name() -> &'static str {
            "Default blanket"
        }
    }

    impl<T: Dispatch<Group = GroupA>> Kita0<GroupA> for T {
        const NAME: &'static str = "Blanket A";
    }
    impl<U: Dispatch<Group = GroupB>> Kita0<GroupB> for U {
        const NAME: &'static str = "Blanket B";
        fn name() -> &'static str {
            "Blanket B"
        }
    }

    impl<T, _TŠČ1> Kita for T
    where
        Self: Kita0<_TŠČ1>,
        T: Dispatch<Group = _TŠČ1>,
    {
        const NAME: &'static str = <Self as Kita0<_TŠČ1>>::NAME;
        fn name() -> &'static str {
            { <Self as Kita0<_TŠČ1>>::name() }
        }
    }
};
*/

#[test]
fn basic() {
    assert_eq!("Blanket A", String::NAME);
    assert_eq!("Blanket A", Vec::<u32>::NAME);
    assert_eq!("Blanket B", u32::NAME);
    assert_eq!("Blanket B", i32::NAME);

    assert_eq!("Default blanket", String::name());
    assert_eq!("Default blanket", Vec::<u32>::name());
    assert_eq!("Blanket B", u32::name());
    assert_eq!("Blanket B", i32::name());
}
