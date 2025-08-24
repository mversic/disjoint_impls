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

    impl<T: Dispatch> Kita for T
    where
        <T as Dispatch>::Group: Dispatch<Group = GroupA>,
    {
        const NAME: &'static str = "Blanket A";
    }

    impl<T: Dispatch> Kita for T
    where
        <T as Dispatch>::Group: Dispatch<Group = GroupB>,
    {
        const NAME: &'static str = "Blanket B";
    }
}

/*
pub trait Kita {
    const NAME: &'static str;
}

const _: () = {
    pub trait Kita0<_ŠČ0: ?Sized> {
        const NAME: &'static str;
    }

    impl<_ŠČ0: Dispatch> Kita0<GroupA> for _ŠČ0
    where
        <_ŠČ0 as Dispatch>::Group: Dispatch<Group = GroupA>,
    {
        const NAME: &'static str = "Blanket A";
    }
    impl<_ŠČ0: Dispatch> Kita0<GroupB> for _ŠČ0
    where
        <_ŠČ0 as Dispatch>::Group: Dispatch<Group = GroupB>,
    {
        const NAME: &'static str = "Blanket B";
    }

    impl<_ŠČ0> Kita for _ŠČ0
    where
        _ŠČ0: Dispatch,
        <_ŠČ0 as Dispatch>::Group: Dispatch,
        Self: Kita0<<<_ŠČ0 as Dispatch>::Group as Dispatch>::Group>,
    {
        const NAME: &'static str = <Self as Kita0<
            <<_ŠČ0 as Dispatch>::Group as Dispatch>::Group,
        >>::NAME;
    }
};
*/

#[test]
fn dispatch_on_assoc_type() {
    assert_eq!(String::NAME, "Blanket A");
    assert_eq!(Vec::NAME, "Blanket A");
    assert_eq!(u32::NAME, "Blanket B");
    assert_eq!(i32::NAME, "Blanket B");
}
