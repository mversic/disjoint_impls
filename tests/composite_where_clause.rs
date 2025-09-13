use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group;
}

pub enum GroupA {}
impl Dispatch for Option<String> {
    type Group = GroupA;
}
impl<T> Dispatch for Option<Vec<T>> {
    type Group = GroupA;
}

pub enum GroupB {}
impl Dispatch for Option<i32> {
    type Group = GroupB;
}
impl Dispatch for Option<u32> {
    type Group = GroupB;
}

disjoint_impls! {
    pub trait Kita {
        const NAME: &'static str;
    }

    impl<T> Kita for T
    where
        Option<T>: Dispatch<Group = GroupA>,
    {
        const NAME: &'static str = "Blanket A";
    }
    impl<T> Kita for T
    where
        Option<T>: Dispatch<Group = GroupB>,
    {
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
        Option<T>: Dispatch<Group = GroupA>,
    {
        const NAME: &'static str = "Blanket A";
    }
    impl<T> Kita0<GroupB> for T
    where
        Option<T>: Dispatch<Group = GroupB>,
    {
        const NAME: &'static str = "Blanket B";
    }

    impl<T, _TŠČ1> Kita for T
    where
        Self: Kita0<_TŠČ1>,
        Option<T>: Dispatch<Group = _TŠČ1>,
    {
        const NAME: &'static str = <Self as Kita0<_TŠČ1>>::NAME;
    }
};
*/

#[test]
fn composite_where_clause() {
    assert_eq!("Blanket A", <String as Kita>::NAME);
    assert_eq!("Blanket A", <Vec::<u32> as Kita>::NAME);
    assert_eq!("Blanket B", <u32 as Kita>::NAME);
    assert_eq!("Blanket B", <i32 as Kita>::NAME);
}
