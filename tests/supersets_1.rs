use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group: ?Sized;
}

pub enum GroupA {}
impl Dispatch for Option<String> {
    type Group = GroupA;
}
impl Dispatch for Option<u32> {
    type Group = GroupA;
}

pub struct GroupB;
impl<T> Dispatch for Option<Vec<T>> {
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
    impl<U> Kita for Vec<U>
    where
        Option<Vec<U>>: Dispatch<Group = GroupB>,
    {
        const NAME: &'static str = "Blanket B";
    }
    impl<T> Kita for Option<T>
    where
        Option<T>: Dispatch<Group = GroupA>,
    {
        const NAME: &'static str = "Blanket C";
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
    impl<U> Kita0<GroupB> for Vec<U>
    where
        Option<Vec<U>>: Dispatch<Group = GroupB>,
    {
        const NAME: &'static str = "Blanket B";
    }

    impl<_TŠČ0> Kita for _TŠČ0
    where
        Option<_TŠČ0>: Dispatch,
        Self: Kita0<<Option<_TŠČ0> as Dispatch>::Group>,
    {
        const NAME: &'static str = <Self as Kita0<
            <Option<_TŠČ0> as Dispatch>::Group,
        >>::NAME;
    }

    impl<T> Kita for Option<T>
    where
        Option<T>: Dispatch<Group = GroupA>,
    {
        const NAME: &'static str = "Blanket C";
    }
};
*/

#[test]
fn supersets_1() {
    assert_eq!("Blanket A", String::NAME);
    assert_eq!("Blanket B", Vec::<u8>::NAME);
    assert_eq!("Blanket C", Option::<String>::NAME);

    assert_eq!("Blanket A", u32::NAME);
    assert_eq!("Blanket B", Vec::<i32>::NAME);
    assert_eq!("Blanket C", Option::<u32>::NAME);
}
