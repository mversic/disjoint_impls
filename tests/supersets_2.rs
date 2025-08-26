use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group: ?Sized;
}

pub enum GroupA {}
pub enum GroupB {}
pub enum GroupC {}
pub enum GroupD {}
impl Dispatch for (String, u32) {
    type Group = GroupA;
}
impl Dispatch for (Vec<String>, u32) {
    type Group = GroupB;
}
impl Dispatch for (String, Vec<u32>) {
    type Group = GroupC;
}
impl Dispatch for (Vec<String>, Vec<u32>) {
    type Group = GroupD;
}

disjoint_impls! {
    pub trait Kita {
        const NAME: &'static str;
    }

    impl<T, U> Kita for (T, U)
    where
        (T, U): Dispatch<Group = GroupA>,
    {
        const NAME: &'static str = "Blanket A";
    }
    impl<T, U> Kita for (Vec<T>, U)
    where
        (Vec<T>, U): Dispatch<Group = GroupB>,
    {
        const NAME: &'static str = "Blanket B";
    }
    impl<T, U> Kita for (T, Vec<U>)
    where
        (T, Vec<U>): Dispatch<Group = GroupC>,
    {
        const NAME: &'static str = "Blanket C";
    }
    impl<T, U> Kita for (Vec<T>, Vec<U>)
    where
        (Vec<T>, Vec<U>): Dispatch<Group = GroupD>,
    {
        const NAME: &'static str = "Blanket D";
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

    impl<_0, _1> Kita0<GroupA> for (_0, _1)
    where
        (_0, _1): Dispatch<Group = GroupA>,
    {
        const NAME: &'static str = "Blanket A";
    }
    impl<_0, _1> Kita0<GroupB> for (Vec<_0>, _1)
    where
        (Vec<_0>, _1): Dispatch<Group = GroupB>,
    {
        const NAME: &'static str = "Blanket B";
    }
    impl<_0, _1> Kita0<GroupC> for (_0, Vec<_1>)
    where
        (_0, Vec<_1>): Dispatch<Group = GroupC>,
    {
        const NAME: &'static str = "Blanket C";
    }
    impl<_0, _1> Kita0<GroupD> for (Vec<_0>, Vec<_1>)
    where
        (Vec<_0>, Vec<_1>): Dispatch<Group = GroupD>,
    {
        const NAME: &'static str = "Blanket D";
    }

    impl<_0, _1> Kita for (_0, _1)
    where
        (_0, _1): Dispatch,
        Self: Kita0<<(_0, _1) as Dispatch>::Group>,
    {
        const NAME: &'static str = <Self as Kita0<
            <(_0, _1) as Dispatch>::Group,
        >>::NAME;
    }
};
*/

#[test]
fn supersets_2() {
    assert_eq!("Blanket A", <(String, u32)>::NAME);
    assert_eq!("Blanket B", <(Vec<String>, u32)>::NAME);
    assert_eq!("Blanket C", <(String, Vec<u32>)>::NAME);
    assert_eq!("Blanket D", <(Vec<String>, Vec<u32>)>::NAME);
}
