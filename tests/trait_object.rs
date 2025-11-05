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

pub trait Kara {}
impl Dispatch for dyn Kara {
    type Group = GroupA;
}

disjoint_impls! {
    pub trait Kita<T> {
        const NAME: &'static str;
    }

    impl<T: Dispatch<Group = GroupA>> Kita<T> for dyn Kara + Sync {
        const NAME: &'static str = "Blanket A";
    }
    impl<T: Dispatch<Group = GroupB>> Kita<T> for dyn Kara + Sync {
        const NAME: &'static str = "Blanket B";
    }
    impl<T: Dispatch<Group = GroupA>> Kita<T> for dyn Kara {
        const NAME: &'static str = "Blanket C";
    }
    impl<T: Dispatch<Group = GroupB>> Kita<T> for dyn Kara {
        const NAME: &'static str = "Blanket D";
    }
}

/*
pub trait Kita<T> {
    const NAME: &'static str;
}

const _: () = {
    pub trait Kita0<_TŠČ1: ?Sized, T> {
        const NAME: &'static str;
    }
    impl<T: Dispatch<Group = GroupA>> Kita0<GroupA, T> for dyn Kara + Sync {
        const NAME: &'static str = "Blanket A";
    }
    impl<T: Dispatch<Group = GroupB>> Kita0<GroupB, T> for dyn Kara + Sync {
        const NAME: &'static str = "Blanket B";
    }

    pub trait Kita1<_TŠČ1: ?Sized, T> {
        const NAME: &'static str;
    }
    impl<T: Dispatch<Group = GroupA>> Kita1<GroupA, T> for dyn Kara {
        const NAME: &'static str = "Blanket C";
    }
    impl<T: Dispatch<Group = GroupB>> Kita1<GroupB, T> for dyn Kara {
        const NAME: &'static str = "Blanket D";
    }

    impl<_TŠČ0> Kita<_TŠČ0> for dyn Kara + Sync
    where
        _TŠČ0: Dispatch,
        Self: Kita0<<_TŠČ0 as Dispatch>::Group, _TŠČ0>,
    {
        const NAME: &'static str = <Self as Kita0<
            <_TŠČ0 as Dispatch>::Group,
            _TŠČ0,
        >>::NAME;
    }
    impl<_TŠČ0> Kita<_TŠČ0> for dyn Kara
    where
        _TŠČ0: Dispatch,
        Self: Kita1<<_TŠČ0 as Dispatch>::Group, _TŠČ0>,
    {
        const NAME: &'static str = <Self as Kita1<
            <_TŠČ0 as Dispatch>::Group,
            _TŠČ0,
        >>::NAME;
    }
};
*/

fn main() {
    assert_eq!(<dyn Kara + Sync as Kita::<String>>::NAME, "Blanket A");
    assert_eq!(<dyn Sync + Kara as Kita::<u32>>::NAME, "Blanket B");
    assert_eq!(<dyn Kara as Kita::<String>>::NAME, "Blanket C");
    assert_eq!(<dyn Kara as Kita::<u32>>::NAME, "Blanket D");
}
