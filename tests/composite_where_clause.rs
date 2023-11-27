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

    impl<T> Kita for T where Option<T>: Dispatch<Group = GroupA> {
        const NAME: &'static str = "Blanket A";
    }
    impl<T> Kita for T where Option<T>: Dispatch<Group = GroupB> {
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

    impl<_0> _Kita0<GroupA> for _0 where Option<_0>: Dispatch<Group = GroupA> {
        const NAME: &'static str = "Blanket A";
    }
    impl<_0> _Kita0<GroupB> for _0 where Option<_0>: Dispatch<Group = GroupB> {
        const NAME: &'static str = "Blanket B";
    }

    impl<_0> Kita for _0 where Option<_0>: Dispatch, Self: _Kita0<<Option<_0> as Dispatch>::Group> {
        const NAME: &'static str = <Self as _Kita0<<Option<_0> as Dispatch>::Group>>::NAME;
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
