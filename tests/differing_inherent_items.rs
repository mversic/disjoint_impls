use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group;
}

pub enum GroupA {}
impl Dispatch for Option<String> {
    type Group = GroupA;
}

pub enum GroupB {}
impl Dispatch for i32 {
    type Group = GroupB;
}

struct Wrapper<T>(T);

disjoint_impls! {
    impl<T> Wrapper<T>
    where
        Option<T>: Dispatch<Group = GroupA>,
    {
        fn kita() -> &'static str {
            "Blanket A"
        }
    }
    impl<T> Wrapper<T>
    where
        T: Dispatch<Group = GroupB>,
    {
        fn kita() -> String {
            "Blanket B".to_owned()
        }
    }
}

/*
const _: () = {
    pub trait Wrapper0<_TŠČ1: ?Sized, _TŠČ0> {
        fn kita_šč() -> &'static str;
    }
    impl<T> Wrapper0<GroupA, T> for Wrapper<T>
    where
        Option<T>: Dispatch<Group = GroupA>,
    {
        fn kita_šč() -> &'static str {
            "Blanket A"
        }
    }
    pub trait Wrapper1<_TŠČ1: ?Sized, _TŠČ0> {
        fn kita_šč() -> String;
    }
    impl<T> Wrapper1<GroupB, T> for Wrapper<T>
    where
        T: Dispatch<Group = GroupB>,
    {
        fn kita_šč() -> String {
            "Blanket B".to_owned()
        }
    }
    impl<_TŠČ0> Wrapper<_TŠČ0>
    where
        Option<_TŠČ0>: Dispatch,
        Self: for<'_dšč> Wrapper0<<Option<_TŠČ0> as Dispatch>::Group, _TŠČ0>,

    {
        fn kita() -> &'static str {
            <Self as Wrapper0<<Option<_TŠČ0> as Dispatch>::Group, _TŠČ0>>::kita_šč()
        }
    }
    impl<_TŠČ0> Wrapper<_TŠČ0>
    where
        _TŠČ0: Dispatch,
        Self: Wrapper1<<_TŠČ0 as Dispatch>::Group, _TŠČ0>,
    {
        fn kita() -> String {
            <Self as Wrapper1<<_TŠČ0 as Dispatch>::Group, _TŠČ0>>::kita_šč()
        }
    }
};
*/

#[test]
fn differing_inherent_items() {
    assert_eq!("Blanket A", Wrapper::<String>::kita());
    assert_eq!("Blanket B".to_owned(), Wrapper::<i32>::kita());
}
