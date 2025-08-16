use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group: ?Sized;
}

pub enum GroupA {}
impl Dispatch for String {
    type Group = GroupA;
}
impl<T> Dispatch for Vec<T> {
    type Group = GroupA;
}

pub struct GroupB;
impl Dispatch for str {
    type Group = GroupB;
}
impl Dispatch for [u8] {
    type Group = GroupB;
}

disjoint_impls! {
    pub trait Kita<T: ?Sized, U: ?Sized> {
        fn kita(&self) -> String;
    }

    impl<T: Dispatch<Group = GroupA>, U: ?Sized, V> Kita<U, V> for T {
        fn kita(&self) -> String {
            "Blanket A".to_owned()
        }
    }

    impl<T: Dispatch<Group = GroupB> + ?Sized, U, V> Kita<U, V> for T {
        fn kita(&self) -> String {
            "Blanket B".to_owned()
        }
    }
}

/*
pub trait Kita<T: ?Sized, U: ?Sized> {
    fn kita(&self) -> String;
}

const _: () = {
    pub trait Kita0<_2: ?Sized, T: ?Sized, U: ?Sized> {
        fn kita(&self) -> String;
    }

    impl<
        _2: Dispatch<Group = GroupA>,
        _0: ?Sized,
        _1,
    > Kita0<GroupA, _0, _1> for _2 {
        fn kita(&self) -> String {
            "Blanket A".to_owned()
        }
    }
    impl<
        _2: Dispatch<Group = GroupB> + ?Sized,
        _0,
        _1,
    > Kita0<GroupB, _0, _1> for _2 {
        fn kita(&self) -> String {
            "Blanket B".to_owned()
        }
    }

    impl<_0: ?Sized, _1: ?Sized, _2: ?Sized> Kita<_0, _1> for _2
    where
        _2: Dispatch,
        Self: Kita0<<_2 as Dispatch>::Group, _0, _1>,
    {
        fn kita(&self) -> String {
            <Self as Kita0<<_2 as Dispatch>::Group, _0, _1>>::kita(self)
        }
    }
};
*/

#[test]
fn unsized_type() {
    assert_eq!(
        "Blanket A",
        <String as Kita<str, u32>>::kita(&String::new())
    );
    assert_eq!("Blanket A", <Vec<u8> as Kita<str, u32>>::kita(&Vec::new()));
    assert_eq!("Blanket B", <str as Kita<u32, u32>>::kita(&""));
    assert_eq!("Blanket B", <[u8] as Kita<u32, u32>>::kita(&[]));
}
