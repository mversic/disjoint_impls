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

    impl<T: Dispatch<Group = GroupB> + ?Sized, U> Kita<U, str> for T {
        fn kita(&self) -> String {
            "Blanket for str".to_owned()
        }
    }
}

/*
pub trait Kita<T: ?Sized, U: ?Sized> {
    fn kita(&self) -> String;
}

const _: () = {
    pub trait Kita0<_TŠČ2: ?Sized, T: ?Sized, U: ?Sized> {
        fn kita(&self) -> String;
    }

    impl<T: Dispatch<Group = GroupA>, U: ?Sized, V> Kita0<GroupA, U, V> for T {
        fn kita(&self) -> String {
            "Blanket A".to_owned()
        }
    }
    impl<T: Dispatch<Group = GroupB> + ?Sized, U, V> Kita0<GroupB, U, V> for T {
        fn kita(&self) -> String {
            "Blanket B".to_owned()
        }
    }
    impl<T: Dispatch<Group = GroupB> + ?Sized, U> Kita0<GroupB, U, str> for T {
        fn kita(&self) -> String {
            "Blanket for str".to_owned()
        }
    }

    impl<U: ?Sized, V: ?Sized, T: ?Sized, _TŠČ3> Kita<U, V> for T
    where
        U:,
        V:,
        Self: Kita0<_TŠČ3, U, V>,
        T: Dispatch<Group = _TŠČ3>,
    {
        fn kita(&self) -> String {
            { <Self as Kita0<_TŠČ3, U, V>>::kita(self) }
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
    assert_eq!("Blanket B", <str as Kita<u32, u32>>::kita(""));
    assert_eq!("Blanket B", <[u8] as Kita<u32, u32>>::kita(&[]));

    assert_eq!("Blanket for str", <str as Kita<u32, str>>::kita(""));
    assert_eq!("Blanket for str", <[u8] as Kita<u32, str>>::kita(&[]));
}
