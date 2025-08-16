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

fn main() {
    assert_eq!("Blanket A", <str as Kita<str, u32>>::kita(&""));
    assert_eq!("Blanket A", <Vec<u8> as Kita<str, str>>::kita(&Vec::new()));
    assert_eq!("Blanket B", <str as Kita<u32, str>>::kita(&""));
    assert_eq!("Blanket B", <[u8] as Kita<str, u32>>::kita(&[]));

    assert_eq!("Blanket A", <String as Kita<str, u32>>::kita(&String::new()));
    assert_eq!("Blanket A", <Vec<u8> as Kita<str, u32>>::kita(&Vec::new()));
    assert_eq!("Blanket B", <str as Kita<u32, u32>>::kita(&""));
    assert_eq!("Blanket B", <[u8] as Kita<u32, u32>>::kita(&[]));
}
