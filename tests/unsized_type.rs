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
impl Dispatch for &str {
    type Group = GroupB;
}
impl Dispatch for [u8] {
    type Group = GroupB;
}

disjoint_impls! {
    pub trait Kita {
        fn kita(&self) -> String;
    }

    impl<T: Dispatch<Group = GroupA>> Kita for T {
        fn kita(&self) -> String {
            "Blanket A".to_owned()
        }
    }

    impl<T: Dispatch<Group = GroupB> + ?Sized> Kita for T {
        fn kita(&self) -> String {
            "Blanket B".to_owned()
        }
    }
}

/*
pub trait Kita {
    fn kita(&self) -> String;
}

const _: () = {
    pub trait _Kita0<_0: ?Sized> {
        fn kita(&self) -> String;
    }

    impl<_0: Dispatch<Group = GroupA>> _Kita0<GroupA> for _0 {
        fn kita(&self) -> String {
            "Blanket A".to_owned()
        }
    }
    impl<_0: Dispatch<Group = GroupB> + ?Sized> _Kita0<GroupB> for _0 {
        fn kita(&self) -> String {
            "Blanket B".to_owned()
        }
    }

    impl<_0> Kita for _0 where _0: ?Sized + Dispatch, Self: _Kita0<<_0 as Dispatch>::Group> {
        fn kita(&self) -> String {
            <Self as _Kita0<<_0 as Dispatch>::Group>>::kita(self)
        }
    }
};
*/

#[test]
fn unsized_type() {
    assert_eq!("Blanket A", String::new().kita());
    assert_eq!("Blanket A", Vec::<u8>::new().kita());
    assert_eq!("Blanket B", b"bytestring"[..].kita());
    assert_eq!("Blanket B", "kita".kita());
}
