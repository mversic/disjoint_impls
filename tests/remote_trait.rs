use disjoint_impls::disjoint_impls;
use remote_trait::ForeignKita;

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

struct LocalType<T>(T);

disjoint_impls! {
    #[disjoint_impls(remote)]
    pub trait ForeignKita<U> {
        fn kita() -> &'static str {
            "Default blanket"
        }
    }

    impl<U, T: Dispatch<Group = GroupA>> ForeignKita<U> for LocalType<T> {
        fn kita() -> &'static str {
            "Blanket A"
        }
    }
    impl<U, T: Dispatch<Group = GroupB>> ForeignKita<U> for LocalType<T> {
        fn kita() -> &'static str {
            "Blanket B"
        }
    }
}

/*
const _: () = {
    pub trait ForeignKita0<_TŠČ1: ?Sized, U> {
        fn kita() -> &'static str {
            "Default blanket"
        }
    }

    impl<U, T: Dispatch<Group = GroupA>> ForeignKita0<GroupA, U> for LocalType<T> {
        fn kita() -> &'static str {
            "Blanket A"
        }
    }
    impl<U, T: Dispatch<Group = GroupB>> ForeignKita0<GroupB, U> for LocalType<T> {
        fn kita() -> &'static str {
            "Blanket B"
        }
    }

    impl<U, T, _TŠČ2> ForeignKita<U> for LocalType<T>
    where
        Self: ForeignKita0<_TŠČ2, U>,
        T: Dispatch<Group = _TŠČ2>,
    {
        fn kita() -> &'static str {
            { <Self as ForeignKita0<_TŠČ2, U>>::kita() }
        }
    }
};
*/

#[test]
fn remote_trait() {
    assert_eq!("Blanket A", <LocalType<String> as ForeignKita<u8>>::kita());
    assert_eq!(
        "Blanket A",
        <LocalType<Vec::<u32>> as ForeignKita<u16>>::kita()
    );
    assert_eq!("Blanket B", <LocalType<u32> as ForeignKita<u32>>::kita());
    assert_eq!("Blanket B", <LocalType<i32> as ForeignKita<u64>>::kita());
}
