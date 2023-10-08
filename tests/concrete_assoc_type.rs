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

disjoint_impls! {
    pub trait Kita {
        type Item;

        fn kita() -> Self::Item;
    }

    impl<T: Dispatch<Group = GroupA>> Kita for T {
        type Item = u32;

        fn kita() -> u32 {
            0
        }
    }
    impl<U: Dispatch<Group = GroupB>> Kita for U {
        type Item = u16;

        fn kita() -> u16 {
            1
        }
    }
}

#[test]
fn main() {
    assert_eq!(0, String::kita());
    assert_eq!(0, Vec::<u32>::kita());
    assert_eq!(1, u32::kita());
    assert_eq!(1, i32::kita());
}
