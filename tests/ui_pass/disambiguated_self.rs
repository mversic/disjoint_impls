use disjoint_impls::disjoint_impls;

trait Dispatch {
    type Group;
}

enum GroupA {}
enum GroupB {}

impl Dispatch for String {
    type Group = GroupA;
}

impl Dispatch for i32 {
    type Group = GroupB;
}

disjoint_impls! {
    trait Kita {
        type Target: Kita;

        fn kita(
            a: <<Self as Kita>::Target as Kita>::Target,
        ) -> <<Self as Kita>::Target as Kita>::Target {
            let b: <<Self as Kita>::Target as Kita>::Target = a;
            b
        }
    }
    impl<T: Dispatch<Group = GroupA>> Kita for T {
        type Target = u8;
    }
    impl<T: Dispatch<Group = GroupB>> Kita for T {
        type Target = i8;
    }
}

impl Kita for u8 {
    type Target = Self;

    fn kita(a: u8) -> u8 {
        a
    }
}

impl Kita for i8 {
    type Target = Self;

    fn kita(a: i8) -> i8 {
        a
    }
}

fn main() {
    assert_eq!(12u8, <String as Kita>::kita(12));
    assert_eq!(-7i8, <i32 as Kita>::kita(-7));
}
