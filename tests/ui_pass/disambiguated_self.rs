use disjoint_impls::disjoint_impls;

trait Dispatch {
    type Group;
}

enum GroupA {}
enum GroupB {}

disjoint_impls! {
    trait Kita {
        type Target: Kita;

        fn kita(a: <<Self as Kita>::Target as Kita>::Target) -> <<Self as Kita>::Target as Kita>::Target {
            <<<Self as Kita>::Target as Kita>::Target as Kita>::kita(a)
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
}

impl Kita for i8 {
    type Target = Self;
}

fn main() {}
