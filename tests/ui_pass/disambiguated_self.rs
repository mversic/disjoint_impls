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

/*
trait Kita {
    type Target: Kita;
    fn kita(
        a: <<Self as Kita>::Target as Kita>::Target,
    ) -> <<Self as Kita>::Target as Kita>::Target {
        let b: <<Self as Kita>::Target as Kita>::Target = a;
        b
    }
}

const _: () = {
    trait Kita0<_TŠČ0: ?Sized>: Kita {
        type Target_šč: Kita;
        fn kita_šč(
            a: <<Self as Kita>::Target as Kita>::Target,
        ) -> <<Self as Kita>::Target as Kita>::Target {
            let b: <<Self as Kita>::Target as Kita>::Target = a;
            b
        }
    }
    impl<T: Dispatch<Group = GroupA>> Kita0<GroupA> for T {
        type Target_šč = u8;
    }
    impl<T: Dispatch<Group = GroupB>> Kita0<GroupB> for T {
        type Target_šč = i8;
    }
    impl<_TŠČ0> Kita for _TŠČ0
    where
        _TŠČ0: Dispatch,
        Self: for<'_dšč> Kita0<<_TŠČ0 as Dispatch>::Group>,

    {
        type Target = <Self as Kita0<<_TŠČ0 as Dispatch>::Group>>::Target_šč;
        fn kita(
            a: <<Self as Kita>::Target as Kita>::Target,
        ) -> <<Self as Kita>::Target as Kita>::Target {
            <Self as Kita0<<_TŠČ0 as Dispatch>::Group>>::kita_šč(a)
        }
    }
};
*/

fn main() {
    assert_eq!(12u8, <String as Kita>::kita(12));
    assert_eq!(-7i8, <i32 as Kita>::kita(-7));
}
