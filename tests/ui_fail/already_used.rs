use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group;
}

disjoint_impls! {
    pub trait Kita {
        type Kara<T>;
        fn kita<T>(a: T);
    }

    impl<T: Dispatch<Group = impl Dispatch>> Kita for T {
        type Kara<T> = ();
        fn kita<T>(a: T) {}
    }
}

fn main() {}
