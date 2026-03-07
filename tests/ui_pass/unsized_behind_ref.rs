use disjoint_impls::disjoint_impls;

disjoint_impls! {
    pub trait DerefFamily {
        type Kind;
    }

    impl<T: ?Sized> DerefFamily for &T {
        type Kind = ();
    }
}

fn main() {}
