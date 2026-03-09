use disjoint_impls::disjoint_impls;

struct Private;

disjoint_impls! {
    trait DerefFamily {
        type Kind;
    }

    impl<T: ?Sized> DerefFamily for &T {
        type Kind = Private;
    }
}

fn main() {}
