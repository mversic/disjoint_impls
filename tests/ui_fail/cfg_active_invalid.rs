use disjoint_impls::disjoint_impls;

disjoint_impls! {
    trait Trait { fn required(); }
    #[cfg(all())]
    impl Trait for u8 {}
}

fn main() {}
