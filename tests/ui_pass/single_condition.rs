use disjoint_impls::disjoint_impls;

// An inactive invalid implementation must produce only a gated diagnostic.
disjoint_impls! {
    trait Required { fn required(); }
    #[cfg(any())]
    impl Required for Missing {}
}

disjoint_impls! {
    trait Arguments { fn call(#[cfg(all())] value: u8) -> u8; }
    impl Arguments for u8 {
        fn call(#[cfg(all())] value: u8) -> u8 { value }
    }
}

disjoint_impls! {
    trait NoArguments { fn call(#[cfg(any())] value: Missing) -> u8; }
    impl NoArguments for u8 {
        fn call(#[cfg(any())] value: Missing) -> u8 { 7 }
    }
}

struct Container<T>(T);
disjoint_impls! {
    #[cfg(any())]
    impl<T> Container<T> { fn absent() -> Missing { unreachable!() } }
    impl Container<u8> { fn present() -> u8 { 9 } }
}

disjoint_impls! {
    trait Nested {
        #[cfg_attr(all(), cfg(all()))]
        fn present() -> u8;
    }
    impl Nested for u8 {
        #[cfg_attr(all(), cfg(all()))]
        fn present() -> u8 { 11 }
    }
}

fn main() {
    assert_eq!(<u8 as Arguments>::call(5), 5);
    assert_eq!(<u8 as NoArguments>::call(), 7);
    assert_eq!(Container::<u8>::present(), 9);
    assert_eq!(<u8 as Nested>::present(), 11);
}
