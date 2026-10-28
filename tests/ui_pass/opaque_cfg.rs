use disjoint_impls::disjoint_impls as dispatch;

dispatch! {
    trait Trait { const VALUE: u8; }

    #[cfg(all())]
    impl Trait for u8 { const VALUE: u8 = 1; }

    // Neither missing items nor overlapping impls in inactive combinations
    // should be validated by the macro.
    #[cfg(not(all()))]
    impl Trait for u8 {}

    #[cfg(any())]
    impl Trait for MissingType {}

    #[cfg_attr(all(), cfg_attr(all(), cfg(all()), doc = "retained"))]
    #[cfg_attr(any(), arbitrary_attribute(arbitrary => tokens))]
    impl Trait for u16 { const VALUE: u8 = 2; }
}

struct Inherent<T>(T);
dispatch! {
    #[cfg(all())]
    impl<T> Inherent<T> { fn value() -> u8 { 3 } }
    #[cfg(any())]
    impl<T> Inherent<T> { fn alternative() {} }
}

// A false trait gate removes its whole schema, including methods with names
// that would conflict if interpreted without their enclosing condition.
dispatch! {
    #[cfg(any())]
    trait Absent { fn duplicate(); fn duplicate(); }
    #[cfg(any())]
    impl Absent for u8 {}
}

fn main() {
    assert_eq!(<u8 as Trait>::VALUE, 1);
    assert_eq!(<u16 as Trait>::VALUE, 2);
    assert_eq!(Inherent::<u8>::value(), 3);
}
