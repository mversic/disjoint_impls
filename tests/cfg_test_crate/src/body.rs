use dispatch_macros::disjoint_impls;

pub trait Dispatch {
    type Group;
}
impl Dispatch for String {
    type Group = u8;
}
impl Dispatch for i32 {
    type Group = u16;
}

// Rust handles these attributes before invoking the macro.
#[cfg(not(feature = "feat_a"))]
disjoint_impls! {
    pub trait FeatureValue { const VALUE: u8; }
    impl<T: Dispatch<Group = u8>> FeatureValue for T { const VALUE: u8 = 1; }
    impl<T: Dispatch<Group = u16>> FeatureValue for T { const VALUE: u8 = 2; }
}

#[cfg_attr(not(feature = "feat_a"), cfg(any()))]
disjoint_impls! {
    pub trait FeatureValue { const VALUE: u8; }
    impl<T: Dispatch<Group = u8>> FeatureValue for T { const VALUE: u8 = 3; }
    impl<T: Dispatch<Group = u16>> FeatureValue for T { const VALUE: u8 = 4; }
}

disjoint_impls! {
    pub trait BodyValue { fn value() -> u8; }
    impl<T: Dispatch<Group = u8>> BodyValue for T {
        fn value() -> u8 {
            #[cfg(feature = "feat_a")]
            { 3 }
            #[cfg_attr(all(), cfg(not(feature = "feat_a")))]
            { 1 }
        }
    }
    impl<T: Dispatch<Group = u16>> BodyValue for T {
        fn value() -> u8 {
            // Includes a non-gating attribute; the macro leaves it untouched.
            #[cfg_attr(feature = "feat_a", allow(unused_variables))]
            let value = <Self as FeatureValue>::VALUE;
            value
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rust_controls_invocations_and_bodies() {
        let expected = if cfg!(feature = "feat_a") { 3 } else { 1 };
        assert_eq!(<String as FeatureValue>::VALUE, expected);
        assert_eq!(<i32 as FeatureValue>::VALUE, expected + 1);
        assert_eq!(<String as BodyValue>::value(), expected);
        assert_eq!(<i32 as BodyValue>::value(), expected + 1);
    }
}
