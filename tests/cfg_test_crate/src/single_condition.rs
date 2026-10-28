use dispatch_macros::disjoint_impls;

trait Dispatch {
    type First;
    type Second;
}
struct A;
struct B;
struct X;
#[cfg(feature = "feat_a")]
struct Y;
struct First;
struct Second;
#[cfg(feature = "feat_a")]
struct Third;
impl Dispatch for First {
    type First = A;
    type Second = X;
}
impl Dispatch for Second {
    type First = B;
    type Second = X;
}
#[cfg(feature = "feat_a")]
impl Dispatch for Third {
    type First = B;
    type Second = Y;
}
#[cfg(feature = "feat_a")]
trait FeatureOnly {}
#[cfg(feature = "feat_a")]
impl FeatureOnly for Third {}

disjoint_impls! {
    trait Label { fn label(&self) -> u8; }
    impl<'a, T: Dispatch<First = A> + ?Sized> Label for &'a T {
        fn label(&self) -> u8 { 1 }
    }
    impl<'a, T: Dispatch<First = B, Second = X> + ?Sized> Label for &'a T {
        fn label(&self) -> u8 { 2 }
    }
    #[cfg(feature = "feat_a")]
    impl<'a, T: Dispatch<First = B, Second = Y> + FeatureOnly + ?Sized> Label for &'a T {
        fn label(&self) -> u8 { 3 }
    }
}

#[test]
fn reference_dispatch_with_conditional_subgroup() {
    assert_eq!((&First).label(), 1);
    assert_eq!((&Second).label(), 2);
    #[cfg(feature = "feat_a")]
    assert_eq!((&Third).label(), 3);
}
