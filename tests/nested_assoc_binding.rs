use disjoint_impls::disjoint_impls;

trait Inner {
    type CType;
}

trait Outer {
    type CType;
}

struct Boxed<R>(R);
struct ConstBox<C>(C);
struct CellBox<C>(C);
struct Plain;
struct Interior;

impl Inner for Plain {
    type CType = u8;
}

impl Inner for Interior {
    type CType = u8;
}

impl Outer for Boxed<Plain> {
    type CType = ConstBox<u8>;
}

impl Outer for Boxed<Interior> {
    type CType = CellBox<u8>;
}

disjoint_impls! {
    trait CarrierKind {
        const NAME: &'static str;
    }

    impl<R: Inner> CarrierKind for Boxed<R>
    where
        Boxed<R>: Outer<CType = ConstBox<<R as Inner>::CType>>,
    {
        const NAME: &'static str = "const";
    }

    impl<R: Inner> CarrierKind for Boxed<R>
    where
        Boxed<R>: Outer<CType = CellBox<<R as Inner>::CType>>,
    {
        const NAME: &'static str = "cell";
    }
}

#[test]
fn nested_assoc_binding_dispatches_by_outer_type() {
    assert_eq!(<Boxed<Plain> as CarrierKind>::NAME, "const");
    assert_eq!(<Boxed<Interior> as CarrierKind>::NAME, "cell");
}
