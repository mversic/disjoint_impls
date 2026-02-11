use disjoint_impls::disjoint_impls;

disjoint_impls! {
    pub trait Kita<T>
    where
        T: Default,
    {
        type GenericAssociatedType<GAT>;

        fn kita<GAT>(_: Self::GenericAssociatedType<GAT>) -> &'static str;
    }

    impl<T: Default, R> Kita<T> for R
    where
        R: std::ops::Deref<Target = str>,
    {
        type GenericAssociatedType<GAT> = GAT;

        fn kita<GAT>(_: Self::GenericAssociatedType<GAT>) -> &'static str {
            "Blanket"
        }
    }
}

/*
pub trait Kita<T>
where
    T: Default,
{
    type GenericAssociatedType<GAT>;
    fn kita<GAT>(_: Self::GenericAssociatedType<GAT>) -> &'static str;
}

const _: () = {
    pub trait Kita0<_TŠČ1: ?Sized, _TŠČ0>: Kita<_TŠČ0>
    where
        _TŠČ0: Default,
    {
        type GenericAssociatedType_šč<GAT>;
        fn kita_šč<GAT>(_: Self::GenericAssociatedType<GAT>) -> &'static str;
    }
    impl<T: Default, R> Kita0<str, T> for R
    where
        R: std::ops::Deref<Target = str>,
    {
        type GenericAssociatedType_šč<GAT> = GAT;
        fn kita_šč<GAT>(_: Self::GenericAssociatedType<GAT>) -> &'static str {
            "Blanket"
        }
    }
    impl<_TŠČ0, _TŠČ1> Kita<_TŠČ0> for _TŠČ1
    where
        _TŠČ0: Default,
        _TŠČ0: Default,
        _TŠČ1: std::ops::Deref,
        Self: Kita0<<_TŠČ1 as std::ops::Deref>::Target, _TŠČ0>,
    {
        type GenericAssociatedType<GAT> = <Self as Kita0<
            <_TŠČ1 as std::ops::Deref>::Target,
            _TŠČ0,
        >>::GenericAssociatedType_šč<GAT>;
        fn kita<GAT>(arg0: Self::GenericAssociatedType<GAT>) -> &'static str {
            <Self as Kita0<<_TŠČ1 as std::ops::Deref>::Target, _TŠČ0>>::kita_šč::<GAT>(arg0)
        }
    }
};
*/

#[test]
fn single_impl() {
    assert_eq!("Blanket", <String as Kita<u32>>::kita(12));
    assert_eq!("Blanket", <Box<str> as Kita<String>>::kita(12));
}
