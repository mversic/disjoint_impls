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

#[test]
fn single_impl() {
    assert_eq!("Blanket", <String as Kita<u32>>::kita(12));
    assert_eq!("Blanket", <Box<str> as Kita<String>>::kita(12));
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
    impl<_0: Default, _1> Kita<_0> for _1
    where
        _1: std::ops::Deref<Target = str>,
    {
        type GenericAssociatedType<GAT> = GAT;

        fn kita<GAT>(_: Self::GenericAssociatedType<GAT>) -> &'static str {
            "Blanket"
        }
    }
};
*/
