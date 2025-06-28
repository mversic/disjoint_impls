use disjoint_impls::disjoint_impls;

disjoint_impls! {
    trait Kita {
        const NAME: &'static str;
    }

    impl<T> Kita for (T,) {
        const NAME: &'static str = "Blanket 1";
    }

    impl<T> Kita for Option<T> {
        const NAME: &'static str = "Blanket 2";
    }
}

/*
trait Kita {
    const NAME: &'static str;
}

const _: () = {
    pub trait _Kita0 {
        const NAME: &'static str;
    }
    pub trait _Kita1 {
        const NAME: &'static str;
    }
    impl<_ŠČ0> _Kita0 for (_ŠČ0,) {
        const NAME: &'static str = "Blanket 1";
    }
    impl<_ŠČ0> _Kita1 for Option<_ŠČ0> {
        const NAME: &'static str = "Blanket 2";
    }
    impl<_ŠČ0> Kita for (_ŠČ0,)
    where
        Self: _Kita0,
    {
        const NAME: &'static str = <Self as _Kita0>::NAME;
    }
    impl<_ŠČ0> Kita for Option<_ŠČ0>
    where
        Self: _Kita1,
    {
        const NAME: &'static str = <Self as _Kita1>::NAME;
    }
};
*/

#[test]
fn non_overlapping_blanket_impls() {
    assert_eq!("Blanket 1", <(String, )>::NAME);
    assert_eq!("Blanket 2", <Option<Vec::<u32>>>::NAME);
}
