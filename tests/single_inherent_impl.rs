use disjoint_impls::disjoint_impls;

pub struct Wrapper<'a, T, const N: usize>(pub &'a T);

disjoint_impls! {
    impl<'a, T, U> Wrapper<'a, (T, U), 12> {
        pub const NAME: &'static str = "Blanket";

        fn kita(_a: T, _b: U) -> &'static str
        where
            T: 'a,
            U: 'a,
        {
            Self::NAME
        }
    }
}

/*
const _: () = {
    pub trait Wrapper0<'_lšč0, _TŠČ0, _TŠČ1> {
        const NAME_šč: &'static str;
        fn kita_šč(_a: _TŠČ0, _b: _TŠČ1) -> &'static str
        where
            _TŠČ0: '_lšč0,
            _TŠČ1: '_lšč0;
    }
    impl<'a, T, U> Wrapper0<'a, T, U> for Wrapper<'a, (T, U), 12> {
        const NAME_šč: &'static str = "Blanket";
        fn kita_šč(_a: T, _b: U) -> &'static str
        where
            T: 'a,
            U: 'a,
        {
            Self::NAME
        }
    }
    impl<'_lšč0, _TŠČ0, _TŠČ1> Wrapper<'_lšč0, (_TŠČ0, _TŠČ1), 12>
    where
        Self: for<'_dšč> Wrapper0<'_lšč0, _TŠČ0, _TŠČ1>,

    {
        fn kita(_a: _TŠČ0, _b: _TŠČ1) -> &'static str
        where
            _TŠČ0: '_lšč0,
            _TŠČ1: '_lšč0,
        {
            <Self as Wrapper0<'_lšč0, _TŠČ0, _TŠČ1>>::kita_šč(_a, _b)
        }
        const NAME: &'static str = <Self as Wrapper0<'_lšč0, _TŠČ0, _TŠČ1>>::NAME_šč;
    }
};
*/

#[test]
fn single_inherent_impl() {
    assert_eq!("Blanket", <Wrapper<(u32, i32), 12>>::kita(42, 420));
    assert_eq!("Blanket", <Wrapper<(u32, i32), 12>>::NAME);
}
