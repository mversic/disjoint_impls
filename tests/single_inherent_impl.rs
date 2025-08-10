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

#[test]
fn single_inherent_impl() {
    assert_eq!("Blanket", <Wrapper<(u32, i32), 12>>::kita(42, 420));
    assert_eq!("Blanket", <Wrapper<(u32, i32), 12>>::NAME);
}

/*
const _: () = {
    impl<'_ŠČ0, _ŠČ1, _ŠČ2> Wrapper<'_ŠČ0, (_ŠČ1, _ŠČ2), 12> {
        pub const NAME: &'static str = "1st Blanket A";

        fn kita(_a: _ŠČ1, _b: _ŠČ2) -> &'static str
        where
            _ŠČ1: '_ŠČ0,
            _ŠČ2: '_ŠČ0,
        {
            Self::NAME
        }
    }
};
*/
