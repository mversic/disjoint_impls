use disjoint_impls::disjoint_impls;

disjoint_impls! {
    pub trait Kita<T> where T: Default {
        fn name(_value: u32) -> &'static str;
    }

    impl<T: Default, R> Kita<T> for R {
        fn name(_value: u32) -> &'static str {
            "Blanket"
        }
    }
}

#[test]
fn single_impl() {
    assert_eq!("Blanket", <String as Kita<u32>>::name(12));
    assert_eq!("Blanket", <u32 as Kita<String>>::name(12));
}

/*
pub trait Kita<T> where T: Default {
    fn name(_: u32) -> &'static str;
}
const _: () = {
    pub trait _Kita0<T> where T: Default {
        fn name(_: u32) -> &'static str;
    }
    impl<_ŠČ0: Default, _ŠČ1> Kita<_ŠČ0> for _ŠČ1 {
        fn name(_value: u32) -> &'static str {
            "Blanket"
        }
    }
    impl<_ŠČ0, _ŠČ1> Kita<_ŠČ0> for _ŠČ1 where _ŠČ0: Default, Self: _Kita0<_ŠČ0> {
        fn name(_value: u32) -> &'static str {
            <Self as _Kita0<_ŠČ0>>::name(_)
        }
    }
};
*/
