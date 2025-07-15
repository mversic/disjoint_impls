use disjoint_impls::disjoint_impls;

disjoint_impls! {
    pub trait Kita<T> where T: Default {
        fn name(_value: u32) -> &'static str;
    }

    impl<T: Default, R> Kita<T> for R where R: std::ops::Deref<Target = str> {
        fn name(_value: u32) -> &'static str {
            "Blanket"
        }
    }
}

#[test]
fn single_impl() {
    assert_eq!("Blanket", <String as Kita<u32>>::name(12));
    assert_eq!("Blanket", <Box<str> as Kita<String>>::name(12));
}

/*
pub trait Kita<T> where T: Default {
    fn name(_: u32) -> &'static str;
}
const _: () = {
    impl<_0: Default, _1> Kita<_0> for _1 where _1: std::ops::Deref<Target = str> {
        fn name(_value: u32) -> &'static str {
            "Blanket"
        }
    }
};
*/
