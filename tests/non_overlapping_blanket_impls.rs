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
    impl<_0> Kita for (_0,) {
        const NAME: &'static str = "Blanket 1";
    }
    impl<_0> Kita for Option<_0> {
        const NAME: &'static str = "Blanket 2";
    }
};
*/

#[test]
fn non_overlapping_blanket_impls() {
    assert_eq!("Blanket 1", <(String,)>::NAME);
    assert_eq!("Blanket 2", <Option<Vec::<u32>>>::NAME);
}
