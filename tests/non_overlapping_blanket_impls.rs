use disjoint_impls::disjoint_impls;

disjoint_impls! {
    trait Kita {
        const NAME: &'static str;
    }

    impl<T> Kita for (T,) {
        const NAME: &'static str = "Blanket 1";
    }

    impl Kita for Option<u32> {
        const NAME: &'static str = "Concrete Option<u32>";
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
    impl Kita for Option<u32> {
        const NAME: &'static str = "Concrete Option<u32>";
    }
};
*/

#[test]
fn non_overlapping_blanket_impls() {
    assert_eq!("Blanket 1", <(String,)>::NAME);
    assert_eq!("Concrete Option<u32>", Option::<u32>::NAME);
}
