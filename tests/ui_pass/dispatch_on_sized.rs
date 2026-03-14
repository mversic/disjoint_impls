use disjoint_impls::disjoint_impls;

trait Dispatch {
    type Group: ?Sized;
}

impl Dispatch for u8 {
    type Group = u8;
}

impl Dispatch for str {
    type Group = str;
}

disjoint_impls! {
    trait Kita {
        const NAME: &str;
    }

    impl<T: Dispatch<Group: Sized>> Kita for T {
        const NAME: &str = "Blanket Sized";
    }
    impl<T: Dispatch<Group = str> + ?Sized> Kita for T {
        const NAME: &str = "Blanket str";
    }
}

fn main() {
    assert_eq!(<u8 as Kita>::NAME, "Blanket Sized");
    assert_eq!(<str as Kita>::NAME, "Blanket str");
}
