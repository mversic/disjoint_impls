use disjoint_impls::disjoint_impls;

trait Supertrait<const FLAG: bool> {}

impl Supertrait<true> for u8 {}
impl Supertrait<false> for u16 {}

disjoint_impls! {
    trait Encode<const FLAG: bool>: Supertrait<FLAG> {
        const VALUE: bool;
    }

    impl Encode<true> for u8 {
        const VALUE: bool = true;
    }
}

fn main() {}
