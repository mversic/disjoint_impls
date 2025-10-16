use disjoint_impls::disjoint_impls;

disjoint_impls! {
    pub trait Kita {
        const NAME: &'static str;
    }
}

fn main() {}
