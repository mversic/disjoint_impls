use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group;
}

disjoint_impls! {
    pub trait Kita {
        const NAME: &'static str;
    }

    impl<T: Dispatch<Group = impl Dispatch>> Kita for T {
        const NAME: &'static str = "Blanket A";
    }
}

fn main() {}
