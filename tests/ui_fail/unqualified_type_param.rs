use disjoint_impls::disjoint_impls;

pub trait Dispatch {}

disjoint_impls! {
    pub trait Kita1 {
        const NAME: &'static str;
    }

    impl<T: Dispatch> Kita1 for T
    where
        T::Group: Dispatch,
    {
        const NAME: &'static str = "Blanket A";
    }
}

disjoint_impls! {
    pub trait Kita2 {
        const NAME: &'static str;
    }

    impl<T> Kita2 for T
    where
        T: Dispatch,
        <T>::Group: Dispatch,
    {
        const NAME: &'static str = "Blanket A";
    }
}

fn main() {}
