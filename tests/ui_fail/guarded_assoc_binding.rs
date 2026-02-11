use disjoint_impls::disjoint_impls;

trait Dispatch {
    type Group;
}

trait Marker {}
enum GroupA {}

disjoint_impls! {
    trait Kita {
        const NAME: &'static str;
    }

    impl<
        #[cfg(feature = "feat_a")]
        T: Dispatch<Group: Marker>,
        #[cfg(not(feature = "feat_a"))]
        T: Dispatch<Group = GroupA>,
    > Kita for T {
        const NAME: &'static str = "A";
    }
}

fn main() {}
