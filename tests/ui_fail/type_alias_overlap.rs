use disjoint_impls::disjoint_impls;

type Alias = Vec<u8>;

trait Dispatch {
    type Group;
}

impl Dispatch for u32 {
    type Group = Vec<u8>;
}

disjoint_impls! {
    trait Kita {}

    impl<T: Dispatch<Group = Alias>> Kita for T {}
    impl<T: Dispatch<Group = Vec<u8>>> Kita for T {}
}
