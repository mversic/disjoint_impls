use disjoint_impls::disjoint_impls;

trait Dispatch {
    type Group;
}

struct FourArray;
impl Dispatch for FourArray {
    type Group = [u8; 4];
}

disjoint_impls! {
    trait Kita {}

    impl<T: Dispatch<Group = [u8; 2 + 2]>> Kita for T {}
    impl<T: Dispatch<Group = [u8; 4]>> Kita for T {}
}
