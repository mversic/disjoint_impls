use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group;
}

pub trait Trait {}

pub enum GroupA {}
impl Dispatch for String {
    type Group = GroupA;
}
impl<T> Dispatch for Vec<T> {
    type Group = GroupA;
}

pub enum GroupB {}
impl Dispatch for i32 {
    type Group = GroupB;
}
impl Dispatch for u32 {
    type Group = GroupB;
}

impl Trait for u32 {}
impl<T> Trait for [T; 1] {}

disjoint_impls! {
    pub trait Kita<T: Trait = u32, const N: usize = 12> {
        const NAME: &'static str;
    }

    impl<T: Dispatch<Group = GroupA>> Kita for T {
        const NAME: &'static str = "Blanket A";
    }
    impl<T: Dispatch<Group = GroupA>> Kita<[T; 1]> for T {
        const NAME: &'static str = "Blanket B";
    }
}

fn main() {}
