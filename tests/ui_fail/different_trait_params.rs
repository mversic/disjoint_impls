use disjoint_impls::disjoint_impls;

pub trait Dispatch<T> {
    type Group;
}

pub enum GroupA {}
impl Dispatch<u32> for String {
    type Group = GroupA;
}
impl<T> Dispatch<i32> for Vec<T> {
    type Group = GroupA;
}

pub enum GroupB {}
impl Dispatch<i32> for i32 {
    type Group = GroupB;
}
impl Dispatch<u32> for u32 {
    type Group = GroupB;
}

disjoint_impls! {
    pub trait Kita {
        const NAME: &'static str;
    }

    impl<T: Dispatch<i32, Group = GroupA>> Kita for T {
        const NAME: &'static str = "Blanket A";
    }
    impl<T: Dispatch<u32, Group = GroupB>> Kita for T {
        const NAME: &'static str = "Blanket B";
    }
}

fn main() {}

