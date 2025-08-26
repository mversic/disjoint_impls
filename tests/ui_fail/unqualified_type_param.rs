use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group;
}

pub enum GroupA {}
pub enum GroupB {}

impl Dispatch for String {
    type Group = i32;
}
impl<T> Dispatch for Vec<T> {
    type Group = u32;
}
impl Dispatch for i32 {
    type Group = GroupA;
}
impl Dispatch for u32 {
    type Group = GroupB;
}

disjoint_impls! {
    pub trait Kita {
        const NAME: &'static str;
    }

    impl<T: Dispatch> Kita for T
    where
        T::Group: Dispatch<Group = GroupA>,
    {
        const NAME: &'static str = "Blanket A";
    }

    impl<T: Dispatch> Kita for T
    where
        T::Group: Dispatch<Group = GroupB>,
    {
        const NAME: &'static str = "Blanket B";
    }
}

fn main() {}
