use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group;
}

pub enum GroupA {}
pub enum GroupB {}
pub enum GroupC {}

disjoint_impls! {
    pub trait Kita {
        const NAME: &'static str;
    }

    impl<T: Dispatch<Group = GroupA>> Kita for T {
        const NAME: &'static str = "Blanket A";
    }
    impl<T: Dispatch<Group = GroupB>> Kita for &T {
        const NAME: &'static str = "Blanket B";
    }
    impl<U: Dispatch<Group = GroupC>> Kita for &mut U {
        const NAME: &'static str = "Blanket C";
    }
}

fn main() {}
