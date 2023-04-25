// NOTE: This test must not compile
pub trait Kita {
    const NAME: &'static str;
}
pub trait Dispatch<T> {
    type Group;
}

pub enum GroupA {}
pub enum GroupB {}

impl Dispatch<GroupA> for String {
    type Group = GroupA;
}
impl Dispatch<GroupB> for String {
    type Group = GroupB;
}

// NOTE: Kita is now implemented twice for String
disjoint::impls! {
    impl<T: Dispatch<GroupA, Group = GroupA>> Kita for T {
        const NAME: &'static str = "Blanket A";
    }
    impl<T: Dispatch<GroupB, Group = GroupB>> Kita for T {
        const NAME: &'static str = "Blanket B";
    }
}

fn main() {}
