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
// TODO: If Dispatch trait is parametrized,
// then T must be the same concrete type in all impls
disjoint::impls! {
    pub trait Kita {
        const NAME: &'static str;
    }

    impl<T: Dispatch<GroupA, Group = GroupA>> Kita for T {
        const NAME: &'static str = "Blanket A";
    }
    impl<T: Dispatch<GroupB, Group = GroupB>> Kita for T {
        const NAME: &'static str = "Blanket B";
    }
}

fn main() {
    unimplemented!("This test must not compile");
    // Look at `TraitBound` type in src/lib.rs
}
