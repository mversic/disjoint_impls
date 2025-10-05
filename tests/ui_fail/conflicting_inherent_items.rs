use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group;
}

pub enum GroupA {}
impl Dispatch for String {
    type Group = GroupA;
}

pub enum GroupB {}
impl Dispatch for i32 {
    type Group = GroupB;
}

struct Wrapper<T>(T);

disjoint_impls! {
    impl<T: Dispatch<Group = GroupA>> Wrapper<T> {
        fn kita() -> &'static str {
            "Blanket A"
        }
    }
    impl<T: Dispatch<Group = GroupB>> Wrapper<T> {
        fn kita() -> String {
            "Blanket B".to_owned()
        }
    }
}

fn main() {}
