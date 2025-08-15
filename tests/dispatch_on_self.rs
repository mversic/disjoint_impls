use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group;
}

pub trait Dispatch2 {
    type Group;
    type Group2;
}

pub enum GroupA {}
impl Dispatch for Option<String> {
    type Group = GroupA;
}

pub enum GroupB {}
impl Dispatch for Option<i32> {
    type Group = GroupB;
}

disjoint_impls! {
    pub trait Kita {
        const NAME: &'static str;
    }

    impl<T> Kita for T where Self: Dispatch<Group = GroupA> {
        const NAME: &'static str = "Blanket A";
    }
    impl<T: Dispatch<Group = GroupB>> Kita for T {
        const NAME: &'static str = "Blanket B";
    }
}

#[test]
fn dispatch_on_self() {
    assert_eq!("Blanket A", String::NAME);
    assert_eq!("Blanket B", i32::NAME);
}
