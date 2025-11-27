use disjoint_impls::disjoint_impls;

trait Dispatch {
    type Group;
}

enum GroupA {}
enum GroupB {}

impl Dispatch for u32 {
    type Group = GroupA;
}

impl Dispatch for i32 {
    type Group = GroupB;
}

disjoint_impls! {
    trait Kita<T> {
        const NAME: &'static str;
    }

    impl<T: Dispatch<Group = GroupA>> Kita<<T as Dispatch>::Group> for T {
        const NAME: &'static str = "Blanket A";
    }

    impl<T: Dispatch<Group = GroupB>> Kita<Box<<T as Dispatch>::Group>> for T {
        const NAME: &'static str = "Blanket B";
    }
}

#[test]
fn fully_qualified_generic_param() {
    assert_eq!("Blanket A", u32::NAME);
    assert_eq!("Blanket B", i32::NAME);
}
