use disjoint_impls::disjoint_impls;

trait Dispatch {
    type Group;
}

enum GroupA {}
enum GroupB {}

impl Dispatch for &str {
    type Group = GroupA;
}

impl Dispatch for i32 {
    type Group = GroupB;
}

struct Wrapper<T>(T);

disjoint_impls! {
    impl<T: Dispatch<Group = GroupA>,> Wrapper<T> {
        fn kita(&self) -> &'static str {
            "Blanket A"
        }
    }

    impl<T: Dispatch<Group = GroupB>> Wrapper<T> {
        fn kita(&self) -> &'static str {
            "Blanket B"
        }
    }
}

#[test]
fn inherent_with_receiver() {
    assert_eq!("Blanket A", Wrapper("foo").kita());
    assert_eq!("Blanket B", Wrapper(42).kita());
}

