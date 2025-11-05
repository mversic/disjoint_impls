use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group;
}

pub enum GroupA {}
impl Dispatch for Option<String> {
    type Group = GroupA;
}

pub enum GroupB {}
impl Dispatch for i32 {
    type Group = GroupB;
}

struct Wrapper<T>(T);

disjoint_impls! {
    impl<T> Wrapper<T>
    where
        Option<T>: Dispatch<Group = GroupA>,
    {
        fn kita() -> &'static str {
            "Blanket A"
        }
    }
    impl<T> Wrapper<T>
    where
        T: Dispatch<Group = GroupB>,
    {
        fn kita() -> String {
            "Blanket B".to_owned()
        }
    }
}

/*
const _: () = {
    impl<T> Wrapper<T>
    where
        Option<T>: Dispatch<Group = GroupA>,
    {
        fn kita() -> &'static str {
            "Blanket A"
        }
    }

    impl<T> Wrapper<T>
    where
        T: Dispatch<Group = GroupB>,
    {
        fn kita() -> String {
            "Blanket B".to_owned()
        }
    }
};
*/

#[test]
fn differing_inherent_items() {
    assert_eq!("Blanket A", Wrapper::<String>::kita());
    assert_eq!("Blanket B".to_owned(), Wrapper::<i32>::kita());
}
