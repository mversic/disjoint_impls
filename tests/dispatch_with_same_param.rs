use disjoint_impls::disjoint_impls;

pub trait Dispatch<T> {
    type Group;
}

pub enum GroupA {}
impl Dispatch<()> for String {
    type Group = GroupA;
}
impl<T> Dispatch<()> for Vec<T> {
    type Group = GroupA;
}

pub enum GroupB {}
impl Dispatch<()> for i32 {
    type Group = GroupB;
}
impl Dispatch<()> for u32 {
    type Group = GroupB;
}

disjoint_impls! {
    pub trait Kita {
        const NAME: &'static str;
    }

    impl<T: Dispatch<(), Group = GroupA>> Kita for T {
        const NAME: &'static str = "1st Blanket A";
    }
    impl<T: Dispatch<(), Group = GroupB>> Kita for T {
        const NAME: &'static str = "1st Blanket B";
    }

    impl<T: Dispatch<u32, Group = GroupA>> Kita for T {
        const NAME: &'static str = "2nd Blanket A";
    }
    impl<T: Dispatch<u32, Group = GroupB>> Kita for T {
        const NAME: &'static str = "2nd Blanket B";
    }
}

/*
*/

#[test]
fn dispatch_with_same_param() {
    assert_eq!("Blanket A", String::NAME);
    assert_eq!("Blanket A", Vec::<u32>::NAME);
    assert_eq!("Blanket B", u32::NAME);
    assert_eq!("Blanket B", i32::NAME);
}
