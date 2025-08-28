use disjoint_impls::disjoint_impls;

pub trait LocalTrait {}

pub trait Dispatch {
    type Group;
}

pub enum LocalType {}
impl Dispatch for String {
    type Group = LocalType;
}
impl<T> Dispatch for Vec<T> {
    type Group = u32;
}

impl Dispatch for u32 {
    type Group = i32;
}

impl LocalTrait for i32 {}

// NOTE: Makes impls overlap
//impl LocalTrait for u32 {}

disjoint_impls! {
    pub trait Kita {
        const NAME: &str;
    }

    impl<T: Dispatch<Group = LocalType>> Kita for T {
        const NAME: &'static str = "Blanket A";
    }
    impl<T: Dispatch<Group = u32>> Kita for T {
        const NAME: &'static str = "Blanket B";
    }
    impl<U: Dispatch<Group = T>, T: LocalTrait> Kita for U {
        const NAME: &'static str = "Blanket C";
    }
}

/*
*/

#[test]
fn assoc_binding_overlap() {
    assert_eq!("Blanket A", String::NAME);
    assert_eq!("Blanket B", Vec::<u32>::NAME);
    assert_eq!("Blanket C", u32::NAME);
}
