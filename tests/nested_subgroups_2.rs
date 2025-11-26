use disjoint_impls::disjoint_impls;

trait LocalTrait {}
impl LocalTrait for i16 {}

pub trait Dispatch1 {
    type Group;
}
pub trait Dispatch2 {
    type Group;
}

pub enum GroupA {}
pub enum GroupB {}

impl Dispatch1 for String {
    type Group = GroupA;
}
impl Dispatch2 for String {
    type Group = GroupA;
}

impl Dispatch1 for Vec<i32> {
    type Group = GroupA;
}
impl Dispatch2 for Vec<i32> {
    type Group = GroupB;
}

impl Dispatch1 for Vec<i16> {
    type Group = i16;
}
impl Dispatch2 for Vec<i16> {
    type Group = i16;
}

impl Dispatch1 for Vec<u16> {
    type Group = i16;
}
impl Dispatch2 for Vec<u16> {
    type Group = GroupB;
}

impl Dispatch1 for i32 {
    type Group = GroupB;
}

disjoint_impls! {
    pub trait Kita<A> {
        const NAME: &'static str;
    }

    impl<T: Dispatch1<Group = GroupA> + Dispatch2<Group = GroupA>> Kita<u32> for T {
        const NAME: &'static str = "Blanket AA";
    }
    impl<T: Dispatch1<Group = GroupA> + Dispatch2<Group = GroupB>> Kita<u32> for T {
        const NAME: &'static str = "Blanket AB";
    }
    impl<T: Dispatch1<Group = S> + Dispatch2<Group = S>, S: LocalTrait> Kita<u32> for T {
        const NAME: &'static str = "Blanket **";
    }
    impl<T: Dispatch1<Group: LocalTrait> + Dispatch2<Group = GroupB>> Kita<u32> for T {
        const NAME: &'static str = "Blanket *B";
    }
    impl<T: Dispatch1<Group = GroupB>> Kita<u32> for T {
        const NAME: &'static str = "Blanket B*";
    }
}

#[test]
fn nested_subgroups_2() {
    assert_eq!("Blanket AA", String::NAME);
    assert_eq!("Blanket AB", Vec::<i32>::NAME);
    assert_eq!("Blanket **", Vec::<i16>::NAME);
    assert_eq!("Blanket *B", Vec::<u16>::NAME);
    assert_eq!("Blanket B*", i32::NAME);
}
