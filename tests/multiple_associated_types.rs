pub trait Kita {
    const NAME: &'static str;
}
pub trait Dispatch {
    type Group1;
    type Group2;
}

pub enum GroupA {}
pub enum GroupB {}

impl Dispatch for String {
    type Group1 = GroupA;
    type Group2 = GroupA;
}
impl<T> Dispatch for Vec<T> {
    type Group1 = GroupA;
    type Group2 = GroupB;
}

impl Dispatch for i32 {
    type Group1 = GroupB;
    type Group2 = GroupA;
}
impl Dispatch for u32 {
    type Group1 = GroupB;
    type Group2 = GroupB;
}

disjoint::impls! {
    impl<T: Dispatch<Group1 = GroupA, Group2 = GroupA>> Kita for T {
        const NAME: &'static str = "Blanket AA";
    }
    impl<T: Dispatch<Group1 = GroupA, Group2 = GroupB>> Kita for T {
        const NAME: &'static str = "Blanket AB";
    }
    impl<T: Dispatch<Group1 = GroupB>> Kita for T {
        const NAME: &'static str = "Blanket B*";
    }
}

// Expands to:
//trait _Kita<T1, T2> {
//    const _NAME: &'static str;
//}
//
//impl<T: Dispatch + _Kita<T::Group1, T::Group2>> Kita for T {
//    const NAME: &'static str = <T as _Kita<T::Group1, T::Group2>>::_NAME;
//}
//
//impl<T> _Kita<GroupA, GroupA> for T {
//    const _NAME: &'static str = "Blanket AA";
//}
//impl<T> _Kita<GroupA, GroupB> for T {
//    const _NAME: &'static str = "Blanket AB";
//}
//impl<T, F> _Kita<GroupB, F> for T {
//    const _NAME: &'static str = "Blanket B*";
//}

fn main() {
    assert_eq!("Blanket AA", String::NAME);
    assert_eq!("Blanket AB", Vec::<u32>::NAME);
    assert_eq!("Blanket B*", u32::NAME);
    assert_eq!("Blanket B*", i32::NAME);
}
