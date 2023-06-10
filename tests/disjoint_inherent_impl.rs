pub trait Dispatch {
    type Group;
}

pub enum GroupA {}
impl Dispatch for String {
    type Group = GroupA;
}
impl<T> Dispatch for Vec<T> {
    type Group = GroupA;
}

pub enum GroupB {}
impl Dispatch for i32 {
    type Group = GroupB;
}
impl Dispatch for u32 {
    type Group = GroupB;
}

pub struct Wrapper<T>(T);

disjoint::impls! {
    impl<T> Wrapper<T> where T: Dispatch<Group = GroupA> {
        const NAME: &'static str = "Blanket A";
    }
    impl<T> Wrapper<T> where T: Dispatch<Group = GroupB> {
        const NAME: &'static str = "Blanket B";
    }
}

/*
const _: () = {
    trait _Wrapper<T0> {
        const _NAME: &'static str;
    }

    impl<T: _Wrapper<T::Group>> Wrapper<T> where T: Dispatch {
        const NAME: &'static str = <T as _Wrapper<T::Group>>::_NAME;
    }
    impl<T> _Wrapper<GroupA> for T where T: Dispatch<Group = GroupA>{
        const _NAME: &'static str = "Blanket A";
    }
    impl<T> _Wrapper<GroupB> for T where T: Dispatch<Group = GroupB>{
        const _NAME: &'static str = "Blanket B";
    }

};
*/

fn main() {
    assert_eq!("Blanket A", <Wrapper<String>>::NAME);
    assert_eq!("Blanket A", <Wrapper<Vec::<u32>>>::NAME);
    assert_eq!("Blanket B", <Wrapper<u32>>::NAME);
    assert_eq!("Blanket B", <Wrapper<i32>>::NAME);
}
