use disjoint_impls::disjoint_impls;

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

disjoint_impls! {
    impl<T> Wrapper<T> where T: Dispatch<Group = GroupA> {
        pub const NAME: &'static str = "Blanket A";
    }
    impl<T> Wrapper<T> where T: Dispatch<Group = GroupB> {
        pub const NAME: &'static str = "Blanket B";
    }
}

/*
const _: () = {
    trait _Wrapper0<T0: ?Sized> {
        const NAME: &'static str;
    }

    impl<T0> _Wrapper0<GroupA> for Wrapper<T0> where T0: Dispatch<Group = GroupA> {
        const NAME: &'static str = "Blanket A";
    }
    impl<T0> _Wrapper0<GroupB> for Wrapper<T0> where T0: Dispatch<Group = GroupB> {
        const NAME: &'static str = "Blanket B";
    }

    impl<T0> Wrapper<T0> where T0: Dispatch, Self: _Wrapper0<<T0 as Dispatch>::Group> {
        const NAME: &'static str = <Self as _Wrapper0<<T0 as Dispatch>::Group>>::NAME;
    }
};
*/

#[test]
fn disjoint_inherent_impl() {
    assert_eq!("Blanket A", <Wrapper<String>>::NAME);
    assert_eq!("Blanket A", <Wrapper<Vec::<u32>>>::NAME);
    assert_eq!("Blanket B", <Wrapper<u32>>::NAME);
    assert_eq!("Blanket B", <Wrapper<i32>>::NAME);
}
