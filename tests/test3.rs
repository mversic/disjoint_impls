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

disjoint::impls! {
    pub trait Kita<U> {
        const NAME: &'static str;
    }

    impl<U: Dispatch<Group = GroupA>> Kita<U> for u32 {
        const NAME: &'static str = "Blanket A";
    }
    impl<U: Dispatch<Group = GroupB>> Kita<U> for u32 {
        const NAME: &'static str = "Blanket B";
    }
}

/*
pub trait Kita<U> {
    const NAME: &'static str;
}

const _: () = {
    trait _Kita<U> {
        const NAME: &'static str;
    }

    impl<U: Dispatch> Kita<U> for u32
    where
        Self: _Kita<U::Group>,
    {
        const NAME: &'static str = <Self as _Kita<<U as Dispatch>::Group>>::NAME;
    }

    impl _Kita<GroupA> for u32 {
        const NAME: &'static str = "Blanket A";
    }
    impl _Kita<GroupB> for u32 {
        const NAME: &'static str = "Blanket B";
    }
};
*/

fn main() {
    assert_eq!("Blanket A", <u32 as Kita<String>>::NAME);
    assert_eq!("Blanket B", <u32 as Kita<u32>>::NAME);
}
