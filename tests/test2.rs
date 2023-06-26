pub trait Dispatch {
    type Group;
}

pub enum GroupA {}
impl Dispatch for Option<String> {
    type Group = GroupA;
}
impl<T> Dispatch for Option<Vec<T>> {
    type Group = GroupA;
}

pub enum GroupB {}
impl Dispatch for Option<i32> {
    type Group = GroupB;
}
impl Dispatch for Option<u32> {
    type Group = GroupB;
}

disjoint::impls! {
    pub trait Kita {
        const NAME: &'static str;
    }

    impl<T> Kita for T where Option<T>: Dispatch<Group = GroupA> {
        const NAME: &'static str = "Blanket A";
    }
    impl<T> Kita for T where Option<T>: Dispatch<Group = GroupB> {
        const NAME: &'static str = "Blanket B";
    }
}

/*
pub trait Kita {
    const NAME: &'static str;
}

const _: () = {
    trait _Kita<T0> {
        const _NAME: &'static str;
    }

    impl<T> Kita for T where Self: Dispatch + _Kita<<Option<T> as Dispatch>::Group> {
        const NAME: &'static str = <Self as _Kita<<Option<T> as Dispatch>::Group>>::_NAME;
    }

    impl<T> _Kita<GroupA> for T where Option<T>: Dispatch<Group = GroupA> {
        const _NAME: &'static str = "Blanket A";
    }
    impl<T> _Kita<GroupB> for T where Option<T>: Dispatch<Group = GroupB> {
        const _NAME: &'static str = "Blanket B";
    }
};
*/

fn main() {
    assert_eq!("Blanket A", Option::<String>::NAME);
    assert_eq!("Blanket A", Option::<Vec::<u32>>::NAME);
    assert_eq!("Blanket B", Option::<u32>::NAME);
    assert_eq!("Blanket B", Option::<i32>::NAME);
}
