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
    pub trait Kita {
        const NAME: &'static str;
    }

    impl<T: IntoIterator<Item = U>, U: Dispatch<Group = GroupA>> Kita for T {
        const NAME: &'static str = "Blanket A";
    }
    impl<T: IntoIterator<Item = U>, U: Dispatch<Group = GroupB>> Kita for T {
        const NAME: &'static str = "Blanket B";
    }
}

/*
pub trait Kita {
    const NAME: &'static str;
}

const _: () = {
    trait _Kita<T0> {
        const NAME: &'static str;
    }

    impl<U: Dispatch + _Kita<U::Group>> Kita for U {
        const NAME: &'static str = <U as _Kita<U::Group>>::NAME;
    }

    impl<U: Dispatch<Group = GroupA>> _Kita<GroupA> for U {
        const NAME: &'static str = "Blanket A";
    }
    impl<U: Dispatch<Group = GroupB>> _Kita<GroupB> for U {
        const NAME: &'static str = "Blanket B";
    }
};
*/

fn main() {
    assert_eq!("Blanket A", String::NAME);
    assert_eq!("Blanket A", Vec::<u32>::NAME);
    assert_eq!("Blanket B", u32::NAME);
    assert_eq!("Blanket B", i32::NAME);
}
