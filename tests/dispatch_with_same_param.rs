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

// NOTE: Type parameters in the Dispatch trait must be the same
disjoint::impls! {
    pub trait Kita {
        const NAME: &'static str;
    }

    impl<T: Dispatch<(), Group = GroupA>> Kita for T {
        const NAME: &'static str = "Blanket A";
    }
    impl<T: Dispatch<(), Group = GroupB>> Kita for T {
        const NAME: &'static str = "Blanket B";
    }
}

/*
pub trait Kita {
    const NAME: &'static str;
}

const _: () = {
    trait _Kita<T> {
        const _NAME: &'static str;
    }

    impl<T: Dispatch<(), Group = GroupA>> _Kita<GroupA> for T {
        const _NAME: &'static str = "Blanket A";
    }
    impl<T: Dispatch<(), Group = GroupB>> _Kita<GroupB> for T {
        const _NAME: &'static str = "Blanket B";
    }

    impl<T: Dispatch<()> + _Kita<T::Group>> Kita for T {
        const NAME: &'static str = <T as _Kita<T::Group>>::_NAME;
    }
};
*/

fn main() {
//    assert_eq!("Blanket A", String::NAME);
//    assert_eq!("Blanket A", Vec::<u32>::NAME);
//    assert_eq!("Blanket B", u32::NAME);
//    assert_eq!("Blanket B", i32::NAME);
}
