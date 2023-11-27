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

// NOTE: Type parameters in the Dispatch trait must be the same
disjoint_impls! {
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
    pub trait _Kita0<_0: ?Sized> {
        const NAME: &'static str;
    }

    impl<_0: Dispatch<(), Group = GroupA>> _Kita0<GroupA> for _0 {
        const NAME: &'static str = "Blanket A";
    }
    impl<_0: Dispatch<(), Group = GroupB>> _Kita0<GroupB> for _0 {
        const NAME: &'static str = "Blanket B";
    }

    impl<_0> Kita for _0 where _0: Dispatch<()>, Self: _Kita0<<_0 as Dispatch<()>>::Group> {
        const NAME: &'static str = <Self as _Kita0<<_0 as Dispatch<()>>::Group>>::NAME;
    }
};
*/

#[test]
fn dispatch_with_same_param() {
    assert_eq!("Blanket A", String::NAME);
    assert_eq!("Blanket A", Vec::<u32>::NAME);
    assert_eq!("Blanket B", u32::NAME);
    assert_eq!("Blanket B", i32::NAME);
}
