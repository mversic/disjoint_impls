use disjoint_impls::disjoint_impls;

trait Bound {}

trait Dispatch {
    type Group: ?Sized;
}

impl Bound for str {}
impl Dispatch for u32 {
    type Group = str;
}
impl Dispatch for i32 {
    type Group = u32;
}

disjoint_impls! {
    pub trait Kita {
        const NAME: &str;
    }

    impl<R: Dispatch<Group: Bound>> Kita for Box<R> {
        const NAME: &str = "Blanket A";
    }
    impl<R: Dispatch<Group = u32>> Kita for Box<R> {
        const NAME: &str = "Blanket B";
    }
}

/*
pub trait Kita {
    const NAME: &str;
}

const _: () = {
    pub trait Kita0<_TŠČ0: ?Sized> {
        const NAME: &str;
    }
    impl<R: Dispatch<Group = _TŠČ0>, _TŠČ0: Bound + ?Sized> Kita0<_TŠČ0>
    for Box<R> {
        const NAME: &str = "Blanket A";
    }
    impl<R: Dispatch<Group = u32>> Kita0<u32> for Box<R> {
        const NAME: &str = "Blanket B";
    }

    impl<_TŠČ0> Kita for Box<_TŠČ0>
    where
        _TŠČ0: Dispatch,
        Self: Kita0<<_TŠČ0 as Dispatch>::Group>,
    {
        const NAME: &str = <Self as Kita0<<_TŠČ0 as Dispatch>::Group>>::NAME;
    }
};
*/

#[test]
fn generic_arg_constraint() {
    assert_eq!(<Box<u32> as Kita>::NAME, "Blanket A");
    assert_eq!(<Box<i32> as Kita>::NAME, "Blanket B");
}
