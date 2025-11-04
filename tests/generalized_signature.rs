use disjoint_impls::disjoint_impls;

trait Dispatch {
    type Group;
}

impl Dispatch for String {
    type Group = bool;
}
impl Dispatch for u32 {
    type Group = u8;
}

disjoint_impls! {
    trait Kita<T, U> {
        const NAME: &'static str;
    }

    impl<B, U> Kita<U, U> for B
    where
        B: Dispatch<Group = bool>,
        U: Dispatch<Group = u8>,
    {
        const NAME: &'static str = "Blanket A";
    }
    impl<B, U> Kita<B, U> for U
    where
        B: Dispatch<Group = bool>,
        U: Dispatch<Group = u8>,
    {
        const NAME: &'static str = "Blanket B";
    }
    impl<B, U> Kita<B, U> for B
    where
        B: Dispatch<Group = bool>,
        U: Dispatch<Group = u8>,
    {
        const NAME: &'static str = "Blanket C";
    }
}

/*
trait Kita<T, U> {
    const NAME: &'static str;
}

const _: () = {
    pub trait Kita0<_TŠČ2: ?Sized, _TŠČ3: ?Sized, _TŠČ4: ?Sized, T, U> {
        const NAME: &'static str;
    }

    impl<B, U> Kita0<bool, u8, u8, U, U> for B
    where
        B: Dispatch<Group = bool>,
        U: Dispatch<Group = u8>,
    {
        const NAME: &'static str = "Blanket A";
    }

    impl<B, U> Kita0<u8, bool, u8, B, U> for U
    where
        B: Dispatch<Group = bool>,
        U: Dispatch<Group = u8>,
    {
        const NAME: &'static str = "Blanket B";
    }
    impl<B, U> Kita0<bool, bool, u8, B, U> for B
    where
        B: Dispatch<Group = bool>,
        U: Dispatch<Group = u8>,
    {
        const NAME: &'static str = "Blanket C";
    }

    impl<_TŠČ0, _TŠČ1, _TŠČ2> Kita<_TŠČ0, _TŠČ1> for _TŠČ2
    where
        Self: Kita0<
            <_TŠČ2 as Dispatch>::Group,
            <_TŠČ0 as Dispatch>::Group,
            <_TŠČ1 as Dispatch>::Group,
            _TŠČ0,
            _TŠČ1,
        >,
        _TŠČ2: Dispatch,
        _TŠČ0: Dispatch,
        _TŠČ1: Dispatch,
    {
        const NAME: &'static str = <Self as Kita0<
            <_TŠČ2 as Dispatch>::Group,
            <_TŠČ0 as Dispatch>::Group,
            <_TŠČ1 as Dispatch>::Group,
            _TŠČ0,
            _TŠČ1,
        >>::NAME;
    }
};
*/

#[test]
fn generalized_signature() {
    assert_eq!(<String as Kita<u32, u32>>::NAME, "Blanket A");
    assert_eq!(<u32 as Kita::<String, u32>>::NAME, "Blanket B");
    assert_eq!(<String as Kita::<String, u32>>::NAME, "Blanket C");
}
