use disjoint_impls::disjoint_impls;

struct LocalType<T>(T);

struct GroupA<T>(T);
struct GroupB<T>(T);

pub trait Dispatch {
    type Group;
}

impl Dispatch for Option<u32> {
    type Group = *const u32;
}

disjoint_impls! {
    trait Kita {
        type Group;

        const NAME: &'static str;
    }

    impl<T, U> Kita for LocalType<T>
    where
        T: Kita<Group = GroupA<U>>,
    {
        type Group = ();

        const NAME: &'static str = "Blanket A";
    }

    impl<T, U> Kita for LocalType<T>
    where
        T: Kita<Group = GroupB<U>>,
    {
        type Group = ();

        const NAME: &'static str = "Blanket B";
    }
}

/*
trait Kita {
    type Group;
    const NAME: &'static str;
}

const _: () = {
    pub trait Kita0<_TŠČ0: ?Sized> {
        type Group;
        const NAME: &'static str;
    }
    impl<T, U> Kita0<GroupA<U>> for LocalType<T>
    where
        T: Kita<Group = GroupA<U>>,
    {
        type Group = ();
        const NAME: &'static str = "Blanket A";
    }
    impl<T, U> Kita0<GroupB<U>> for LocalType<T>
    where
        T: Kita<Group = GroupB<U>>,
    {
        type Group = ();
        const NAME: &'static str = "Blanket B";
    }
    impl<_TŠČ0> Kita for LocalType<_TŠČ0>
    where
        _TŠČ0: Kita,
        Self: Kita0<<_TŠČ0 as Kita>::Group>,
    {
        type Group = <Self as Kita0<<_TŠČ0 as Kita>::Group>>::Group;
        const NAME: &'static str = <Self as Kita0<<_TŠČ0 as Kita>::Group>>::NAME;
    }
};
*/

disjoint_impls! {
    pub trait Kita2 {
        const NAME: &'static str;
    }

    impl<'a, R: Kita2> Kita2 for Option<&'a R>
    where
        Option<R>: Dispatch<Group = *const R>,
    {
        const NAME: &'static str = "Blanket A";
    }

    impl<'a, R: Dispatch> Kita2 for Option<&'a R>
    where
        Option<R>: Dispatch<Group = Option<<R as Dispatch>::Group>>,
    {
        const NAME: &'static str = "Blanket B";
    }
}

/*
pub trait Kita2 {
    const NAME: &'static str;
}

const _: () = {
    pub trait Kita20<_TŠČ0: ?Sized> {
        const NAME: &'static str;
    }
    impl<'a, R: Kita2> Kita20<*const R> for Option<&'a R>
    where
        Option<R>: Dispatch<Group = *const R>,
    {
        const NAME: &'static str = "Blanket A";
    }
    impl<'a, R: Dispatch> Kita20<<Option<R> as Dispatch>::Group> for Option<&'a R>
    where
        Option<R>: Dispatch<Group = Option<<R as Dispatch>::Group>>,
    {
        const NAME: &'static str = "Blanket B";
    }
    impl<'_lšč0, _TŠČ0: '_lšč0> Kita2 for Option<&'_lšč0 _TŠČ0>
    where
        Option<_TŠČ0>: Dispatch,
        Self: Kita20<<Option<_TŠČ0> as Dispatch>::Group>,
    {
        const NAME: &'static str = <Self as Kita20<
            <Option<_TŠČ0> as Dispatch>::Group,
        >>::NAME;
    }
};
*/

impl Kita for u32 {
    type Group = GroupA<i32>;

    const NAME: &'static str = "u32 impl";
}

impl Kita for i32 {
    type Group = GroupB<u32>;
    const NAME: &'static str = "i32 impl";
}

impl Kita2 for u32 {
    const NAME: &'static str = "Blanket A";
}

#[test]
fn derivative_assoc_binding() {
    assert_eq!("u32 impl", <u32 as Kita>::NAME);
    assert_eq!("i32 impl", <i32 as Kita>::NAME);

    assert_eq!("Blanket A", <LocalType<u32> as Kita>::NAME);
    assert_eq!("Blanket B", <LocalType<i32> as Kita>::NAME);

    assert_eq!("Blanket A", <Option<&u32> as Kita2>::NAME);
}
