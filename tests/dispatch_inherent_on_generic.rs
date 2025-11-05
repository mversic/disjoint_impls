use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group;
}

trait A {
    type A;
}

impl A for u8 {
    type A = [u16; 1];
}
impl A for i8 {
    type A = [i16; 2];
}

impl Dispatch for String {
    type Group = (u32, u32);
}
impl<T> Dispatch for Vec<T> {
    type Group = (u32, String);
}

impl Dispatch for i32 {
    type Group = [(String, String); 1];
}
impl Dispatch for u32 {
    type Group = [(String, u32); 1];
}

impl Dispatch for u8 {
    type Group = (i8,);
}
impl Dispatch for i8 {
    type Group = (u8,);
}

struct Wrapper<T, U, V>(T, U, V);

disjoint_impls! {
    impl<T: Dispatch<Group = (U, V)>, U, V> Wrapper<T, (U,), V> {
        fn kita(_a: (U,), _b: V) -> String {
            "Generic Blanket A".to_owned()
        }
    }
    impl<T: Dispatch<Group = [(U, V); 1]>, U: ToString, V> Wrapper<T, (V,), U> {
        fn kita(_a: (V,), b: U) -> String {
            b.to_string()
        }
    }

    impl<T: Dispatch<Group = (U,)>, U: A<A = [Z; 1]>, Z: ToString> Wrapper<T, u32, U> {
        fn kara(_a: (U,), b: Z) -> String {
            b.to_string()
        }
    }
    impl<T: Dispatch<Group = (U,)>, U: A<A = [Z; 2]>, Z> Wrapper<T, u32, U> {
        fn kara(_a: (U,), _b: Z) -> String {
            "Generic Blanket B".to_owned()
        }
    }
}

/*
const _: () = {
    pub trait Wrapper0<_TŠČ4: ?Sized, _TŠČ0, _TŠČ1, _TŠČ2, _TŠČ3> {
        fn kita(_a: (_TŠČ1,), _b: _TŠČ2) -> String;
    }
    impl<T: Dispatch<Group = (U, V)>, U, V> Wrapper0<(U, V), T, U, V, (U, V)>
    for Wrapper<T, (U,), V> {
        fn kita(_a: (U,), _b: V) -> String {
            "Generic Blanket A".to_owned()
        }
    }
    impl<
        T: Dispatch<Group = [(U, V); 1]>,
        U: ToString,
        V,
    > Wrapper0<[(U, V); 1], T, V, U, [(U, V); 1]> for Wrapper<T, (V,), U> {
        fn kita(_a: (V,), b: U) -> String {
            b.to_string()
        }
    }

    pub trait Wrapper1<
        _TŠČ4: ?Sized,
        _TŠČ5: ?Sized,
        _TŠČ0,
        _TŠČ1,
        _TŠČ2,
        const _CŠČ0: usize,
    > {
        fn kara(_a: (_TŠČ1,), b: _TŠČ2) -> String;
    }
    impl<
        T: Dispatch<Group = (U,)>,
        U: A<A = [Z; 1]>,
        Z: ToString,
    > Wrapper1<[Z; 1], (U,), T, U, Z, 1> for Wrapper<T, u32, U> {
        fn kara(_a: (U,), b: Z) -> String {
            b.to_string()
        }
    }
    impl<
        T: Dispatch<Group = (U,)>,
        U: A<A = [Z; 2]>,
        Z,
    > Wrapper1<[Z; 2], (U,), T, U, Z, 2> for Wrapper<T, u32, U> {
        fn kara(_a: (U,), _b: Z) -> String {
            "Generic Blanket B".to_owned()
        }
    }

    impl<_TŠČ0, _TŠČ1, _TŠČ2, _TŠČ3> Wrapper<_TŠČ0, (_TŠČ1,), _TŠČ2>
    where
        _TŠČ0: Dispatch<Group = _TŠČ3>,
        Self: Wrapper0<_TŠČ3, _TŠČ0, _TŠČ1, _TŠČ2, _TŠČ3>,
    {
        fn kita(_a: (_TŠČ1,), _b: _TŠČ2) -> String {
            <Self as Wrapper0<_TŠČ3, _TŠČ0, _TŠČ1, _TŠČ2, _TŠČ3>>::kita(_a, _b)
        }
    }
    impl<_TŠČ0, _TŠČ1, _TŠČ2, const _CŠČ0: usize> Wrapper<_TŠČ0, u32, _TŠČ1>
    where
        _TŠČ1: A<A = [_TŠČ2; _CŠČ0]>,
        _TŠČ0: Dispatch,
        Self: Wrapper1<
            [_TŠČ2; _CŠČ0],
            <_TŠČ0 as Dispatch>::Group,
            _TŠČ0,
            _TŠČ1,
            _TŠČ2,
            _CŠČ0,
        >,
    {
        fn kara(_a: (_TŠČ1,), b: _TŠČ2) -> String {
            <Self as Wrapper1<
                [_TŠČ2; _CŠČ0],
                <_TŠČ0 as Dispatch>::Group,
                _TŠČ0,
                _TŠČ1,
                _TŠČ2,
                _CŠČ0,
            >>::kara(_a, b)
        }
    }
};
 */

#[test]
fn dispatch_inherent_on_generic() {
    let a_arg1 = 42;
    let a_arg2 = 420;

    let b_arg2 = "moja".to_owned();
    let b_arg3 = "kita".to_owned();

    assert_eq!(
        "Generic Blanket A",
        Wrapper::<String, (u32,), u32>::kita((a_arg1,), a_arg2)
    );
    assert_eq!(
        "Generic Blanket A",
        Wrapper::<Vec<u32>, (u32,), String>::kita((a_arg1,), b_arg2)
    );
    assert_eq!(
        "kita",
        Wrapper::<u32, (u32,), String>::kita((a_arg1,), b_arg3)
    );

    assert_eq!(
        "Generic Blanket B",
        Wrapper::<u8, u32, i8>::kara((42_i8,), 42_i16)
    );
    assert_eq!("420", Wrapper::<i8, u32, u8>::kara((42_u8,), 420_u16));
}
