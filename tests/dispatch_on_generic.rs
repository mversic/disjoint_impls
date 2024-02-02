use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group;
}

trait A {
    type A;
}

impl A for u8 {
    type A = u16;
}
impl A for i8 {
    type A = i16;
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

disjoint_impls! {
    pub trait Kita<U, V> {
        fn kita(_a: U, _b: V) -> String {
            "Default Blanket".to_owned()
        }
    }

    impl<T: Dispatch<Group = (U, V)>, U, V> Kita<(U,), V> for T {
        fn kita(_a: (U,), _b: V) -> String {
            "Generic Blanket A".to_owned()
        }
    }
    impl<T: Dispatch<Group = [(U, V); 1]>, U: ToString, V> Kita<(V,), U> for T {
        fn kita(_a: (V,), b: U) -> String {
            b.to_string()
        }
    }

    impl<T: Dispatch<Group = (U,)>, U: A<A = A>, A> Kita<u32, U> for T {}
}

/*
pub trait Kita<U, V> {
    fn kita(_a: U, _b: V) -> String {
        "Default Blanket".to_owned()
    }
}

const _: () = {
    pub trait _Kita0<_2: ?Sized, _3: ?Sized, U, V> {
        fn kita(_a: U, _b: V) -> String {
            "Default Blanket".to_owned()
        }
    }
    pub trait _Kita1<_2: ?Sized, U, V> {
        fn kita(_a: U, _b: V) -> String {
            "Default Blanket".to_owned()
        }
    }

    impl<_1: Dispatch<Group = (_0,)>, _0: A<A = _2>, _2> _Kita0<_2, (_0,), u32, _0> for _1 {}
    impl<_2: Dispatch<Group = (_0, _1)>, _0, _1> _Kita1<(_0, _1), (_0,), _1> for _2 {
        fn kita(_a: (_0,), _b: _1) -> String {
            "Generic Blanket A".to_owned()
        }
    }
    impl<_2: Dispatch<Group = [(_1, _0); 1]>, _1: ToString, _0> _Kita1<[(_1, _0); 1], (_0,), _1> for _2 {
        fn kita(_a: (_0,), b: _1) -> String {
            b.to_string()
        }
    }

    impl<_1, _0> Kita<u32, _0> for _1 where _1: Dispatch, _0: A, Self: _Kita0<<_0 as A>::A, <_1 as Dispatch>::Group, u32, _0> {
        fn kita(_a: u32, _b: _0) -> String {
            <Self as _Kita0<<_0 as A>::A, <_1 as Dispatch>::Group, u32, _0>>::kita(_a, _b)
        }
    }
    impl<_1, _0, _2> Kita<(_0,), _1> for _2 where _2: Dispatch, Self: _Kita1<<_2 as Dispatch>::Group, (_0,), _1> {
        fn kita(_a: (_0,), _b: _1) -> String {
            <Self as _Kita1<<_2 as Dispatch>::Group, (_0,), _1>>::kita(_a, _b)
        }
    }
};
*/

#[test]
fn dispatch_on_generic() {
    let a_arg1 = 42;
    let a_arg2 = 420;

    let b_arg1 = "Generic Blanket B".to_owned();
    let b_arg2 = "moja".to_owned();
    let b_arg3 = "kita".to_owned();

    assert_eq!("Generic Blanket A", String::kita((a_arg1,), a_arg2));
    assert_eq!("Generic Blanket A", Vec::<u32>::kita((a_arg1,), b_arg2));
    assert_eq!("Generic Blanket B", i32::kita((b_arg3,), b_arg1.clone()));
    assert_eq!("Generic Blanket B", u32::kita((a_arg2,), b_arg1));

    assert_eq!("Default Blanket", u8::kita(a_arg2, 8_i8));
    assert_eq!("Default Blanket", i8::kita(a_arg2, 8_u8));
}
