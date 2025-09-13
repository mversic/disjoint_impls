use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group;
}

trait Tr {
    type A;
}

impl Tr for u8 {
    type A = u16;
}
impl Tr for i8 {
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

    impl<T: Dispatch<Group = (U,)>, U: Tr<A = A>, A> Kita<u32, U> for T {}
}

/*
pub trait Kita<U, V> {
    fn kita(_a: U, _b: V) -> String {
        "Default Blanket".to_owned()
    }
}

const _: () = {
    pub trait Kita0<_TŠČ2: ?Sized, U, V> {
        fn kita(_a: U, _b: V) -> String {
            "Default Blanket".to_owned()
        }
    }

    impl<T: Dispatch<Group = (U, V)>, U, V> Kita0<(U, V), (U,), V> for T {
        fn kita(_a: (U,), _b: V) -> String {
            "Generic Blanket A".to_owned()
        }
    }
    impl<T: Dispatch<Group = [(U, V); 1]>, U: ToString, V> Kita0<[(U, V); 1], (V,), U>
    for T {
        fn kita(_a: (V,), b: U) -> String {
            b.to_string()
        }
    }

    impl<U, V, T, _TŠČ3> Kita<(U,), V> for T
    where
        Self: Kita0<_TŠČ3, (U,), V>,
        T: Dispatch<Group = _TŠČ3>,
    {
        fn kita(_a: (U,), _b: V) -> String {
            { <Self as Kita0<_TŠČ3, (U,), V>>::kita(_a, _b) }
        }
    }
    impl<T: Dispatch<Group = (U,)>, U: Tr<A = A>, A> Kita<u32, U> for T {}
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
