//use disjoint_impls::disjoint_impls;
//
//pub trait Dispatch {
//    type Group;
//}
//
//trait A {
//    type A;
//}
//
//impl A for u8 {
//    type A = [u16; 1];
//}
//impl A for i8 {
//    type A = [i16; 2];
//}
//
//impl Dispatch for String {
//    type Group = (u32, u32);
//}
//impl<T> Dispatch for Vec<T> {
//    type Group = (u32, String);
//}
//
//impl Dispatch for i32 {
//    type Group = [(String, String); 1];
//}
//impl Dispatch for u32 {
//    type Group = [(String, u32); 1];
//}
//
//impl Dispatch for u8 {
//    type Group = (i8,);
//}
//impl Dispatch for i8 {
//    type Group = (u8,);
//}
//
//struct Wrapper<T, U, V>(T, U, V);
//
//disjoint_impls! {
//    impl<T: Dispatch<Group = (U, V)>, U, V> Wrapper<T, (U,), V> {
//        fn kita(_a: (U,), _b: V) -> String {
//            "Generic Blanket A".to_owned()
//        }
//    }
//    impl<T: Dispatch<Group = [(U, V); 1]>, U: ToString, V> Wrapper<T, (V,), U> {
//        fn kita(_a: (V,), b: U) -> String {
//            b.to_string()
//        }
//    }
//
//    impl<T: Dispatch<Group = (U,)>, U: A<A = [A; 1]>, A: ToString> Wrapper<T, u32, U> {
//        fn kara(_a: (U,), b: A) -> String {
//            b.to_string()
//        }
//    }
//    impl<T: Dispatch<Group = (U,)>, U: A<A = [A; 2]>, A> Wrapper<T, u32, U> {
//        fn kara(_a: (U,), _b: A) -> String {
//            "Generic Blanket B".to_owned()
//        }
//    }
//}
//
//const _: () = {
//    pub trait Wrapper0<_3: ?Sized, _0, _1, _2> {
//        fn kita(_a: (_1,), _b: _2) -> String;
//    }
//    pub trait Wrapper1<_3: ?Sized, _4: ?Sized, _0, _1, _2> {
//        fn kara(_a: (_1,), b: _2) -> String;
//    }
//
//    impl<
//        _0: Dispatch<Group = (_1, _2)>,
//        _1,
//        _2,
//    > Wrapper0<(_1, _2), _0, _1, _2>
//    for Wrapper<_0, (_1,), _2> {
//        fn kita(_a: (_1,), _b: _2) -> String {
//            "Generic Blanket A".to_owned()
//        }
//    }
//    impl<
//        _0: Dispatch<Group = [(_2, _1); 1]>,
//        _2: ToString,
//        _1,
//    > Wrapper0<[(_2, _1); 1], _0, _1, _2>
//    for Wrapper<_0, (_1,), _2> {
//        fn kita(_a: (_1,), b: _2) -> String {
//            b.to_string()
//        }
//    }
//
//    impl<
//        _0: Dispatch<Group = (_1,)>,
//        _1: A<A = [_2; 1]>,
//        _2: ToString,
//    > Wrapper1<(_1,), [_2; 1], _0, _1, _2>
//    for Wrapper<_0, u32, _1> {
//        fn kara(_a: (_1,), b: _2) -> String {
//            b.to_string()
//        }
//    }
//    impl<
//        _0: Dispatch<Group = (_1,)>,
//        _1: A<A = [_2; 2]>,
//        _2,
//    > Wrapper1<(_1,), [_2; 2], _0, _1, _2>
//    for Wrapper<_0, u32, _1> {
//        fn kara(_a: (_1,), _b: _2) -> String {
//            "Generic Blanket B".to_owned()
//        }
//    }
//
//    impl<_0, _1, _2> Wrapper<_0, (_1,), _2>
//    where
//        _0: Dispatch,
//        Self: Wrapper0<<_0 as Dispatch>::Group, _0, _1, _2>,
//    {
//        fn kita(_a: (_1,), _b: _2) -> String {
//            <Self as Wrapper0<
//                <_0 as Dispatch>::Group,
//                _0,
//                _1,
//                _2,
//            >>::kita(_a, _b)
//        }
//    }
//    impl<_0, _1, _2> Wrapper<_0, u32, _1>
//    where
//        _0: Dispatch,
//        _1: A,
//        Self: Wrapper1<
//            <_0 as Dispatch>::Group,
//            <_1 as A>::A,
//            _0,
//            _1,
//            _2,
//        >,
//        Self: Wrapper1Constraint<Bound = (_2,)>,
//    {
//        fn kara(_a: (_1,), b: _2) -> String {
//            <Self as Wrapper1<
//                <_0 as Dispatch>::Group,
//                <_1 as A>::A,
//                _0,
//                _1,
//                _2,
//            >>::kara(_a, b)
//        }
//    }
//
//    disjoint_impls::disjoint_impls! {
//        trait Wrapper1Constraint {
//            type Bound: ?Sized;
//        }
//
//        impl<
//            _0: Dispatch<Group = (_1,)>,
//            _1: A<A = [_2; 1]>,
//            _2: ToString,
//        > Wrapper1Constraint for Wrapper<_0, u32, _1> {
//            type Bound = (_2,);
//        }
//        impl<
//            _0: Dispatch<Group = (_1,)>,
//            _1: A<A = [_2; 2]>,
//            _2,
//        > Wrapper1Constraint for Wrapper<_0, u32, _1> {
//            type Bound = (_2,);
//        }
//    }
//};
//
//#[test]
//fn dispatch_inherent_on_generic() {
//    let a_arg1 = 42;
//    let a_arg2 = 420;
//
//    let b_arg2 = "moja".to_owned();
//    let b_arg3 = "kita".to_owned();
//
//    assert_eq!(
//        "Generic Blanket A",
//        Wrapper::<String, (u32,), u32>::kita((a_arg1,), a_arg2)
//    );
//    assert_eq!(
//        "Generic Blanket A",
//        Wrapper::<Vec<u32>, (u32,), String>::kita((a_arg1,), b_arg2)
//    );
//    assert_eq!(
//        "kita",
//        Wrapper::<u32, (u32,), String>::kita((a_arg1,), b_arg3)
//    );
//
//    assert_eq!(
//        "Generic Blanket B",
//        Wrapper::<u8, u32, i8>::kara((42_i8,), 42_i16)
//    );
//    assert_eq!("420", Wrapper::<i8, u32, u8>::kara((42_u8,), 420_u16));
//}
