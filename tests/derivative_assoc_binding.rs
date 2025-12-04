use disjoint_impls::disjoint_impls;

struct LocalType<T>(T);

struct GroupA<T>(T);
struct GroupB<T>(T);

impl Kita for u32 {
    type Group = GroupA<i32>;

    const NAME: &'static str = "u32 impl";
}

impl Kita for i32 {
    type Group = GroupB<u32>;
    const NAME: &'static str = "i32 impl";
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

#[test]
fn derivative_assoc_binding() {
    assert_eq!("u32 impl", <u32 as Kita>::NAME);
    assert_eq!("i32 impl", <i32 as Kita>::NAME);

    assert_eq!("Blanket A", <LocalType<u32> as Kita>::NAME);
    assert_eq!("Blanket B", <LocalType<i32> as Kita>::NAME);
}
