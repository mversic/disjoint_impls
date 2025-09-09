use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group;
}

pub enum GroupA {}
impl Dispatch for Counted<String> {
    type Group = GroupA;
}
impl<T> Dispatch for Counted<Vec<T>> {
    type Group = GroupA;
}

pub enum GroupB {}
impl Dispatch for i32 {
    type Group = GroupB;
}
impl Dispatch for u32 {
    type Group = GroupB;
}

trait Counter {
    fn decrement(&mut self) -> u32;
}

struct Counted<T>(u32, T);
impl<T> Counter for Counted<T> {
    fn decrement(&mut self) -> u32 {
        self.0 = self.0.saturating_sub(1);
        self.0
    }
}

disjoint_impls! {
    pub trait Kita {
        type Item;

        fn kita(&mut self) -> Self::Item;
    }

    impl<T: Dispatch<Group = GroupA> + Counter> Kita for T {
        type Item = u32;

        fn kita(&mut self) -> u32 {
            let _value = self.decrement();
            let _value = Self::decrement(self);
            let value = T::decrement(self);

            if value == 0 {
                return 0;
            }

            // FIXME: Support this!
            //let _value = self.kita();
            //let _value = Self::kita(self);
            //let value = T::kita(self);
            let _value = <Self as Kita>::kita(self);
            let value = <T as Kita>::kita(self);

            value + 1
        }
    }
    impl<U: Dispatch<Group = GroupB> + Default> Kita for U {
        type Item = U;

        fn kita(&mut self) -> Self::Item {
            <U as Default>::default()
        }
    }

    impl<T: Dispatch<Group = GroupA>> Kita for (T,) {
        type Item = u32;

        fn kita(&mut self) -> u32 {
            let a: Self::Item = <u32 as Kita>::kita(&mut 33);
            a
        }
    }
    impl<U: Dispatch<Group = GroupB> + Default> Kita for (U,) {
        type Item = U;

        fn kita(&mut self) -> Self::Item {
            <U as Default>::default()
        }
    }
}

#[test]
fn recursive_call() {
    assert_eq!(0, Counted::<String>::kita(&mut Counted(2, "a".to_string())));
    assert_eq!(1, Counted::<Vec::<u32>>::kita(&mut Counted(12, vec![])));
    assert_eq!(0, u32::kita(&mut 12));
    assert_eq!(0, i32::kita(&mut 42));
}
