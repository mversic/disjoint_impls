use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group;
}

pub enum GroupA {}
impl Dispatch for String {
    type Group = GroupA;
}
impl<T> Dispatch for Vec<T> {
    type Group = GroupA;
}

pub enum GroupB {}
impl Dispatch for i32 {
    type Group = GroupB;
}
impl Dispatch for u32 {
    type Group = GroupB;
}

disjoint_impls! {
    pub unsafe trait Kita<U>: Dispatch {
        unsafe fn kita() -> &'static str {
            "Default blanket"
        }
    }

    unsafe impl<U, T: Dispatch<Group = GroupA>> Kita<U> for T {
        unsafe fn kita() -> &'static str {
            "Blanket A"
        }
    }
    unsafe impl<U, T: Dispatch<Group = GroupB>> Kita<U> for T {
        unsafe fn kita() -> &'static str {
            "Blanket B"
        }
    }
}

/*
pub unsafe trait Kita<U>: Dispatch {
    unsafe fn kita() -> &'static str {
        "Default blanket"
    }
}

const _: () = {
    pub unsafe trait Kita0<_1: ?Sized, U>: Dispatch {
        unsafe fn kita() -> &'static str {
            "Default blanket"
        }
    }
    unsafe impl<_0, _1: Dispatch<Group = GroupA>> Kita0<GroupA, _0> for _1 {
        unsafe fn kita() -> &'static str {
            "Blanket A"
        }
    }
    unsafe impl<_0, _1: Dispatch<Group = GroupB>> Kita0<GroupB, _0> for _1 {
        unsafe fn kita() -> &'static str {
            "Blanket B"
        }
    }

    unsafe impl<_0, _1> Kita<_0> for _1
    where
        _1: Dispatch,
        Self: Kita0<<_1 as Dispatch>::Group, _0>,
    {
        unsafe fn kita() -> &'static str {
            unsafe { <Self as Kita0<<_1 as Dispatch>::Group, _0>>::kita() }
        }
    }
};
*/

#[test]
fn unsafety() {
    unsafe {
        assert_eq!("Blanket A", <String as Kita<u8>>::kita());
        assert_eq!("Blanket A", <Vec::<u32> as Kita<u16>>::kita());
        assert_eq!("Blanket B", <u32 as Kita<u32>>::kita());
        assert_eq!("Blanket B", <i32 as Kita<u64>>::kita());
    }
}
