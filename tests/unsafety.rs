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
    /// # Safety
    ///
    /// This trait is unsafe
    pub unsafe trait Kita<U>: Dispatch {
        /// # Safety
        ///
        /// This function is unsafe
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
/// # Safety
///
/// This trait is unsafe
pub unsafe trait Kita<U>: Dispatch {
    /// # Safety
    ///
    /// This function is unsafe
    unsafe fn kita() -> &'static str {
        "Default blanket"
    }
}

const _: () = {
    /// # Safety
    ///
    /// This trait is unsafe
    pub unsafe trait Kita0<_TŠČ1: ?Sized, U>: Dispatch {
        /// # Safety
        ///
        /// This function is unsafe
        unsafe fn kita() -> &'static str {
            "Default blanket"
        }
    }

    unsafe impl<U, T: Dispatch<Group = GroupA>> Kita0<GroupA, U> for T {
        unsafe fn kita() -> &'static str {
            "Blanket A"
        }
    }

    unsafe impl<U, T: Dispatch<Group = GroupB>> Kita0<GroupB, U> for T {
        unsafe fn kita() -> &'static str {
            "Blanket B"
        }
    }

    unsafe impl<U, T> Kita<U> for T
    where
        Self: Dispatch,
        Self: Kita0<<T as Dispatch>::Group, U>,
        T: Dispatch,
    {
        unsafe fn kita() -> &'static str {
            unsafe { <Self as Kita0<<T as Dispatch>::Group, U>>::kita() }
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
