[![License: Apache 2.0](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://github.com/mversic/disjoint_impls/blob/master/LICENSE)
[![Crates.io](https://img.shields.io/crates/v/disjoint_impls.svg)](https://crates.io/crates/disjoint_impls)

Crate will be maintained (at least) until [this idiom](https://github.com/rust-lang/rust/issues/20400) is allowed by the Rust compiler directly

## Description

This library enables you to write certain types of disjoint impls that Rust compiler doesn't (yet?) allow.
Namely, disjoint impls where a type is bounded by an associated type. One would expect the following
syntax to compile without the need to invoke `disjoint_impls!`, but it doesn't:

```rs
use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group;
}

pub enum GroupA {}
impl Dispatch for String {
    type Group = GroupA;
}

pub enum GroupB {}
impl Dispatch for i32 {
    type Group = GroupB;
}

// Basic example
disjoint_impls! {
    pub trait BasicKita {
        const BASIC_NAME: &'static str;

        fn basic_name() -> &'static str {
            "Default blanket"
        }
    }

    impl<T: Dispatch<Group = GroupA>> BasicKita for T {
        const BASIC_NAME: &'static str = "Blanket A";
    }
    impl<U: Dispatch<Group = GroupB>> BasicKita for U {
        const BASIC_NAME: &'static str = "Blanket B";

        fn basic_name() -> &'static str {
            "Blanket B"
        }
    }
}

// Complex example
disjoint_impls! {
    pub trait ComplexKita {
        const COMPLEX_NAME: &'static str;
    }

    impl<T: Dispatch<Group = GroupA>, U: Dispatch<Group = GroupA>> ComplexKita for (T, U) {
        const COMPLEX_NAME: &'static str = "Blanket AA";
    }
    impl<U, T> ComplexKita for (U, T)
    where
        U: Dispatch<Group = GroupA>,
        T: Dispatch<Group = GroupB>
    {
        const COMPLEX_NAME: &'static str = "Blanket AB";
    }
    impl<T: Dispatch<Group = GroupB>, U: Dispatch> ComplexKita for (T, U) {
        const COMPLEX_NAME: &'static str = "Blanket B*";
    }

    impl<T: Dispatch<Group = GroupA>> ComplexKita for T {
        const COMPLEX_NAME: &'static str = "Blanket A";
    }
    impl<U: Dispatch<Group = GroupB>> ComplexKita for U {
        const COMPLEX_NAME: &'static str = "Blanket B";
    }
}

fn main() {
    assert_eq!("Blanket A", String::BASIC_NAME);
    assert_eq!("Blanket B", i32::BASIC_NAME);

    assert_eq!("Default blanket", String::basic_name());
    assert_eq!("Blanket B", i32::basic_name());

    assert_eq!("Blanket A", String::COMPLEX_NAME);
    assert_eq!("Blanket B", i32::COMPLEX_NAME);

    assert_eq!("Blanket AA", <(String, String)>::COMPLEX_NAME);
    assert_eq!("Blanket AB", <(String, i32)>::COMPLEX_NAME);
    assert_eq!("Blanket B*", <(i32, String)>::COMPLEX_NAME);
}
```

Other much more complex examples can be found in `tests`
