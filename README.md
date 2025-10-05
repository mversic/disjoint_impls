[![License: Apache 2.0](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://github.com/mversic/disjoint_impls/blob/master/LICENSE)
[![Crates.io](https://img.shields.io/crates/v/disjoint_impls.svg)](https://crates.io/crates/disjoint_impls)

Crate will be maintained (at least) until [this idiom](https://github.com/rust-lang/rust/issues/20400) is allowed by the Rust compiler directly

## Description

Enables writing non-overlapping (*disjoint*) impls distinguished by a set of associated types.

```rs
use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group;
}

pub enum GroupA {}
pub enum GroupB {}

impl Dispatch for String {
    type Group = GroupA;
}
impl Dispatch for i32 {
    type Group = GroupB;
}

impl Dispatch for Option<String> {
    type Group = GroupA;
}
impl Dispatch for Option<i32> {
    type Group = GroupB;
}

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

    impl<T> ComplexKita for T where Option<T>: Dispatch<Group = GroupA> {
        const COMPLEX_NAME: &'static str = "Blanket A";
    }
    impl<U> ComplexKita for U where Option<U>: Dispatch<Group = GroupB> {
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

Other, much more complex examples can be found in tests.

## Foreign(remote) traits

For traits defined outside the current crate (a.k.a. foreign or remote traits), duplicate
the trait definition inside the macro and annotate it with `#[disjoint_impls(remote)]`.

```rs
use disjoint_impls::disjoint_impls;
// A foreign trait must be brought into scope so
// the `disjoint_impls!` macro can refer to it.
use remote_trait::ForeignKita;

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

// (orphan rule): You can define blanket impls only
// for types that are defined in the current crate
pub struct LocalType<T>(T);

disjoint_impls! {
    // Trait annotated with `#[disjoint_impls(remote)]` must be an exact duplicate
    // of the foreign/remote trait it refers to (values and function bodies excluded)
    #[disjoint_impls(remote)]
    pub trait ForeignKita<U> {
        fn kita() -> &'static str;
    }

    impl<T: Dispatch<Group = GroupA>, U> ForeignKita<U> for LocalType<T> {
        fn kita() -> &'static str {
            "Blanket A"
        }
    }
    impl<T: Dispatch<Group = GroupB>, U> ForeignKita<U> for LocalType<T> {
        fn kita() -> &'static str {
            "Blanket B"
        }
    }
}
```
