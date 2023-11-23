[![License: Apache 2.0](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://github.com/mversic/disjoint_impls/blob/master/LICENSE)
[![Crates.io](https://img.shields.io/crates/v/disjoint_impls.svg)](https://crates.io/crates/disjoint_impls)

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

disjoint_impls! {
    pub trait Kita {
        const NAME: &'static str;

        fn name() -> &'static str {
            "Default blanket"
        }
    }

    impl<T: Dispatch<Group = GroupA>> Kita for T {
        const NAME: &'static str = "Blanket A";
    }
    impl<U: Dispatch<Group = GroupB>> Kita for U {
        const NAME: &'static str = "Blanket B";

        fn name() -> &'static str {
            "Blanket B"
        }
    }
}

fn main() {
    assert_eq!("Blanket A", String::NAME);
    assert_eq!("Blanket B", i32::NAME);

    assert_eq!("Default blanket", String::name());
    assert_eq!("Blanket B", i32::name());
}
```

Other much more complex examples can be found in `tests`
