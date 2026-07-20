# Disjoint Impls

[<img alt="crates.io" src="https://img.shields.io/crates/v/disjoint_impls.svg?style=for-the-badge&color=fc8d62&logo=rust" height="20">](https://crates.io/crates/disjoint_impls)
[<img alt="docs.rs" src="https://img.shields.io/badge/docs.rs-disjoint_impls-66c2a5?style=for-the-badge&labelColor=555555&logo=docs.rs" height="20">](https://docs.rs/disjoint_impls)
[<img alt="CI" src="https://img.shields.io/github/actions/workflow/status/mversic/disjoint_impls/main.yaml?style=for-the-badge&label=CI" height="20">](https://github.com/mversic/disjoint_impls/actions/workflows/main.yaml)

Crate will be maintained (at least) until [this idiom](https://github.com/rust-lang/rust/issues/20400) is allowed by the Rust compiler directly

## Description

Enables writing non-overlapping (*disjoint*) impls distinguished by a set of associated types.

Works for trait and inherent implementations alike (no special syntax).

### Trait implementations

```rs
use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group;
}

disjoint_impls! {
    pub trait Kita {}

    impl<T: Dispatch<Group = u32>> Kita for T {}
    impl<T: Dispatch<Group = i32>> Kita for T {}
}
```

### Inherent implementations

```rs
use disjoint_impls::disjoint_impls;

pub trait Dispatch {
    type Group;
}

struct Wrapper<T>(T);

disjoint_impls! {
    impl<T: Dispatch<Group = u32>> Wrapper<T> {}
    impl<T: Dispatch<Group = i32>> Wrapper<T> {}
}
```

### Foreign(remote) traits

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

// (orphan rule): You can define blanket impls only
// for types that are defined in the current crate
pub struct LocalType<T>(T);

disjoint_impls! {
    #[disjoint_impls(remote)]
    pub trait ForeignKita {
        fn kita() {
            unreachable!("method is accessed through the original trait");
        }
    }

    impl<T: Dispatch<Group = u32>> ForeignKita for LocalType<T> {}
    impl<T: Dispatch<Group = i32>> ForeignKita for LocalType<T> {}
}
```

Other, much more complex examples, can be found in tests.
