/// Trait that has blanket impls defined in another crate.
pub trait ForeignKita<U> {
    fn kita() -> &'static str {
        "Default blanket"
    }
}
