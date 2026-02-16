use disjoint_impls::disjoint_impls;

#[cfg(not(feature = "feat_a"))]
pub struct LocalType<T>(T);

pub trait Dispatch1 {
    type Group;
}
pub trait Dispatch2 {
    type Group;
}

pub enum GroupA {}
pub enum GroupB {}

impl Dispatch1 for String {
    type Group = GroupA;
}

impl Dispatch2 for String {
    type Group = GroupA;
}
impl Dispatch1 for i32 {
    type Group = GroupB;
}
impl Dispatch2 for i32 {
    type Group = GroupB;
}

pub trait WithoutFeatA {
    type Group;
}
impl WithoutFeatA for GroupA {
    type Group = GroupA;
}

disjoint_impls! {
    pub trait Kita {
        const NAME: &'static str;
    }

    impl<
        #[cfg(not(feature = "feat_a"))] T: Dispatch1,
        #[cfg(feature = "feat_a")] T: Dispatch2,
    > Kita for T
    where
        T: Dispatch1<Group: WithoutFeatA>,
        T: Dispatch2<Group = GroupA>,
    {
        #[cfg(not(feature = "feat_a"))]
        const NAME: &'static str = "Blanket A without feat_a";

        #[cfg(feature = "feat_a")]
        const NAME: &'static str = "Blanket A with feat_a";
    }

    #[cfg(not(feature = "feat_a"))]
    impl<T: Dispatch1<Group = GroupB>> Kita for LocalType<T> {
        const NAME: &'static str = "Blanket B without feat_a";
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[cfg(not(feature = "feat_a"))]
    fn kita_without_feat_a() {
        assert_eq!("Blanket A without feat_a", String::NAME);
        assert_eq!("Blanket B without feat_a", LocalType::<i32>::NAME);
    }

    #[test]
    #[cfg(feature = "feat_a")]
    fn kita_with_feat_a() {
        assert_eq!("Blanket A with feat_a", String::NAME);
    }
}
