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

pub trait WithoutFeatA {}
impl WithoutFeatA for GroupA {}

disjoint_impls! {
    pub trait Kita {
        const NAME: &'static str;
    }

    impl<
        #[cfg(not(feature = "feat_a"))] T: Dispatch<Group: WithoutFeatA>,
        #[cfg(feature = "feat_a")] T: Dispatch<Group = GroupA>,
    > Kita for T {
        #[cfg(not(feature = "feat_a"))]
        const NAME: &'static str = "Blanket A without feat_a";

        #[cfg(feature = "feat_a")]
        const NAME: &'static str = "Blanket A with feat_a";
    }

    impl<T: Dispatch<Group = GroupB>> Kita for T {
        const NAME: &'static str = "Blanket B";
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[cfg(not(feature = "feat_a"))]
    fn kita_without_feat_a() {
        assert_eq!("Blanket A without feat_a", String::NAME);
        assert_eq!("Blanket B", i32::NAME);
    }

    #[test]
    #[cfg(feature = "feat_a")]
    fn kita_with_feat_a() {
        assert_eq!("Blanket A with feat_a", String::NAME);
        assert_eq!("Blanket B", i32::NAME);
    }
}
