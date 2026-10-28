use dispatch_macros::disjoint_impls;

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

disjoint_impls! {
    pub trait HasTelemetry {
        type Group;

        #[cfg(feature = "feat_a")]
        type Telemetry;

        #[cfg(feature = "feat_a")]
        const ENABLED: bool;

        #[cfg(feature = "feat_a")]
        fn telemetry(&self);
    }

    impl<T: Dispatch1<Group = GroupA>> HasTelemetry for T {
        type Group = GroupA;

        #[cfg(feature = "feat_a")]
        type Telemetry = ();

        #[cfg(feature = "feat_a")]
        const ENABLED: bool = true;

        #[cfg(feature = "feat_a")]
        fn telemetry(&self) {}
    }

    impl<T: Dispatch1<Group = GroupB>> HasTelemetry for T {
        type Group = GroupB;

        #[cfg(feature = "feat_a")]
        type Telemetry = ();

        #[cfg(feature = "feat_a")]
        const ENABLED: bool = true;

        #[cfg(feature = "feat_a")]
        fn telemetry(&self) {}
    }
}

disjoint_impls! {
    #[cfg_attr(feature = "feat_a", cfg(feature = "feat_a"))]
    pub trait CfgAttrDispatch {
        const NAME: &'static str;
    }

    impl<T: Dispatch1<Group = GroupA>> CfgAttrDispatch for T {
        const NAME: &'static str = "Group A";
    }

    #[cfg_attr(feature = "feat_a", cfg(any()))]
    impl<T: Dispatch1<Group = GroupB>> CfgAttrDispatch for T {
        const NAME: &'static str = "Group B";
    }
}

disjoint_impls! {
    pub trait CfgArgs {
        fn sum(
            &self,
            #[cfg(feature = "feat_a")] direct: u32,
            #[cfg_attr(not(feature = "feat_a"), cfg(any()))] via_attr: u32,
        ) -> u32;

        fn choose(
            &self,
            #[cfg(feature = "feat_a")] value: u32,
            #[cfg(not(feature = "feat_a"))] value: &'static str,
        ) -> u32;
    }

    impl<T: Dispatch1<Group = GroupA>> CfgArgs for T {
        fn sum(
            &self,
            #[cfg(feature = "feat_a")] direct: u32,
            #[cfg_attr(not(feature = "feat_a"), cfg(any()))] via_attr: u32,
        ) -> u32 {
            #[cfg(feature = "feat_a")]
            {
                direct + via_attr
            }
            #[cfg(not(feature = "feat_a"))]
            {
                100
            }
        }

        fn choose(
            &self,
            #[cfg(feature = "feat_a")] value: u32,
            #[cfg(not(feature = "feat_a"))] value: &'static str,
        ) -> u32 {
            #[cfg(feature = "feat_a")]
            {
                value
            }
            #[cfg(not(feature = "feat_a"))]
            {
                value.parse().unwrap()
            }
        }
    }

    impl<T: Dispatch1<Group = GroupB>> CfgArgs for T {
        fn sum(
            &self,
            #[cfg(feature = "feat_a")] direct: u32,
            #[cfg_attr(not(feature = "feat_a"), cfg(any()))] via_attr: u32,
        ) -> u32 {
            #[cfg(feature = "feat_a")]
            {
                direct + via_attr + 10
            }
            #[cfg(not(feature = "feat_a"))]
            {
                110
            }
        }

        fn choose(
            &self,
            #[cfg(feature = "feat_a")] value: u32,
            #[cfg(not(feature = "feat_a"))] value: &'static str,
        ) -> u32 {
            #[cfg(feature = "feat_a")]
            {
                value + 10
            }
            #[cfg(not(feature = "feat_a"))]
            {
                value.parse::<u32>().unwrap() + 10
            }
        }
    }
}

disjoint_impls! {
    pub trait CfgMethods {
        #[cfg(feature = "feat_a")]
        fn feature_value(&self) -> u32;

        fn fallback(&self) -> u32 {
            0
        }
    }

    impl<T: Dispatch1<Group = GroupA>> CfgMethods for T {
        #[cfg(feature = "feat_a")]
        fn feature_value(&self) -> u32 {
            1
        }

        #[cfg_attr(not(feature = "feat_a"), cfg(any()))]
        fn fallback(&self) -> u32 {
            11
        }
    }

    impl<T: Dispatch1<Group = GroupB>> CfgMethods for T {
        #[cfg(feature = "feat_a")]
        fn feature_value(&self) -> u32 {
            2
        }

        #[cfg_attr(not(feature = "feat_a"), cfg(any()))]
        fn fallback(&self) -> u32 {
            22
        }
    }
}

disjoint_impls! {
    pub trait AdditiveCfg { const GROUP: &'static str; }
    impl<T: Dispatch1<Group = GroupA>> AdditiveCfg for T {
        const GROUP: &'static str = "A";
    }
    #[cfg(feature = "feat_a")]
    impl<T: Dispatch1<Group = GroupB>> AdditiveCfg for T {
        const GROUP: &'static str = "B";
    }
}

#[cfg(feature = "feat_a")]
pub struct FeatureOnlyType;
#[cfg(feature = "feat_a")]
pub enum FeatureOnlyGroup {}
#[cfg(feature = "feat_a")]
impl Dispatch1 for FeatureOnlyType {
    type Group = FeatureOnlyGroup;
}

disjoint_impls! {
    pub trait SingletonGate { const VALUE: u8; }
    impl SingletonGate for u8 { const VALUE: u8 = 1; }
    #[cfg(feature = "feat_a")]
    impl SingletonGate for FeatureOnlyType { const VALUE: u8 = 2; }
}

disjoint_impls! {
    pub trait SharedGate { const VALUE: u8; }
    impl<T: Dispatch1<Group = GroupA>> SharedGate for T { const VALUE: u8 = 3; }
    #[cfg(feature = "feat_a")]
    impl<T: Dispatch1<Group = FeatureOnlyGroup>> SharedGate for T { const VALUE: u8 = 4; }
}

// All members of this group share a gate. Its entire generated scope must be
// gated, including references to a feature-only dispatch trait.
#[cfg(feature = "feat_a")]
pub trait FeatureDispatch {
    type Group;
}
#[cfg(feature = "feat_a")]
impl FeatureDispatch for FeatureOnlyType {
    type Group = GroupA;
}
disjoint_impls! {
    pub trait WholeGroupGate { const VALUE: u8; }
    #[cfg(feature = "feat_a")]
    impl<T: FeatureDispatch<Group = GroupA>> WholeGroupGate for T { const VALUE: u8 = 5; }
    #[cfg(feature = "feat_a")]
    impl<T: FeatureDispatch<Group = GroupB>> WholeGroupGate for T { const VALUE: u8 = 6; }
}

#[cfg(test)]
mod body;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn optimized_gates_keep_unconditional_impls() {
        assert_eq!(<u8 as SingletonGate>::VALUE, 1);
        assert_eq!(<String as SharedGate>::VALUE, 3);
    }

    #[test]
    #[cfg(feature = "feat_a")]
    fn optimized_gates_enable_feature_only_types() {
        assert_eq!(<FeatureOnlyType as SingletonGate>::VALUE, 2);
        assert_eq!(<FeatureOnlyType as SharedGate>::VALUE, 4);
        assert_eq!(<FeatureOnlyType as WholeGroupGate>::VALUE, 5);
    }

    #[test]
    fn additive_cfg_keeps_base_impl() {
        assert_eq!(<String as AdditiveCfg>::GROUP, "A");
    }

    #[test]
    #[cfg(feature = "feat_a")]
    fn additive_cfg_adds_feature_impl() {
        assert_eq!(<i32 as AdditiveCfg>::GROUP, "B");
    }

    #[test]
    #[cfg(not(feature = "feat_a"))]
    fn kita_without_feat_a() {
        assert_eq!("Blanket A without feat_a", <String as Kita>::NAME);
        assert_eq!("Blanket B without feat_a", <LocalType<i32> as Kita>::NAME);
    }

    #[test]
    #[cfg(feature = "feat_a")]
    fn kita_with_feat_a() {
        assert_eq!("Blanket A with feat_a", <String as Kita>::NAME);
    }

    #[test]
    fn cfg_trait_items_are_preserved() {
        fn assert_group_a<T: HasTelemetry<Group = GroupA>>() {}
        fn assert_group_b<T: HasTelemetry<Group = GroupB>>() {}
        assert_group_a::<String>();
        assert_group_b::<i32>();

        #[cfg(feature = "feat_a")]
        {
            fn assert_telemetry<T: HasTelemetry<Telemetry = ()>>() {}
            assert_telemetry::<String>();
            assert_telemetry::<i32>();
            assert!(<String as HasTelemetry>::ENABLED);
            assert!(<i32 as HasTelemetry>::ENABLED);
            String::new().telemetry();
            0i32.telemetry();
        }
    }

    #[test]
    fn cfg_attr_top_level_variants() {
        assert_eq!("Group A", <String as CfgAttrDispatch>::NAME);

        #[cfg(not(feature = "feat_a"))]
        assert_eq!("Group B", <i32 as CfgAttrDispatch>::NAME);
    }

    #[test]
    fn cfg_attributes_on_arguments_are_forwarded() {
        #[cfg(not(feature = "feat_a"))]
        {
            assert_eq!(100, String::new().sum());
            assert_eq!(110, 0i32.sum());
            assert_eq!(7, String::new().choose("7"));
            assert_eq!(17, 0i32.choose("7"));
        }

        #[cfg(feature = "feat_a")]
        {
            assert_eq!(5, String::new().sum(2, 3));
            assert_eq!(15, 0i32.sum(2, 3));
            assert_eq!(7, String::new().choose(7));
            assert_eq!(17, 0i32.choose(7));
        }
    }

    #[test]
    fn cfg_methods_are_pruned_before_validation() {
        #[cfg(not(feature = "feat_a"))]
        {
            assert_eq!(0, String::new().fallback());
            assert_eq!(0, 0i32.fallback());
        }

        #[cfg(feature = "feat_a")]
        {
            assert_eq!(1, String::new().feature_value());
            assert_eq!(2, 0i32.feature_value());
            assert_eq!(11, String::new().fallback());
            assert_eq!(22, 0i32.fallback());
        }
    }
}

#[cfg(test)]
mod single_condition;
