//use disjoint_impls::disjoint_impls;
//
//disjoint_impls! {
//    pub trait ExternC {
//        type CType: ReprC + ?Sized;
//    }
//
//    impl<R: ReprFamily + SizeFamily<Kind: Thin> + ExternC + MutabilityFamily<Kind = Exclusive> + ?Sized> ExternC for &R
//    where
//        Self: ReprFamily<Kind: UnstableOrNonRobust>,
//    {
//        type CType = *const R::CType;
//    }
//    impl<R: ReprFamily + SizeFamily<Kind: Thin> + ExternC + MutabilityFamily<Kind = Interior> + ?Sized> ExternC for &R
//    where
//        Self: ReprFamily<Kind: UnstableOrNonRobust>,
//    {
//        type CType = *mut R::CType;
//    }
//    impl<R: ReprFamily<Kind = Stable<K>> + SizeFamily<Kind = MetaSized<SliceLike>> + MutabilityFamily<Kind = Exclusive> + ?Sized, K> ExternC for &R
//    where
//        Self: ReprFamily<Kind = Unstable>,
//        R: Wide<Data: ExternC, Metadata = usize>,
//        <<R as Wide>::Data as ExternC>::CType: Sized,
//    {
//        type CType = CSlice<<R::Data as ExternC>::CType>;
//    }
//    impl<R: ReprFamily<Kind = Stable<K>> + SizeFamily<Kind = MetaSized<SliceLike>> + MutabilityFamily<Kind = Interior> + ?Sized, K> ExternC for &R
//    where
//        Self: ReprFamily<Kind = Unstable>,
//        R: Wide<Data: ExternC, Metadata = usize>,
//        <<R as Wide>::Data as ExternC>::CType: Sized,
//    {
//        type CType = CSliceMut<<R::Data as ExternC>::CType>;
//    }
//    #[cfg(feature = "alloc")]
//    impl<R: ReprFamily<Kind = Unstable> + SizeFamily<Kind = MetaSized<K>> + ?Sized, K> ExternC for &R
//    where
//        Self: ReprFamily<Kind = Unstable>,
//        R: ToOwned<Owned: ExternC<CType: BorrowCast>>,
//    {
//        type CType = <<R::Owned as ExternC>::CType as BorrowCast>::AsConst;
//    }
//}

fn main() {}
