module Data.Shim.Range.RangeType where

import Data.Shim.Polar
import Shapes

data RangeType (tw :: Polarity -> k -> Type) (polarity :: Polarity) (pq :: (k, k)) where
    MkRangeType :: tw (InvertPolarity polarity) p -> tw polarity q -> RangeType tw polarity '( p, q)

instance (TestEquality (tw polarity), TestEquality (tw (InvertPolarity polarity))) =>
             TestEquality (RangeType tw polarity) where
    testEquality (MkRangeType pa qa) (MkRangeType pb qb) = do
        Refl <- testEquality pa pb
        Refl <- testEquality qa qb
        return Refl

data MPolarRangeType w mpolarity where
    SingleMPolarRangeType
        :: Is PolarityType polarity => Some (RangeType w polarity) -> MPolarRangeType w ('Just polarity)
    BothMPolarRangeType
        :: (forall polarity. Is PolarityType polarity => Some (RangeType w polarity)) -> MPolarRangeType w 'Nothing

instance ( Is MPolarityType mpolarity
         , Semigroup (Some (RangeType w 'Negative))
         , Semigroup (Some (RangeType w 'Positive))
         ) => Semigroup (MPolarRangeType w mpolarity) where
    (<>) =
        case representative @_ @MPolarityType @mpolarity of
            MPositiveType -> \(SingleMPolarRangeType a) (SingleMPolarRangeType b) -> SingleMPolarRangeType $ a <> b
            MNegativeType -> \(SingleMPolarRangeType a) (SingleMPolarRangeType b) -> SingleMPolarRangeType $ a <> b
            MBothType ->
                \(BothMPolarRangeType a) (BothMPolarRangeType b) ->
                    BothMPolarRangeType $ let
                        x :: forall polarity. Is PolarityType polarity
                          => Some (RangeType w polarity)
                        x =
                            case polarityType @polarity of
                                PositiveType -> a @polarity <> b @polarity
                                NegativeType -> a @polarity <> b @polarity
                        in x

instance (Is MPolarityType mpolarity, Monoid (Some (RangeType w 'Negative)), Monoid (Some (RangeType w 'Positive))) =>
             Monoid (MPolarRangeType w mpolarity) where
    mappend = (<>)
    mempty =
        case representative @_ @MPolarityType @mpolarity of
            MPositiveType -> SingleMPolarRangeType mempty
            MNegativeType -> SingleMPolarRangeType mempty
            MBothType ->
                BothMPolarRangeType $ let
                    x :: forall polarity. Is PolarityType polarity
                      => Some (RangeType w polarity)
                    x =
                        case polarityType @polarity of
                            PositiveType -> mempty
                            NegativeType -> mempty
                    in x

type instance ConvertMPolarity (MPolarRangeType w mpolarity) = mpolarity

instance ToMPolar (MPolarRangeType w mpolarity) where
    type ToMPolarConvert (MPolarRangeType w mpolarity) polarity = Some (RangeType w polarity)
    toMPolarSingle = SingleMPolarRangeType
    toMPolarBoth = BothMPolarRangeType

instance FromMPolar (MPolarRangeType w mpolarity) where
    type FromMPolarConvert (MPolarRangeType w mpolarity) polarity = Some (RangeType w polarity)
    fromMPolarSingle (SingleMPolarRangeType aw) = aw
    fromMPolarBoth (BothMPolarRangeType aw) = aw
