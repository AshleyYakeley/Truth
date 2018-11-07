module Language.Expression.Dolan.MPolarity where

import Language.Expression.Dolan.Polarity
import Language.Expression.Dolan.Range
import Shapes

-- The only purpose of this module is for parsing types simultaneously as positive and negative.
-- For example, "Ref T" ==> "Ref {-T,+T}""
data MPolarity (mpolarity :: Maybe TypePolarity) where
    PositiveMPolarity :: MPolarity ('Just 'PositivePolarity)
    NegativeMPolarity :: MPolarity ('Just 'NegativePolarity)
    BothMPolarity :: MPolarity 'Nothing

instance Is MPolarity ('Just 'PositivePolarity) where
    representative = PositiveMPolarity

instance Is MPolarity ('Just 'NegativePolarity) where
    representative = NegativeMPolarity

instance Is MPolarity 'Nothing where
    representative = BothMPolarity

instance Representative MPolarity where
    getRepWitness PositiveMPolarity = Dict
    getRepWitness NegativeMPolarity = Dict
    getRepWitness BothMPolarity = Dict

type family InvertMPolarity (mpolarity :: Maybe TypePolarity) :: Maybe TypePolarity where
    InvertMPolarity ('Just polarity) = 'Just (InvertPolarity polarity)
    InvertMPolarity 'Nothing = 'Nothing

isMPolarity ::
       forall polarity r. IsTypePolarity polarity
    => (Is MPolarity ('Just polarity) => r)
    -> r
isMPolarity v =
    case whichTypePolarity @polarity of
        Left Refl -> v
        Right Refl -> v

invertMPolarity ::
       forall mpolarity r. Is MPolarity mpolarity
    => (Is MPolarity (InvertMPolarity mpolarity) => r)
    -> r
invertMPolarity v =
    case representative @_ @MPolarity @mpolarity of
        PositiveMPolarity -> v
        NegativeMPolarity -> v
        BothMPolarity -> v

data MPolarType w mpolarity where
    SingleMPolarType :: IsTypePolarity polarity => AnyW (w polarity) -> MPolarType w ('Just polarity)
    BothMPolarType :: (forall polarity. IsTypePolarity polarity => AnyW (w polarity)) -> MPolarType w 'Nothing

type family ConvertMPolarity t :: Maybe TypePolarity

class ToMPolar t where
    type ToMPolarConvert t (polarity :: TypePolarity) = (mpc :: Type) | mpc -> polarity
    toMPolarSingle ::
           forall polarity. (IsTypePolarity polarity, ConvertMPolarity t ~ 'Just polarity)
        => ToMPolarConvert t polarity
        -> t
    toMPolarBoth ::
           ConvertMPolarity t ~ 'Nothing
        => (forall polarity. IsTypePolarity polarity => ToMPolarConvert t polarity)
        -> t

toMPolar ::
       forall t. (ToMPolar t, Is MPolarity (ConvertMPolarity t))
    => (forall polarity. IsTypePolarity polarity => ToMPolarConvert t polarity)
    -> t
toMPolar ct =
    case representative @_ @MPolarity @(ConvertMPolarity t) of
        PositiveMPolarity -> toMPolarSingle ct
        NegativeMPolarity -> toMPolarSingle ct
        BothMPolarity -> toMPolarBoth ct

class FromMPolar t where
    type FromMPolarConvert t (polarity :: TypePolarity) :: Type
    fromMPolarSingle ::
           forall polarity. (IsTypePolarity polarity, ConvertMPolarity t ~ 'Just polarity)
        => t
        -> FromMPolarConvert t polarity
    fromMPolarBoth ::
           forall polarity. (IsTypePolarity polarity, ConvertMPolarity t ~ 'Nothing)
        => t
        -> FromMPolarConvert t polarity

type instance ConvertMPolarity (MPolarType w mpolarity) = mpolarity

instance ToMPolar (MPolarType w mpolarity) where
    type ToMPolarConvert (MPolarType w mpolarity) polarity = AnyW (w polarity)
    toMPolarSingle = SingleMPolarType
    toMPolarBoth = BothMPolarType

instance FromMPolar (MPolarType w mpolarity) where
    type FromMPolarConvert (MPolarType w mpolarity) polarity = AnyW (w polarity)
    fromMPolarSingle (SingleMPolarType aw) = aw
    fromMPolarBoth (BothMPolarType aw) = aw

data MPolarRangeType w mpolarity where
    SingleMPolarRangeType
        :: IsTypePolarity polarity => AnyInKind (RangeType w polarity) -> MPolarRangeType w ('Just polarity)
    BothMPolarRangeType
        :: (forall polarity. IsTypePolarity polarity => AnyInKind (RangeType w polarity)) -> MPolarRangeType w 'Nothing

instance ( Is MPolarity mpolarity
         , Semigroup (AnyInKind (RangeType w 'NegativePolarity))
         , Semigroup (AnyInKind (RangeType w 'PositivePolarity))
         ) => Semigroup (MPolarRangeType w mpolarity) where
    (<>) =
        case representative @_ @MPolarity @mpolarity of
            PositiveMPolarity -> \(SingleMPolarRangeType a) (SingleMPolarRangeType b) -> SingleMPolarRangeType $ a <> b
            NegativeMPolarity -> \(SingleMPolarRangeType a) (SingleMPolarRangeType b) -> SingleMPolarRangeType $ a <> b
            BothMPolarity ->
                \(BothMPolarRangeType a) (BothMPolarRangeType b) ->
                    BothMPolarRangeType $ let
                        x :: forall polarity. IsTypePolarity polarity
                          => AnyInKind (RangeType w polarity)
                        x =
                            case whichTypePolarity @polarity of
                                Left Refl -> a @polarity <> b @polarity
                                Right Refl -> a @polarity <> b @polarity
                        in x

instance ( Is MPolarity mpolarity
         , Monoid (AnyInKind (RangeType w 'NegativePolarity))
         , Monoid (AnyInKind (RangeType w 'PositivePolarity))
         ) => Monoid (MPolarRangeType w mpolarity) where
    mappend = (<>)
    mempty =
        case representative @_ @MPolarity @mpolarity of
            PositiveMPolarity -> SingleMPolarRangeType mempty
            NegativeMPolarity -> SingleMPolarRangeType mempty
            BothMPolarity ->
                BothMPolarRangeType $ let
                    x :: forall polarity. IsTypePolarity polarity
                      => AnyInKind (RangeType w polarity)
                    x =
                        case whichTypePolarity @polarity of
                            Left Refl -> mempty
                            Right Refl -> mempty
                    in x

type instance ConvertMPolarity (MPolarRangeType w mpolarity) =
     mpolarity

instance ToMPolar (MPolarRangeType w mpolarity) where
    type ToMPolarConvert (MPolarRangeType w mpolarity) polarity = AnyInKind (RangeType w polarity)
    toMPolarSingle = SingleMPolarRangeType
    toMPolarBoth = BothMPolarRangeType

instance FromMPolar (MPolarRangeType w mpolarity) where
    type FromMPolarConvert (MPolarRangeType w mpolarity) polarity = AnyInKind (RangeType w polarity)
    fromMPolarSingle (SingleMPolarRangeType aw) = aw
    fromMPolarBoth (BothMPolarRangeType aw) = aw

type instance ConvertMPolarity (arg -> t) = ConvertMPolarity t

instance (FromMPolar arg, ToMPolar t, ConvertMPolarity arg ~ ConvertMPolarity t) => ToMPolar (arg -> t) where
    type ToMPolarConvert (arg -> t) polarity = FromMPolarConvert arg polarity -> ToMPolarConvert t polarity
    toMPolarSingle f arg = toMPolarSingle $ f $ fromMPolarSingle arg
    toMPolarBoth f arg = let
        ff :: forall polarity. IsTypePolarity polarity
           => ToMPolarConvert t polarity
        ff = f @polarity $ fromMPolarBoth @_ @polarity arg
        in toMPolarBoth ff

newtype InvertMPolarType w mpolarity = MkInvertMPolarType
    { unInvertMPolarType :: MPolarType w (InvertMPolarity mpolarity)
    }

type instance ConvertMPolarity (InvertMPolarType w mpolarity) =
     mpolarity

instance FromMPolar (InvertMPolarType w mpolarity) where
    type FromMPolarConvert (InvertMPolarType w mpolarity) polarity = AnyW (w (InvertPolarity polarity))
    fromMPolarSingle (MkInvertMPolarType (SingleMPolarType aw)) = aw
    fromMPolarBoth ::
           forall polarity. (IsTypePolarity polarity, mpolarity ~ 'Nothing)
        => InvertMPolarType w mpolarity
        -> AnyW (w (InvertPolarity polarity))
    fromMPolarBoth (MkInvertMPolarType (BothMPolarType aw)) = invertPolarity @polarity aw
