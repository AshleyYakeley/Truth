{-# OPTIONS -Wno-redundant-constraints #-}

module Data.Shim.Polar.MPolarity where

import Shapes

import Data.Shim.Polar.Polarity

-- The only purpose of this module is for parsing types simultaneously as positive and negative.
-- For example, "WholeModel T" ==> "WholeModel {-T,+T}""
data MPolarityType (mpolarity :: Maybe Polarity) where
    MPositiveType :: MPolarityType ('Just 'Positive)
    MNegativeType :: MPolarityType ('Just 'Negative)
    MBothType :: MPolarityType 'Nothing

instance Is MPolarityType ('Just 'Positive) where
    representative = MPositiveType

instance Is MPolarityType ('Just 'Negative) where
    representative = MNegativeType

instance Is MPolarityType 'Nothing where
    representative = MBothType

instance Representative MPolarityType where
    getRepWitness MPositiveType = Dict
    getRepWitness MNegativeType = Dict
    getRepWitness MBothType = Dict

type family InvertMPolarity (mpolarity :: Maybe Polarity) :: Maybe Polarity where
    InvertMPolarity ('Just polarity) = 'Just (InvertPolarity polarity)
    InvertMPolarity 'Nothing = 'Nothing

isMPolarity ::
    forall polarity r.
    Is PolarityType polarity =>
    (Is MPolarityType ('Just polarity) => r) ->
    r
isMPolarity v =
    case polarityType @polarity of
        PositiveType -> v
        NegativeType -> v

invertMPolarity ::
    forall mpolarity r.
    Is MPolarityType mpolarity =>
    (Is MPolarityType (InvertMPolarity mpolarity) => r) ->
    r
invertMPolarity v =
    case representative @_ @MPolarityType @mpolarity of
        MPositiveType -> v
        MNegativeType -> v
        MBothType -> v

data MPolarW (w :: Polarity -> k -> Type) (mpolarity :: Maybe Polarity) where
    SingleMPolarW ::
        forall k (w :: Polarity -> k -> Type) (polarity :: Polarity).
        Is PolarityType polarity =>
        Some (w polarity) ->
        MPolarW w ('Just polarity)
    BothMPolarW ::
        forall k (w :: Polarity -> k -> Type).
        (forall polarity. Is PolarityType polarity => Some (w polarity)) ->
        MPolarW w 'Nothing

mapMPolarW ::
    forall k1 k2 (w1 :: Polarity -> k1 -> Type) (w2 :: Polarity -> k2 -> Type) (mpolarity :: Maybe Polarity).
    (forall polarity. Is PolarityType polarity => Some (w1 polarity) -> Some (w2 polarity)) ->
    MPolarW w1 mpolarity ->
    MPolarW w2 mpolarity
mapMPolarW f (SingleMPolarW awpt) = SingleMPolarW $ f awpt
mapMPolarW f (BothMPolarW awpt) = BothMPolarW $ f awpt

mapMPolarWM ::
    forall m k1 k2 (w1 :: Polarity -> k1 -> Type) (w2 :: Polarity -> k2 -> Type) (mpolarity :: Maybe Polarity).
    Monad m =>
    (forall polarity. Is PolarityType polarity => Some (w1 polarity) -> m (Some (w2 polarity))) ->
    MPolarW w1 mpolarity ->
    m (MPolarW w2 mpolarity)
mapMPolarWM mf (SingleMPolarW awpt) = do
    awpt' <- mf awpt
    return $ SingleMPolarW awpt'
mapMPolarWM mf (BothMPolarW awpt) = do
    awptPos <- mf awpt
    awprNeg <- mf awpt
    return $ bothMPolarW awptPos awprNeg

toMPolarWM ::
    forall m k (w :: Polarity -> k -> Type) (mpolarity :: Maybe Polarity).
    (Is MPolarityType mpolarity, Monad m) =>
    (forall polarity. Is PolarityType polarity => m (Some (w polarity))) ->
    m (MPolarW w mpolarity)
toMPolarWM mpt =
    case representative @_ @MPolarityType @mpolarity of
        MPositiveType -> do
            pt <- mpt
            return $ SingleMPolarW pt
        MNegativeType -> do
            pt <- mpt
            return $ SingleMPolarW pt
        MBothType -> do
            ptpos <- mpt
            ptneg <- mpt
            return $ bothMPolarW ptpos ptneg

forMPolarW ::
    forall m k1 k2 (w1 :: Polarity -> k1 -> Type) (w2 :: Polarity -> k2 -> Type) (mpolarity :: Maybe Polarity).
    Monad m =>
    MPolarW w1 mpolarity ->
    (forall polarity. Is PolarityType polarity => Some (w1 polarity) -> m (Some (w2 polarity))) ->
    m (MPolarW w2 mpolarity)
forMPolarW mpw mf = mapMPolarWM mf mpw

bothMPolarW :: forall w. Some (w 'Positive) -> Some (w 'Negative) -> MPolarW w 'Nothing
bothMPolarW posw negw = let
    bothw ::
        forall polarity.
        Is PolarityType polarity =>
        Some (w polarity)
    bothw =
        case polarityType @polarity of
            PositiveType -> posw
            NegativeType -> negw
    in BothMPolarW bothw

type family ConvertMPolarity t :: Maybe Polarity

class ToMPolar t where
    type ToMPolarConvert t (polarity :: Polarity) = (mpc :: Type) | mpc -> polarity
    toMPolarSingle ::
        forall polarity.
        (Is PolarityType polarity, ConvertMPolarity t ~ 'Just polarity) =>
        ToMPolarConvert t polarity ->
        t
    toMPolarBoth ::
        ConvertMPolarity t ~ 'Nothing =>
        (forall polarity. Is PolarityType polarity => ToMPolarConvert t polarity) ->
        t

toMPolar ::
    forall t.
    (ToMPolar t, Is MPolarityType (ConvertMPolarity t)) =>
    (forall polarity. Is PolarityType polarity => ToMPolarConvert t polarity) ->
    t
toMPolar ct =
    case representative @_ @MPolarityType @(ConvertMPolarity t) of
        MPositiveType -> toMPolarSingle ct
        MNegativeType -> toMPolarSingle ct
        MBothType -> toMPolarBoth ct

class FromMPolar t where
    type FromMPolarConvert t (polarity :: Polarity) :: Type
    fromMPolarSingle ::
        forall polarity.
        (Is PolarityType polarity, ConvertMPolarity t ~ 'Just polarity) =>
        t ->
        FromMPolarConvert t polarity
    fromMPolarBoth ::
        forall polarity.
        (Is PolarityType polarity, ConvertMPolarity t ~ 'Nothing) =>
        t ->
        FromMPolarConvert t polarity

type instance ConvertMPolarity (MPolarW _ mpolarity) = mpolarity

instance ToMPolar (MPolarW w mpolarity) where
    type ToMPolarConvert (MPolarW w mpolarity) polarity = Some (w polarity)
    toMPolarSingle = SingleMPolarW
    toMPolarBoth = BothMPolarW

instance FromMPolar (MPolarW w mpolarity) where
    type FromMPolarConvert (MPolarW w mpolarity) polarity = Some (w polarity)
    fromMPolarSingle (SingleMPolarW aw) = aw
    fromMPolarBoth (BothMPolarW aw) = aw

type instance ConvertMPolarity (_ -> t) = ConvertMPolarity t

instance (FromMPolar arg, ToMPolar t, ConvertMPolarity arg ~ ConvertMPolarity t) => ToMPolar (arg -> t) where
    type ToMPolarConvert (arg -> t) polarity = FromMPolarConvert arg polarity -> ToMPolarConvert t polarity
    toMPolarSingle f arg = toMPolarSingle $ f $ fromMPolarSingle arg
    toMPolarBoth f arg = let
        ff ::
            forall polarity.
            Is PolarityType polarity =>
            ToMPolarConvert t polarity
        ff = f @polarity $ fromMPolarBoth @_ @polarity arg
        in toMPolarBoth ff

newtype InvertMPolarW (w :: Polarity -> Type -> Type) (mpolarity :: Maybe Polarity) = MkInvertMPolarW
    { unInvertMPolarW :: MPolarW w (InvertMPolarity mpolarity)
    }

type instance ConvertMPolarity (InvertMPolarW _ mpolarity) = mpolarity

instance FromMPolar (InvertMPolarW w mpolarity) where
    type FromMPolarConvert (InvertMPolarW w mpolarity) polarity = Some (w (InvertPolarity polarity))
    fromMPolarSingle (MkInvertMPolarW (SingleMPolarW aw)) = aw
    fromMPolarBoth ::
        forall polarity.
        (Is PolarityType polarity, mpolarity ~ 'Nothing) =>
        InvertMPolarW w mpolarity ->
        Some (w (InvertPolarity polarity))
    fromMPolarBoth (MkInvertMPolarW (BothMPolarW aw)) = withInvertPolarity @polarity aw
