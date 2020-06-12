module Data.Shim.Polarity where

import Shapes

data Polarity
    = Positive
    | Negative

data PolarityType (polarity :: Polarity) where
    PositiveType :: PolarityType 'Positive
    NegativeType :: PolarityType 'Negative

instance TestEquality PolarityType where
    testEquality PositiveType PositiveType = Just Refl
    testEquality NegativeType NegativeType = Just Refl
    testEquality _ _ = Nothing

instance Representative PolarityType where
    getRepWitness PositiveType = Dict
    getRepWitness NegativeType = Dict

instance Is PolarityType 'Positive where
    representative = PositiveType

instance Is PolarityType 'Negative where
    representative = NegativeType

type family InvertPolarity polarity = (inv :: Polarity) | inv -> polarity where
    InvertPolarity 'Positive = 'Negative
    InvertPolarity 'Negative = 'Positive

polarityType ::
       forall (polarity :: Polarity). Is PolarityType polarity
    => PolarityType polarity
polarityType = representative @_ @_ @polarity

isInvertPolarity ::
       forall polarity. Is PolarityType polarity
    => Dict (Is PolarityType (InvertPolarity polarity))
isInvertPolarity =
    case polarityType @polarity of
        PositiveType -> Dict
        NegativeType -> Dict

invertPolarity ::
       forall polarity r. Is PolarityType polarity
    => (Is PolarityType (InvertPolarity polarity) => r)
    -> r
invertPolarity v =
    case isInvertPolarity @polarity of
        Dict -> v

isInvertInvertPolarity ::
       forall polarity. Is PolarityType polarity
    => InvertPolarity (InvertPolarity polarity) :~: polarity
isInvertInvertPolarity =
    case polarityType @polarity of
        PositiveType -> Refl
        NegativeType -> Refl

samePolarity ::
       forall (p1 :: Polarity) (p2 :: Polarity). (Is PolarityType p1, Is PolarityType p2)
    => Either (p1 :~: p2) (p1 :~: InvertPolarity p2)
samePolarity =
    case (polarityType @p1, polarityType @p2) of
        (PositiveType, PositiveType) -> Left Refl
        (PositiveType, NegativeType) -> Right Refl
        (NegativeType, PositiveType) -> Right Refl
        (NegativeType, NegativeType) -> Left Refl
