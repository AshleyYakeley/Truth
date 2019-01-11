module Language.Expression.Polarity where

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

isInvertPolarity ::
       forall polarity. Is PolarityType polarity
    => Dict (Is PolarityType (InvertPolarity polarity))
isInvertPolarity =
    case representative @_ @_ @polarity of
        PositiveType -> Dict
        NegativeType -> Dict

invertPolarity ::
       forall polarity r. Is PolarityType polarity
    => (Is PolarityType (InvertPolarity polarity) => r)
    -> r
invertPolarity v =
    case isInvertPolarity @polarity of
        Dict -> v

type family ConvertType polarity (a :: k) (b :: k) :: Type where
    ConvertType 'Positive (a :: k) (b :: k) = KindFunction k a b
    ConvertType 'Negative (a :: k) (b :: k) = KindFunction k b a
