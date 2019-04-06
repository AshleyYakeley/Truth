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

type family PolarMapType (cat :: k -> k -> Type) polarity (a :: k) (b :: k) :: Type where
    PolarMapType (cat :: k -> k -> Type) 'Positive (a :: k) (b :: k) = cat a b
    PolarMapType (cat :: k -> k -> Type) 'Negative (a :: k) (b :: k) = cat b a

type ConvertType polarity (a :: k) (b :: k) = PolarMapType KindFunction polarity a b
