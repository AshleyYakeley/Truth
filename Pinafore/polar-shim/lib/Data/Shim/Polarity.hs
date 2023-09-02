module Data.Shim.Polarity where

import Shapes

data Polarity
    = Positive
    | Negative
    deriving (Eq)

type PolarityType :: Polarity -> Type
data PolarityType polarity where
    PositiveType :: PolarityType 'Positive
    NegativeType :: PolarityType 'Negative

instance Show (PolarityType polarity) where
    show PositiveType = "+"
    show NegativeType = "-"

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

instance WitnessValue PolarityType where
    type WitnessValueType PolarityType = Polarity
    witnessToValue :: forall (t :: Polarity). PolarityType t -> Polarity
    witnessToValue PositiveType = Positive
    witnessToValue NegativeType = Negative
    valueToWitness :: forall r. Polarity -> (forall (t :: Polarity). PolarityType t -> r) -> r
    valueToWitness Positive call = call PositiveType
    valueToWitness Negative call = call NegativeType

polaritySymbol :: Polarity -> Text
polaritySymbol Positive = "+"
polaritySymbol Negative = "-"

type family InvertPolarity polarity = (inv :: Polarity) | inv -> polarity where
    InvertPolarity 'Positive = 'Negative
    InvertPolarity 'Negative = 'Positive

invertPolarity :: PolarityType polarity -> PolarityType (InvertPolarity polarity)
invertPolarity PositiveType = NegativeType
invertPolarity NegativeType = PositiveType

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

withInvertPolarity ::
       forall polarity r. Is PolarityType polarity
    => (Is PolarityType (InvertPolarity polarity) => r)
    -> r
withInvertPolarity v =
    case isInvertPolarity @polarity of
        Dict -> v

isInvertInvertPolarity ::
       forall polarity. Is PolarityType polarity
    => InvertPolarity (InvertPolarity polarity) :~: polarity
isInvertInvertPolarity =
    case polarityType @polarity of
        PositiveType -> Refl
        NegativeType -> Refl

samePolarityType ::
       forall (p1 :: Polarity) (p2 :: Polarity).
       PolarityType p1
    -> PolarityType p2
    -> Either (p1 :~: p2) (p1 :~: InvertPolarity p2)
samePolarityType PositiveType PositiveType = Left Refl
samePolarityType PositiveType NegativeType = Right Refl
samePolarityType NegativeType PositiveType = Right Refl
samePolarityType NegativeType NegativeType = Left Refl

samePolarity ::
       forall (p1 :: Polarity) (p2 :: Polarity). (Is PolarityType p1, Is PolarityType p2)
    => Either (p1 :~: p2) (p1 :~: InvertPolarity p2)
samePolarity = samePolarityType (polarityType @p1) (polarityType @p2)
