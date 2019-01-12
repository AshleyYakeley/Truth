module Language.Expression.Dolan.PType where

import Language.Expression.Polarity
import Language.Expression.TypeF
import Language.Expression.TypeMappable
import Shapes

type PTypeF (wit :: Polarity -> k -> Type) (polarity :: Polarity) = TypeF (wit polarity) polarity

mkPTypeF ::
       forall polarity (k :: Type) wit (t :: k). (Category (KindMorphism k (->)), Is PolarityType polarity)
    => wit polarity t
    -> PTypeF wit polarity t
mkPTypeF = mkTypeF

class ToTypeF wit t where
    toTypeF :: PTypeF wit 'Positive t

class FromTypeF wit t where
    fromTypeF :: PTypeF wit 'Negative t

toValue ::
       forall wit t. ToTypeF wit t
    => t
    -> AnyValue (wit 'Positive)
toValue = toTypeFAnyValue $ toTypeF @wit @t

type PTypeMappable (wit :: Polarity -> k -> Type) = TypeMappable (wit 'Positive) (wit 'Negative)

mapPTypes ::
       forall wit a. PTypeMappable wit a
    => (forall t. wit 'Positive t -> PTypeF wit 'Positive t)
    -> (forall t. wit 'Negative t -> PTypeF wit 'Negative t)
    -> a
    -> a
mapPTypes = mapTypes
