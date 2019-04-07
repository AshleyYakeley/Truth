module Language.Expression.Dolan.PType where

import Language.Expression.Polarity
import Language.Expression.TypeF
import Language.Expression.TypeMappable
import Shapes

type GenPTypeF (cat :: k -> k -> Type) (wit :: Polarity -> k -> Type) (polarity :: Polarity)
     = GenTypeF cat (wit polarity) polarity

type PTypeF (wit :: Polarity -> k -> Type) (polarity :: Polarity) = TypeF (wit polarity) polarity

mkGenPTypeF ::
       forall polarity (k :: Type) cat wit (t :: k). (Category cat, Is PolarityType polarity)
    => wit polarity t
    -> GenPTypeF cat wit polarity t
mkGenPTypeF = mkGenTypeF

mkPTypeF ::
       forall polarity (k :: Type) wit (t :: k). (Category (KindFunction :: k -> k -> Type), Is PolarityType polarity)
    => wit polarity t
    -> PTypeF wit polarity t
mkPTypeF = mkTypeF

type PTypeMappable (cat :: k -> k -> Type) (wit :: Polarity -> k -> Type)
     = TypeMappable cat (wit 'Positive) (wit 'Negative)

mapPTypes ::
       forall wit a. PTypeMappable (->) wit a
    => (forall t. wit 'Positive t -> PTypeF wit 'Positive t)
    -> (forall t. wit 'Negative t -> PTypeF wit 'Negative t)
    -> a
    -> a
mapPTypes = mapTypes
