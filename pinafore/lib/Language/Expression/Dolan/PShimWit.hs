module Language.Expression.Dolan.PShimWit where

import Data.Shim.JMShimWit
import Data.Shim.Polarity
import Data.Shim.ShimWit
import Language.Expression.WitnessMappable
import Shapes

type PShimWit (cat :: k -> k -> Type) (wit :: Polarity -> k -> Type) (polarity :: Polarity)
     = ShimWit cat (wit polarity) polarity

type PJMShimWit (wit :: Polarity -> k -> Type) (polarity :: Polarity) = JMShimWit (wit polarity) polarity

mkPShimWit ::
       forall polarity (k :: Type) cat wit (t :: k). (InCategory cat, Is PolarityType polarity, InKind t)
    => wit polarity t
    -> PShimWit cat wit polarity t
mkPShimWit = mkShimWit

mkPJMShimWit ::
       forall polarity (k :: Type) wit (t :: k). (CoercibleKind k, Is PolarityType polarity, InKind t)
    => wit polarity t
    -> PJMShimWit wit polarity t
mkPJMShimWit = mkJMShimWit

type PShimWitMappable (cat :: k -> k -> Type) (wit :: Polarity -> k -> Type)
     = WitnessMappable (PShimWit cat wit 'Positive) (PShimWit cat wit 'Negative)

mapPShimWitsM ::
       forall m cat wit a. (InCategory cat, Monad m, PShimWitMappable cat wit a)
    => (forall t. InKind t => wit 'Positive t -> m (PShimWit cat wit 'Positive t))
    -> (forall t. InKind t => wit 'Negative t -> m (PShimWit cat wit 'Negative t))
    -> a
    -> m a
mapPShimWitsM mapPos mapNeg = mapWitnessesM (chainShimWitM mapPos) (chainShimWitM mapNeg)

mapPShimWits ::
       forall cat wit a. (InCategory cat, PShimWitMappable cat wit a)
    => (forall t. InKind t => wit 'Positive t -> PShimWit cat wit 'Positive t)
    -> (forall t. InKind t => wit 'Negative t -> PShimWit cat wit 'Negative t)
    -> a
    -> a
mapPShimWits mapPos mapNeg = mapWitnesses (chainShimWit mapPos) (chainShimWit mapNeg)
