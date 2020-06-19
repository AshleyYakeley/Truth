module Language.Expression.Dolan.PShimWit where

import Data.Shim
import Language.Expression.Common
import Shapes

type PShimWit :: forall k. ShimKind k -> (Polarity -> k -> Type) -> Polarity -> k -> Type
type PShimWit cat wit polarity = ShimWit cat (wit polarity) polarity

type PShimWitMappable (cat :: ShimKind k) (wit :: Polarity -> k -> Type)
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
