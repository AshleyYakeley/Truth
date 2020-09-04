module Language.Expression.Dolan.PShimWit where

import Data.Shim
import Language.Expression.Common
import Shapes

type PShimWit :: forall k. ShimKind k -> (Polarity -> k -> Type) -> Polarity -> k -> Type
type PShimWit shim wit polarity = ShimWit shim (wit polarity) polarity

type PShimWitMappable (shim :: ShimKind k) (wit :: Polarity -> k -> Type)
     = WitnessMappable (PShimWit shim wit 'Positive) (PShimWit shim wit 'Negative)

mapPShimWitsM ::
       forall m shim wit a. (InCategory shim, Monad m, PShimWitMappable shim wit a)
    => (forall t. InKind t => wit 'Positive t -> m (PShimWit shim wit 'Positive t))
    -> (forall t. InKind t => wit 'Negative t -> m (PShimWit shim wit 'Negative t))
    -> a
    -> m a
mapPShimWitsM mapPos mapNeg = mapWitnessesM (chainShimWitM mapPos) (chainShimWitM mapNeg)

mapPShimWits ::
       forall shim wit a. (InCategory shim, PShimWitMappable shim wit a)
    => (forall t. InKind t => wit 'Positive t -> PShimWit shim wit 'Positive t)
    -> (forall t. InKind t => wit 'Negative t -> PShimWit shim wit 'Negative t)
    -> a
    -> a
mapPShimWits mapPos mapNeg = mapWitnesses (chainShimWit mapPos) (chainShimWit mapNeg)

chainPShimWit2 ::
       forall (shim :: ShimKind Type) (w :: Polarity -> Type -> Type) (polarity :: Polarity) (a :: Type) (b :: Type).
       (JoinMeetIsoCategory shim, Is PolarityType polarity)
    => (forall a' b'. w polarity a' -> w polarity b' -> PShimWit shim w polarity (JoinMeetType polarity a' b'))
    -> PShimWit shim w polarity a
    -> PShimWit shim w polarity b
    -> PShimWit shim w polarity (JoinMeetType polarity a b)
chainPShimWit2 f (MkShimWit ta conva) (MkShimWit tb convb) = ccontramap (iPolarPair conva convb) $ f ta tb
