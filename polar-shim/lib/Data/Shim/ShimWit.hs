module Data.Shim.ShimWit where

import Data.Shim.JoinMeet
import Shapes

type ShimWit :: forall k. ShimKind k -> (k -> Type) -> k -> Type
data ShimWit shim wit t where
    MkShimWit
        :: forall (k :: Type) (shim :: ShimKind k) (wit :: k -> Type) (t :: k) (t' :: k). InKind t'
        => wit t'
        -> shim t t'
        -> ShimWit shim wit t

instance AllWitnessConstraint Show wit => Show (ShimWit shim wit t) where
    show (MkShimWit t _) = showAllWitness t

instance AllWitnessConstraint Show wit => AllWitnessConstraint Show (ShimWit shim wit) where
    allWitnessConstraint = Dict

mkShimWit ::
       forall (k :: Type) (shim :: ShimKind k) wit (t :: k). (InKind t, InCategory shim)
    => wit t
    -> ShimWit shim wit t
mkShimWit t = MkShimWit t cid

shimWitToAnyW :: forall (k :: Type) (shim :: ShimKind k) (wit :: k -> Type) (a :: k). ShimWit shim wit a -> AnyW wit
shimWitToAnyW (MkShimWit t _) = MkAnyW t

mapShimWit ::
       forall (k :: Type) (shim :: ShimKind k) wit (a :: k) (b :: k). (InCategory shim, InKind a, InKind b)
    => shim b a
    -> ShimWit shim wit a
    -> ShimWit shim wit b
mapShimWit ab (MkShimWit t conv) = MkShimWit t $ conv <.> ab

chainShimWitM ::
       forall m (k :: Type) (shim :: ShimKind k) (wita :: k -> Type) (witb :: k -> Type) (t' :: k).
       (Functor m, InCategory shim, InKind t')
    => (forall (t :: k). InKind t => wita t -> m (ShimWit shim witb t))
    -> ShimWit shim wita t'
    -> m (ShimWit shim witb t')
chainShimWitM f (MkShimWit t conv) = fmap (mapShimWit conv) $ f t

chainShimWit ::
       forall (k :: Type) (shim :: ShimKind k) (wita :: k -> Type) (witb :: k -> Type) (t' :: k).
       (InCategory shim, InKind t')
    => (forall (t :: k). InKind t => wita t -> ShimWit shim witb t)
    -> ShimWit shim wita t'
    -> ShimWit shim witb t'
chainShimWit f (MkShimWit t conv) = mapShimWit conv $ f t
