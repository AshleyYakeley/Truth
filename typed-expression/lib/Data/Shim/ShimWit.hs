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
