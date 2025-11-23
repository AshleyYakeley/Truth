module Data.Shim.Mono.ShimWit where

import Shapes

import Data.Shim.Mono.General

type ShimWit :: forall k. ShimKind k -> (k -> Type) -> k -> Type
data ShimWit shim wit t where
    MkShimWit ::
        forall (k :: Type) (shim :: ShimKind k) (wit :: k -> Type) (t :: k) (t' :: k).
        wit t' ->
        shim t t' ->
        ShimWit shim wit t

instance AllConstraint Show wit => Show (ShimWit shim wit t) where
    show (MkShimWit t _) = allShow t

instance AllConstraint Show wit => AllConstraint Show (ShimWit shim wit) where
    allConstraint = Dict

mkShimWit ::
    forall (k :: Type) (shim :: ShimKind k) wit (t :: k).
    Category shim =>
    wit t ->
    ShimWit shim wit t
mkShimWit t = MkShimWit t id

shimWitToSome :: forall (k :: Type) (shim :: ShimKind k) (wit :: k -> Type) (a :: k). ShimWit shim wit a -> Some wit
shimWitToSome (MkShimWit t _) = MkSome t

mapShimWit ::
    forall (k :: Type) (shim :: ShimKind k) wit (a :: k) (b :: k).
    Category shim =>
    shim b a ->
    ShimWit shim wit a ->
    ShimWit shim wit b
mapShimWit ab (MkShimWit t conv) = MkShimWit t $ conv . ab

switchShimWit ::
    forall (k :: Type) (shim1 :: ShimKind k) (shim2 :: ShimKind k) wit (a :: k).
    (forall b. shim1 a b -> shim2 a b) ->
    ShimWit shim1 wit a ->
    ShimWit shim2 wit a
switchShimWit f (MkShimWit t conv) = MkShimWit t $ f conv

endoShimWit ::
    forall m k (shim :: ShimKind k) (w :: k -> Type).
    Functor m =>
    EndoM' m w ->
    EndoM' m (ShimWit shim w)
endoShimWit emw = MkEndoM $ \(MkShimWit wa conv) -> fmap (\wa' -> MkShimWit wa' conv) $ unEndoM emw wa

chainShimWitM ::
    forall m (k :: Type) (shim :: ShimKind k) (wita :: k -> Type) (witb :: k -> Type) (t' :: k).
    (Functor m, Category shim) =>
    (forall (t :: k). wita t -> m (ShimWit shim witb t)) ->
    ShimWit shim wita t' ->
    m (ShimWit shim witb t')
chainShimWitM f (MkShimWit t conv) = fmap (mapShimWit conv) $ f t

chainShimWit ::
    forall (k :: Type) (shim :: ShimKind k) (wita :: k -> Type) (witb :: k -> Type) (t' :: k).
    Category shim =>
    (forall (t :: k). wita t -> ShimWit shim witb t) ->
    ShimWit shim wita t' ->
    ShimWit shim witb t'
chainShimWit f (MkShimWit t conv) = mapShimWit conv $ f t

reShimWit ::
    forall (k :: Type) (shim :: ShimKind k) (wita :: k -> Type) (witb :: k -> Type) (t' :: k).
    (forall (t :: k). wita t -> witb t) ->
    ShimWit shim wita t' ->
    ShimWit shim witb t'
reShimWit f (MkShimWit t conv) = MkShimWit (f t) conv
