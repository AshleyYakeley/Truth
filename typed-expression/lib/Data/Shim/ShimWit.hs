module Data.Shim.ShimWit where

import Data.Shim.Polarity
import Shapes

data ShimWit (shim :: k -> k -> Type) (wit :: k -> Type) (polarity :: Polarity) (t :: k) :: Type where
    MkShimWit
        :: forall (k :: Type) (shim :: k -> k -> Type) wit polarity (t :: k) (t' :: k). InKind t'
        => wit t'
        -> PolarMapType shim polarity t t'
        -> ShimWit shim wit polarity t

mkShimWit ::
       forall (k :: Type) (shim :: k -> k -> Type) polarity wit (t :: k).
       (InKind t, InCategory shim, Is PolarityType polarity)
    => wit t
    -> ShimWit shim wit polarity t
mkShimWit t =
    case representative @_ @_ @polarity of
        PositiveType -> MkShimWit t cid
        NegativeType -> MkShimWit t cid

unShimWit :: ShimWit shim wit polarity t -> (forall t'. wit t' -> PolarMapType shim polarity t t' -> r) -> r
unShimWit (MkShimWit t conv) cont = cont t conv

mapShimWit ::
       forall (k :: Type) (shim :: k -> k -> Type) wit polarity (a :: k) (b :: k).
       (InCategory shim, Is PolarityType polarity, InKind a, InKind b)
    => PolarMapType shim polarity b a
    -> ShimWit shim wit polarity a
    -> ShimWit shim wit polarity b
mapShimWit ab (MkShimWit t conv) =
    MkShimWit t $
    case representative @_ @_ @polarity of
        PositiveType -> conv <.> ab
        NegativeType -> ab <.> conv

instance Contravariant (ShimWit (->) wit 'Positive) where
    contramap = mapShimWit

instance InCategory shim => CatFunctor (CatDual shim) (->) (ShimWit shim wit 'Positive) where
    cfmap (MkCatDual ab) = mapShimWit ab

instance Functor (ShimWit (->) wit 'Negative) where
    fmap = mapShimWit

instance InCategory shim => CatFunctor shim (->) (ShimWit shim wit 'Negative) where
    cfmap ab = mapShimWit ab

chainShimWitM ::
       forall m (k :: Type) (shim :: k -> k -> Type) (wita :: k -> Type) (witb :: k -> Type) polarity (t' :: k).
       (Monad m, InCategory shim, Is PolarityType polarity, InKind t')
    => (forall (t :: k). InKind t => wita t -> m (ShimWit shim witb polarity t))
    -> ShimWit shim wita polarity t'
    -> m (ShimWit shim witb polarity t')
chainShimWitM f (MkShimWit t conv) = do
    tf <- f t
    return $ mapShimWit conv tf

chainShimWit ::
       forall (k :: Type) (shim :: k -> k -> Type) (wita :: k -> Type) (witb :: k -> Type) polarity (t' :: k).
       (InCategory shim, Is PolarityType polarity, InKind t')
    => (forall (t :: k). InKind t => wita t -> ShimWit shim witb polarity t)
    -> ShimWit shim wita polarity t'
    -> ShimWit shim witb polarity t'
chainShimWit f (MkShimWit t conv) = mapShimWit conv $ f t

class ToShimWit (shim :: k -> k -> Type) (wit :: k -> Type) (t :: k) where
    toShimWit :: ShimWit shim wit 'Positive t

class FromShimWit (shim :: k -> k -> Type) (wit :: k -> Type) (t :: k) where
    fromShimWit :: ShimWit shim wit 'Negative t

type ToListShimWit shim wit = Is (ListType (Compose Dict (ToShimWit shim wit)))

dictToShimWit :: Compose Dict (ToShimWit shim wit) t -> ShimWit shim wit 'Positive t
dictToShimWit (Compose Dict) = toShimWit

toListShimWit :: ToListShimWit shim wit lt => ListType (ShimWit shim wit 'Positive) lt
toListShimWit = mapListType dictToShimWit representative
