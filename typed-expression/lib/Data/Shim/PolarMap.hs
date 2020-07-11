module Data.Shim.PolarMap where

import Data.Shim.JoinMeet
import Data.Shim.Polarity
import Shapes

type family PolarMapType (shim :: ShimKind k) polarity (a :: k) (b :: k) :: Type where
    PolarMapType (shim :: k -> k -> Type) 'Positive (a :: k) (b :: k) = shim a b
    PolarMapType (shim :: k -> k -> Type) 'Negative (a :: k) (b :: k) = shim b a

type PolarMap :: forall k. ShimKind k -> Polarity -> ShimKind k
newtype PolarMap shim polarity a b = MkPolarMap
    { unPolarMap :: PolarMapType shim polarity a b
    }

instance forall polarity k (shim :: ShimKind k). (Is PolarityType polarity, Category shim) =>
             Category (PolarMap shim polarity) where
    id =
        case polarityType @polarity of
            PositiveType -> MkPolarMap id
            NegativeType -> MkPolarMap id
    (.) =
        case polarityType @polarity of
            PositiveType -> \(MkPolarMap p) (MkPolarMap q) -> MkPolarMap $ p . q
            NegativeType -> \(MkPolarMap p) (MkPolarMap q) -> MkPolarMap $ q . p

instance forall polarity k (shim :: ShimKind k). (Is PolarityType polarity, Groupoid shim) =>
             Groupoid (PolarMap shim polarity) where
    invert =
        case polarityType @polarity of
            PositiveType -> \(MkPolarMap p) -> MkPolarMap $ invert p
            NegativeType -> \(MkPolarMap p) -> MkPolarMap $ invert p

instance forall polarity k (shim :: ShimKind k). (Is PolarityType polarity, InCategory shim) =>
             InCategory (PolarMap shim polarity) where
    cid =
        case polarityType @polarity of
            PositiveType -> MkPolarMap cid
            NegativeType -> MkPolarMap cid
    (<.>) =
        case polarityType @polarity of
            PositiveType -> \(MkPolarMap p) (MkPolarMap q) -> MkPolarMap $ p <.> q
            NegativeType -> \(MkPolarMap p) (MkPolarMap q) -> MkPolarMap $ q <.> p

instance forall polarity k (shim :: ShimKind k). (Is PolarityType polarity, InGroupoid shim) =>
             InGroupoid (PolarMap shim polarity) where
    cinvert =
        case polarityType @polarity of
            PositiveType -> \(MkPolarMap p) -> MkPolarMap $ cinvert p
            NegativeType -> \(MkPolarMap p) -> MkPolarMap $ cinvert p

invertPolarMap ::
       forall polarity k (shim :: ShimKind k) (a :: k) (b :: k). Is PolarityType polarity
    => PolarMap shim polarity a b
    -> PolarMap shim (InvertPolarity polarity) b a
invertPolarMap =
    case polarityType @polarity of
        PositiveType -> \(MkPolarMap f) -> MkPolarMap f
        NegativeType -> \(MkPolarMap f) -> MkPolarMap f

uninvertPolarMap ::
       forall polarity k (shim :: ShimKind k) (a :: k) (b :: k). Is PolarityType polarity
    => PolarMap shim (InvertPolarity polarity) a b
    -> PolarMap shim polarity b a
uninvertPolarMap =
    case polarityType @polarity of
        PositiveType -> \(MkPolarMap f) -> MkPolarMap f
        NegativeType -> \(MkPolarMap f) -> MkPolarMap f

isoPolarForwards ::
       forall polarity k (shim :: ShimKind k) (a :: k) (b :: k). Is PolarityType polarity
    => Isomorphism shim a b
    -> PolarMap shim polarity a b
isoPolarForwards =
    case polarityType @polarity of
        PositiveType -> MkPolarMap . isoForwards
        NegativeType -> MkPolarMap . isoBackwards

isoPolarBackwards ::
       forall polarity k (shim :: ShimKind k) (a :: k) (b :: k). Is PolarityType polarity
    => Isomorphism shim a b
    -> PolarMap shim polarity b a
isoPolarBackwards =
    case polarityType @polarity of
        PositiveType -> MkPolarMap . isoBackwards
        NegativeType -> MkPolarMap . isoForwards

reshimPolarMap ::
       forall polarity k (shim1 :: ShimKind k) (shim2 :: ShimKind k) (a :: k) (b :: k). Is PolarityType polarity
    => (forall a' b'. shim1 a' b' -> shim2 a' b')
    -> PolarMap shim1 polarity a b
    -> PolarMap shim2 polarity a b
reshimPolarMap f =
    case polarityType @polarity of
        PositiveType -> \(MkPolarMap p) -> MkPolarMap $ f p
        NegativeType -> \(MkPolarMap p) -> MkPolarMap $ f p
