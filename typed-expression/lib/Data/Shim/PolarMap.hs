module Data.Shim.PolarMap where

import Data.Shim.Polarity
import Data.Shim.PolyMap
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
