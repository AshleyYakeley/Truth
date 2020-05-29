module Data.Shim.PolarMap where

import Data.Shim.Polarity
import Shapes

type family PolarMapType (shim :: k -> k -> Type) polarity (a :: k) (b :: k) :: Type where
    PolarMapType (shim :: k -> k -> Type) 'Positive (a :: k) (b :: k) = shim a b
    PolarMapType (shim :: k -> k -> Type) 'Negative (a :: k) (b :: k) = shim b a

type ConvertType polarity (a :: k) (b :: k) = PolarMapType KindFunction polarity a b

newtype PolarMap (shim :: k -> k -> Type) polarity (a :: k) (b :: k) = MkPolarMap
    { unPolarMap :: PolarMapType shim polarity a b
    }

instance forall polarity k (shim :: k -> k -> Type). (Is PolarityType polarity, Category shim) =>
             Category (PolarMap shim polarity) where
    id =
        case polarityType @polarity of
            PositiveType -> MkPolarMap id
            NegativeType -> MkPolarMap id
    (.) =
        case polarityType @polarity of
            PositiveType -> \(MkPolarMap p) (MkPolarMap q) -> MkPolarMap $ p . q
            NegativeType -> \(MkPolarMap p) (MkPolarMap q) -> MkPolarMap $ q . p

instance forall polarity k (shim :: k -> k -> Type). (Is PolarityType polarity, InCategory shim) =>
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
       forall polarity k (shim :: k -> k -> Type) (a :: k) (b :: k). Is PolarityType polarity
    => PolarMap shim polarity a b
    -> PolarMap shim (InvertPolarity polarity) b a
invertPolarMap =
    case polarityType @polarity of
        PositiveType -> \(MkPolarMap f) -> MkPolarMap f
        NegativeType -> \(MkPolarMap f) -> MkPolarMap f

uninvertPolarMap ::
       forall polarity k (shim :: k -> k -> Type) (a :: k) (b :: k). Is PolarityType polarity
    => PolarMap shim (InvertPolarity polarity) a b
    -> PolarMap shim polarity b a
uninvertPolarMap =
    case polarityType @polarity of
        PositiveType -> \(MkPolarMap f) -> MkPolarMap f
        NegativeType -> \(MkPolarMap f) -> MkPolarMap f
