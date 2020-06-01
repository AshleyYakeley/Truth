module Data.Shim.PolarMap where

import Data.Shim.Polarity
import Data.Shim.PolyMap
import Shapes

type family PolarMapType (map :: MapKind k) polarity (a :: k) (b :: k) :: Type where
    PolarMapType (map :: k -> k -> Type) 'Positive (a :: k) (b :: k) = map a b
    PolarMapType (map :: k -> k -> Type) 'Negative (a :: k) (b :: k) = map b a

type PolarMap :: forall k. MapKind k -> Polarity -> MapKind k
newtype PolarMap map polarity a b = MkPolarMap
    { unPolarMap :: PolarMapType map polarity a b
    }

instance forall polarity k (map :: MapKind k). (Is PolarityType polarity, Category map) =>
             Category (PolarMap map polarity) where
    id =
        case polarityType @polarity of
            PositiveType -> MkPolarMap id
            NegativeType -> MkPolarMap id
    (.) =
        case polarityType @polarity of
            PositiveType -> \(MkPolarMap p) (MkPolarMap q) -> MkPolarMap $ p . q
            NegativeType -> \(MkPolarMap p) (MkPolarMap q) -> MkPolarMap $ q . p

instance forall polarity k (map :: MapKind k). (Is PolarityType polarity, InCategory map) =>
             InCategory (PolarMap map polarity) where
    cid =
        case polarityType @polarity of
            PositiveType -> MkPolarMap cid
            NegativeType -> MkPolarMap cid
    (<.>) =
        case polarityType @polarity of
            PositiveType -> \(MkPolarMap p) (MkPolarMap q) -> MkPolarMap $ p <.> q
            NegativeType -> \(MkPolarMap p) (MkPolarMap q) -> MkPolarMap $ q <.> p

invertPolarMap ::
       forall polarity k (map :: MapKind k) (a :: k) (b :: k). Is PolarityType polarity
    => PolarMap map polarity a b
    -> PolarMap map (InvertPolarity polarity) b a
invertPolarMap =
    case polarityType @polarity of
        PositiveType -> \(MkPolarMap f) -> MkPolarMap f
        NegativeType -> \(MkPolarMap f) -> MkPolarMap f

uninvertPolarMap ::
       forall polarity k (map :: MapKind k) (a :: k) (b :: k). Is PolarityType polarity
    => PolarMap map (InvertPolarity polarity) a b
    -> PolarMap map polarity b a
uninvertPolarMap =
    case polarityType @polarity of
        PositiveType -> \(MkPolarMap f) -> MkPolarMap f
        NegativeType -> \(MkPolarMap f) -> MkPolarMap f
