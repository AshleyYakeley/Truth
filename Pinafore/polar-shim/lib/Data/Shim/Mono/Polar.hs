module Data.Shim.Mono.Polar where

import Data.Shim.Mono.General
import Data.Shim.Polar
import Shapes

type family PolarShimType (shim :: ShimKind k) polarity (a :: k) (b :: k) :: Type where
    PolarShimType (shim :: k -> k -> Type) 'Positive (a :: k) (b :: k) = shim a b
    PolarShimType (shim :: k -> k -> Type) 'Negative (a :: k) (b :: k) = shim b a

type PolarShim :: forall k. ShimKind k -> Polarity -> ShimKind k
newtype PolarShim shim polarity a b = MkPolarShim
    { unPolarShim :: PolarShimType shim polarity a b
    }

instance forall polarity k (shim :: ShimKind k) (a :: k) (b :: k). Show (PolarShimType shim polarity a b) =>
             Show (PolarShim shim polarity a b) where
    show (MkPolarShim shim) = show shim

instance forall polarity k (shim :: ShimKind k). (Is PolarityType polarity, Category shim) =>
             Category (PolarShim shim polarity) where
    id =
        case polarityType @polarity of
            PositiveType -> MkPolarShim id
            NegativeType -> MkPolarShim id
    (.) =
        case polarityType @polarity of
            PositiveType -> \(MkPolarShim p) (MkPolarShim q) -> MkPolarShim $ p . q
            NegativeType -> \(MkPolarShim p) (MkPolarShim q) -> MkPolarShim $ q . p

instance forall polarity k (shim :: ShimKind k). (Is PolarityType polarity, Groupoid shim) =>
             Groupoid (PolarShim shim polarity) where
    invert =
        case polarityType @polarity of
            PositiveType -> \(MkPolarShim p) -> MkPolarShim $ invert p
            NegativeType -> \(MkPolarShim p) -> MkPolarShim $ invert p

instance forall polarity (shim :: ShimKind Type). (Is PolarityType polarity, CartesianShim shim) =>
             CartesianShim (PolarShim shim polarity) where
    funcShim =
        case polarityType @polarity of
            PositiveType -> \(MkPolarShim ab) (MkPolarShim pq) -> MkPolarShim $ funcShim ab pq
            NegativeType -> \(MkPolarShim ab) (MkPolarShim pq) -> MkPolarShim $ funcShim ab pq
    pairShim =
        case polarityType @polarity of
            PositiveType -> \(MkPolarShim ab) (MkPolarShim pq) -> MkPolarShim $ pairShim ab pq
            NegativeType -> \(MkPolarShim ab) (MkPolarShim pq) -> MkPolarShim $ pairShim ab pq
    eitherShim =
        case polarityType @polarity of
            PositiveType -> \(MkPolarShim ab) (MkPolarShim pq) -> MkPolarShim $ eitherShim ab pq
            NegativeType -> \(MkPolarShim ab) (MkPolarShim pq) -> MkPolarShim $ eitherShim ab pq

invertPolarShim ::
       forall polarity k (shim :: ShimKind k) (a :: k) (b :: k). Is PolarityType polarity
    => PolarShim shim polarity a b
    -> PolarShim shim (InvertPolarity polarity) b a
invertPolarShim =
    case polarityType @polarity of
        PositiveType -> \(MkPolarShim f) -> MkPolarShim f
        NegativeType -> \(MkPolarShim f) -> MkPolarShim f

uninvertPolarShim ::
       forall polarity k (shim :: ShimKind k) (a :: k) (b :: k). Is PolarityType polarity
    => PolarShim shim (InvertPolarity polarity) a b
    -> PolarShim shim polarity b a
uninvertPolarShim =
    case polarityType @polarity of
        PositiveType -> \(MkPolarShim f) -> MkPolarShim f
        NegativeType -> \(MkPolarShim f) -> MkPolarShim f

isoPolarForwards ::
       forall polarity k (shim :: ShimKind k) (a :: k) (b :: k). Is PolarityType polarity
    => Isomorphism shim a b
    -> PolarShim shim polarity a b
isoPolarForwards =
    case polarityType @polarity of
        PositiveType -> MkPolarShim . isoForwards
        NegativeType -> MkPolarShim . isoBackwards

isoPolarBackwards ::
       forall polarity k (shim :: ShimKind k) (a :: k) (b :: k). Is PolarityType polarity
    => Isomorphism shim a b
    -> PolarShim shim polarity b a
isoPolarBackwards =
    case polarityType @polarity of
        PositiveType -> MkPolarShim . isoBackwards
        NegativeType -> MkPolarShim . isoForwards

reshimPolarShim ::
       forall polarity k (shim1 :: ShimKind k) (shim2 :: ShimKind k) (a :: k) (b :: k). Is PolarityType polarity
    => (forall a' b'. shim1 a' b' -> shim2 a' b')
    -> PolarShim shim1 polarity a b
    -> PolarShim shim2 polarity a b
reshimPolarShim f =
    case polarityType @polarity of
        PositiveType -> \(MkPolarShim p) -> MkPolarShim $ f p
        NegativeType -> \(MkPolarShim p) -> MkPolarShim $ f p

lazyPolarShim ::
       forall (shim :: ShimKind Type) polarity a b. (LazyCategory shim, Is PolarityType polarity)
    => PolarShim shim polarity a b
    -> PolarShim shim polarity a b
lazyPolarShim (MkPolarShim ab) =
    MkPolarShim $
    case polarityType @polarity of
        PositiveType -> iLazy ab
        NegativeType -> iLazy ab

isoPolarShimShim ::
       forall polarity k (shim :: ShimKind k) (pa :: k) (pb :: k) (qa :: k) (qb :: k).
       (Is PolarityType polarity, IsoMapShim shim)
    => String
    -> (KindFunction pa pb -> KindFunction qa qb)
    -> (KindFunction pb pa -> KindFunction qb qa)
    -> PolarShim shim polarity pa pb
    -> PolarShim shim polarity qa qb
isoPolarShimShim t f1 f2 (MkPolarShim pp) =
    MkPolarShim $
    case polarityType @polarity of
        PositiveType -> isoMapShim t f1 f2 pp
        NegativeType -> isoMapShim t f2 f1 pp
