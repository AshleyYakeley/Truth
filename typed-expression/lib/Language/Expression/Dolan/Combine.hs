{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Combine
    ( joinMeetShimWit
    ) where

import Data.Shim
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

joinMeetType ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity (a :: Type) (b :: Type).
       (JoinMeetIsoCategory (pshim Type), Is PolarityType polarity)
    => DolanType ground polarity a
    -> DolanType ground polarity b
    -> PShimWit (pshim Type) (DolanType ground) polarity (JoinMeetType polarity a b)
joinMeetType NilDolanType tb = MkShimWit tb iPolarL2
joinMeetType (ConsDolanType ta tr) tb =
    case joinMeetType tr tb of
        MkShimWit trb convrb -> MkShimWit (ConsDolanType ta trb) $ iPolarPair cid convrb <.> iPolarSwapL

joinMeetShimWit ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) (polarity :: Polarity) (a :: Type) (b :: Type).
       (JoinMeetIsoCategory (pshim Type), Is PolarityType polarity)
    => PShimWit (pshim Type) (DolanType ground) polarity a
    -> PShimWit (pshim Type) (DolanType ground) polarity b
    -> PShimWit (pshim Type) (DolanType ground) polarity (JoinMeetType polarity a b)
joinMeetShimWit = chainPShimWit2 joinMeetType

instance forall (ground :: GroundTypeKind) (polarity :: Polarity). (IsDolanGroundType ground, Is PolarityType polarity) =>
             Semigroup (AnyInKind (RangeType (DolanType ground) polarity)) where
    MkAnyInKind (MkRangeType tp1 tq1) <> MkAnyInKind (MkRangeType tp2 tq2) =
        invertPolarity @polarity $
        case (MkAnyW tp1 <> MkAnyW tp2, MkAnyW tq1 <> MkAnyW tq2) of
            (MkAnyW tp12, MkAnyW tq12) -> MkAnyInKind (MkRangeType tp12 tq12)

instance forall (ground :: GroundTypeKind) (polarity :: Polarity). (IsDolanGroundType ground, Is PolarityType polarity) =>
             Monoid (AnyInKind (RangeType (DolanType ground) polarity)) where
    mappend = (<>)
    mempty = MkAnyInKind (MkRangeType NilDolanType NilDolanType)
