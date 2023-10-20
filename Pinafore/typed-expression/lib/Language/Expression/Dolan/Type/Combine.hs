{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Type.Combine
    ( joinMeetType
    , joinMeetShimWit
    ) where

import Data.Shim
import Language.Expression.Dolan.Type.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

joinMeetType ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) polarity (a :: Type) (b :: Type).
       (JoinMeetIsoShim (pshim Type), Is PolarityType polarity)
    => DolanType ground polarity a
    -> DolanType ground polarity b
    -> PShimWit (pshim Type) (DolanType ground) polarity (JoinMeetType polarity a b)
joinMeetType NilDolanType tb = MkShimWit tb iPolarL2
joinMeetType (ConsDolanType ta tr) tb =
    case joinMeetType tr tb of
        MkShimWit trb convrb -> MkShimWit (ConsDolanType ta trb) $ iPolarPair id convrb . iPolarSwapL

joinMeetShimWit ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) (polarity :: Polarity) (a :: Type) (b :: Type).
       (JoinMeetIsoShim (pshim Type), Is PolarityType polarity)
    => PShimWit (pshim Type) (DolanType ground) polarity a
    -> PShimWit (pshim Type) (DolanType ground) polarity b
    -> PShimWit (pshim Type) (DolanType ground) polarity (JoinMeetType polarity a b)
joinMeetShimWit = chainPShimWit2 joinMeetType

instance forall (ground :: GroundTypeKind) (polarity :: Polarity). (IsDolanGroundType ground, Is PolarityType polarity) =>
             Semigroup (Some (RangeType (DolanType ground) polarity)) where
    MkSome (MkRangeType tp1 tq1) <> MkSome (MkRangeType tp2 tq2) =
        withInvertPolarity @polarity $
        case (MkSome tp1 <> MkSome tp2, MkSome tq1 <> MkSome tq2) of
            (MkSome tp12, MkSome tq12) -> MkSome (MkRangeType tp12 tq12)

instance forall (ground :: GroundTypeKind) (polarity :: Polarity). (IsDolanGroundType ground, Is PolarityType polarity) =>
             Monoid (Some (RangeType (DolanType ground) polarity)) where
    mappend = (<>)
    mempty = MkSome (MkRangeType NilDolanType NilDolanType)
