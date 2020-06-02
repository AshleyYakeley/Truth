module Language.Expression.Dolan.Type where

import Data.Shim
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Variance
import Language.Expression.TypeVariable
import Shapes

type IsDolanPolyShim :: PolyMapKind -> Constraint
type IsDolanPolyShim pshim = (DolanVarianceInCategory pshim, Category (pshim Type), Shim (pshim Type))

type DolanType :: GroundTypeKind -> Polarity -> Type -> Type
data DolanType ground polarity t where
    NilDolanType :: forall (ground :: GroundTypeKind) polarity. DolanType ground polarity (LimitType polarity)
    ConsDolanType
        :: forall (ground :: GroundTypeKind) polarity t1 tr.
           DolanSingularType ground polarity t1
        -> DolanType ground polarity tr
        -> DolanType ground polarity (JoinMeetType polarity t1 tr)

-- | This is \"soft\" typing: it mostly represents types, but relies on unsafe coercing to and from a raw type ('UVar') for type variables.
type DolanSingularType :: GroundTypeKind -> Polarity -> Type -> Type
data DolanSingularType ground polarity t where
    GroundDolanSingularType
        :: forall (ground :: GroundTypeKind) (polarity :: Polarity) (dv :: DolanVariance) t ta.
           ground dv t
        -> DolanArguments dv (DolanType ground) t polarity ta
        -> DolanSingularType ground polarity ta
    VarDolanSingularType
        :: forall (ground :: GroundTypeKind) polarity name.
           SymbolType name
        -> DolanSingularType ground polarity (UVar name)

singleDolanShimWit ::
       forall (shim :: MapKind Type) (ground :: GroundTypeKind) (polarity :: Polarity) t.
       (JoinMeetCategory shim, Is PolarityType polarity)
    => PShimWit shim (DolanSingularType ground) polarity t
    -> PShimWit shim (DolanType ground) polarity t
singleDolanShimWit (MkShimWit st conv) = ccontramap conv $ MkShimWit (singleDolanType st) polar1

singleDolanType ::
       forall (ground :: GroundTypeKind) (polarity :: Polarity) (t :: Type).
       DolanSingularType ground polarity t
    -> DolanType ground polarity (JoinMeetType polarity t (LimitType polarity))
singleDolanType st = ConsDolanType st NilDolanType

joinMeetDolanTypes ::
       forall (shim :: MapKind Type) (ground :: GroundTypeKind) polarity (a :: Type) (b :: Type) r.
       (JoinMeetCategory shim, Is PolarityType polarity)
    => DolanType ground polarity a
    -> DolanType ground polarity b
    -> (forall ab. DolanType ground polarity ab -> PolarMap shim polarity a ab -> PolarMap shim polarity b ab -> r)
    -> r
joinMeetDolanTypes NilDolanType tb cont = cont tb polarLimit cid
joinMeetDolanTypes (ConsDolanType ta tr) tb cont =
    joinMeetDolanTypes tr tb $ \trb conva convb -> cont (ConsDolanType ta trb) (polarBimap cid conva) (polar2 <.> convb)

joinMeetDolanShimWit ::
       forall (shim :: MapKind Type) (ground :: GroundTypeKind) (polarity :: Polarity) (a :: Type) (b :: Type).
       (JoinMeetCategory shim, Is PolarityType polarity)
    => PShimWit shim (DolanType ground) polarity a
    -> PShimWit shim (DolanType ground) polarity b
    -> PShimWit shim (DolanType ground) polarity (JoinMeetType polarity a b)
joinMeetDolanShimWit (MkShimWit ta conva) (MkShimWit tb convb) =
    ccontramap (polarBimap conva convb) $
    joinMeetDolanTypes ta tb $ \tab conva' convb' -> MkShimWit tab $ polarF conva' convb'

instance forall (ground :: GroundTypeKind) (polarity :: Polarity). Is PolarityType polarity =>
             Semigroup (AnyW (DolanType ground polarity)) where
    MkAnyW NilDolanType <> tb = tb
    MkAnyW (ConsDolanType ta tr) <> tb =
        case MkAnyW tr <> tb of
            MkAnyW trb -> MkAnyW $ ConsDolanType ta trb

instance forall (ground :: GroundTypeKind) (polarity :: Polarity). Is PolarityType polarity =>
             Monoid (AnyW (DolanType ground polarity)) where
    mappend = (<>)
    mempty = MkAnyW NilDolanType

instance forall (ground :: GroundTypeKind) (polarity :: Polarity). Is PolarityType polarity =>
             Semigroup (AnyInKind (RangeType (DolanType ground) polarity)) where
    MkAnyInKind (MkRangeType tp1 tq1) <> MkAnyInKind (MkRangeType tp2 tq2) =
        invertPolarity @polarity $
        case (MkAnyW tp1 <> MkAnyW tp2, MkAnyW tq1 <> MkAnyW tq2) of
            (MkAnyW tp12, MkAnyW tq12) -> MkAnyInKind (MkRangeType tp12 tq12)

instance forall (ground :: GroundTypeKind) (polarity :: Polarity). Is PolarityType polarity =>
             Monoid (AnyInKind (RangeType (DolanType ground) polarity)) where
    mappend = (<>)
    mempty = MkAnyInKind (MkRangeType NilDolanType NilDolanType)

class (IsDolanPolyShim (DolanPolyShim ground)) => IsDolanGroundType (ground :: GroundTypeKind) where
    groundTypeVarianceType ::
           forall (dv :: DolanVariance) (t :: DolanVarianceKind dv). ground dv t -> DolanVarianceType dv
    groundTypeVarianceMap ::
           forall (dv :: DolanVariance) (t :: DolanVarianceKind dv). ground dv t -> DolanVarianceMap dv t
    groundTypeTestEquality ::
           forall (dva :: DolanVariance) (ta :: DolanVarianceKind dva) (dvb :: DolanVariance) (tb :: DolanVarianceKind dvb).
           ground dva ta
        -> ground dvb tb
        -> Maybe (dva :~: dvb, ta :~~: tb)
