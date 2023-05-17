module Language.Expression.Dolan.Unifier.Constraint where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Occur
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

type UnifierConstraint :: GroundTypeKind -> Type -> Type
data UnifierConstraint ground t where
    -- | var <: type
    LEUnifierConstraint
        :: forall (ground :: GroundTypeKind) polarity tv t.
           TypeVarT tv
        -> PolarityType polarity
        -> DolanType ground polarity t
        -> Bool
        -> UnifierConstraint ground (DolanShim ground tv t)
    -- | var :> type
    GEUnifierConstraint
        :: forall (ground :: GroundTypeKind) polarity tv t.
           TypeVarT tv
        -> PolarityType polarity
        -> DolanType ground polarity t
        -> Bool
        -> UnifierConstraint ground (DolanShim ground t tv)

instance forall (ground :: GroundTypeKind) t. IsDolanGroundType ground => Show (UnifierConstraint ground t) where
    show (LEUnifierConstraint var polwit wt recflag) =
        withRepresentative polwit $
        mif recflag "REC " <> show var <> " <: " <> showDolanType wt <> " [" <> show polwit <> "]"
    show (GEUnifierConstraint var polwit wt recflag) =
        withRepresentative polwit $
        mif recflag "REC " <> show var <> " :> " <> showDolanType wt <> " [" <> show polwit <> "]"

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => AllConstraint Show (UnifierConstraint ground) where
    allConstraint = Dict

leUnifierConstraint ::
       forall (ground :: GroundTypeKind) polarity tv p. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => TypeVarT tv
    -> DolanType ground polarity p
    -> UnifierConstraint ground (DolanShim ground tv p)
leUnifierConstraint var pt = LEUnifierConstraint var representative pt (occursInType var pt)

geUnifierConstraint ::
       forall (ground :: GroundTypeKind) polarity tv p. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => TypeVarT tv
    -> DolanType ground polarity p
    -> UnifierConstraint ground (DolanShim ground p tv)
geUnifierConstraint var pt = GEUnifierConstraint var representative pt (occursInType var pt)

leSingleUnifierConstraint ::
       forall (ground :: GroundTypeKind) polarity tv p. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => TypeVarT tv
    -> DolanSingularType ground polarity p
    -> UnifierConstraint ground (DolanShim ground tv (JoinMeetType polarity p (LimitType polarity)))
leSingleUnifierConstraint var spt = leUnifierConstraint var (singleDolanType spt)

geSingleUnifierConstraint ::
       forall (ground :: GroundTypeKind) polarity tv p. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => TypeVarT tv
    -> DolanSingularType ground polarity p
    -> UnifierConstraint ground (DolanShim ground (JoinMeetType polarity p (LimitType polarity)) tv)
geSingleUnifierConstraint var spt = geUnifierConstraint var (singleDolanType spt)
