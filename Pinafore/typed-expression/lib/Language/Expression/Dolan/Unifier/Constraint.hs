module Language.Expression.Dolan.Unifier.Constraint where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Occur
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

type FlipType :: GroundTypeKind -> Polarity -> Type -> Type
data FlipType ground polarity t
    = NormalFlipType (DolanType ground polarity t)
    | InvertFlipType (DolanType ground (InvertPolarity polarity) t)

type UnifierConstraint :: GroundTypeKind -> Type -> Type
data UnifierConstraint ground t where
    MkUnifierConstraint
        :: forall (ground :: GroundTypeKind) polarity tv t.
           TypeVarT tv
        -> PolarityType polarity
        -> FlipType ground polarity t
        -> Bool
        -> UnifierConstraint ground (PolarMapType (DolanShim ground) polarity t tv)

instance forall (ground :: GroundTypeKind) t. IsDolanGroundType ground => Show (UnifierConstraint ground t) where
    show (MkUnifierConstraint var PositiveType (NormalFlipType wt) recflag) =
        mif recflag "REC " <> show var <> " :> " <> showDolanType wt <> " [+]"
    show (MkUnifierConstraint var NegativeType (NormalFlipType wt) recflag) =
        mif recflag "REC " <> show var <> " <: " <> showDolanType wt <> " [-]"
    show (MkUnifierConstraint var PositiveType (InvertFlipType wt) recflag) =
        mif recflag "REC " <> show var <> " :> " <> showDolanType wt <> " [- INV]"
    show (MkUnifierConstraint var NegativeType (InvertFlipType wt) recflag) =
        mif recflag "REC " <> show var <> " <: " <> showDolanType wt <> " [+ INV]"

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => AllConstraint Show (UnifierConstraint ground) where
    allConstraint = Dict

leUnifierConstraint ::
       forall (ground :: GroundTypeKind) polarity tv p. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => TypeVarT tv
    -> DolanType ground polarity p
    -> UnifierConstraint ground (DolanShim ground tv p)
leUnifierConstraint var pt =
    case polarityType @polarity of
        PositiveType -> MkUnifierConstraint var representative (InvertFlipType pt) (occursInType var pt)
        NegativeType -> MkUnifierConstraint var representative (NormalFlipType pt) (occursInType var pt)

geUnifierConstraint ::
       forall (ground :: GroundTypeKind) polarity tv p. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => TypeVarT tv
    -> DolanType ground polarity p
    -> UnifierConstraint ground (DolanShim ground p tv)
geUnifierConstraint var pt =
    case polarityType @polarity of
        PositiveType -> MkUnifierConstraint var representative (NormalFlipType pt) (occursInType var pt)
        NegativeType -> MkUnifierConstraint var representative (InvertFlipType pt) (occursInType var pt)

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
