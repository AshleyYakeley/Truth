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
        :: forall (ground :: GroundTypeKind) polarity name t.
           SymbolType name
        -> PolarityType polarity
        -> DolanType ground polarity t
        -> Bool
        -> UnifierConstraint ground (DolanPolyShim ground Type (UVarT name) t)
    -- | var :> type
    GEUnifierConstraint
        :: forall (ground :: GroundTypeKind) polarity name t.
           SymbolType name
        -> PolarityType polarity
        -> DolanType ground polarity t
        -> Bool
        -> UnifierConstraint ground (DolanPolyShim ground Type t (UVarT name))

instance forall (ground :: GroundTypeKind) t. IsDolanGroundType ground => Show (UnifierConstraint ground t) where
    show (LEUnifierConstraint var polwit wt recflag) =
        withRepresentative polwit $ mif recflag "REC " <> show var <> " <: " <> showDolanType wt
    show (GEUnifierConstraint var polwit wt recflag) =
        withRepresentative polwit $ mif recflag "REC " <> show var <> " :> " <> showDolanType wt

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground =>
             AllWitnessConstraint Show (UnifierConstraint ground) where
    allWitnessConstraint = Dict

leUnifierConstraint ::
       forall (ground :: GroundTypeKind) polarity name p. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => SymbolType name
    -> DolanType ground polarity p
    -> UnifierConstraint ground (DolanPolyShim ground Type (UVarT name) p)
leUnifierConstraint var pt = LEUnifierConstraint var representative pt (occursInType var pt)

geUnifierConstraint ::
       forall (ground :: GroundTypeKind) polarity name p. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => SymbolType name
    -> DolanType ground polarity p
    -> UnifierConstraint ground (DolanPolyShim ground Type p (UVarT name))
geUnifierConstraint var pt = GEUnifierConstraint var representative pt (occursInType var pt)

leSingleUnifierConstraint ::
       forall (ground :: GroundTypeKind) polarity name p. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => SymbolType name
    -> DolanSingularType ground polarity p
    -> UnifierConstraint ground (DolanPolyShim ground Type (UVarT name) (JoinMeetType polarity p (LimitType polarity)))
leSingleUnifierConstraint var spt = leUnifierConstraint var (singleDolanType spt)

geSingleUnifierConstraint ::
       forall (ground :: GroundTypeKind) polarity name p. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => SymbolType name
    -> DolanSingularType ground polarity p
    -> UnifierConstraint ground (DolanPolyShim ground Type (JoinMeetType polarity p (LimitType polarity)) (UVarT name))
geSingleUnifierConstraint var spt = geUnifierConstraint var (singleDolanType spt)
