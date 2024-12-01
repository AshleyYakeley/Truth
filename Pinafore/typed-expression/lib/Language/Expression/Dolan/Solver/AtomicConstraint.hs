module Language.Expression.Dolan.Solver.AtomicConstraint where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.TypeSystem
import Shapes

type AtomicConstraint :: GroundTypeKind -> Type -> Type
data AtomicConstraint ground t where
    MkAtomicConstraint
        :: forall (ground :: GroundTypeKind) polarity tv t.
           TypeVarT tv
        -> PolarityType polarity
        -> FlipType ground polarity t
        -> AtomicConstraint ground (PolarShimType (DolanShim ground) polarity t tv)

instance forall (ground :: GroundTypeKind) t. ShowGroundType ground => Show (AtomicConstraint ground t) where
    show (MkAtomicConstraint var PositiveType (NormalFlipType wt)) = show var <> " :> " <> allShow wt <> " [+]"
    show (MkAtomicConstraint var NegativeType (NormalFlipType wt)) = show var <> " <: " <> allShow wt <> " [-]"
    show (MkAtomicConstraint var PositiveType (InvertFlipType wt)) = show var <> " :> " <> allShow wt <> " [- INV]"
    show (MkAtomicConstraint var NegativeType (InvertFlipType wt)) = show var <> " <: " <> allShow wt <> " [+ INV]"

instance forall (ground :: GroundTypeKind). ShowGroundType ground => AllConstraint Show (AtomicConstraint ground) where
    allConstraint = Dict

mkAtomicConstraint ::
       forall (ground :: GroundTypeKind) polarity tv t. Is PolarityType polarity
    => TypeVarT tv
    -> FlipType ground polarity t
    -> AtomicConstraint ground (PolarShimType (DolanShim ground) polarity t tv)
mkAtomicConstraint var ft = MkAtomicConstraint var representative ft

isPureAtomicConstraint ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => AtomicConstraint ground a
    -> Maybe a
isPureAtomicConstraint (MkAtomicConstraint depvar pol (NormalFlipType tw)) =
    withRepresentative pol $ do
        MkShimWit t (MkPolarShim conv) <- dolanToMaybeTypeShim tw
        case t of
            VarDolanSingularType v -> do
                Refl <- testEquality v depvar
                return conv
            _ -> Nothing
isPureAtomicConstraint _ = Nothing

type AtomicPuzzle :: GroundTypeKind -> Type -> Type
type AtomicPuzzle ground = Expression (AtomicConstraint ground)

type AtomicPuzzleExpression :: GroundTypeKind -> Type -> Type
type AtomicPuzzleExpression ground = TSOpenSolverExpression (DolanTypeSystem ground) (AtomicPuzzle ground)

joinAtomicConstraints ::
       forall (ground :: GroundTypeKind) a b. IsDolanGroundType ground
    => AtomicConstraint ground a
    -> AtomicConstraint ground b
    -> Maybe (AtomicPuzzle ground (a, b))
joinAtomicConstraints (MkAtomicConstraint va pa (NormalFlipType ta)) (MkAtomicConstraint vb pb (NormalFlipType tb)) = do
    Refl <- testEquality va vb
    Refl <- testEquality pa pb
    Just $
        case pa of
            PositiveType ->
                case joinMeetType ta tb of
                    MkShimWit tab (MkPolarShim conv) -> let
                        mapconv shimab = let
                            shconv = shimab . conv
                            in (shconv . join1, shconv . join2)
                        in fmap mapconv $ varExpression $ MkAtomicConstraint va pa (NormalFlipType tab)
            NegativeType ->
                case joinMeetType ta tb of
                    MkShimWit tab (MkPolarShim conv) -> let
                        mapconv shimab = let
                            shconv = conv . shimab
                            in (meet1 . shconv, meet2 . shconv)
                        in fmap mapconv $ varExpression $ MkAtomicConstraint va pa (NormalFlipType tab)
joinAtomicConstraints _ _ = Nothing
