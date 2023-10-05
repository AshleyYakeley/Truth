module Language.Expression.Dolan.Solver.Puzzle where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.FlipType
import Language.Expression.Dolan.Simplify.AutomateRecursion
import Language.Expression.Dolan.Solver.AtomicConstraint
import Language.Expression.Dolan.Solver.WholeConstraint
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeResult
import Language.Expression.Dolan.TypeSystem
import Shapes

type Substitution :: GroundTypeKind -> Type
data Substitution ground where
    MkSubstitution
        :: forall (ground :: GroundTypeKind) polarity nv t.
           PolarityType polarity
        -> TypeVarT (JoinMeetType polarity nv t)
        -> TypeVarT nv
        -> TypeResult ground (DolanIsoShimWit ground polarity (JoinMeetType polarity nv t))
        -> Maybe (DolanType ground (InvertPolarity polarity) t)
        -> Substitution ground

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => Show (Substitution ground) where
    show (MkSubstitution pol oldvar newvar mt mi) = let
        invpol = invertPolarity pol
        st =
            case mToMaybe mt of
                Just (MkShimWit t _) -> withRepresentative pol $ showDolanType t
                Nothing -> "FAILS"
        si =
            case mi of
                Just invtype -> "; INV " <> withRepresentative invpol (showDolanType invtype)
                Nothing -> ""
        in "{" <>
           show oldvar <>
           show pol <> " => " <> st <> "; " <> show oldvar <> show invpol <> " => " <> show newvar <> si <> "}"

type Piece :: GroundTypeKind -> Type -> Type
data Piece ground t where
    WholePiece :: forall (ground :: GroundTypeKind) t. WholeConstraint ground t -> Piece ground t
    AtomicPiece :: forall (ground :: GroundTypeKind) t. AtomicConstraint ground t -> Piece ground t

purePiece ::
       forall (ground :: GroundTypeKind) t. IsDolanGroundType ground
    => Piece ground t
    -> Bool
purePiece (WholePiece (MkWholeConstraint (NormalFlipType _) (NormalFlipType _))) = True
purePiece (AtomicPiece (MkAtomicConstraint _ _ (NormalFlipType _))) = True
purePiece _ = False

instance forall (ground :: GroundTypeKind) t. IsDolanGroundType ground => Show (Piece ground t) where
    show (WholePiece wc) = "whole: " <> show wc
    show (AtomicPiece ac) = "atomic: " <> show ac

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => AllConstraint Show (Piece ground) where
    allConstraint = Dict

type Puzzle :: GroundTypeKind -> Type -> Type
type Puzzle ground = Expression (Piece ground)

wholeConstraintPuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => WholeConstraint ground a
    -> Puzzle ground a
wholeConstraintPuzzle (MkWholeConstraint (NormalFlipType ta) (NormalFlipType tb)) =
    case (automateRecursionInType ta, automateRecursionInType tb) of
        (MkPosShimWit ta' conva, MkNegShimWit tb' convb) ->
            fmap (\conv -> convb . conv . conva) $
            varExpression $ WholePiece $ MkWholeConstraint (NormalFlipType ta') (NormalFlipType tb')
wholeConstraintPuzzle constr = varExpression $ WholePiece constr

atomicConstraintPuzzle :: forall (ground :: GroundTypeKind) a. AtomicConstraint ground a -> Puzzle ground a
atomicConstraintPuzzle ac = varExpression $ AtomicPiece ac

flipUnifyPuzzle ::
       forall (ground :: GroundTypeKind) a b. IsDolanGroundType ground
    => FlipType ground 'Positive a
    -> FlipType ground 'Negative b
    -> Puzzle ground (DolanShim ground a b)
flipUnifyPuzzle fta ftb = wholeConstraintPuzzle $ MkWholeConstraint fta ftb

puzzleUnify ::
       forall (ground :: GroundTypeKind) pola polb a b.
       (IsDolanGroundType ground, Is PolarityType pola, Is PolarityType polb)
    => DolanType ground pola a
    -> DolanType ground polb b
    -> Puzzle ground (DolanShim ground a b)
puzzleUnify ta tb = let
    fta =
        case polarityType @pola of
            PositiveType -> NormalFlipType ta
            NegativeType -> InvertFlipType ta
    ftb =
        case polarityType @polb of
            PositiveType -> InvertFlipType tb
            NegativeType -> NormalFlipType tb
    in flipUnifyPuzzle fta ftb

type PuzzleExpression :: GroundTypeKind -> Type -> Type
type PuzzleExpression ground = TSOpenSolverExpression (DolanTypeSystem ground) (Puzzle ground)

puzzleExpression ::
       forall (ground :: GroundTypeKind) a. ()
    => Puzzle ground a
    -> PuzzleExpression ground a
puzzleExpression = solverExpressionLiftType

puzzleExpressionUnify ::
       forall (ground :: GroundTypeKind) pola polb a b.
       (IsDolanGroundType ground, Is PolarityType pola, Is PolarityType polb)
    => DolanType ground pola a
    -> DolanType ground polb b
    -> PuzzleExpression ground (DolanShim ground a b)
puzzleExpressionUnify ta tb = puzzleExpression $ puzzleUnify ta tb

puzzleUnifySingular ::
       forall (ground :: GroundTypeKind) pola polb a b.
       (IsDolanGroundType ground, Is PolarityType pola, Is PolarityType polb)
    => DolanSingularType ground pola a
    -> DolanSingularType ground polb b
    -> Puzzle ground (DolanShim ground a b)
puzzleUnifySingular ta tb =
    fmap (\conv -> iJoinMeetL1 @_ @polb . conv . iJoinMeetR1 @_ @pola) $
    puzzleUnify (singleDolanType ta) (singleDolanType tb)

bisubstituteAtomicConstraint ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => SolverBisubstitution ground
    -> AtomicConstraint ground a
    -> TypeResult ground (Puzzle ground a)
bisubstituteAtomicConstraint bisub@(MkBisubstitution oldvar _ mwq) (MkAtomicConstraint depvar PositiveType ftw)
    | Just Refl <- testEquality oldvar depvar = do
        MkShimWit tq (MkPolarShim (MkPolyMapT convq)) <- mwq
        MkShimWit ftw' (MkPolarShim (MkPolyMapT convw)) <- bisubstituteFlipType bisub ftw
        return $ fmap (\conv -> isoForwards convq . conv . isoForwards convw) $ flipUnifyPuzzle ftw' (NormalFlipType tq)
bisubstituteAtomicConstraint bisub@(MkBisubstitution oldvar mwp _) (MkAtomicConstraint depvar NegativeType ftw)
    | Just Refl <- testEquality oldvar depvar = do
        MkShimWit tp (MkPolarShim (MkPolyMapT convp)) <- mwp
        MkShimWit ftw' (MkPolarShim (MkPolyMapT convw)) <- bisubstituteFlipType bisub ftw
        return $ fmap (\conv -> isoForwards convw . conv . isoForwards convp) $ flipUnifyPuzzle (NormalFlipType tp) ftw'
bisubstituteAtomicConstraint _ ac
    | Just conv <- isPureAtomicConstraint ac = return $ pure conv
bisubstituteAtomicConstraint bisub (MkAtomicConstraint depvar PositiveType (NormalFlipType tw)) = do
    MkShimWit tp (MkPolarShim (MkPolyMapT conv)) <- bisubstituteType bisub tw
    return $
        fmap (\pv -> iMeetL1 . pv . isoForwards conv) $
        flipUnifyPuzzle (NormalFlipType tp) (NormalFlipType $ singleDolanType $ VarDolanSingularType depvar)
bisubstituteAtomicConstraint bisub (MkAtomicConstraint depvar NegativeType (NormalFlipType tw)) = do
    MkShimWit tp (MkPolarShim (MkPolyMapT conv)) <- bisubstituteType bisub tw
    return $
        fmap (\pv -> isoForwards conv . pv . iJoinR1) $
        flipUnifyPuzzle (NormalFlipType $ singleDolanType $ VarDolanSingularType depvar) (NormalFlipType tp)
bisubstituteAtomicConstraint _ ac = return $ atomicConstraintPuzzle ac
