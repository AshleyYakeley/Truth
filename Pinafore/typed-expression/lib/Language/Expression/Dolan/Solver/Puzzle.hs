module Language.Expression.Dolan.Solver.Puzzle where

import Data.Shim
import Shapes

import Language.Expression.Common
import Language.Expression.Dolan.Simplify.Solve
import Language.Expression.Dolan.Solver.AtomicConstraint
import Language.Expression.Dolan.Solver.WholeConstraint
import Language.Expression.Dolan.SubtypeChain
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeResult
import Language.Expression.Dolan.TypeSystem
import Language.Expression.TypeSystem

type Substitution :: GroundTypeKind -> Type
data Substitution ground where
    MkSubstitution ::
        forall (ground :: GroundTypeKind) polarity nv t.
        PolarityType polarity ->
        TypeVarT (JoinMeetType polarity nv t) ->
        TypeVarT nv ->
        TypeResult ground (DolanShimWit ground polarity (JoinMeetType polarity nv t)) ->
        Maybe (DolanType ground (InvertPolarity polarity) t) ->
        Substitution ground

instance forall (ground :: GroundTypeKind). ShowGroundType ground => Show (Substitution ground) where
    show (MkSubstitution pol oldvar newvar mt mi) = let
        invpol = invertPolarity pol
        st =
            case mToMaybe mt of
                Just (MkShimWit t _) -> withRepresentative pol $ allShow t
                Nothing -> "FAILS"
        si =
            case mi of
                Just invtype -> "; INV " <> withRepresentative invpol (allShow invtype)
                Nothing -> ""
        in "{"
            <> show oldvar
            <> show pol
            <> " => "
            <> st
            <> "; "
            <> show oldvar
            <> show invpol
            <> " => "
            <> show newvar
            <> si
            <> "}"

type Piece :: GroundTypeKind -> Type -> Type
data Piece ground t where
    WholePiece :: forall (ground :: GroundTypeKind) t. WholeConstraint ground t -> Piece ground t
    AtomicPiece :: forall (ground :: GroundTypeKind) t. AtomicConstraint ground t -> Piece ground t

instance forall (ground :: GroundTypeKind) t. ShowGroundType ground => Show (Piece ground t) where
    show (WholePiece wc) = "whole: " <> show wc
    show (AtomicPiece ac) = "atomic: " <> show ac

instance forall (ground :: GroundTypeKind). ShowGroundType ground => AllConstraint Show (Piece ground) where
    allConstraint = Dict

type Puzzle :: GroundTypeKind -> Type -> Type
type Puzzle ground = Expression (Piece ground)

simplifyWholeConstraintINTERNAL :: Bool
simplifyWholeConstraintINTERNAL = False

wholeConstraintPuzzle ::
    forall (ground :: GroundTypeKind) a.
    IsDolanGroundType ground =>
    WholeConstraint ground a ->
    DolanRenameTypeM ground (Puzzle ground a)
wholeConstraintPuzzle (MkWholeConstraint (NormalFlipType ta) (NormalFlipType tb))
    | simplifyWholeConstraintINTERNAL = do
        MkShimWit ta' (MkPolarShim conva) <- solveSimplify ta
        MkShimWit tb' (MkPolarShim convb) <- solveSimplify tb
        return
            $ fmap (\conv -> convb . conv . conva)
            $ varExpression
            $ WholePiece
            $ MkWholeConstraint (NormalFlipType ta') (NormalFlipType tb')
wholeConstraintPuzzle constr = return $ varExpression $ WholePiece constr

atomicConstraintPuzzle :: forall (ground :: GroundTypeKind) a. AtomicConstraint ground a -> Puzzle ground a
atomicConstraintPuzzle ac = varExpression $ AtomicPiece ac

atomicPuzzle ::
    forall (ground :: GroundTypeKind) polarity v t.
    Is PolarityType polarity =>
    TypeVarT v ->
    FlipType ground polarity t ->
    Puzzle ground (PolarShimType (DolanShim ground) polarity t v)
atomicPuzzle var ft = atomicConstraintPuzzle $ mkAtomicConstraint var ft

flipUnifyPuzzle ::
    forall (ground :: GroundTypeKind) a b.
    IsDolanGroundType ground =>
    FlipType ground 'Positive a ->
    FlipType ground 'Negative b ->
    DolanRenameTypeM ground (Puzzle ground (DolanShim ground a b))
flipUnifyPuzzle fta ftb = wholeConstraintPuzzle $ MkWholeConstraint fta ftb

puzzleUnify ::
    forall (ground :: GroundTypeKind) pola polb a b.
    (IsDolanGroundType ground, Is PolarityType pola, Is PolarityType polb) =>
    DolanType ground pola a ->
    DolanType ground polb b ->
    DolanRenameTypeM ground (Puzzle ground (DolanShim ground a b))
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
    forall (ground :: GroundTypeKind) a.
    () =>
    Puzzle ground a ->
    PuzzleExpression ground a
puzzleExpression = solverExpressionLiftType

puzzleExpressionUnify ::
    forall (ground :: GroundTypeKind) pola polb a b.
    (IsDolanGroundType ground, Is PolarityType pola, Is PolarityType polb) =>
    DolanType ground pola a ->
    DolanType ground polb b ->
    DolanRenameTypeM ground (PuzzleExpression ground (DolanShim ground a b))
puzzleExpressionUnify ta tb = fmap puzzleExpression $ puzzleUnify ta tb

puzzleUnifySingular ::
    forall (ground :: GroundTypeKind) pola polb a b.
    (IsDolanGroundType ground, Is PolarityType pola, Is PolarityType polb) =>
    DolanSingularType ground pola a ->
    DolanSingularType ground polb b ->
    DolanRenameTypeM ground (Puzzle ground (DolanShim ground a b))
puzzleUnifySingular ta tb =
    fmap (fmap (\conv -> iJoinMeetL1 @_ @polb . conv . iJoinMeetR1 @_ @pola))
        $ puzzleUnify (singleDolanType ta) (singleDolanType tb)
