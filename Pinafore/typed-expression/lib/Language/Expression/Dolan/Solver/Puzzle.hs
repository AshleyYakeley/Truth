module Language.Expression.Dolan.Solver.Puzzle where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Solver.AtomicConstraint
import Language.Expression.Dolan.Solver.FlipType
import Language.Expression.Dolan.Solver.UnifierM
import Language.Expression.Dolan.Solver.WholeConstraint
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

type Piece :: GroundTypeKind -> Type -> Type
data Piece ground t where
    WholePiece :: forall (ground :: GroundTypeKind) t. WholeConstraint ground t -> Piece ground t
    AtomicPiece :: forall (ground :: GroundTypeKind) t. AtomicConstraint ground t -> Piece ground t

instance forall (ground :: GroundTypeKind) t. IsDolanGroundType ground => Show (Piece ground t) where
    show (WholePiece wc) = "whole: " <> show wc
    show (AtomicPiece ac) = "atomic: " <> show ac

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => AllConstraint Show (Piece ground) where
    allConstraint = Dict

type Puzzle :: GroundTypeKind -> Type -> Type
type Puzzle ground = Expression (Piece ground)

wholeConstraintPuzzle :: forall (ground :: GroundTypeKind) a. WholeConstraint ground a -> Puzzle ground a
wholeConstraintPuzzle constr = varExpression $ WholePiece constr

atomicConstraintPuzzle :: forall (ground :: GroundTypeKind) a. AtomicConstraint ground a -> Puzzle ground a
atomicConstraintPuzzle ac = varExpression $ AtomicPiece ac

flipUnifyPuzzle ::
       forall (ground :: GroundTypeKind) a b.
       FlipType ground 'Positive a
    -> FlipType ground 'Negative b
    -> Puzzle ground (DolanShim ground a b)
flipUnifyPuzzle fta ftb = wholeConstraintPuzzle $ MkWholeConstraint fta ftb

puzzleUnify ::
       forall (ground :: GroundTypeKind) pola polb a b. (Is PolarityType pola, Is PolarityType polb)
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
       forall (ground :: GroundTypeKind) pola polb a b. (Is PolarityType pola, Is PolarityType polb)
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

bisubSubstitution ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => UnifierBisubstitution ground
    -> AtomicConstraint ground a
    -> UnifierM ground (Puzzle ground a)
bisubSubstitution bisub@(MkBisubstitution oldvar _ mwq) (MkAtomicConstraint depvar PositiveType ftw _)
    | Just Refl <- testEquality oldvar depvar = do
        MkShimWit tq (MkPolarMap convq) <- mwq
        MkShimWit ftw' (MkPolarMap convw) <- bisubstituteFlipType bisub ftw
        return $ fmap (\conv -> convq . conv . convw) $ flipUnifyPuzzle ftw' (NormalFlipType tq)
bisubSubstitution bisub@(MkBisubstitution oldvar mwp _) (MkAtomicConstraint depvar NegativeType ftw _)
    | Just Refl <- testEquality oldvar depvar = do
        MkShimWit tp (MkPolarMap convp) <- mwp
        MkShimWit ftw' (MkPolarMap convw) <- bisubstituteFlipType bisub ftw
        return $ fmap (\conv -> convw . conv . convp) $ flipUnifyPuzzle (NormalFlipType tp) ftw'
bisubSubstitution _ ac
    | Just conv <- isPureAtomicConstraint ac = return $ pure conv
bisubSubstitution bisub (MkAtomicConstraint depvar PositiveType (NormalFlipType tw) _) = do
    MkShimWit tp (MkPolarMap conv) <- bisubstituteType bisub tw
    return $ fmap (\pv -> pv . conv) $ atomicConstraintPuzzle $ mkAtomicConstraint depvar $ NormalFlipType tp
bisubSubstitution bisub (MkAtomicConstraint depvar NegativeType (NormalFlipType tw) _) = do
    MkShimWit tp (MkPolarMap conv) <- bisubstituteType bisub tw
    return $ fmap (\pv -> conv . pv) $ atomicConstraintPuzzle $ mkAtomicConstraint depvar $ NormalFlipType tp
bisubSubstitution _ ac = return $ atomicConstraintPuzzle ac

bisubstitutesWholeConstraint ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => [UnifierBisubstitution ground]
    -> WholeConstraint ground a
    -> UnifierM ground (Puzzle ground a)
bisubstitutesWholeConstraint bisubs wc = do
    MkShimWit wc' (MkCatDual conv) <- bisubstitutesWholeConstraintShim bisubs $ mkShimWit wc
    return $ fmap conv $ wholeConstraintPuzzle wc'

bisubstituteAtomicConstraint ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => UnifierBisubstitution ground
    -> AtomicConstraint ground a
    -> UnifierM ground (Puzzle ground a)
bisubstituteAtomicConstraint = bisubSubstitution

bisubstitutesAtomicConstraint ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => [UnifierBisubstitution ground]
    -> AtomicConstraint ground a
    -> UnifierM ground (Puzzle ground a)
bisubstitutesAtomicConstraint [] ac = return $ atomicConstraintPuzzle ac
bisubstitutesAtomicConstraint (s:ss) ac = do
    puzzle <- bisubstituteAtomicConstraint s ac
    bisubstitutesPuzzle ss puzzle

bisubstitutesPiece ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => [UnifierBisubstitution ground]
    -> Piece ground a
    -> UnifierM ground (Puzzle ground a)
bisubstitutesPiece newchanges (WholePiece wc) = bisubstitutesWholeConstraint newchanges wc
bisubstitutesPiece newchanges (AtomicPiece ac) = bisubstitutesAtomicConstraint newchanges ac

bisubstitutesPuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => [UnifierBisubstitution ground]
    -> Puzzle ground a
    -> UnifierM ground (Puzzle ground a)
bisubstitutesPuzzle substs = mapExpressionWitnessesM $ bisubstitutesPiece substs
