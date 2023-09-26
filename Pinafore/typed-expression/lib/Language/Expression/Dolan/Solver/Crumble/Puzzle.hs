module Language.Expression.Dolan.Solver.Crumble.Puzzle
    ( crumblePuzzle
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Solver.AtomicSubstitute
import Language.Expression.Dolan.Solver.Crumble.Crumbler
import Language.Expression.Dolan.Solver.Crumble.Type
import Language.Expression.Dolan.Solver.FlipType
import Language.Expression.Dolan.Solver.Puzzle
import Language.Expression.Dolan.Solver.UnifierM
import Language.Expression.Dolan.Solver.WholeConstraint
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

type PuzzleCrumbler (ground :: GroundTypeKind)
     = Crumbler (WholeConstraint ground) (SolverM ground) (DolanOpenExpression ground)

solvePiece ::
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => Piece ground a
    -> SolverM ground (PuzzleExpression ground a)
solvePiece (WholePiece constr) = lift $ crumbleConstraint constr
solvePiece (AtomicPiece ac) = fmap pure $ substituteAtomicConstraint ac

bisubstituteEachMemo ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => [SolverBisubstitution ground]
    -> PuzzleCrumbler ground a
    -> PuzzleCrumbler ground a
bisubstituteEachMemo [] = id
bisubstituteEachMemo bisubs =
    mapEachMemo $ \wc -> do
        MkShimWit wc' conv <- lift $ lift $ runUnifierM $ bisubstitutesWholeConstraintShim bisubs $ mkShimWit wc
        return $ MkShimWit wc' $ isoForwards conv

processPieceAndRest ::
       forall (ground :: GroundTypeKind) a b. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => Piece ground a
    -> Puzzle ground (a -> b)
    -> PuzzleCrumbler ground b
processPieceAndRest piece puzzlerest =
    MkCrumbler $ do
        (MkSolverExpression conspuzzle rexpr, substs) <- lift $ lift $ runWriterT $ solvePiece piece
        lift $ tell substs
        let
            doRest :: PuzzleCrumbler ground b
            doRest =
                MkCrumbler $ do
                    puzzlerest' <- lift $ lift $ lift $ runUnifierM $ applySubstsToPuzzle substs puzzlerest
                    oexpr <- unCrumbler $ processPuzzle $ liftA2 (,) conspuzzle puzzlerest'
                    return $ liftA2 (\tt f l -> snd (f l) $ tt $ fst $ f l) rexpr oexpr
        unCrumbler $ bisubstituteEachMemo (fmap substBisubstitution substs) doRest -- causes broken shims

processPuzzle ::
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => Puzzle ground a
    -> PuzzleCrumbler ground a
processPuzzle (ClosedExpression a) = crumblerPure a
processPuzzle (OpenExpression piece@(WholePiece wconstr@MkWholeConstraint {}) puzzlerest) =
    memoise iLazy wconstr (processPuzzle puzzlerest) $ processPieceAndRest piece $ fmap (\ta t -> (t, ta t)) puzzlerest
processPuzzle (OpenExpression piece puzzlerest) = processPieceAndRest piece puzzlerest

crumblePuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => (String -> NameRigidity)
    -> Puzzle ground a
    -> DolanTypeCheckM ground (DolanOpenExpression ground a, [SolverBisubstitution ground])
crumblePuzzle rigidity puzzle = let
    ?rigidity = rigidity
    in do
           (a, substs) <- runWriterT $ runCrumbler $ processPuzzle puzzle
           return (a, fmap substBisubstitution substs)
