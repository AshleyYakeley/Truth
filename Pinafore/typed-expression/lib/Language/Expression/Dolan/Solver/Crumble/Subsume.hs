module Language.Expression.Dolan.Solver.Crumble.Subsume
    ( solveSubsumePuzzle
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Solver.AtomicConstraint
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

type SubsumeCrumbler (ground :: GroundTypeKind)
     = Crumbler (WholeConstraint ground) (SolverM ground) (AtomicPuzzleExpression ground)

processPiece ::
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => Piece ground a
    -> SubsumeCrumbler ground a
processPiece (AtomicPiece ac) = crumblerLift $ solverExpressionLiftType $ varExpression ac
processPiece (WholePiece wc@MkWholeConstraint {}) =
    memoise iLazy wc $
    MkCrumbler $ do
        MkSolverExpression puzzle expr <- lift $ lift $ crumbleConstraint wc
        oexpr <- unCrumbler $ processPuzzle puzzle
        return $ liftA2 (\ts lt l -> ts $ lt l) (solverExpressionLiftValue expr) oexpr

processPuzzle ::
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => Puzzle ground a
    -> SubsumeCrumbler ground a
processPuzzle = solveExpression processPiece

substPuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => Expression (AtomicConstraint ground) a
    -> SolverM ground (Puzzle ground a)
substPuzzle (ClosedExpression a) = return $ pure a
substPuzzle (OpenExpression ac expr) = do
    (t, subst) <- lift $ getAtomicConstraint ac
    tell [subst]
    puzzle <- mapExpressionWitnessesM (\ac' -> lift $ lift $ runUnifierM $ substituteAtomicConstraint subst ac') expr
    return $ fmap (\ta -> ta t) puzzle

completePuzzle ::
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => Puzzle ground a
    -> SolverM ground (DolanOpenExpression ground a)
completePuzzle (ClosedExpression a) = return $ pure a
completePuzzle puzzle = do
    MkSolverExpression ap expr <- runCrumbler $ processPuzzle puzzle
    puzzle' <- substPuzzle ap
    oexpr <- completePuzzle puzzle'
    return $ expr <*> oexpr

solveSubsumePuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => (String -> NameRigidity)
    -> Puzzle ground a
    -> DolanTypeCheckM ground (DolanOpenExpression ground a, [SolverBisubstitution ground])
solveSubsumePuzzle rigidity puzzle = let
    ?rigidity = rigidity
    in do
           (a, substs) <- runWriterT $ completePuzzle puzzle
           return (a, fmap substBisubstitution substs)
