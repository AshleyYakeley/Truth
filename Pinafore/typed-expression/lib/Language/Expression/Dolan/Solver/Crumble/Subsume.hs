module Language.Expression.Dolan.Solver.Crumble.Subsume
    ( solveSubsumePuzzle
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Solver.AtomicConstraint
import Language.Expression.Dolan.Solver.AtomicSubstitute
import Language.Expression.Dolan.Solver.Crumble.Crumbler
import Language.Expression.Dolan.Solver.Crumble.Type
import Language.Expression.Dolan.Solver.Puzzle
import Language.Expression.Dolan.Solver.WholeConstraint
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeResult
import Language.Expression.Dolan.TypeSystem
import Shapes

type SubsumeCrumbler (ground :: GroundTypeKind)
     = Crumbler (WholeConstraint ground) (CrumbleM ground) (AtomicPuzzleExpression ground)

processPiece ::
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => Piece ground a
    -> SubsumeCrumbler ground a
processPiece (AtomicPiece ac) = crumblerLift $ solverExpressionLiftType $ varExpression ac
processPiece (WholePiece constr@MkWholeConstraint {}) =
    memoise iLazy constr $
    MkCrumbler $ do
        pexprs <- lift $ crumbleConstraint constr
        MkWUnlift unlift <- askUnlift
        lift $
            forFirstCrumbleM pexprs $ \(MkSolverExpression puzzle expr) -> do
                oexpr <- unlift $ unCrumbler $ processPuzzle puzzle
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
    (t, subst) <- lift $ liftToCrumbleM $ getAtomicConstraint ac
    tell [subst]
    puzzle <- mapExpressionWitnessesM (\ac' -> lift $ liftResultToCrumbleM $ substituteAtomicConstraint subst ac') expr
    return $ fmap (\ta -> ta t) puzzle

completePuzzle ::
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => Puzzle ground a
    -> SolverM ground (DolanOpenExpression ground a)
completePuzzle (ClosedExpression a) = return $ pure a
completePuzzle puzzle = do
    MkSolverExpression ap expr <- lift $ runCrumbler $ processPuzzle puzzle
    puzzle' <- substPuzzle ap
    oexpr <- completePuzzle puzzle'
    return $ expr <*> oexpr

solveSubsumePuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => (String -> NameRigidity)
    -> Puzzle ground a
    -> DolanTypeCheckM ground (DolanOpenExpression ground a, [SolverBisubstitution ground])
solveSubsumePuzzle _ (ClosedExpression a) = return (pure a, [])
solveSubsumePuzzle rigidity puzzle = let
    ?rigidity = rigidity
    in do
           (a, substs) <- runCrumbleM $ runWriterT $ completePuzzle puzzle
           return (a, fmap substBisubstitution substs)
