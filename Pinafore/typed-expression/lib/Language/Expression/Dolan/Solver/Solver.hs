module Language.Expression.Dolan.Solver.Solver
    ( solvePuzzle
    , rigidSolvePuzzle
    ) where

import Language.Expression.Common
import Language.Expression.Dolan.Solver.AtomicSubstitute
import Language.Expression.Dolan.Solver.Crumble.Subsume
import Language.Expression.Dolan.Solver.Crumble.Unify
import Language.Expression.Dolan.Solver.CrumbleM
import Language.Expression.Dolan.Solver.Puzzle
import Language.Expression.Dolan.Solver.WholeConstraint
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

crumblePuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => Puzzle ground a
    -> CrumbleM ground (DolanOpenExpression ground a, [SolverBisubstitution ground])
crumblePuzzle (ClosedExpression a) = return (pure a, [])
crumblePuzzle puzzle =
    partitionExpression purePiece puzzle $ \upuzzle spuzzle -> do
        (exprba, usubs) <- solveUnifyPuzzle upuzzle
        spuzzle' <- liftResultToCrumbleM $ bisubstitutesPuzzle usubs spuzzle
        (MkSolverExpression spuzzle'' exprb, ssubs) <- subsumePuzzleStep spuzzle'
        (exprc, rsubs) <- crumblePuzzle spuzzle''
        return (exprba <*> (exprb <*> exprc), usubs <> ssubs <> rsubs)

solvePuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => Puzzle ground a
    -> DolanTypeCheckM ground (DolanOpenExpression ground a, [SolverBisubstitution ground])
solvePuzzle puzzle = do
    rigidity <- renamerGetNameRigidity
    runCrumbleM rigidity $ crumblePuzzle puzzle

rigidSolvePuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => Puzzle ground a
    -> DolanTypeCheckM ground (DolanOpenExpression ground a)
rigidSolvePuzzle puzzle = fmap fst $ runCrumbleM (\_ -> RigidName) $ solveUnifyPuzzle puzzle
