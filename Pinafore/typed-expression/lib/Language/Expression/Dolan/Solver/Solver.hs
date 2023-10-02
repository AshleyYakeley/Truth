module Language.Expression.Dolan.Solver.Solver
    ( solvePuzzle
    , rigidSolvePuzzle
    ) where

import Language.Expression.Common
import Language.Expression.Dolan.Solver.AtomicSubstitute
import Language.Expression.Dolan.Solver.Crumble.Subsume
import Language.Expression.Dolan.Solver.Crumble.Unify
import Language.Expression.Dolan.Solver.Puzzle
import Language.Expression.Dolan.Solver.WholeConstraint
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

solvePuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => Puzzle ground a
    -> DolanTypeCheckM ground (DolanOpenExpression ground a, [SolverBisubstitution ground])
solvePuzzle puzzle =
    partitionExpression purePiece puzzle $ \upuzzle spuzzle -> do
        rigidity <- renamerGetNameRigidity
        (exprba, usubs) <- solveUnifyPuzzle rigidity upuzzle
        spuzzle' <- lift $ runTypeResult $ bisubstitutesPuzzle usubs spuzzle
        (exprb, ssubs) <- solveSubsumePuzzle rigidity spuzzle'
        return (exprba <*> exprb, usubs <> ssubs)

rigidSolvePuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => Puzzle ground a
    -> DolanTypeCheckM ground (DolanOpenExpression ground a)
rigidSolvePuzzle puzzle = fmap fst $ solveUnifyPuzzle (\_ -> RigidName) puzzle
