module Language.Expression.Dolan.Solver.Solve
    ( solvePuzzle
    ) where

import Language.Expression.Common
import Language.Expression.Dolan.Solver.Crumble.Unify
import Language.Expression.Dolan.Solver.CrumbleM
import Language.Expression.Dolan.Solver.Puzzle
import Language.Expression.Dolan.Solver.WholeConstraint
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

solvePuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => Puzzle ground a
    -> CrumbleM ground (DolanOpenExpression ground a, [SolverBisubstitution ground])
solvePuzzle (ClosedExpression a) = return (pure a, [])
solvePuzzle puzzle = do
    upuzzle <- runExpressionM pieceToUnify puzzle
    solveUnifyPuzzle upuzzle
