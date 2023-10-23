module Language.Expression.Dolan.Solver.Solve
    ( solvePuzzle
    ) where

import Language.Expression.Common
import Language.Expression.Dolan.Solver.Crumble.Subsume
import Language.Expression.Dolan.Solver.Crumble.Unify
import Language.Expression.Dolan.Solver.CrumbleM
import Language.Expression.Dolan.Solver.Puzzle
import Language.Expression.Dolan.Solver.Substitute
import Language.Expression.Dolan.Solver.WholeConstraint
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeResult
import Language.Expression.Dolan.TypeSystem
import Shapes

separatePiece ::
       forall (ground :: GroundTypeKind) t. IsDolanSubtypeGroundType ground
    => Piece ground t
    -> CrumbleM ground (Either (UnifyPuzzle ground t) (Puzzle ground t))
separatePiece piece = do
    rupuzzle <- try $ pieceToUnify piece
    return $
        case rupuzzle of
            SuccessResult upuzzle -> Left upuzzle
            FailureResult (_ :: TypeError ground) -> Right $ varExpression piece

solvePuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => Puzzle ground a
    -> CrumbleM ground (DolanOpenExpression ground a, [SolverBisubstitution ground])
solvePuzzle (ClosedExpression a) = return (pure a, [])
solvePuzzle puzzle =
    partitionExpressionM separatePiece puzzle $ \upuzzle spuzzle -> do
        (exprba, usubs) <- solveUnifyPuzzle upuzzle
        spuzzle' <- bisubstitutesPuzzle usubs spuzzle
        (MkSolverExpression spuzzle'' exprb, ssubs) <- subsumePuzzleStep spuzzle'
        (exprc, rsubs) <- solvePuzzle spuzzle''
        return (exprba <*> (exprb <*> exprc), usubs <> ssubs <> rsubs)
