module Language.Expression.Dolan.Solver.Solve
    ( solvePuzzle
    ) where

import Data.Graph (SCC(..), stronglyConnComp)
import Language.Expression.Common
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.FreeVars
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

solvePuzzle1 ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => Puzzle ground a
    -> CrumbleM ground (DolanOpenExpression ground a, [SolverBisubstitution ground], [SolverBisubstitution ground])
solvePuzzle1 (ClosedExpression a) = return (pure a, [], [])
solvePuzzle1 puzzle =
    partitionExpressionM separatePiece puzzle $ \upuzzle spuzzle -> do
        (exprba, usubs) <- solveUnifyPuzzle upuzzle
        spuzzle' <- applyBisubsToPuzzle usubs spuzzle
        (MkSolverExpression spuzzle'' exprb, ssubs) <- subsumePuzzleStep spuzzle'
        (exprc, rusubs, rssubs) <- solvePuzzle1 spuzzle''
        return (exprba <*> (exprb <*> exprc), usubs <> rusubs, ssubs <> rssubs)

sortSubs ::
       forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground
    => [SolverBisubstitution ground]
    -> CrumbleM ground [SolverBisubstitution ground]
sortSubs [] = return []
sortSubs [sub] = return [sub]
sortSubs subs = let
    toNode :: SolverBisubstitution ground -> (SolverBisubstitution ground, String, [String])
    toNode sub@(MkBisubstitution var mtp mtn) =
        (sub, typeVarName var, fmap someTypeVarName $ toList $ freeTypeVariables mtp <> freeTypeVariables mtn)
    sccs :: [SCC (SolverBisubstitution ground)]
    sccs = reverse $ stronglyConnComp $ fmap toNode subs
    sccToSub :: SCC (SolverBisubstitution ground) -> CrumbleM ground (SolverBisubstitution ground)
    sccToSub (AcyclicSCC bisub) = return bisub
    sccToSub (CyclicSCC _) = throw $ InternalTypeError @ground "bad subsumption substitutions"
    in for sccs sccToSub

solvePuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => Puzzle ground a
    -> CrumbleM ground (DolanOpenExpression ground a, [SolverBisubstitution ground])
solvePuzzle (ClosedExpression a) = return (pure a, [])
solvePuzzle puzzle = do
    (expr, usubs, ssubs) <- solvePuzzle1 puzzle
    ssubs' <- sortSubs ssubs
    return (expr, usubs <> ssubs')
