module Language.Expression.Dolan.Solver.Solver
    ( solvePuzzle
    , rigidSolvePuzzle
    , unifierSubtypeConversionAsGeneralAs
    ) where

import Language.Expression.Common
import Language.Expression.Dolan.Solver.AtomicSubstitute
import Language.Expression.Dolan.Solver.Crumble.Puzzle
import Language.Expression.Dolan.Solver.Crumble.Type
import Language.Expression.Dolan.Solver.FlipType
import Language.Expression.Dolan.Solver.Puzzle
import Language.Expression.Dolan.Solver.UnifierM
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Variance
import Shapes

processPuzzle ::
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => Puzzle ground a
    -> SolverM ground (DolanOpenExpression ground a)
processPuzzle = crumblePuzzle

solvePuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => Puzzle ground a
    -> DolanTypeCheckM ground (DolanOpenExpression ground a, [SolverBisubstitution ground])
solvePuzzle puzzle =
    partitionExpression purePiece puzzle $ \upuzzle spuzzle -> do
        rigidity <- renamerGetNameRigidity
    -- (_a, subs) <-
        (exprba, usubs) <-
            runWriterT $ let
                ?rigidity = rigidity
                in processPuzzle upuzzle
        spuzzle' <- lift $ runUnifierM $ bisubstitutesPuzzle usubs spuzzle
        (exprb, ssubs) <-
            runWriterT $ let
                ?rigidity = rigidity
                in processPuzzle spuzzle'
        return (exprba <*> exprb, usubs <> ssubs)

rigidSolvePuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => Puzzle ground a
    -> DolanTypeCheckM ground (DolanOpenExpression ground a)
rigidSolvePuzzle puzzle =
    fmap fst $
    runWriterT $ let
        ?rigidity = \_ -> RigidName
        in processPuzzle puzzle

unifierSubtypeConversionAsGeneralAs ::
       forall (ground :: GroundTypeKind) (dva :: DolanVariance) (gta :: DolanVarianceKind dva) (dvb :: DolanVariance) (gtb :: DolanVarianceKind dvb).
       IsDolanSubtypeGroundType ground
    => SubtypeConversion ground dva gta dvb gtb
    -> SubtypeConversion ground dva gta dvb gtb
    -> DolanM ground Bool
unifierSubtypeConversionAsGeneralAs = makeSCAGA (\p -> fmap fst $ solvePuzzle p)
