module Language.Expression.Dolan.Unifier.Solver
    ( solvePuzzle
    , rigidSolvePuzzle
    , unifierSubtypeConversionAsGeneralAs
    ) where

import Language.Expression.Common
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Unifier.AtomicSubstitute
import Language.Expression.Dolan.Unifier.Crumble.Puzzle
import Language.Expression.Dolan.Unifier.Crumble.Type
import Language.Expression.Dolan.Unifier.FlipType
import Language.Expression.Dolan.Unifier.Puzzle
import Language.Expression.Dolan.Variance
import Shapes

processPuzzle ::
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => Puzzle ground a
    -> SolverM ground (DolanOpenExpression ground a)
processPuzzle (ClosedExpression a) = return $ pure a
processPuzzle puzzle = do
    MkSolverExpression ap expr <- lift $ crumblePuzzle puzzle
    puzzle' <- substituteAtomicPuzzle ap
    expr' <- processPuzzle puzzle'
    return $ expr <*> expr'

solvePuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => Puzzle ground a
    -> DolanTypeCheckM ground (DolanOpenExpression ground a, [UnifierBisubstitution ground])
solvePuzzle puzzle = do
    rigidity <- renamerGetNameRigidity
    (a, subs) <-
        runWriterT $ let
            ?rigidity = rigidity
            in processPuzzle puzzle
    return (a, subs)

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
