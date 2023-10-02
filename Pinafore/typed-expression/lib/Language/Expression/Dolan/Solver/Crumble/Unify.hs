module Language.Expression.Dolan.Solver.Crumble.Unify
    ( solveUnifyPuzzle
    ) where

import Data.Shim
import Language.Expression.Common
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

type UnifyCrumbler (ground :: GroundTypeKind)
     = Crumbler (WholeConstraint ground) (SolverM ground) (DolanOpenExpression ground)

solvePiece ::
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => Piece ground a
    -> SolverM ground (PuzzleExpression ground a)
solvePiece (WholePiece constr) =
    lift $ do
        exprs <- crumbleConstraint constr
        case exprs of
            expr :| [] -> return expr
            _ ->
                case constr of
                    MkWholeConstraint fta ftb -> throw $ ConvertTypeError fta ftb
solvePiece (AtomicPiece ac) = fmap pure $ solveAtomicConstraint ac

substituteEachMemo ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => [Substitution ground]
    -> UnifyCrumbler ground a
    -> UnifyCrumbler ground a
substituteEachMemo [] = id
substituteEachMemo substs = let
    bisubs = fmap substBisubstitution substs
    in mapEachMemo $ \wc -> do
           MkShimWit wc' conv <- lift $ liftResultToCrumbleM $ bisubstitutesWholeConstraintShim bisubs $ mkShimWit wc
           return $ MkShimWit wc' $ isoForwards conv

processPiece ::
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => Piece ground a
    -> UnifyCrumbler ground a
processPiece piece =
    MkCrumbler $ do
        MkSolverExpression conspuzzle rexpr <- lift $ solvePiece piece
        oexpr <- unCrumbler $ processPuzzle conspuzzle
        return $ liftA2 (\ta lt l -> ta $ lt l) rexpr oexpr

processRest ::
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => [Substitution ground]
    -> Puzzle ground a
    -> UnifyCrumbler ground a
processRest substs puzzlerest =
    MkCrumbler $ do
        puzzlerest' <- lift $ lift $ liftResultToCrumbleM $ applySubstsToPuzzle substs puzzlerest
        unCrumbler $ substituteEachMemo substs $ processPuzzle puzzlerest'

monoReaderHoist ::
       forall r ma mb a b. (Monad ma, Monad mb)
    => (ma a -> mb b)
    -> (ReaderT r ma a -> ReaderT r mb b)
monoReaderHoist mm (ReaderT rma) = ReaderT $ \r -> mm $ rma r

strict :: Bool
strict = True

processPieceAndRest ::
       forall (ground :: GroundTypeKind) a b. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => Piece ground a
    -> Puzzle ground (a -> b)
    -> UnifyCrumbler ground b
processPieceAndRest piece puzzlerest =
    MkCrumbler $ do
        (MkSolverExpression conspuzzle rexpr, substs) <- lift $ lift $ runWriterT $ solvePiece piece
        lift $ tell substs
        unCrumbler $
            substituteEachMemo substs $
            MkCrumbler $ do
                puzzlerest' <- lift $ lift $ liftResultToCrumbleM $ applySubstsToPuzzle substs puzzlerest
                oexpr <- unCrumbler $ processPuzzle $ liftA2 (,) conspuzzle puzzlerest'
                return $ liftA2 (\tt f l -> snd (f l) $ tt $ fst $ f l) rexpr oexpr

processPuzzle ::
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => Puzzle ground a
    -> UnifyCrumbler ground a
processPuzzle (ClosedExpression a) = pure a
processPuzzle (OpenExpression piece puzzlerest)
    | strict =
        MkCrumbler $ do
            (aexpr, substs) <-
                monoReaderHoist listen $
                unCrumbler $
                case piece of
                    WholePiece wconstr@MkWholeConstraint {} -> memoise iLazy wconstr $ processPiece piece
                    _ -> processPiece piece
            rexpr <- unCrumbler $ processRest substs puzzlerest
            return $ liftA2 (\lt lta -> lta <*> lt) aexpr rexpr
processPuzzle (OpenExpression piece@(WholePiece wconstr@MkWholeConstraint {}) puzzlerest) =
    memoiseBranch iLazy wconstr (processPuzzle puzzlerest) $
    processPieceAndRest piece $ fmap (\ta t -> (t, ta t)) puzzlerest
processPuzzle (OpenExpression piece puzzlerest) = processPieceAndRest piece puzzlerest

solveUnifyPuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => (String -> NameRigidity)
    -> Puzzle ground a
    -> DolanTypeCheckM ground (DolanOpenExpression ground a, [SolverBisubstitution ground])
solveUnifyPuzzle rigidity puzzle = let
    ?rigidity = rigidity
    in do
           (a, substs) <- runCrumbleM $ runWriterT $ runCrumbler $ processPuzzle puzzle
           return (a, fmap substBisubstitution substs)
