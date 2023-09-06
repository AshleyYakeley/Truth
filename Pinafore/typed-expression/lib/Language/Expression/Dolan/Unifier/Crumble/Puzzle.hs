module Language.Expression.Dolan.Unifier.Crumble.Puzzle
    ( crumblePuzzle
    ) where

import Control.Applicative.Wrapped
import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Unifier.AtomicConstraint
import Language.Expression.Dolan.Unifier.Crumble.Type
import Language.Expression.Dolan.Unifier.Puzzle
import Language.Expression.Dolan.Unifier.WholeConstraint
import Shapes

type PuzzleCrumbler :: GroundTypeKind -> [Type] -> Type -> Type
newtype PuzzleCrumbler ground rlist a = MkPuzzleCrumbler
    { unPuzzleCrumbler :: ReaderT (ListType (WholeConstraint ground) rlist) (DolanTypeCheckM ground) (AtomicPuzzleExpression ground (ListProduct rlist -> a))
    }

instance forall (ground :: GroundTypeKind) rlist. Functor (DolanM ground) => Functor (PuzzleCrumbler ground rlist) where
    fmap ab (MkPuzzleCrumbler ruha) = MkPuzzleCrumbler $ (fmap $ fmap $ fmap ab) ruha

instance forall (ground :: GroundTypeKind) rlist. Monad (DolanM ground) => Applicative (PuzzleCrumbler ground rlist) where
    pure a = MkPuzzleCrumbler $ pure $ pure $ pure a
    MkPuzzleCrumbler ruhab <*> MkPuzzleCrumbler ruha = MkPuzzleCrumbler $ liftA2 (liftA2 (<*>)) ruhab ruha

instance forall (ground :: GroundTypeKind) rlist. MonadPlus (DolanM ground) => Alternative (PuzzleCrumbler ground rlist) where
    empty = MkPuzzleCrumbler empty
    MkPuzzleCrumbler p <|> MkPuzzleCrumbler q = MkPuzzleCrumbler $ p <|> q

instance forall (ground :: GroundTypeKind) rlist. Monad (DolanM ground) =>
             WrappedApplicative (PuzzleCrumbler ground rlist) where
    type WAInnerM (PuzzleCrumbler ground rlist) = DolanTypeCheckM ground
    wexec msa =
        MkPuzzleCrumbler $ do
            MkPuzzleCrumbler sa <- lift $ msa
            sa
    whoist mm (MkPuzzleCrumbler sb) = MkPuzzleCrumbler $ hoist mm sb

runPuzzleCrumbler ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => PuzzleCrumbler ground '[] a
    -> DolanTypeCheckM ground (AtomicPuzzleExpression ground a)
runPuzzleCrumbler (MkPuzzleCrumbler rma) = fmap (fmap $ \ua -> ua ()) $ runReaderT rma NilListType

solverAsk ::
       forall (ground :: GroundTypeKind) rlist a. IsDolanSubtypeGroundType ground
    => ListElementType rlist a
    -> PuzzleCrumbler ground rlist a
solverAsk lelem = MkPuzzleCrumbler $ return $ pure $ listProductGetElement lelem

addConstraint ::
       forall (ground :: GroundTypeKind) rlist t a. IsDolanSubtypeGroundType ground
    => WholeConstraint ground t
    -> PuzzleCrumbler ground (t ': rlist) a
    -> PuzzleCrumbler ground rlist (t -> a)
addConstraint wconstr solver =
    MkPuzzleCrumbler $
    withReaderT (\seen' -> ConsListType wconstr seen') $
    fmap (fmap $ \tla l t -> tla (t, l)) $ unPuzzleCrumbler $ solver

addFixConstraint ::
       forall (ground :: GroundTypeKind) rlist t a. IsDolanSubtypeGroundType ground
    => WholeConstraint ground t
    -> PuzzleCrumbler ground (t ': rlist) (t, a)
    -> PuzzleCrumbler ground rlist a
addFixConstraint (wconstr@MkWholeConstraint {}) solver = let
    fixconv f = let
        ~(conv, a) = f $ iLazy conv
        in a
    in fmap fixconv $ addConstraint wconstr solver

puzzlePSolver ::
       forall (ground :: GroundTypeKind) rlist a. IsDolanSubtypeGroundType ground
    => AtomicPuzzle ground a
    -> PuzzleCrumbler ground rlist a
puzzlePSolver puzzle = MkPuzzleCrumbler $ pure $ fmap (\a _ -> a) $ solverExpressionLiftType puzzle

processPiece ::
       forall (ground :: GroundTypeKind) rlist a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => Piece ground a
    -> PuzzleCrumbler ground rlist a
processPiece (WholePiece wconstr) =
    MkPuzzleCrumbler $ do
        MkSolverExpression puzzle expr <- lift $ crumbleConstraint wconstr
        pexpr <- unPuzzleCrumbler $ processPuzzle puzzle
        return $ liftA2 (\lt ta -> ta . lt) pexpr (solverExpressionLiftValue expr)
processPiece (AtomicPiece ac) = puzzlePSolver $ varExpression ac

-- | For debugging only. Switching this off will cause the solver to hang on recursive puzzles.
solveRecursive :: Bool
solveRecursive = True

processPuzzle ::
       forall (ground :: GroundTypeKind) rlist a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => Puzzle ground a
    -> PuzzleCrumbler ground rlist a
processPuzzle (ClosedExpression a) = pure a
processPuzzle (OpenExpression piece@(WholePiece wconstr) puzzlerest) =
    MkPuzzleCrumbler $ do
        seen <- ask
        unPuzzleCrumbler $
            case lookUpListElement wconstr seen of
                Just lelem
                    | solveRecursive -> liftA2 (\conv f -> f conv) (solverAsk lelem) (processPuzzle puzzlerest)
                _ ->
                    addFixConstraint wconstr $
                    liftA2 (\t ta -> (t, ta t)) (processPiece piece) (processPuzzle puzzlerest)
processPuzzle (OpenExpression piece puzzlerest) = liftA2 (\a ab -> ab a) (processPiece piece) (processPuzzle puzzlerest)

crumblePuzzle ::
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => Puzzle ground a
    -> DolanTypeCheckM ground (AtomicPuzzleExpression ground a)
crumblePuzzle puzzle = runPuzzleCrumbler $ processPuzzle puzzle
