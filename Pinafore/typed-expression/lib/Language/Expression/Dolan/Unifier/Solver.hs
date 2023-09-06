module Language.Expression.Dolan.Unifier.Solver
    ( solvePuzzle
    , rigidSolvePuzzle
    ) where

import Control.Applicative.Wrapped
import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Unifier.AtomicConstraint
import Language.Expression.Dolan.Unifier.Crumble
import Language.Expression.Dolan.Unifier.FlipType
import Language.Expression.Dolan.Unifier.Puzzle
import Language.Expression.Dolan.Unifier.Substitution
import Language.Expression.Dolan.Unifier.UnifierM
import Language.Expression.Dolan.Unifier.WholeConstraint
import Shapes

type SolverM :: GroundTypeKind -> Type -> Type
type SolverM ground = WriterT [UnifierBisubstitution ground] (DolanTypeCheckM ground)

type SolverA :: GroundTypeKind -> (Type -> Type) -> [Type] -> Type -> Type
type SolverA ground f rlist a
     = ReaderT (ListType (WholeConstraint ground) rlist) (SolverM ground) (f (ListProduct rlist -> a))

type Solver :: GroundTypeKind -> (Type -> Type) -> [Type] -> Type -> Type
newtype Solver ground f rlist a = MkSolver
    { unSolver :: SolverA ground f rlist a
    }

instance forall (ground :: GroundTypeKind) f rlist. (Functor (DolanM ground), Functor f) =>
             Functor (Solver ground f rlist) where
    fmap ab (MkSolver ruha) = MkSolver $ (fmap $ fmap $ fmap ab) ruha

instance forall (ground :: GroundTypeKind) f rlist. (Monad (DolanM ground), Applicative f) =>
             Applicative (Solver ground f rlist) where
    pure a = MkSolver $ pure $ pure $ pure a
    MkSolver ruhab <*> MkSolver ruha = MkSolver $ liftA2 (liftA2 (<*>)) ruhab ruha

instance forall (ground :: GroundTypeKind) f rlist. (MonadPlus (DolanM ground), Applicative f) =>
             Alternative (Solver ground f rlist) where
    empty = MkSolver empty
    MkSolver p <|> MkSolver q = MkSolver $ p <|> q

instance forall (ground :: GroundTypeKind) f rlist. (Monad (DolanM ground), Applicative f) =>
             WrappedApplicative (Solver ground f rlist) where
    type WAInnerM (Solver ground f rlist) = SolverM ground
    wexec msa =
        MkSolver $ do
            MkSolver sa <- lift $ msa
            sa
    whoist mm (MkSolver sb) = MkSolver $ hoist mm sb

runSolver ::
       forall (ground :: GroundTypeKind) f a. (IsDolanSubtypeGroundType ground, Applicative f)
    => Solver ground f '[] a
    -> SolverM ground (f a)
runSolver (MkSolver rma) = fmap (fmap $ \ua -> ua ()) $ runReaderT rma NilListType

solverAsk ::
       forall (ground :: GroundTypeKind) f rlist a. (IsDolanSubtypeGroundType ground, Applicative f)
    => ListElementType rlist a
    -> Solver ground f rlist a
solverAsk lelem = MkSolver $ return $ pure $ listProductGetElement lelem

addConstraint ::
       forall (ground :: GroundTypeKind) f rlist t a. (IsDolanSubtypeGroundType ground, Applicative f)
    => WholeConstraint ground t
    -> Solver ground f (t ': rlist) a
    -> Solver ground f rlist (t -> a)
addConstraint wconstr solver =
    MkSolver $
    withReaderT (\seen' -> ConsListType wconstr seen') $ fmap (fmap $ \tla l t -> tla (t, l)) $ unSolver $ solver

addFixConstraint ::
       forall (ground :: GroundTypeKind) f rlist t a. (IsDolanSubtypeGroundType ground, Applicative f)
    => WholeConstraint ground t
    -> Solver ground f (t ': rlist) (t, a)
    -> Solver ground f rlist a
addFixConstraint (wconstr@MkWholeConstraint {}) solver = let
    fixconv f = let
        ~(conv, a) = f $ iLazy conv
        in a
    in fmap fixconv $ addConstraint wconstr solver

applySubstToAtomicPuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => Substitution ground
    -> AtomicPuzzle ground a
    -> UnifierM ground (Puzzle ground a)
applySubstToAtomicPuzzle subst puzzle = mapExpressionWitnessesM (runAtomicChange $ substituteAtomicChange subst) puzzle

puzzleSolverPiece ::
       forall (ground :: GroundTypeKind) a b. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => AtomicConstraint ground a
    -> AtomicPuzzle ground (a -> b)
    -> SolverM ground (Puzzle ground b)
puzzleSolverPiece piece puzzlerest = do
    (a, subst, bisub) <- lift $ solveAtomicConstraint piece
    tell [bisub]
    puzzlerest' <- lift $ lift $ runUnifierM $ applySubstToAtomicPuzzle subst puzzlerest
    return $ fmap (\ab -> ab a) puzzlerest'

puzzleSolver ::
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => AtomicPuzzle ground a
    -> SolverM ground (Puzzle ground a)
puzzleSolver (ClosedExpression a) = return $ pure a
puzzleSolver (OpenExpression piece puzzlerest) = puzzleSolverPiece piece puzzlerest

type AtomicPuzzle :: GroundTypeKind -> Type -> Type
type AtomicPuzzle ground = Expression (AtomicConstraint ground)

type AtomicPuzzleExpression :: GroundTypeKind -> Type -> Type
type AtomicPuzzleExpression ground = TSOpenSolverExpression (DolanTypeSystem ground) (AtomicPuzzle ground)

puzzlePSolver ::
       forall (ground :: GroundTypeKind) rlist a. IsDolanSubtypeGroundType ground
    => AtomicPuzzle ground a
    -> Solver ground (AtomicPuzzleExpression ground) rlist a
puzzlePSolver puzzle = MkSolver $ pure $ fmap (\a _ -> a) $ solverExpressionLiftType puzzle

crumblePiece ::
       forall (ground :: GroundTypeKind) rlist a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => Piece ground a
    -> Solver ground (AtomicPuzzleExpression ground) rlist a
crumblePiece (WholePiece wconstr) =
    MkSolver $ do
        MkSolverExpression puzzle expr <- lift $ lift $ solveWholeConstraint wconstr
        pexpr <- unSolver $ crumblePuzzle puzzle
        return $ liftA2 (\lt ta -> ta . lt) pexpr (solverExpressionLiftValue expr)
crumblePiece (AtomicPiece ac) = puzzlePSolver $ varExpression ac

-- | For debugging only. Switching this off will cause the solver to hang on recursive puzzles.
solveRecursive :: Bool
solveRecursive = True

crumblePuzzle ::
       forall (ground :: GroundTypeKind) rlist a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => Puzzle ground a
    -> Solver ground (AtomicPuzzleExpression ground) rlist a
crumblePuzzle (ClosedExpression a) = pure a
crumblePuzzle (OpenExpression piece@(WholePiece wconstr) puzzlerest) =
    MkSolver $ do
        seen <- ask
        unSolver $
            case lookUpListElement wconstr seen of
                Just lelem
                    | solveRecursive -> liftA2 (\conv f -> f conv) (solverAsk lelem) (crumblePuzzle puzzlerest)
                _ ->
                    addFixConstraint wconstr $
                    liftA2 (\t ta -> (t, ta t)) (crumblePiece piece) (crumblePuzzle puzzlerest)
crumblePuzzle (OpenExpression piece puzzlerest) = liftA2 (\a ab -> ab a) (crumblePiece piece) (crumblePuzzle puzzlerest)

solvePuzzleAll ::
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => Puzzle ground a
    -> SolverM ground (DolanOpenExpression ground a)
solvePuzzleAll (ClosedExpression a) = return $ pure a
solvePuzzleAll puzzle = do
    MkSolverExpression ap expr <- runSolver $ crumblePuzzle puzzle
    puzzle' <- puzzleSolver ap
    expr' <- solvePuzzleAll puzzle'
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
            in solvePuzzleAll puzzle
    return (a, subs)

rigidSolvePuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => Puzzle ground a
    -> DolanTypeCheckM ground (DolanOpenExpression ground a)
rigidSolvePuzzle puzzle =
    fmap fst $
    runWriterT $ let
        ?rigidity = \_ -> RigidName
        in solvePuzzleAll puzzle
