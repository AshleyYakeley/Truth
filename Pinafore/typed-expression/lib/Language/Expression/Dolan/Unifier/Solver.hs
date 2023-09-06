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
import Language.Expression.Dolan.Unifier.Crumble
import Language.Expression.Dolan.Unifier.FlipType
import Language.Expression.Dolan.Unifier.Puzzle
import Language.Expression.Dolan.Unifier.UnifierM
import Language.Expression.Dolan.Unifier.WholeConstraint
import Shapes

type SolverM :: GroundTypeKind -> Type -> Type
type SolverM ground = WriterT [UnifierBisubstitution ground] (DolanTypeCheckM ground)

type Solver :: GroundTypeKind -> Type -> Type
newtype Solver ground a = MkSolver
    { unSolver :: forall (rlist :: [Type]).
                          ReaderT (ListType (WholeConstraint ground) rlist) (SolverM ground) (DolanOpenExpression ground (ListProduct rlist -> a))
    }

instance forall (ground :: GroundTypeKind). Functor (DolanM ground) => Functor (Solver ground) where
    fmap ab (MkSolver ruha) = MkSolver $ (fmap $ fmap $ fmap ab) ruha

instance forall (ground :: GroundTypeKind). Monad (DolanM ground) => Applicative (Solver ground) where
    pure a = MkSolver $ pure $ pure $ pure a
    MkSolver ruhab <*> MkSolver ruha =
        MkSolver $ (\uhab uha -> (\hab ha h -> hab h $ ha h) <$> uhab <*> uha) <$> ruhab <*> ruha

instance forall (ground :: GroundTypeKind). MonadPlus (DolanM ground) => Alternative (Solver ground) where
    empty = MkSolver empty
    MkSolver p <|> MkSolver q = MkSolver $ p <|> q

instance forall (ground :: GroundTypeKind). Monad (DolanM ground) => WrappedApplicative (Solver ground) where
    type WAInnerM (Solver ground) = SolverM ground
    wexec msa =
        MkSolver $ do
            MkSolver sa <- lift $ msa
            sa
    whoist mm (MkSolver sb) = MkSolver $ hoist mm sb

runSolver ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => Solver ground a
    -> DolanTypeCheckM ground (DolanOpenExpression ground a, [UnifierBisubstitution ground])
runSolver (MkSolver rma) = runWriterT $ fmap (fmap $ \ua -> ua ()) $ runReaderT rma NilListType

-- | For debugging only. Switching this off will cause the solver to hang on recursive puzzles.
solveRecursive :: Bool
solveRecursive = True

puzzleSolverPiece ::
       forall (ground :: GroundTypeKind) a b. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => Piece ground a
    -> Puzzle ground (a -> b)
    -> Solver ground b
puzzleSolverPiece piece puzzlerest =
    MkSolver $ do
        (MkSolverExpression conspuzzle rexpr, substsout, bisubs) <- lift $ lift $ solvePiece piece
        lift $ tell bisubs
        puzzlerest' <-
            lift $ lift $ lift $ runUnifierM $ applyChangesToPuzzle (fmap substituteAtomicChange substsout) puzzlerest
        oexpr <- unSolver $ puzzleSolver $ liftA2 (,) conspuzzle puzzlerest'
        return $ liftA2 (\tt f l -> snd (f l) $ tt $ fst $ f l) rexpr oexpr

puzzleSolver ::
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => Puzzle ground a
    -> Solver ground a
puzzleSolver (ClosedExpression a) = pure a
puzzleSolver (OpenExpression piece@(WholePiece [] (wconstr@MkWholeConstraint {})) puzzlerest) =
    MkSolver $ do
        seen <- ask
        case lookUpListElement wconstr seen of
            Just lelem
                | solveRecursive -> do
                    oexpr <- unSolver $ puzzleSolver puzzlerest
                    return $ fmap (\lta l -> lta l (listProductGetElement lelem l)) oexpr
            _ ->
                withReaderT (\seen' -> ConsListType wconstr seen') $ do
                    expr <- unSolver $ puzzleSolverPiece piece $ fmap (\ab a -> (a, ab a)) puzzlerest
                    let
                        fixconv f l = let
                            ~(conv, a) = f (iLazy conv, l)
                            in a
                    return $ fmap fixconv expr
puzzleSolver (OpenExpression piece puzzlerest) = puzzleSolverPiece piece puzzlerest

solvePuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => Puzzle ground a
    -> DolanTypeCheckM ground (DolanOpenExpression ground a, [UnifierBisubstitution ground])
solvePuzzle puzzle = do
    rigidity <- renamerGetNameRigidity
    (a, subs) <-
        runSolver $ let
            ?rigidity = rigidity
            in puzzleSolver puzzle
    return (a, subs)

rigidSolvePuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => Puzzle ground a
    -> DolanTypeCheckM ground (DolanOpenExpression ground a)
rigidSolvePuzzle puzzle =
    fmap fst $
    runSolver $ let
        ?rigidity = \_ -> RigidName
        in puzzleSolver puzzle
