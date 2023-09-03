module Language.Expression.Dolan.Unifier.Solver
    ( solvePuzzle
    ) where

import Control.Applicative.Wrapped
import Language.Expression.Common
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Unifier.Crumble
import Language.Expression.Dolan.Unifier.Piece
import Language.Expression.Dolan.Unifier.Puzzle
import Language.Expression.Dolan.Unifier.UnifierM
import Shapes

type Solver :: GroundTypeKind -> Type -> Type
newtype Solver ground a = MkSolver
    { _unSolver :: forall (rlist :: [Type]).
                           ReaderT (ListType (PuzzlePiece ground) rlist) (SolverM ground) (DolanOpenExpression ground (ListProduct rlist -> a))
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

solverLiftExpression ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => DolanOpenExpression ground a
    -> Solver ground a
solverLiftExpression expr = MkSolver $ pure $ fmap (\a _ -> a) expr

puzzleSolver ::
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => Puzzle ground a
    -> Solver ground a
puzzleSolver (ClosedExpression a) = pure a
puzzleSolver (OpenExpression piece puzzle) =
    wexec $ do
        (MkSolverExpression conspuzzle expr, bisubs) <- lift $ solvePiece piece
        tell bisubs
        puzzle' <- lift $ lift $ runUnifierM $ bisubstitutesPuzzle bisubs puzzle
        return $
            liftA2 (\tt (t, ta) -> ta $ tt t) (solverLiftExpression expr) (puzzleSolver $ liftA2 (,) conspuzzle puzzle')

solvePuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => Puzzle ground a
    -> DolanTypeCheckM ground (DolanOpenExpression ground a, [UnifierBisubstitution ground])
solvePuzzle puzzle = do
    rigidity <- renamerGetNameRigidity
    runSolver $ let
        ?rigidity = rigidity
        in puzzleSolver puzzle
