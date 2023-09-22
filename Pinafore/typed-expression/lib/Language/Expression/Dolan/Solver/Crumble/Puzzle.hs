module Language.Expression.Dolan.Solver.Crumble.Puzzle
    ( crumblePuzzle
    ) where

import Control.Applicative.Wrapped
import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Solver.AtomicSubstitute
import Language.Expression.Dolan.Solver.Crumble.Type
import Language.Expression.Dolan.Solver.FlipType
import Language.Expression.Dolan.Solver.Puzzle
import Language.Expression.Dolan.Solver.UnifierM
import Language.Expression.Dolan.Solver.WholeConstraint
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

type PuzzleCrumbler :: GroundTypeKind -> [Type] -> Type -> Type
newtype PuzzleCrumbler ground rlist a = MkPuzzleCrumbler
    { unPuzzleCrumbler :: ReaderT (ListType (WholeConstraint ground) rlist) (SolverM ground) (DolanOpenExpression ground (ListProduct rlist -> a))
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
            MkPuzzleCrumbler sa <- lift $ lift msa
            sa
    whoist mm (MkPuzzleCrumbler sb) = MkPuzzleCrumbler $ hoist (hoist mm) sb

runPuzzleCrumbler ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => PuzzleCrumbler ground '[] a
    -> SolverM ground (DolanOpenExpression ground a)
runPuzzleCrumbler (MkPuzzleCrumbler rma) = fmap (fmap $ \ua -> ua ()) $ runReaderT rma NilListType

retrieveMemo ::
       forall (ground :: GroundTypeKind) rlist a. IsDolanSubtypeGroundType ground
    => ListElementType rlist a
    -> PuzzleCrumbler ground rlist a
retrieveMemo lelem = MkPuzzleCrumbler $ return $ pure $ listProductGetElement lelem

dropMemos ::
       forall (ground :: GroundTypeKind) rlist a. IsDolanSubtypeGroundType ground
    => PuzzleCrumbler ground '[] a
    -> PuzzleCrumbler ground rlist a
dropMemos pc = MkPuzzleCrumbler $ withReaderT (\_ -> NilListType) $ fmap (fmap $ \ua _ -> ua ()) $ unPuzzleCrumbler pc

addMemo ::
       forall (ground :: GroundTypeKind) rlist t a. IsDolanSubtypeGroundType ground
    => WholeConstraint ground t
    -> PuzzleCrumbler ground (t ': rlist) a
    -> PuzzleCrumbler ground rlist (t -> a)
addMemo wconstr pc =
    MkPuzzleCrumbler $
    withReaderT (\seen' -> ConsListType wconstr seen') $ fmap (fmap $ \tla l t -> tla (t, l)) $ unPuzzleCrumbler pc

addFixMemo ::
       forall (ground :: GroundTypeKind) rlist t a. IsDolanSubtypeGroundType ground
    => WholeConstraint ground t
    -> PuzzleCrumbler ground (t ': rlist) (t, a)
    -> PuzzleCrumbler ground rlist a
addFixMemo (wconstr@MkWholeConstraint {}) solver = let
    fixconv f = let
        ~(conv, a) = f $ iLazy conv
        in a
    in fmap fixconv $ addMemo wconstr solver

mapEachMemo ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => (forall t. WholeConstraint ground t -> SolverM ground (WholeConstraintShim ground t))
    -> (forall rlist. PuzzleCrumbler ground rlist a)
    -> (forall rlist. PuzzleCrumbler ground rlist a)
mapEachMemo ff = let
    mapInside ::
           forall b.
           (forall rlist.
                    ListType (WholeConstraint ground) rlist -> SolverM ground (DolanOpenExpression ground (ListProduct rlist -> b)))
        -> (forall rlist.
                    ListType (WholeConstraint ground) rlist -> SolverM ground (DolanOpenExpression ground (ListProduct rlist -> b)))
    mapInside sma NilListType = sma NilListType
    mapInside sma (ConsListType w ww) = do
        MkShimWit w' conv <- ff w
        fmap (fmap $ \lta (a, l) -> lta l $ isoForwards conv a) $
            mapInside (\ww' -> fmap (fmap $ \tla l t -> tla (t, l)) $ sma $ ConsListType w' ww') ww
    in \rma -> MkPuzzleCrumbler $ ReaderT $ mapInside $ runReaderT $ unPuzzleCrumbler rma

solvePiece ::
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => Piece ground a
    -> SolverM ground (PuzzleExpression ground a, [Substitution ground])
solvePiece (WholePiece constr _) = do
    pexpr <- lift $ crumbleConstraint constr
    return (pexpr, [])
solvePiece (AtomicPiece ac) = do
    (conv, sub) <- substituteAtomicConstraint ac
    return (pure conv, [sub])

bisubstituteEachMemo ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => [SolverBisubstitution ground]
    -> (forall rlist. PuzzleCrumbler ground rlist a)
    -> (forall rlist. PuzzleCrumbler ground rlist a)
bisubstituteEachMemo bisubs =
    mapEachMemo $ \wc -> lift $ lift $ runUnifierM $ bisubstitutesWholeConstraintShim bisubs $ mkShimWit wc

-- | For debugging. Switching this off will cause the solver to hang on some recursive puzzles.
solveSubstConstraints :: Maybe Bool
solveSubstConstraints = Just True

processPiece ::
       forall (ground :: GroundTypeKind) rlist a b.
       (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => Piece ground a
    -> Puzzle ground (a -> b)
    -> PuzzleCrumbler ground rlist b
processPiece piece puzzlerest =
    MkPuzzleCrumbler $ do
        ((MkSolverExpression conspuzzle rexpr, substs), bisubs) <- lift $ lift $ runWriterT $ solvePiece piece
        lift $ tell bisubs
        let
            doRest :: forall rlist'. PuzzleCrumbler ground rlist' b
            doRest =
                MkPuzzleCrumbler $ do
                    puzzlerest' <- lift $ lift $ lift $ runUnifierM $ applySubstsToPuzzle substs puzzlerest
                    oexpr <- unPuzzleCrumbler $ processPuzzle $ liftA2 (,) conspuzzle puzzlerest'
                    return $ liftA2 (\tt f l -> snd (f l) $ tt $ fst $ f l) rexpr oexpr
        unPuzzleCrumbler $
            case solveSubstConstraints of
                Just True -> bisubstituteEachMemo bisubs doRest -- causes broken shims
                Just False -> dropMemos doRest -- doesn't always terminate
                Nothing -> doRest -- doesn't always terminate, may also cause bad coercions

-- | For debugging. Switching this off will cause the solver to hang on recursive puzzles.
solveRecursive :: Bool
solveRecursive = True

processPuzzle ::
       forall (ground :: GroundTypeKind) rlist a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => Puzzle ground a
    -> PuzzleCrumbler ground rlist a
processPuzzle (ClosedExpression a) = pure a
processPuzzle (OpenExpression piece@(WholePiece wconstr True) puzzlerest) =
    MkPuzzleCrumbler $ do
        seen <- ask
        unPuzzleCrumbler $
            case (lookUpListElement wconstr seen) of
                Just lelem
                    | solveRecursive -> liftA2 (\conv f -> f conv) (retrieveMemo lelem) (processPuzzle puzzlerest)
                _ -> addFixMemo wconstr $ processPiece piece $ fmap (\ta t -> (t, ta t)) puzzlerest
processPuzzle (OpenExpression piece puzzlerest) = processPiece piece puzzlerest

crumblePuzzle ::
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => Puzzle ground a
    -> SolverM ground (DolanOpenExpression ground a)
crumblePuzzle puzzle = runPuzzleCrumbler $ processPuzzle puzzle
