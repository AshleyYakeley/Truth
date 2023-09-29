module Language.Expression.Dolan.Solver.Crumble.Subsume
    ( solveSubsumePuzzle
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Solver.AtomicConstraint
import Language.Expression.Dolan.Solver.AtomicSubstitute
import Language.Expression.Dolan.Solver.Crumble.Crumbler
import Language.Expression.Dolan.Solver.Crumble.Type
import Language.Expression.Dolan.Solver.FlipType
import Language.Expression.Dolan.Solver.Puzzle
import Language.Expression.Dolan.Solver.UnifierM
import Language.Expression.Dolan.Solver.WholeConstraint
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

type SubsumeCrumbler (ground :: GroundTypeKind)
     = Crumbler (WholeConstraint ground) (SolverM ground) (Compose Maybe (AtomicPuzzleExpression ground))

processPiece ::
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => Piece ground a
    -> SubsumeCrumbler ground a
processPiece (AtomicPiece ac) = crumblerLift $ liftComposeInner $ solverExpressionLiftType $ varExpression ac
processPiece (WholePiece constr@MkWholeConstraint {}) =
    memoise iLazy constr $
    MkCrumbler $ do
        pexprs <- lift $ lift $ crumbleConstraint constr
        fmap Compose $
            forFirst pexprs $ \(MkSolverExpression puzzle expr) -> do
                Compose moexpr <- unCrumbler $ processPuzzle puzzle
                return $ fmap (liftA2 (\ts lt l -> ts $ lt l) (solverExpressionLiftValue expr)) moexpr

processPuzzle ::
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => Puzzle ground a
    -> SubsumeCrumbler ground a
processPuzzle = solveExpression processPiece

substPuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => Expression (AtomicConstraint ground) a
    -> SolverM ground (Puzzle ground a)
substPuzzle (ClosedExpression a) = return $ pure a
substPuzzle (OpenExpression ac expr) = do
    (t, subst) <- lift $ getAtomicConstraint ac
    tell [subst]
    puzzle <- mapExpressionWitnessesM (\ac' -> lift $ lift $ runUnifierM $ substituteAtomicConstraint subst ac') expr
    return $ fmap (\ta -> ta t) puzzle

completePuzzle ::
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground, ?rigidity :: String -> NameRigidity)
    => Puzzle ground a
    -> SolverM ground (DolanOpenExpression ground a)
completePuzzle (ClosedExpression a) = return $ pure a
completePuzzle puzzle@(OpenExpression piece _) = do
    Compose mpexpr <- runCrumbler $ processPuzzle puzzle
    case mpexpr of
        Just (MkSolverExpression ap expr) -> do
            puzzle' <- substPuzzle ap
            oexpr <- completePuzzle puzzle'
            return $ expr <*> oexpr
        Nothing ->
            lift $
            lift $
            case piece of
                WholePiece (MkWholeConstraint fta ftb) ->
                    flipToType fta $ \ta -> flipToType ftb $ \tb -> throwTypeConvertError ta tb
                AtomicPiece (MkAtomicConstraint var pol ft) ->
                    case pol of
                        PositiveType ->
                            flipToType ft $ \t ->
                                throwTypeConvertError t (singleDolanType @ground @'Negative $ VarDolanSingularType var)
                        NegativeType ->
                            flipToType ft $ \t ->
                                throwTypeConvertError (singleDolanType @ground @'Positive $ VarDolanSingularType var) t

solveSubsumePuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => (String -> NameRigidity)
    -> Puzzle ground a
    -> DolanTypeCheckM ground (DolanOpenExpression ground a, [SolverBisubstitution ground])
solveSubsumePuzzle _ (ClosedExpression a) = return (pure a, [])
solveSubsumePuzzle rigidity puzzle = let
    ?rigidity = rigidity
    in do
           (a, substs) <- runWriterT $ completePuzzle puzzle
           return (a, fmap substBisubstitution substs)
