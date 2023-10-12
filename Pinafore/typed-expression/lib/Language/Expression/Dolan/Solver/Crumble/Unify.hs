module Language.Expression.Dolan.Solver.Crumble.Unify
    ( UnifyWholeConstraint(..)
    , UnifyPiece(..)
    , UnifyPuzzle
    , pieceToUnify
    , solveUnifyPuzzle
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Solver.AtomicConstraint
import Language.Expression.Dolan.Solver.Crumble.Crumbler
import Language.Expression.Dolan.Solver.Crumble.Presubstitution
import Language.Expression.Dolan.Solver.Crumble.Type
import Language.Expression.Dolan.Solver.CrumbleM
import Language.Expression.Dolan.Solver.Puzzle
import Language.Expression.Dolan.Solver.WholeConstraint
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeResult
import Language.Expression.Dolan.TypeSystem
import Shapes

data UnifyWholeConstraint (ground :: GroundTypeKind) t where
    MkUnifyWholeConstraint
        :: forall (ground :: GroundTypeKind) a b.
           DolanType ground 'Positive a
        -> DolanType ground 'Negative b
        -> UnifyWholeConstraint ground (DolanShim ground a b)

instance forall (ground :: GroundTypeKind) t. IsDolanGroundType ground => Show (UnifyWholeConstraint ground t) where
    show (MkUnifyWholeConstraint tp tn) = allShow tp <> " <: " <> allShow tn

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => AllConstraint Show (UnifyWholeConstraint ground) where
    allConstraint = Dict

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => TestEquality (UnifyWholeConstraint ground) where
    testEquality (MkUnifyWholeConstraint ta1 tb1) (MkUnifyWholeConstraint ta2 tb2) = do
        Refl <- testEquality ta1 ta2
        Refl <- testEquality tb1 tb2
        return Refl

data UnifyVariableConstraint (ground :: GroundTypeKind) t where
    MkUnifyVariableConstraint
        :: forall (ground :: GroundTypeKind) ta tb tv.
           TypeVarT tv
        -> DolanType ground 'Positive ta
        -> DolanType ground 'Negative tb
        -> UnifyVariableConstraint ground (DolanShim ground ta tb -> (DolanShim ground ta tv, DolanShim ground tv tb))

instance forall (ground :: GroundTypeKind) t. IsDolanGroundType ground => Show (UnifyVariableConstraint ground t) where
    show (MkUnifyVariableConstraint var tp tn) = allShow tp <> " <: " <> show var <> " <: " <> allShow tn

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground =>
             AllConstraint Show (UnifyVariableConstraint ground) where
    allConstraint = Dict

data UnifyPiece (ground :: GroundTypeKind) t where
    WholeUnifyPiece :: forall (ground :: GroundTypeKind) t. UnifyWholeConstraint ground t -> UnifyPiece ground t
    AtomicUnifyPiece
        :: forall (ground :: GroundTypeKind) ta tb tv.
           TypeVarT tv
        -> DolanType ground 'Positive ta
        -> DolanType ground 'Negative tb
        -> UnifyPiece ground (DolanShim ground ta tv, DolanShim ground tv tb)
    VariableUnifyPiece :: forall (ground :: GroundTypeKind) t. UnifyVariableConstraint ground t -> UnifyPiece ground t

instance forall (ground :: GroundTypeKind) t. IsDolanGroundType ground => Show (UnifyPiece ground t) where
    show (WholeUnifyPiece wc) = "whole: " <> show wc
    show (AtomicUnifyPiece var tp tn) =
        "atomic: " <> showDolanType tp <> " <: " <> show var <> " <: " <> showDolanType tn
    show (VariableUnifyPiece vc) = "variable: " <> show vc

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => AllConstraint Show (UnifyPiece ground) where
    allConstraint = Dict

matchWholeUnifyPiece ::
       forall (ground :: GroundTypeKind) t. IsDolanGroundType ground
    => UnifyPiece ground t
    -> Maybe (UnifyWholeConstraint ground t)
matchWholeUnifyPiece (WholeUnifyPiece wconstr) = Just wconstr
matchWholeUnifyPiece _ = Nothing

type UnifyPuzzle (ground :: GroundTypeKind) = Expression (UnifyPiece ground)

type UnifyPuzzleExpression (ground :: GroundTypeKind)
     = TSOpenSolverExpression (DolanTypeSystem ground) (UnifyPuzzle ground)

type VarPuzzle (ground :: GroundTypeKind) = Expression (UnifyVariableConstraint ground)

type VarPuzzleExpression (ground :: GroundTypeKind) = TSOpenSolverExpression (DolanTypeSystem ground) (VarPuzzle ground)

type UnifyCrumbler (ground :: GroundTypeKind)
     = Crumbler (UnifyWholeConstraint ground) (CrumbleM ground) (VarPuzzleExpression ground)

pieceToUnify ::
       forall (ground :: GroundTypeKind) a. IsDolanGroundType ground
    => Piece ground a
    -> TypeResult ground (UnifyPuzzle ground a)
pieceToUnify (WholePiece (MkWholeConstraint (NormalFlipType ta) (NormalFlipType tb))) =
    return $ varExpression $ WholeUnifyPiece $ MkUnifyWholeConstraint ta tb
pieceToUnify (AtomicPiece (MkAtomicConstraint var PositiveType (NormalFlipType t))) =
    return $ fmap fst $ varExpression $ AtomicUnifyPiece var t NilDolanType
pieceToUnify (AtomicPiece (MkAtomicConstraint var NegativeType (NormalFlipType t))) =
    return $ fmap snd $ varExpression $ AtomicUnifyPiece var NilDolanType t
pieceToUnify _ = throwExc $ InternalTypeError "inverted solver piece"

solveWholeConstraint ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => UnifyWholeConstraint ground a
    -> CrumbleM ground (UnifyPuzzleExpression ground a)
solveWholeConstraint (MkUnifyWholeConstraint (NormalFlipType -> fta) (NormalFlipType -> ftb)) = do
    exprs <- crumbleConstraint $ MkWholeConstraint fta ftb
    case exprs of
        MkSolverExpression puzzle dexpr :| [] -> do
            vexpr <- liftResultToCrumbleM $ mapExpressionM pieceToUnify puzzle
            return $ MkSolverExpression vexpr dexpr
        _ -> throw $ ConvertTypeError fta ftb

mergeAtomicPuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => UnifyPuzzle ground a
    -> UnifyPuzzle ground a
mergeAtomicPuzzle =
    combineExpressionWitnesses $ \wa wb ->
        case (wa, wb) of
            (AtomicUnifyPiece vara tpa tqa, AtomicUnifyPiece varb tpb tqb)
                | Just Refl <- testEquality vara varb ->
                    Just $
                    case (joinMeetType tpa tpb, joinMeetType tqa tqb) of
                        (MkShimWit tpab (MkPolarShim convp), MkShimWit tqab (MkPolarShim convq)) ->
                            fmap
                                (\(convp1, convq1) ->
                                     ( (convp1 . convp . join1, meet1 . convq . convq1)
                                     , (convp1 . convp . join2, meet2 . convq . convq1))) $
                            varExpression $ AtomicUnifyPiece vara tpab tqab
            (VariableUnifyPiece (MkUnifyVariableConstraint vara tpa tqa), VariableUnifyPiece (MkUnifyVariableConstraint varb tpb tqb))
                | Just Refl <- testEquality vara varb ->
                    Just $ let
                        twpab = joinMeetType @ground @(DolanPolyIsoShim ground) tpa tpb
                        twqab = joinMeetType @ground @(DolanPolyIsoShim ground) tqa tqb
                        in case (twpab, twqab) of
                               (MkShimWit tpab convp, MkShimWit tqab convq) -> let
                                   vc = varExpression $ VariableUnifyPiece $ MkUnifyVariableConstraint vara tpab tqab
                                   wcpaqa = varExpression $ WholeUnifyPiece $ MkUnifyWholeConstraint tpa tqa -- already memoised
                                   wcpaqb = varExpression $ WholeUnifyPiece $ MkUnifyWholeConstraint tpa tqb
                                   wcpbqa = varExpression $ WholeUnifyPiece $ MkUnifyWholeConstraint tpb tqa
                                   wcpbqb = varExpression $ WholeUnifyPiece $ MkUnifyWholeConstraint tpb tqb -- already memoised
                                   in (\cvc cpaqa cpaqb cpbqa cpbqb -> let
                                           cpq =
                                               polarPolyIsoPositive convq .
                                               meetf (joinf cpaqa cpbqa) (joinf cpaqb cpbqb) .
                                               polarPolyIsoNegative convp
                                           (cp, cq) = cvc cpq
                                           convpp = cp . polarPolyIsoPositive convp
                                           convqq = polarPolyIsoNegative convq . cq
                                           in ( \_ -> (convpp . join1, meet1 . convqq)
                                              , \_ -> (convpp . join2, meet2 . convqq))) <$>
                                      vc <*>
                                      wcpaqa <*>
                                      wcpaqb <*>
                                      wcpbqa <*>
                                      wcpbqb
            _ -> Nothing

atomicToVariablePuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => UnifyPuzzle ground a
    -> UnifyPuzzle ground a
atomicToVariablePuzzle =
    mapExpression $ \case
        AtomicUnifyPiece var tp tq ->
            varExpression (VariableUnifyPiece (MkUnifyVariableConstraint var tp tq)) <*>
            varExpression (WholeUnifyPiece $ MkUnifyWholeConstraint tp tq)
        piece -> varExpression piece

toPureVariablePuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => UnifyPuzzle ground a
    -> Maybe (Expression (UnifyVariableConstraint ground) a)
toPureVariablePuzzle =
    mapExactExpressionM $ \case
        VariableUnifyPiece vconstr -> Just vconstr
        _ -> Nothing

processWholeConstraint ::
       forall (ground :: GroundTypeKind) a b. IsDolanSubtypeGroundType ground
    => UnifyWholeConstraint ground a
    -> UnifyPuzzle ground (a -> b)
    -> UnifyCrumbler ground b
processWholeConstraint wconstr@MkUnifyWholeConstraint {} puzzlerest =
    memoiseBranch iLazy wconstr (processPuzzle puzzlerest) $
    MkCrumbler $ do
        MkSolverExpression puzzle1 dexpr1 <- lift $ solveWholeConstraint wconstr
        MkSolverExpression puzzle2 dexpr2 <- unCrumbler $ processPuzzle $ liftA2 (,) puzzle1 puzzlerest
        return $
            MkSolverExpression puzzle2 $
            liftA2
                (\tc tla t1 l -> let
                     (t, cb) = tla t1 l
                     c = tc t
                     in (c, cb c))
                dexpr1
                dexpr2

processPuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => UnifyPuzzle ground a
    -> UnifyCrumbler ground a
processPuzzle (ClosedExpression a) = pure a
processPuzzle puzzle =
    case findFirstExpression matchWholeUnifyPiece puzzle processWholeConstraint of
        Just r -> r
        Nothing -> let
            puzzle' = atomicToVariablePuzzle $ mergeAtomicPuzzle puzzle
            in case toPureVariablePuzzle puzzle' of
                   Just vpuzzle -> crumblerLift $ solverExpressionLiftType vpuzzle
                   Nothing -> processPuzzle puzzle'

constraintToPresub ::
       forall (ground :: GroundTypeKind) t. IsDolanGroundType ground
    => UnifyVariableConstraint ground t
    -> WriterT [Presubstitution ground] (DolanTypeCheckM ground) t
constraintToPresub (MkUnifyVariableConstraint var ta tb) = do
    (presub, t) <- lift $ assignPresubstitution var ta tb
    tell [presub]
    return t

applyEachEvery ::
       forall (ground :: GroundTypeKind). IsDolanGroundType ground
    => [Presubstitution ground]
    -> DolanTypeCheckM ground [Presubstitution ground]
applyEachEvery [] = return []
applyEachEvery (p:pp) = do
    pp1 <- applyEachEvery pp
    pp2 <- for pp1 $ presubstitute p
    p1 <- unEndoM (mconcat $ fmap (\p1 -> MkEndoM $ presubstitute p1) pp2) p
    return $ p1 : pp2

solveVPuzzle ::
       forall (ground :: GroundTypeKind) t. IsDolanGroundType ground
    => VarPuzzle ground t
    -> DolanTypeCheckM ground (t, [SolverBisubstitution ground])
solveVPuzzle (ClosedExpression a) = return (a, [])
solveVPuzzle vpuzzle = do
    (t, presubs) <- runWriterT $ solveExpression constraintToPresub vpuzzle
    presubs' <- applyEachEvery presubs
    bisubs <- for presubs' preBisubstitution
    return $ (t, bisubs)

solveUnifyPuzzle ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => UnifyPuzzle ground a
    -> CrumbleM ground (DolanOpenExpression ground a, [SolverBisubstitution ground])
solveUnifyPuzzle (ClosedExpression a) = return (pure a, [])
solveUnifyPuzzle puzzle = do
    MkSolverExpression vpuzzle expr <- runCrumbler $ processPuzzle puzzle
    (t, bisubs) <- liftToCrumbleM $ solveVPuzzle vpuzzle
    return $ (fmap (\ta -> ta t) expr, bisubs)
