module Language.Expression.Dolan.Solver.Crumble.Unify
    ( UnifyPiece (..)
    , UnifyPuzzle
    , pieceToUnify
    , solveUnifyPuzzle
    )
where

import Data.Shim
import Shapes

import Language.Expression.Common
import Language.Expression.Dolan.Simplify.Solve
import Language.Expression.Dolan.Solver.AtomicConstraint
import Language.Expression.Dolan.Solver.Crumble.Crumbler
import Language.Expression.Dolan.Solver.Crumble.Presubstitution
import Language.Expression.Dolan.Solver.Crumble.Type
import Language.Expression.Dolan.Solver.CrumbleM
import Language.Expression.Dolan.Solver.Puzzle
import Language.Expression.Dolan.Solver.WholeConstraint
import Language.Expression.Dolan.SubtypeChain
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeResult
import Language.Expression.Dolan.TypeSystem
import Language.Expression.TypeSystem

data UnifyVariableConstraint (ground :: GroundTypeKind) t where
    MkUnifyVariableConstraint ::
        forall (ground :: GroundTypeKind) ta tb tv.
        TypeVarT tv ->
        MixedType ground 'Positive ta ->
        MixedType ground 'Negative tb ->
        UnifyVariableConstraint ground (DolanShim ground ta tb -> (DolanShim ground ta tv, DolanShim ground tv tb))

instance forall (ground :: GroundTypeKind) t. ShowGroundType ground => Show (UnifyVariableConstraint ground t) where
    show (MkUnifyVariableConstraint var tp tn) = show tp <> " <: " <> show var <> " <: " <> show tn

instance forall (ground :: GroundTypeKind). ShowGroundType ground => AllConstraint Show (UnifyVariableConstraint ground) where
    allConstraint = Dict

data UnifyPiece (ground :: GroundTypeKind) t where
    WholeUnifyPiece :: forall (ground :: GroundTypeKind) t. WholeConstraint ground t -> UnifyPiece ground t
    AtomicUnifyPiece ::
        forall (ground :: GroundTypeKind) v a b.
        TypeVarT v ->
        MixedType ground 'Positive a ->
        MixedType ground 'Negative b ->
        UnifyPiece ground (DolanShim ground a v, DolanShim ground v b)
    VariableUnifyPiece :: forall (ground :: GroundTypeKind) t. UnifyVariableConstraint ground t -> UnifyPiece ground t

unifyAtomic ::
    forall (ground :: GroundTypeKind) v a b.
    IsDolanGroundType ground =>
    TypeVarT v ->
    MixedShimWit ground 'Positive a ->
    MixedShimWit ground 'Negative b ->
    UnifyPuzzle ground (DolanShim ground a v, DolanShim ground v b)
unifyAtomic var (MkShimWit ta (MkPolarShim conva)) (MkShimWit tb (MkPolarShim convb)) =
    fmap (\(av, vb) -> (av . conva, convb . vb)) $ varExpression $ AtomicUnifyPiece var ta tb

simplifyWholeUnifyPieceINTERNAL :: Bool
simplifyWholeUnifyPieceINTERNAL = True

unifyFlipTypes ::
    forall (ground :: GroundTypeKind) a b.
    IsDolanGroundType ground =>
    FlipType ground 'Positive a ->
    FlipType ground 'Negative b ->
    DolanRenameTypeM ground (UnifyPuzzle ground (DolanShim ground a b))
unifyFlipTypes (NormalFlipType ta) (NormalFlipType tb)
    | simplifyWholeUnifyPieceINTERNAL = do
        MkShimWit ta' (MkPolarShim conva) <- solveSimplify ta
        MkShimWit tb' (MkPolarShim convb) <- solveSimplify tb
        return
            $ fmap (\conv -> convb . conv . conva)
            $ varExpression
            $ WholeUnifyPiece
            $ MkWholeConstraint (NormalFlipType ta') (NormalFlipType tb')
unifyFlipTypes ta tb = return $ varExpression $ WholeUnifyPiece $ MkWholeConstraint ta tb

unifyDolanTypes ::
    forall (ground :: GroundTypeKind) a b.
    IsDolanGroundType ground =>
    DolanType ground 'Positive a ->
    DolanType ground 'Negative b ->
    DolanRenameTypeM ground (UnifyPuzzle ground (DolanShim ground a b))
unifyDolanTypes ta tb = unifyFlipTypes (NormalFlipType ta) (NormalFlipType tb)

unifyDolanInvType ::
    forall (ground :: GroundTypeKind) a b.
    IsDolanGroundType ground =>
    DolanType ground 'Positive a ->
    InvertedType ground 'Negative b ->
    DolanRenameTypeM ground (UnifyPuzzle ground (DolanShim ground a b))
unifyDolanInvType _ NilInvertedType = return $ pure termf
unifyDolanInvType t (ConsInvertedType t1 tr) = do
    p1 <- unifyFlipTypes (NormalFlipType t) (InvertFlipType t1)
    pr <- unifyDolanInvType t tr
    return $ liftA2 meetf p1 pr

unifyInvDolanType ::
    forall (ground :: GroundTypeKind) a b.
    IsDolanGroundType ground =>
    InvertedType ground 'Positive a ->
    DolanType ground 'Negative b ->
    DolanRenameTypeM ground (UnifyPuzzle ground (DolanShim ground a b))
unifyInvDolanType NilInvertedType _ = return $ pure initf
unifyInvDolanType (ConsInvertedType t1 tr) t = do
    p1 <- unifyFlipTypes (InvertFlipType t1) (NormalFlipType t)
    pr <- unifyInvDolanType tr t
    return $ liftA2 joinf p1 pr

unifyInvInvType ::
    forall (ground :: GroundTypeKind) a b.
    IsDolanGroundType ground =>
    InvertedType ground 'Positive a ->
    DolanType ground 'Positive b ->
    DolanRenameTypeM ground (UnifyPuzzle ground (DolanShim ground a b))
unifyInvInvType NilInvertedType _ = return $ pure initf
unifyInvInvType (ConsInvertedType t1 tr) t = do
    p1 <- unifyFlipTypes (InvertFlipType t1) (InvertFlipType t)
    pr <- unifyInvInvType tr t
    return $ liftA2 joinf p1 pr

unifyInvInvTypes ::
    forall (ground :: GroundTypeKind) a b.
    IsDolanGroundType ground =>
    InvertedType ground 'Positive a ->
    InvertedType ground 'Negative b ->
    DolanRenameTypeM ground (UnifyPuzzle ground (DolanShim ground a b))
unifyInvInvTypes _ NilInvertedType = return $ pure termf
unifyInvInvTypes it (ConsInvertedType t1 tr) = do
    p1 <- unifyInvInvType it t1
    pr <- unifyInvInvTypes it tr
    return $ liftA2 meetf p1 pr

unifyMixedTypes ::
    forall (ground :: GroundTypeKind) a b.
    IsDolanGroundType ground =>
    MixedType ground 'Positive a ->
    MixedType ground 'Negative b ->
    DolanRenameTypeM ground (UnifyPuzzle ground (DolanShim ground a b))
unifyMixedTypes (MkMixedType nta ita) (MkMixedType ntb itb) = do
    pnn <- unifyDolanTypes nta ntb
    pni <- unifyDolanInvType nta itb
    pin <- unifyInvDolanType ita ntb
    pii <- unifyInvInvTypes ita itb
    return
        $ (\convnn convni convin convii -> meetf (joinf convnn convin) (joinf convni convii))
        <$> pnn
        <*> pni
        <*> pin
        <*> pii

instance forall (ground :: GroundTypeKind) t. ShowGroundType ground => Show (UnifyPiece ground t) where
    show (WholeUnifyPiece wc) = "whole: " <> show wc
    show (AtomicUnifyPiece var tp tn) = "atomic: " <> show tp <> " <: " <> show var <> " <: " <> show tn
    show (VariableUnifyPiece vc) = "variable: " <> show vc

instance forall (ground :: GroundTypeKind). ShowGroundType ground => AllConstraint Show (UnifyPiece ground) where
    allConstraint = Dict

matchWholeUnifyPiece :: forall (ground :: GroundTypeKind) t. UnifyPiece ground t -> Maybe (WholeConstraint ground t)
matchWholeUnifyPiece (WholeUnifyPiece wconstr) = Just wconstr
matchWholeUnifyPiece _ = Nothing

type UnifyPuzzle (ground :: GroundTypeKind) = Expression (UnifyPiece ground)

type UnifyPuzzleExpression (ground :: GroundTypeKind) =
    TSOpenSolverExpression (DolanTypeSystem ground) (UnifyPuzzle ground)

type VarPuzzle (ground :: GroundTypeKind) = Expression (UnifyVariableConstraint ground)

type VarPuzzleExpression (ground :: GroundTypeKind) = TSOpenSolverExpression (DolanTypeSystem ground) (VarPuzzle ground)

type UnifyCrumbler (ground :: GroundTypeKind) =
    Crumbler (WholeConstraint ground) (CrumbleM ground) (VarPuzzleExpression ground)

pieceToUnify ::
    forall (ground :: GroundTypeKind) a.
    IsDolanGroundType ground =>
    Piece ground a ->
    CrumbleM ground (UnifyPuzzle ground a)
pieceToUnify (WholePiece (MkWholeConstraint fta ftb)) = liftToCrumbleM $ unifyFlipTypes fta ftb
pieceToUnify (AtomicPiece (MkAtomicConstraint var PositiveType ft)) =
    return $ fmap fst $ unifyAtomic var (flipMixedType ft) nilMixedType
pieceToUnify (AtomicPiece (MkAtomicConstraint var NegativeType ft)) =
    return $ fmap snd $ unifyAtomic var nilMixedType (flipMixedType ft)

solveWholeConstraint ::
    forall (ground :: GroundTypeKind) a.
    IsDolanGroundType ground =>
    WholeConstraint ground a ->
    CrumbleM ground (NonEmpty (UnifyPuzzleExpression ground a))
solveWholeConstraint wc = do
    exprs <- crumbleConstraint wc
    for exprs $ \(MkSolverExpression puzzle dexpr) -> do
        vexpr <- runExpressionM pieceToUnify puzzle
        return $ MkSolverExpression vexpr dexpr

mergeAtomicPuzzle ::
    forall (ground :: GroundTypeKind) a.
    IsDolanGroundType ground =>
    UnifyPuzzle ground a ->
    DolanRenameTypeM ground (UnifyPuzzle ground a)
mergeAtomicPuzzle =
    combineExpressionWitnessesM $ \wa wb ->
        case (wa, wb) of
            (AtomicUnifyPiece vara tpa tqa, AtomicUnifyPiece varb tpb tqb)
                | Just Refl <- testEquality vara varb ->
                    return
                        $ Just
                        $ case (joinMeetMixedType tpa tpb, joinMeetMixedType tqa tqb) of
                            (MkShimWit tpab (MkPolarShim convp), MkShimWit tqab (MkPolarShim convq)) ->
                                fmap
                                    ( \(convp1, convq1) ->
                                        ( (convp1 . convp . join1, meet1 . convq . convq1)
                                        , (convp1 . convp . join2, meet2 . convq . convq1)
                                        )
                                    )
                                    $ varExpression
                                    $ AtomicUnifyPiece vara tpab tqab
            (VariableUnifyPiece (MkUnifyVariableConstraint vara tpa tqa), VariableUnifyPiece (MkUnifyVariableConstraint varb tpb tqb))
                | Just Refl <- testEquality vara varb -> let
                    twpab = joinMeetMixedType @ground @(DolanPolyIsoShim ground) tpa tpb
                    twqab = joinMeetMixedType @ground @(DolanPolyIsoShim ground) tqa tqb
                    in case (twpab, twqab) of
                        (MkShimWit tpab convp, MkShimWit tqab convq) -> do
                            let vc = varExpression $ VariableUnifyPiece $ MkUnifyVariableConstraint vara tpab tqab
                            wcpaqa <- unifyMixedTypes tpa tqa -- already memoised
                            wcpaqb <- unifyMixedTypes tpa tqb
                            wcpbqa <- unifyMixedTypes tpb tqa
                            wcpbqb <- unifyMixedTypes tpb tqb -- already memoised
                            return
                                $ Just
                                $ ( \cvc cpaqa cpaqb cpbqa cpbqb -> let
                                        cpq =
                                            polarPolyIsoPositive convq
                                                . meetf (joinf cpaqa cpbqa) (joinf cpaqb cpbqb)
                                                . polarPolyIsoNegative convp
                                        (cp, cq) = cvc cpq
                                        convpp = cp . polarPolyIsoPositive convp
                                        convqq = polarPolyIsoNegative convq . cq
                                        in ( \_ -> (convpp . join1, meet1 . convqq)
                                           , \_ -> (convpp . join2, meet2 . convqq)
                                           )
                                  )
                                <$> vc
                                <*> wcpaqa
                                <*> wcpaqb
                                <*> wcpbqa
                                <*> wcpbqb
            _ -> return Nothing

atomicToVariablePuzzle ::
    forall (ground :: GroundTypeKind) a.
    IsDolanGroundType ground =>
    UnifyPuzzle ground a ->
    DolanRenameTypeM ground (UnifyPuzzle ground a)
atomicToVariablePuzzle =
    runExpressionM $ \case
        AtomicUnifyPiece var tp tq -> do
            let vp = varExpression (VariableUnifyPiece (MkUnifyVariableConstraint var tp tq))
            wp <- unifyMixedTypes tp tq
            return $ vp <*> wp
        piece -> return $ varExpression piece

toPureVariablePuzzle ::
    forall (ground :: GroundTypeKind) a.
    UnifyPuzzle ground a ->
    Maybe (Expression (UnifyVariableConstraint ground) a)
toPureVariablePuzzle =
    mapExactExpressionM $ \case
        VariableUnifyPiece vconstr -> Just vconstr
        _ -> Nothing

firstSuccess ::
    forall e m a b.
    MonadCatch e m =>
    NonEmpty a ->
    (a -> m b) ->
    m b
firstSuccess (a0 :| []) amb = amb a0
firstSuccess (a0 :| (a1 : aa)) amb = catch (amb a0) (\(_ :: e) -> firstSuccess @e (a1 :| aa) amb)

processWholeConstraint ::
    forall (ground :: GroundTypeKind) a b.
    IsDolanGroundType ground =>
    WholeConstraint ground a ->
    UnifyPuzzle ground (a -> b) ->
    UnifyCrumbler ground b
processWholeConstraint wconstr@MkWholeConstraint{} puzzlerest =
    memoiseBranch iLazy wconstr (processPuzzle puzzlerest)
        $ MkCrumbler
        $ do
            upexprs <- lift $ solveWholeConstraint wconstr
            firstSuccess @(TypeError ground) upexprs $ \(MkSolverExpression puzzle1 dexpr1) -> do
                MkSolverExpression puzzle2 dexpr2 <- unCrumbler $ processPuzzle $ liftA2 (,) puzzle1 puzzlerest
                return
                    $ MkSolverExpression puzzle2
                    $ liftA2
                        ( \tc tla t1 l -> let
                            (t, cb) = tla t1 l
                            c = tc t
                            in (c, cb c)
                        )
                        dexpr1
                        dexpr2

processPuzzle ::
    forall (ground :: GroundTypeKind) a.
    IsDolanGroundType ground =>
    UnifyPuzzle ground a ->
    UnifyCrumbler ground a
processPuzzle (ClosedExpression a) = pure a
processPuzzle puzzle =
    case findFirstExpression matchWholeUnifyPiece puzzle processWholeConstraint of
        Just r -> r
        Nothing ->
            wbind
                ( liftToCrumbleM $ do
                    apuzzle <- mergeAtomicPuzzle puzzle
                    atomicToVariablePuzzle apuzzle
                )
                $ \puzzle' ->
                    case toPureVariablePuzzle puzzle' of
                        Just vpuzzle -> crumblerLift $ solverExpressionLiftType vpuzzle
                        Nothing -> processPuzzle puzzle'

constraintToPresub ::
    forall (ground :: GroundTypeKind) t.
    IsDolanGroundType ground =>
    UnifyVariableConstraint ground t ->
    WriterT [Presubstitution ground] (DolanRenameTypeM ground) t
constraintToPresub (MkUnifyVariableConstraint var ta tb) = do
    (presub, t) <- lift $ assignPresubstitution var ta tb
    tell [presub]
    return t

applyEachEvery ::
    forall (ground :: GroundTypeKind).
    IsDolanGroundType ground =>
    [Presubstitution ground] ->
    DolanRenameTypeM ground [Presubstitution ground]
applyEachEvery [] = return []
applyEachEvery (x : pp) = do
    pp1 <- applyEachEvery pp
    x1 <- unEndoM (concatmap (\p -> MkEndoM $ presubstitute p) pp1) x
    pp2 <- for pp1 $ presubstitute x1
    return $ x1 : pp2

solveVPuzzle ::
    forall (ground :: GroundTypeKind) t.
    IsDolanGroundType ground =>
    VarPuzzle ground t ->
    DolanRenameTypeM ground (t, [SolverBisubstitution ground])
solveVPuzzle (ClosedExpression a) = return (a, [])
solveVPuzzle vpuzzle = do
    (t, presubs) <- runWriterT $ runExpression constraintToPresub vpuzzle
    presubs' <- applyEachEvery presubs
    bisubs <- for presubs' preBisubstitution
    return $ (t, bisubs)

solveUnifyPuzzle ::
    forall (ground :: GroundTypeKind) a.
    IsDolanGroundType ground =>
    UnifyPuzzle ground a ->
    CrumbleM ground (DolanOpenExpression ground a, [SolverBisubstitution ground])
solveUnifyPuzzle (ClosedExpression a) = return (pure a, [])
solveUnifyPuzzle puzzle = do
    MkSolverExpression vpuzzle expr <- runCrumbler $ processPuzzle puzzle
    (t, bisubs) <- liftToCrumbleM $ solveVPuzzle vpuzzle
    return $ (fmap (\ta -> ta t) expr, bisubs)
