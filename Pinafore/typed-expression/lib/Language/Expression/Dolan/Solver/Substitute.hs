{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Dolan.Solver.Substitute
    ( getAtomicConstraint
    , SolverM
    , solveAtomicConstraint
    , applySubstToAtomicConstraint
    , applyBisubsToPuzzle
    , substBisubstitution
    )
where

import Data.Shim
import Shapes

import Language.Expression.Common
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Invert
import Language.Expression.Dolan.Solver.AtomicConstraint
import Language.Expression.Dolan.Solver.Puzzle
import Language.Expression.Dolan.Solver.WholeConstraint
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.SubtypeChain
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeResult
import Language.Expression.Dolan.TypeSystem
import Language.Expression.TypeSystem

type SolverM :: GroundTypeKind -> Type -> Type
type SolverM ground = WriterT [Substitution ground] (DolanRenameTypeM ground)

substBisubstitution ::
    forall (ground :: GroundTypeKind).
    IsDolanGroundType ground =>
    Substitution ground ->
    SolverBisubstitution ground
substBisubstitution (MkSubstitution (pol :: _ polarity) oldvar newvar mt _) =
    withRepresentative pol
        $ withInvertPolarity @polarity
        $ let
            newVarWit = shimWitToDolan $ MkShimWit (VarDolanSingularType newvar) $ invertPolarShim polar1
            in mkPolarBisubstitution oldvar mt $ return newVarWit

getAtomicConstraint ::
    forall (ground :: GroundTypeKind) a.
    IsDolanSubtypeGroundType ground =>
    AtomicConstraint ground a ->
    DolanRenameTypeM ground (a, Substitution ground)
getAtomicConstraint (MkAtomicConstraint oldvar (pol :: _ polarity) (fptw :: _ pt)) =
    withRepresentative pol $ do
        MkSomeTypeVarT (newvar :: TypeVarT newtv) <- renamerGenerateFreeTypeVarT
        withInvertPolarity @polarity
            $ assignTypeVarT @(JoinMeetType polarity newtv pt) oldvar
            $ do
                let
                    newVarWit :: DolanShimWit ground (InvertPolarity polarity) (JoinMeetType polarity newtv pt)
                    newVarWit = shimWitToDolan $ MkShimWit (VarDolanSingularType newvar) $ invertPolarShim polar1
                substwit <-
                    if occursInFlipType oldvar fptw
                        then do
                            MkSomeTypeVarT recvar <- renamerGenerateFreeTypeVarT
                            assignSameTypeVarT oldvar recvar
                                $ return
                                $ \ptw' ->
                                    shimWitToDolan
                                        $ recursiveDolanShimWit recvar
                                        $ joinMeetShimWit
                                            (varDolanShimWit newvar)
                                            (bothBisubstitute oldvar (varDolanShimWit recvar) newVarWit ptw')
                        else return $ \ptw' -> joinMeetShimWit (varDolanShimWit newvar) ptw'
                subst <-
                    case fptw of
                        NormalFlipType ptw ->
                            return $ MkSubstitution pol oldvar newvar (return $ substwit $ mkPolarShimWit ptw) Nothing
                        InvertFlipType ptw ->
                            case isInvertInvertPolarity @polarity of
                                Refl -> do
                                    rigidity <- renamerGetNameRigidity
                                    return
                                        $ MkSubstitution
                                            pol
                                            oldvar
                                            newvar
                                            (fmap substwit $ invertTypeM rigidity ptw)
                                            (Just ptw)
                return (unPolarShim $ polar2 @(DolanShim ground) @polarity @newtv @pt, subst)

solveAtomicConstraint ::
    forall (ground :: GroundTypeKind) a.
    IsDolanSubtypeGroundType ground =>
    AtomicConstraint ground a ->
    SolverM ground a
solveAtomicConstraint ac = do
    (a, subst) <- lift $ getAtomicConstraint ac
    tell [subst]
    return a

applyBisubsToPiece ::
    forall (ground :: GroundTypeKind) a.
    IsDolanSubtypeGroundType ground =>
    [SolverBisubstitution ground] ->
    Piece ground a ->
    DolanRenameTypeM ground (Puzzle ground a)
applyBisubsToPiece newchanges (WholePiece wc) = do
    MkShimWit wc' (MkCatDual conv) <- lift $ lift $ applyBisubsToWholeConstraintShim newchanges $ mkShimWit wc
    return $ fmap conv $ varExpression $ WholePiece wc'
applyBisubsToPiece newchanges (AtomicPiece ac) = applyBisubsToAtomicConstraint newchanges ac

applyBisubsToPuzzle ::
    forall (ground :: GroundTypeKind) a.
    IsDolanSubtypeGroundType ground =>
    [SolverBisubstitution ground] ->
    Puzzle ground a ->
    DolanRenameTypeM ground (Puzzle ground a)
applyBisubsToPuzzle substs = runExpressionM $ applyBisubsToPiece substs

applyBisubToAtomicConstraint ::
    forall (ground :: GroundTypeKind) a.
    IsDolanSubtypeGroundType ground =>
    SolverBisubstitution ground ->
    AtomicConstraint ground a ->
    DolanRenameTypeM ground (Puzzle ground a)
applyBisubToAtomicConstraint bisub@(MkBisubstitution oldvar _ mwq) (MkAtomicConstraint depvar PositiveType ftw)
    | Just Refl <- testEquality oldvar depvar = do
        MkShimWit tq (MkPolarShim convq) <- lift $ lift mwq
        MkShimWit ftw' (MkPolarShim convw) <- lift $ lift $ bisubstituteFlipType bisub ftw
        puzzle <- flipUnifyPuzzle ftw' (NormalFlipType tq)
        return $ fmap (\conv -> convq . conv . convw) puzzle
applyBisubToAtomicConstraint bisub@(MkBisubstitution oldvar mwp _) (MkAtomicConstraint depvar NegativeType ftw)
    | Just Refl <- testEquality oldvar depvar = do
        MkShimWit tp (MkPolarShim convp) <- lift $ lift mwp
        MkShimWit ftw' (MkPolarShim convw) <- lift $ lift $ bisubstituteFlipType bisub ftw
        puzzle <- flipUnifyPuzzle (NormalFlipType tp) ftw'
        return $ fmap (\conv -> convw . conv . convp) puzzle
applyBisubToAtomicConstraint _ ac
    | Just conv <- isPureAtomicConstraint ac = return $ pure conv
applyBisubToAtomicConstraint bisub (MkAtomicConstraint depvar PositiveType (NormalFlipType tw)) = do
    MkShimWit tp (MkPolarShim conv) <- lift $ lift $ bisubstituteType bisub tw
    puzzle <- flipUnifyPuzzle (NormalFlipType tp) (NormalFlipType $ singleDolanType $ VarDolanSingularType depvar)
    return $ fmap (\pv -> iMeetL1 . pv . conv) puzzle
applyBisubToAtomicConstraint bisub (MkAtomicConstraint depvar NegativeType (NormalFlipType tw)) = do
    MkShimWit tp (MkPolarShim conv) <- lift $ lift $ bisubstituteType bisub tw
    puzzle <- flipUnifyPuzzle (NormalFlipType $ singleDolanType $ VarDolanSingularType depvar) (NormalFlipType tp)
    return $ fmap (\pv -> conv . pv . iJoinR1) puzzle
applyBisubToAtomicConstraint _ ac = return $ atomicConstraintPuzzle ac

applyBisubsToAtomicConstraint ::
    forall (ground :: GroundTypeKind) a.
    IsDolanSubtypeGroundType ground =>
    [SolverBisubstitution ground] ->
    AtomicConstraint ground a ->
    DolanRenameTypeM ground (Puzzle ground a)
applyBisubsToAtomicConstraint [] ac = return $ atomicConstraintPuzzle ac
applyBisubsToAtomicConstraint (s : ss) ac = do
    puzzle <- applyBisubToAtomicConstraint s ac
    applyBisubsToPuzzle ss puzzle

applySubstToAtomicConstraint ::
    forall (ground :: GroundTypeKind) a.
    IsDolanSubtypeGroundType ground =>
    Substitution ground ->
    AtomicConstraint ground a ->
    DolanRenameTypeM ground (Puzzle ground a)
applySubstToAtomicConstraint (MkSubstitution substpol oldvar newvar _ (Just st)) (MkAtomicConstraint depvar unipol fvt)
    | Just Refl <- testEquality oldvar depvar = do
        let p1 = atomicConstraintPuzzle (MkAtomicConstraint newvar unipol fvt)
        p2 <-
            case (substpol, unipol) of
                (NegativeType, PositiveType) ->
                    flipToType fvt $ \vt -> do
                        puzzle <- puzzleUnify vt st
                        return $ fmap (\convm conv -> meetf conv convm) puzzle
                (PositiveType, NegativeType) ->
                    flipToType fvt $ \vt -> do
                        puzzle <- puzzleUnify st vt
                        return $ fmap (\convm conv -> joinf conv convm) puzzle
                (NegativeType, NegativeType) -> return $ pure $ \conv -> conv . meet1
                (PositiveType, PositiveType) -> return $ pure $ \conv -> join1 . conv
        return $ liftA2 (\t a -> a t) p1 p2
applySubstToAtomicConstraint sub ac = applyBisubToAtomicConstraint (substBisubstitution sub) ac

type WholeConstraintShim :: GroundTypeKind -> Type -> Type
type WholeConstraintShim ground = ShimWit (CatDual (->)) (WholeConstraint ground)

applyBisubToWholeConstraint ::
    forall (ground :: GroundTypeKind) a.
    IsDolanGroundType ground =>
    SolverBisubstitution ground ->
    WholeConstraint ground a ->
    TypeResult ground (WholeConstraintShim ground a)
applyBisubToWholeConstraint bisub (MkWholeConstraint fta ftb) = do
    MkShimWit fta' (MkPolarShim conva) <- bisubstituteFlipType bisub fta
    MkShimWit ftb' (MkPolarShim convb) <- bisubstituteFlipType bisub ftb
    return $ MkShimWit (MkWholeConstraint fta' ftb') $ MkCatDual $ \conv -> convb . conv . conva

applyBisubToWholeConstraintShim ::
    forall (ground :: GroundTypeKind) a.
    IsDolanGroundType ground =>
    SolverBisubstitution ground ->
    WholeConstraintShim ground a ->
    TypeResult ground (WholeConstraintShim ground a)
applyBisubToWholeConstraintShim bisub = chainShimWitM $ applyBisubToWholeConstraint bisub

applyBisubsToWholeConstraintShim ::
    forall (ground :: GroundTypeKind) a.
    IsDolanGroundType ground =>
    [SolverBisubstitution ground] ->
    WholeConstraintShim ground a ->
    TypeResult ground (WholeConstraintShim ground a)
applyBisubsToWholeConstraintShim bisubs = unEndoM $ concatmap (MkEndoM . applyBisubToWholeConstraintShim) bisubs
