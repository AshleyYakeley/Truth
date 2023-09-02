{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Unifier
    ( unifierSubtypeConversionAsGeneralAs
    , invertType
    , subtypeSingularType
    , invertedPolarSubtype
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Combine
import Language.Expression.Dolan.Solver
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Unifier.Build
import Language.Expression.Dolan.Unifier.Constraint
import Language.Expression.Dolan.Unifier.UnifierM
import Language.Expression.Dolan.Variance
import Shapes

type DolanUnifier :: GroundTypeKind -> Type -> Type
type DolanUnifier ground = Expression (UnifierConstraint ground)

type DolanUnifierExpression :: GroundTypeKind -> Type -> Type
type DolanUnifierExpression ground = DolanSolverExpression ground (UnifierConstraint ground)

type UnifierBisubstitution :: GroundTypeKind -> Type
type UnifierBisubstitution ground = Bisubstitution ground (DolanShim ground) (UnifierM ground)

bisubstitutePositiveVar ::
       forall (ground :: GroundTypeKind) tv t. IsDolanSubtypeGroundType ground
    => TypeVarT tv
    -> DolanType ground 'Positive t
    -> DolanUnifier ground (DolanShim ground t tv)
bisubstitutePositiveVar _ NilDolanType = pure initf
bisubstitutePositiveVar vn (ConsDolanType t1 tr) =
    OpenExpression (geSingleUnifierConstraint vn t1) $
    fmap (\fr f1 -> joinf (f1 . join1) fr) $ bisubstitutePositiveVar vn tr

bisubstituteNegativeVar ::
       forall (ground :: GroundTypeKind) tv t. IsDolanSubtypeGroundType ground
    => TypeVarT tv
    -> DolanType ground 'Negative t
    -> DolanUnifier ground (DolanShim ground tv t)
bisubstituteNegativeVar _ NilDolanType = pure termf
bisubstituteNegativeVar vn (ConsDolanType t1 tr) =
    OpenExpression (leSingleUnifierConstraint vn t1) $
    fmap (\fr f1 -> meetf (meet1 . f1) fr) $ bisubstituteNegativeVar vn tr

bindUnifierMWit ::
       forall (ground :: GroundTypeKind) polarity wit t r. (IsDolanSubtypeGroundType ground)
    => UnifierM ground (DolanShimWit ground polarity t)
    -> (forall t'. DolanType ground polarity t' -> PolarMapType (DolanShim ground) polarity t t' -> Solver ground wit r)
    -> Solver ground wit r
bindUnifierMWit mst call = wbindUnifierM mst $ \(MkShimWit wt (MkPolarMap conv)) -> call wt conv

bisubstituteUnifier ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => UnifierBisubstitution ground
    -> DolanUnifier ground a
    -> UnifierSolver ground a
bisubstituteUnifier _ (ClosedExpression a) = pure a
bisubstituteUnifier bisub@(MkBisubstitution _ vsub mwp _) (OpenExpression (MkUnifierConstraint vwit NegativeType (InvertFlipType tw) _) expr)
    | Just Refl <- testEquality vsub vwit =
        bindUnifierMWit mwp $ \tp convp -> do
            conv <- unifyTypes tp tw
            val' <- bisubstituteUnifier bisub expr
            pure $ val' $ conv . convp
bisubstituteUnifier bisub (OpenExpression (MkUnifierConstraint vwit NegativeType (NormalFlipType tw) _) expr)
    | Just (MkShimWit (VarDolanSingularType v) (MkPolarMap conv)) <- dolanToMaybeTypeShim tw
    , Just Refl <- testEquality v vwit = do
        val' <- bisubstituteUnifier bisub expr
        pure $ val' conv
bisubstituteUnifier bisub@(MkBisubstitution _ vsub mwp _) (OpenExpression (MkUnifierConstraint vwit NegativeType (NormalFlipType tw) _) expr)
    | Just Refl <- testEquality vsub vwit =
        bindUnifierMWit mwp $ \tp convp ->
            bindUnifierMWit (bisubstituteType bisub tw) $ \tw' convw -> do
                conv <- unifyTypes tp tw'
                val' <- bisubstituteUnifier bisub expr
                pure $ val' $ convw . conv . convp
bisubstituteUnifier bisub@(MkBisubstitution _ vsub _ mwq) (OpenExpression (MkUnifierConstraint vwit PositiveType (InvertFlipType tw) _) expr)
    | Just Refl <- testEquality vsub vwit =
        bindUnifierMWit mwq $ \tq convq -> do
            conv <- unifyTypes tw tq
            val' <- bisubstituteUnifier bisub expr
            pure $ val' $ convq . conv
bisubstituteUnifier bisub (OpenExpression (MkUnifierConstraint vwit PositiveType (NormalFlipType tw) _) expr)
    | Just (MkShimWit (VarDolanSingularType v) (MkPolarMap conv)) <- dolanToMaybeTypeShim tw
    , Just Refl <- testEquality v vwit = do
        val' <- bisubstituteUnifier bisub expr
        pure $ val' conv
bisubstituteUnifier bisub@(MkBisubstitution _ vsub _ mwq) (OpenExpression (MkUnifierConstraint vwit PositiveType (NormalFlipType tw) _) expr)
    | Just Refl <- testEquality vsub vwit =
        bindUnifierMWit mwq $ \tq convq ->
            bindUnifierMWit (bisubstituteType bisub tw) $ \tw' convw -> do
                conv <- unifyTypes tw' tq
                val' <- bisubstituteUnifier bisub expr
                pure $ val' $ convq . conv . convw
bisubstituteUnifier bisub (OpenExpression (MkUnifierConstraint vn NegativeType (NormalFlipType tw) _) expr) =
    bindUnifierMWit (bisubstituteType bisub tw) $ \tp' conv -> do
        val' <- bisubstituteUnifier bisub expr
        pv <- solverLiftTypeExpression $ bisubstituteNegativeVar vn tp'
        pure $ val' $ conv . pv
bisubstituteUnifier bisub (OpenExpression (MkUnifierConstraint vn PositiveType (NormalFlipType tw) _) expr) =
    bindUnifierMWit (bisubstituteType bisub tw) $ \tp' conv -> do
        val' <- bisubstituteUnifier bisub expr
        pv <- solverLiftTypeExpression $ bisubstitutePositiveVar vn tp'
        pure $ val' $ pv . conv
bisubstituteUnifier bisub (OpenExpression subwit expr) = solverOpenExpression subwit $ bisubstituteUnifier bisub expr

type InvertSubstitution :: GroundTypeKind -> Type
data InvertSubstitution ground where
    MkInvertSubstitution
        :: forall (ground :: GroundTypeKind) polarity tv tv' t. (JoinMeetType (InvertPolarity polarity) tv' t ~ tv)
        => TypeVarT tv
        -> PolarityType polarity
        -> TypeVarT tv'
        -> DolanType ground polarity t
        -> InvertSubstitution ground

invertSubstitute ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => InvertSubstitution ground
    -> DolanUnifier ground a
    -> UnifierSolver ground a
invertSubstitute _ (ClosedExpression a) = pure a
invertSubstitute sub@(MkInvertSubstitution oldvar substpol newvar st) (OpenExpression (MkUnifierConstraint depvar unipol fvt recv) expr)
    | Just Refl <- testEquality oldvar depvar = let
        solv' = invertSubstitute sub expr
        in solverOpenExpression (MkUnifierConstraint newvar unipol fvt recv) $
           case (substpol, unipol) of
               (PositiveType, PositiveType) -> do
                   fa <- solv'
                   convm <-
                       case fvt of
                           NormalFlipType vt -> unifyTypes vt st
                           InvertFlipType vt -> unifyTypes vt st
                   pure $ \conv -> fa $ meetf conv convm
               (NegativeType, NegativeType) -> do
                   fa <- solv'
                   convm <-
                       case fvt of
                           NormalFlipType vt -> unifyTypes st vt
                           InvertFlipType vt -> unifyTypes st vt
                   pure $ \conv -> fa $ joinf conv convm
               (PositiveType, NegativeType) -> do
                   fa <- solv'
                   pure $ \conv -> fa $ conv . meet1
               (NegativeType, PositiveType) -> do
                   fa <- solv'
                   pure $ \conv -> fa $ join1 . conv
invertSubstitute sub (OpenExpression subwit expr) = solverOpenExpression subwit $ invertSubstitute sub expr

-- | For debugging only. Switching this on will cause issue #206.
genNewName :: Bool
genNewName = False

type ConstraintSolver (ground :: GroundTypeKind) = WriterT [UnifierBisubstitution ground] (DolanTypeCheckM ground)

tellBisubstitution ::
       forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground
    => UnifierBisubstitution ground
    -> ConstraintSolver ground ()
tellBisubstitution bisub = tell [bisub]

runUnifierExpression ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => DolanUnifierExpression ground a
    -> ConstraintSolver ground (TSOpenExpression (DolanTypeSystem ground) a)
runUnifierExpression (MkSolverExpression texpr vexpr) = do
    expr <- runUnifier texpr
    return $ vexpr <*> expr

dpolar1 ::
       forall (ground :: GroundTypeKind) polarity a b. IsDolanSubtypeGroundType ground
    => PolarityType polarity
    -> PolarMap (DolanShim ground) (InvertPolarity polarity) (JoinMeetType polarity a b) a
dpolar1 PositiveType = MkPolarMap join1
dpolar1 NegativeType = MkPolarMap meet1

dpolar2 ::
       forall (ground :: GroundTypeKind) polarity a b. IsDolanSubtypeGroundType ground
    => PolarityType polarity
    -> PolarMapType (DolanShim ground) polarity b (JoinMeetType polarity a b)
dpolar2 PositiveType = join2
dpolar2 NegativeType = meet2

runUnifier ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => DolanUnifier ground a
    -> ConstraintSolver ground (TSOpenExpression (DolanTypeSystem ground) a)
runUnifier (ClosedExpression a) = return $ pure a
runUnifier (OpenExpression (MkUnifierConstraint oldvar (pol :: _ polarity) (fptw :: _ pt) recursive) expr) = do
    MkSomeTypeVarT (newvar :: TypeVarT newtv) <-
        if genNewName
            then lift renamerGenerateFreeUVar
            else return $ MkSomeTypeVarT oldvar
    withRepresentative pol $
        withInvertPolarity @polarity $
        assignTypeVarT @(JoinMeetType polarity newtv pt) oldvar $ do
            let
                newVarWit :: DolanShimWit ground (InvertPolarity polarity) (JoinMeetType polarity newtv pt)
                newVarWit = shimWitToDolan $ MkShimWit (VarDolanSingularType newvar) $ dpolar1 @ground pol
            substwit <-
                if recursive
                    then do
                        MkSomeTypeVarT recvar <- lift renamerGenerateFreeUVar
                        assignSameTypeVarT oldvar recvar $
                            return $ \ptw' ->
                                shimWitToDolan $
                                recursiveDolanShimWit recvar $
                                joinMeetShimWit
                                    (varDolanShimWit newvar)
                                    (bothBisubstitute oldvar (varDolanShimWit recvar) newVarWit ptw')
                    else return $ \ptw' -> joinMeetShimWit (varDolanShimWit newvar) ptw'
            solver <-
                case fptw of
                    NormalFlipType ptw -> do
                        let
                            bisub =
                                mkPolarBisubstitution
                                    False
                                    oldvar
                                    (return $ substwit $ mkPolarShimWit ptw)
                                    (return newVarWit)
                        tellBisubstitution bisub
                        return $ bisubstituteUnifier bisub expr
                    InvertFlipType ptw ->
                        case isInvertInvertPolarity @polarity of
                            Refl -> do
                                rigidity <- lift renamerGetNameRigidity
                                let
                                    bisub =
                                        mkPolarBisubstitution
                                            False
                                            oldvar
                                            (return newVarWit)
                                            (do
                                                 ptw' <- invertTypeM rigidity ptw
                                                 return $ substwit ptw')
                                tellBisubstitution bisub
                                return $
                                    invertSubstitute (MkInvertSubstitution oldvar (invertPolarity pol) newvar ptw) expr
            expr' <- lift $ runSolver solver
            expr'' <- runUnifierExpression expr'
            return $ fmap (\sa -> sa (dpolar2 @ground @polarity @newtv @pt pol)) $ expr''

instance forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground => UnifyTypeSystem (DolanTypeSystem ground) where
    type Unifier (DolanTypeSystem ground) = DolanUnifier ground
    type UnifierSubstitutions (DolanTypeSystem ground) = [UnifierBisubstitution ground]
    unifyNegWitnesses ta tb =
        return $ uuLiftNegShimWit @(DolanTypeSystem ground) $ joinMeetShimWit (mkPolarShimWit ta) (mkPolarShimWit tb)
    unifyPosWitnesses ta tb =
        return $ uuLiftPosShimWit @(DolanTypeSystem ground) $ joinMeetShimWit (mkPolarShimWit ta) (mkPolarShimWit tb)
    unifyPosNegWitnesses tq tp = fmap MkComposeShim $ runSolver $ unifyTypes tq tp
    solveUnifier u = runWriterT $ runUnifier u
    unifierPosSubstitute bisubs t = lift $ runUnifierM $ bisubstitutesType bisubs t
    unifierNegSubstitute bisubs t = lift $ runUnifierM $ bisubstitutesType bisubs t

instance forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground =>
             SubsumeTypeSystem (DolanTypeSystem ground) where
    type Subsumer (DolanTypeSystem ground) = DolanUnifier ground
    type SubsumerSubstitutions (DolanTypeSystem ground) = [UnifierBisubstitution ground]
    usubSubsumer [] subsumer = return $ solverExpressionLiftType subsumer
    usubSubsumer (s:ss) subsumer = do
        subsumer' <- runSolver $ bisubstituteUnifier s subsumer
        usubSubsumerExpression @(DolanTypeSystem ground) ss subsumer'
    solveSubsumer = solveUnifier @(DolanTypeSystem ground)
    subsumerPosSubstitute subs t = lift $ runUnifierM $ bisubstitutesType subs t
    subsumerNegSubstitute subs t = lift $ runUnifierM $ bisubstitutesType subs t
    subsumePosWitnesses tinf tdecl = runSolver $ unifyTypes tinf tdecl

-- used for simplification, where all vars are fixed
checkSameVar ::
       forall (ground :: GroundTypeKind) t. IsDolanSubtypeGroundType ground
    => UnifierConstraint ground t
    -> DolanTypeCheckM ground t
checkSameVar (MkUnifierConstraint va polwit (NormalFlipType (ConsDolanType (VarDolanSingularType vb) NilDolanType)) _)
    | Just Refl <- testEquality va vb =
        return $
        case polwit of
            PositiveType -> iJoinL1
            NegativeType -> iMeetR1
checkSameVar (MkUnifierConstraint va polwit (InvertFlipType (ConsDolanType (VarDolanSingularType vb) NilDolanType)) _)
    | Just Refl <- testEquality va vb =
        return $
        case polwit of
            PositiveType -> iMeetL1
            NegativeType -> iJoinR1
checkSameVar _ = empty

evalDolanUnifierExpression ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => DolanUnifierExpression ground a
    -> DolanTypeCheckM ground (DolanUnifier ground a)
evalDolanUnifierExpression (MkSolverExpression tt (ClosedExpression v)) = return $ fmap v tt
evalDolanUnifierExpression _ = empty

-- used for simplification, where all vars are fixed
subtypeSingularType ::
       forall (ground :: GroundTypeKind) polarity a b. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity a
    -> DolanSingularType ground polarity b
    -> DolanTypeCheckM ground (DolanPolarMap ground polarity a b)
subtypeSingularType ta tb = do
    uexpr <- runSolver $ subsumeSingularTypes ta tb
    expr <- evalDolanUnifierExpression uexpr
    solveExpression checkSameVar expr

invertedPolarSubtype ::
       forall (ground :: GroundTypeKind) polarity a b. (Is PolarityType polarity, IsDolanSubtypeGroundType ground)
    => DolanType ground (InvertPolarity polarity) a
    -> DolanType ground polarity b
    -> DolanTypeCheckM ground (DolanUnifier ground (DolanPolarMap ground polarity a b))
invertedPolarSubtype ta tb = do
    uexpr <-
        runSolver $
        case polarityType @polarity of
            PositiveType -> fmap MkPolarMap $ unifyTypes @ground ta tb
            NegativeType -> fmap MkPolarMap $ unifyTypes tb ta
    evalDolanUnifierExpression uexpr

runCheckUnifier ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => UnifierSolver ground a
    -> DolanTypeCheckM ground Bool
runCheckUnifier us =
    altIs $ do
        MkSolverExpression expr _ <- runSolver us
        _ <- solveUnifier @(DolanTypeSystem ground) expr
        return ()

unifierSubtypeConversionAsGeneralAs ::
       forall (ground :: GroundTypeKind) (dva :: DolanVariance) (gta :: DolanVarianceKind dva) (dvb :: DolanVariance) (gtb :: DolanVarianceKind dvb).
       IsDolanSubtypeGroundType ground
    => SubtypeConversion ground dva gta dvb gtb
    -> SubtypeConversion ground dva gta dvb gtb
    -> DolanM ground Bool
unifierSubtypeConversionAsGeneralAs = subtypeConversionAsGeneralAs runCheckUnifier unifySubtypeContext
