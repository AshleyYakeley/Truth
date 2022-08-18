{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Unifier
    ( invertType
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
import Shapes

type DolanUnifier :: GroundTypeKind -> Type -> Type
type DolanUnifier ground = Expression (UnifierConstraint ground)

type DolanUnifierExpression :: GroundTypeKind -> Type -> Type
type DolanUnifierExpression ground = DolanSolverExpression ground (UnifierConstraint ground)

type UnifierBisubstitution :: GroundTypeKind -> Type
type UnifierBisubstitution ground = Bisubstitution ground (DolanShim ground) (UnifierM ground)

bisubstitutePositiveVar ::
       forall (ground :: GroundTypeKind) name t. IsDolanSubtypeGroundType ground
    => SymbolType name
    -> DolanType ground 'Positive t
    -> DolanUnifier ground (DolanShim ground t (UVarT name))
bisubstitutePositiveVar _ NilDolanType = pure initf
bisubstitutePositiveVar vn (ConsDolanType t1 tr) =
    OpenExpression (geSingleUnifierConstraint vn t1) $
    fmap (\fr f1 -> joinf (f1 . join1) fr) $ bisubstitutePositiveVar vn tr

bisubstituteNegativeVar ::
       forall (ground :: GroundTypeKind) name t. IsDolanSubtypeGroundType ground
    => SymbolType name
    -> DolanType ground 'Negative t
    -> DolanUnifier ground (DolanShim ground (UVarT name) t)
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
bisubstituteUnifier bisub@(MkBisubstitution _ vsub mwp _) (OpenExpression (LEUnifierConstraint vwit polwit tw _) expr)
    | Just Refl <- testEquality vsub vwit =
        withRepresentative polwit $
        bindUnifierMWit mwp $ \tp convp -> do
            conv <- unifyTypes tp tw
            val' <- bisubstituteUnifier bisub expr
            pure $ val' $ conv . convp
bisubstituteUnifier bisub@(MkBisubstitution _ vsub _ mwq) (OpenExpression (GEUnifierConstraint vwit polwit tw _) expr)
    | Just Refl <- testEquality vsub vwit =
        withRepresentative polwit $
        bindUnifierMWit mwq $ \tq convq -> do
            conv <- unifyTypes tw tq
            val' <- bisubstituteUnifier bisub expr
            pure $ val' $ convq . conv
bisubstituteUnifier bisub (OpenExpression (LEUnifierConstraint vn NegativeType tw _) expr) =
    bindUnifierMWit (bisubstituteType bisub tw) $ \tp' conv -> do
        val' <- bisubstituteUnifier bisub expr
        pv <- solverLiftExpression $ bisubstituteNegativeVar vn tp'
        pure $ val' $ conv . pv
bisubstituteUnifier bisub (OpenExpression (GEUnifierConstraint vn PositiveType tw _) expr) =
    bindUnifierMWit (bisubstituteType bisub tw) $ \tp' conv -> do
        val' <- bisubstituteUnifier bisub expr
        pv <- solverLiftExpression $ bisubstitutePositiveVar vn tp'
        pure $ val' $ pv . conv
bisubstituteUnifier bisub (OpenExpression subwit expr) = solverOpenExpression subwit $ bisubstituteUnifier bisub expr

type InvertSubstitution :: GroundTypeKind -> Type
data InvertSubstitution ground where
    MkInvertSubstitution
        :: forall (ground :: GroundTypeKind) polarity name name' t.
           (JoinMeetType (InvertPolarity polarity) (UVarT name') t ~ UVarT name)
        => SymbolType name
        -> PolarityType polarity
        -> SymbolType name'
        -> DolanType ground polarity t
        -> InvertSubstitution ground

invertSubstitute ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => InvertSubstitution ground
    -> DolanUnifier ground a
    -> UnifierSolver ground a
invertSubstitute _ (ClosedExpression a) = pure a
invertSubstitute sub@(MkInvertSubstitution oldvar PositiveType newvar _) (OpenExpression (LEUnifierConstraint depvar pol vt recv) expr)
    | Just Refl <- testEquality oldvar depvar =
        solverOpenExpression (LEUnifierConstraint newvar pol vt recv) $ do
            fa <- invertSubstitute sub expr
            pure $ \conv -> fa $ conv . meet1
invertSubstitute sub@(MkInvertSubstitution oldvar NegativeType newvar st) (OpenExpression (LEUnifierConstraint depvar pol vt recv) expr)
    | Just Refl <- testEquality oldvar depvar =
        solverOpenExpression (LEUnifierConstraint newvar pol vt recv) $ do
            fa <- invertSubstitute sub expr
            convm <- withRepresentative pol $ unifyTypes st vt
            pure $ \conv -> fa $ joinf conv convm
invertSubstitute sub@(MkInvertSubstitution oldvar NegativeType newvar _) (OpenExpression (GEUnifierConstraint depvar pol vt recv) expr)
    | Just Refl <- testEquality oldvar depvar =
        solverOpenExpression (GEUnifierConstraint newvar pol vt recv) $ do
            fa <- invertSubstitute sub expr
            pure $ \conv -> fa $ join1 . conv
invertSubstitute sub@(MkInvertSubstitution oldvar PositiveType newvar st) (OpenExpression (GEUnifierConstraint depvar pol vt recv) expr)
    | Just Refl <- testEquality oldvar depvar =
        solverOpenExpression (GEUnifierConstraint newvar pol vt recv) $ do
            fa <- invertSubstitute sub expr
            convm <- withRepresentative pol $ unifyTypes vt st
            pure $ \conv -> fa $ meetf conv convm
invertSubstitute sub (OpenExpression subwit expr) = solverOpenExpression subwit $ invertSubstitute sub expr

-- | possibly can be switched off, but safer on
genNewNamePlain :: Bool
genNewNamePlain = True

-- | switching this off will break the unifier
genNewNameRec :: Bool
genNewNameRec = True

runUnifierExpression ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => DolanUnifierExpression ground a
    -> WriterT [UnifierBisubstitution ground] (DolanTypeCheckM ground) (TSOpenExpression (DolanTypeSystem ground) a)
runUnifierExpression (MkSolverExpression texpr vexpr) = do
    expr <- runUnifier texpr
    return $ vexpr <*> expr

runUnifier ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => DolanUnifier ground a
    -> WriterT [UnifierBisubstitution ground] (DolanTypeCheckM ground) (TSOpenExpression (DolanTypeSystem ground) a)
runUnifier (ClosedExpression a) = return $ pure a
runUnifier (OpenExpression (LEUnifierConstraint oldvar NegativeType (ptw :: _ pt) False) expr) = do
    MkAnyVar (newvar :: SymbolType newname) <-
        if genNewNamePlain
            then lift renamerGenerateFreeUVar
            else return $ MkAnyVar oldvar
    assignUVarT @(MeetType (UVarT newname) pt) oldvar $ do
        let
            bisub =
                mkPolarBisubstitution
                    False
                    oldvar
                    (return $ joinMeetShimWit (varDolanShimWit newvar) (mkPolarShimWit ptw))
                    (return $ singleDolanShimWit $ MkShimWit (VarDolanSingularType newvar) $ MkPolarMap meet1)
        expr' <- lift $ runSolver $ bisubstituteUnifier bisub expr
        expr'' <- runUnifierExpression @ground expr'
        tell [bisub]
        return $ fmap (\sa -> sa meet2) $ expr''
runUnifier (OpenExpression (LEUnifierConstraint oldvar PositiveType (ptw :: _ pt) False) expr) = do
    MkAnyVar (newvar :: SymbolType newname) <-
        if genNewNamePlain
            then lift renamerGenerateFreeUVar
            else return $ MkAnyVar oldvar
    assignUVarT @(MeetType (UVarT newname) pt) oldvar $ do
        let
            bisub =
                mkPolarBisubstitution
                    False
                    oldvar
                    (return $ singleDolanShimWit $ MkShimWit (VarDolanSingularType newvar) $ MkPolarMap meet1)
                    (do
                         tq <- invertTypeM ptw
                         return $ joinMeetShimWit (varDolanShimWit newvar) tq)
        expr' <- lift $ runSolver $ invertSubstitute (MkInvertSubstitution oldvar PositiveType newvar ptw) expr
        expr'' <- runUnifierExpression expr'
        tell [bisub]
        return $ fmap (\sa -> sa meet2) $ expr''
runUnifier (OpenExpression (GEUnifierConstraint oldvar PositiveType (ptw :: _ pt) False) expr) = do
    MkAnyVar (newvar :: SymbolType newname) <-
        if genNewNamePlain
            then lift renamerGenerateFreeUVar
            else return $ MkAnyVar oldvar
    assignUVarT @(JoinType (UVarT newname) pt) oldvar $ do
        let
            bisub =
                mkPolarBisubstitution
                    False
                    oldvar
                    (return $ joinMeetShimWit (varDolanShimWit newvar) (mkPolarShimWit ptw))
                    (return $ singleDolanShimWit $ MkShimWit (VarDolanSingularType newvar) $ MkPolarMap join1)
        expr' <- lift $ runSolver $ bisubstituteUnifier bisub expr
        expr'' <- runUnifierExpression expr'
        tell [bisub]
        return $ fmap (\sa -> sa join2) $ expr''
runUnifier (OpenExpression (GEUnifierConstraint oldvar NegativeType (ptw :: _ pt) False) expr) = do
    MkAnyVar (newvar :: SymbolType newname) <-
        if genNewNamePlain
            then lift renamerGenerateFreeUVar
            else return $ MkAnyVar oldvar
    assignUVarT @(JoinType (UVarT newname) pt) oldvar $ do
        let
            bisub =
                mkPolarBisubstitution
                    False
                    oldvar
                    (return $ singleDolanShimWit $ MkShimWit (VarDolanSingularType newvar) $ MkPolarMap join1)
                    (do
                         tq <- invertTypeM ptw
                         return $ joinMeetShimWit (varDolanShimWit newvar) tq)
        expr' <- lift $ runSolver $ invertSubstitute (MkInvertSubstitution oldvar NegativeType newvar ptw) expr
        expr'' <- runUnifierExpression expr'
        tell [bisub]
        return $ fmap (\sa -> sa join2) $ expr''
runUnifier (OpenExpression (LEUnifierConstraint (oldvar :: SymbolType oldname) NegativeType (ptw :: _ pt) True) expr) = do
    MkAnyVar (newvar :: SymbolType newname) <-
        if genNewNameRec
            then lift renamerGenerateFreeUVar
            else return $ MkAnyVar oldvar
    assignUVarT @(MeetType (UVarT newname) pt) oldvar $ do
        recvarname <- lift renamerGenerateFree
        let
            bisub =
                mkPolarBisubstitution
                    False
                    oldvar
                    (return $
                     singleDolanShimWit $
                     recursiveRenameDolanShimWit oldvar recvarname $
                     joinMeetShimWit (varDolanShimWit newvar) (mkPolarShimWit ptw))
                    (return $ singleDolanShimWit $ MkShimWit (VarDolanSingularType newvar) $ MkPolarMap meet1)
        expr' <- lift $ runSolver $ bisubstituteUnifier bisub expr
        expr'' <- runUnifierExpression expr'
        tell [bisub]
        return $ fmap (\sa -> sa meet2) $ expr''
runUnifier (OpenExpression (LEUnifierConstraint (oldvar :: SymbolType oldname) PositiveType (ptw :: _ pt) True) expr) = do
    MkAnyVar (newvar :: SymbolType newname) <-
        if genNewNameRec
            then lift renamerGenerateFreeUVar
            else return $ MkAnyVar oldvar
    assignUVarT @(MeetType (UVarT newname) pt) oldvar $ do
        recvarname <- lift renamerGenerateFree
        let
            bisub =
                mkPolarBisubstitution
                    False
                    oldvar
                    (return $ singleDolanShimWit $ MkShimWit (VarDolanSingularType newvar) $ MkPolarMap meet1)
                    (do
                         tq <- invertTypeM ptw
                         return $
                             singleDolanShimWit $
                             recursiveRenameDolanShimWit oldvar recvarname $ joinMeetShimWit (varDolanShimWit newvar) tq)
        expr' <- lift $ runSolver $ invertSubstitute (MkInvertSubstitution oldvar PositiveType newvar ptw) expr
        expr'' <- runUnifierExpression expr'
        tell [bisub]
        return $ fmap (\sa -> sa meet2) $ expr''
runUnifier (OpenExpression (GEUnifierConstraint (oldvar :: SymbolType oldname) PositiveType (ptw :: _ pt) True) expr) = do
    MkAnyVar (newvar :: SymbolType newname) <-
        if genNewNameRec
            then lift renamerGenerateFreeUVar
            else return $ MkAnyVar oldvar
    assignUVarT @(JoinType (UVarT newname) pt) oldvar $ do
        recvarname <- lift renamerGenerateFree
        let
            bisub =
                mkPolarBisubstitution
                    False
                    oldvar
                    (return $
                     singleDolanShimWit $
                     recursiveRenameDolanShimWit oldvar recvarname $
                     joinMeetShimWit (varDolanShimWit newvar) (mkPolarShimWit ptw))
                    (return $ singleDolanShimWit $ MkShimWit (VarDolanSingularType newvar) $ MkPolarMap join1)
        expr' <- lift $ runSolver $ bisubstituteUnifier bisub expr
        expr'' <- runUnifierExpression expr'
        tell [bisub]
        return $ fmap (\sa -> sa join2) $ expr''
runUnifier (OpenExpression (GEUnifierConstraint (oldvar :: SymbolType oldname) NegativeType (ptw :: _ pt) True) expr) = do
    MkAnyVar (newvar :: SymbolType newname) <-
        if genNewNameRec
            then lift renamerGenerateFreeUVar
            else return $ MkAnyVar oldvar
    assignUVarT @(JoinType (UVarT newname) pt) oldvar $ do
        recvarname <- lift renamerGenerateFree
        let
            bisub =
                mkPolarBisubstitution
                    False
                    oldvar
                    (return $ singleDolanShimWit $ MkShimWit (VarDolanSingularType newvar) $ MkPolarMap join1)
                    (do
                         tq <- invertTypeM ptw
                         return $
                             singleDolanShimWit $
                             recursiveRenameDolanShimWit oldvar recvarname $ joinMeetShimWit (varDolanShimWit newvar) tq)
        expr' <- lift $ runSolver $ invertSubstitute (MkInvertSubstitution oldvar NegativeType newvar ptw) expr
        expr'' <- runUnifierExpression expr'
        tell [bisub]
        return $ fmap (\sa -> sa join2) $ expr''

instance forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground => UnifyTypeSystem (DolanTypeSystem ground) where
    type Unifier (DolanTypeSystem ground) = DolanUnifier ground
    type UnifierSubstitutions (DolanTypeSystem ground) = [UnifierBisubstitution ground]
    unifyNegWitnesses ta tb =
        return $ uuLiftNegShimWit @(DolanTypeSystem ground) $ joinMeetShimWit (mkPolarShimWit ta) (mkPolarShimWit tb)
    unifyPosWitnesses ta tb =
        return $ uuLiftPosShimWit @(DolanTypeSystem ground) $ joinMeetShimWit (mkPolarShimWit ta) (mkPolarShimWit tb)
    unifyPosNegWitnesses tq tp = fmap MkComposeShim $ runSolver $ unifyTypes tq tp
    solveUnifier u = fmap (\(a, subs) -> (a, reverse subs)) $ runWriterT $ runUnifier u
    unifierPosSubstitute bisubs t = lift $ runUnifierM $ bisubstitutesType bisubs t
    unifierNegSubstitute bisubs t = lift $ runUnifierM $ bisubstitutesType bisubs t

instance forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground =>
             SubsumeTypeSystem (DolanTypeSystem ground) where
    type Subsumer (DolanTypeSystem ground) = DolanUnifier ground
    type SubsumerSubstitutions (DolanTypeSystem ground) = [UnifierBisubstitution ground]
    usubSubsumer [] subsumer = return $ solverLiftTypeExpression subsumer
    usubSubsumer (s:ss) subsumer = do
        subsumer' <- runSolver $ bisubstituteUnifier s subsumer
        usubSubsumerExpression @(DolanTypeSystem ground) ss subsumer'
    solveSubsumer = solveUnifier @(DolanTypeSystem ground)
    subsumerNegSubstitute subs t = lift $ runUnifierM $ bisubstitutesType subs t
    subsumePosWitnesses tinf tdecl = runSolver $ unifyTypes tinf tdecl

-- used for simplification, where all vars are fixed
checkSameVar ::
       forall (ground :: GroundTypeKind) t. IsDolanSubtypeGroundType ground
    => UnifierConstraint ground t
    -> DolanTypeCheckM ground t
checkSameVar (LEUnifierConstraint va polwit (ConsDolanType (VarDolanSingularType vb) NilDolanType) _)
    | Just Refl <- testEquality va vb =
        return $
        case polwit of
            PositiveType -> iJoinR1
            NegativeType -> iMeetR1
checkSameVar (GEUnifierConstraint va polwit (ConsDolanType (VarDolanSingularType vb) NilDolanType) _)
    | Just Refl <- testEquality va vb =
        return $
        case polwit of
            PositiveType -> iJoinL1
            NegativeType -> iMeetL1
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
