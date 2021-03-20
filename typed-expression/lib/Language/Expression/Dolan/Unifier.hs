{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Unifier
    ( unifyTypesTT
    , invertType
    , subtypeSingularType
    , invertedPolarSubtype
    ) where

import Control.Applicative.Wrapped
import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Combine
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Solver
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Unifier.Constraint
import Language.Expression.Dolan.Unifier.UnifierM
import Language.Expression.Dolan.Unroll
import Shapes

type UnifierBisubstitution :: GroundTypeKind -> Type
type UnifierBisubstitution ground = Bisubstitution ground (DolanPolyShim ground Type) (UnifierM ground)

type DolanUnifier :: GroundTypeKind -> Type -> Type
type DolanUnifier ground = Expression (UnifierConstraint ground)

type UnifierSolver :: GroundTypeKind -> Type -> Type
type UnifierSolver ground = Solver ground (UnifierConstraint ground)

type UnificationSolver :: GroundTypeKind -> Type -> Type -> Type
type UnificationSolver ground a b = UnifierSolver ground (DolanPolyShim ground Type a b)

unifySubtypeContext ::
       forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground
    => SubtypeContext (DolanType ground) (DolanPolyShim ground Type) (UnifierSolver ground)
unifySubtypeContext = MkSubtypeContext unifyTypesTT

unifyGroundTypes ::
       forall (ground :: GroundTypeKind) pola polb dva gta ta dvb gtb tb.
       (IsDolanSubtypeGroundType ground, Is PolarityType pola, Is PolarityType polb)
    => ground dva gta
    -> DolanArguments dva (DolanType ground) gta pola ta
    -> ground dvb gtb
    -> DolanArguments dvb (DolanType ground) gtb polb tb
    -> UnificationSolver ground ta tb
unifyGroundTypes gta argsa gtb argsb = subtypeGroundTypes unifySubtypeContext gta argsa gtb argsb

fromJoinMeetLimit ::
       forall (shim :: ShimKind Type) polarity t. (Is PolarityType polarity, JoinMeetIsoCategory shim)
    => shim (JoinMeetType polarity t (LimitType polarity)) t
fromJoinMeetLimit =
    case polarityType @polarity of
        PositiveType -> iJoinL1
        NegativeType -> iMeetL1

toJoinMeetLimit ::
       forall (shim :: ShimKind Type) polarity t. (Is PolarityType polarity, JoinMeetIsoCategory shim)
    => shim t (JoinMeetType polarity t (LimitType polarity))
toJoinMeetLimit =
    case polarityType @polarity of
        PositiveType -> iJoinR1
        NegativeType -> iMeetR1

unifyTypesSS ::
       forall (ground :: GroundTypeKind) pola polb a b.
       (IsDolanSubtypeGroundType ground, Is PolarityType pola, Is PolarityType polb)
    => DolanSingularType ground pola a
    -> DolanSingularType ground polb b
    -> UnificationSolver ground a b
unifyTypesSS (VarDolanSingularType na) (VarDolanSingularType nb)
    | Just Refl <- testEquality na nb = pure id
unifyTypesSS (VarDolanSingularType na) tb =
    wbind renamerGetNameRigidity $ \isrigid ->
        case isrigid $ witnessToValue na of
            RigidName -> empty
            FreeName ->
                fmap (\conv -> fromJoinMeetLimit @_ @polb . conv) $
                solverLiftExpression $ varExpression $ leSingleUnifierConstraint na tb
unifyTypesSS ta (VarDolanSingularType nb) =
    wbind renamerGetNameRigidity $ \isrigid ->
        case isrigid $ witnessToValue nb of
            RigidName -> empty
            FreeName ->
                fmap (\conv -> conv . toJoinMeetLimit @_ @pola) $
                solverLiftExpression $ varExpression $ geSingleUnifierConstraint nb ta
unifyTypesSS (GroundDolanSingularType gta argsa) (GroundDolanSingularType gtb argsb) =
    unifyGroundTypes gta argsa gtb argsb
unifyTypesSS sta@(RecursiveDolanSingularType _ _) stb = solveRecursiveSingularTypes unifyTypesTT sta stb
unifyTypesSS sta stb@(RecursiveDolanSingularType _ _) = solveRecursiveSingularTypes unifyTypesTT sta stb

unifyTypesSTN ::
       forall (ground :: GroundTypeKind) pola a b. (IsDolanSubtypeGroundType ground, Is PolarityType pola)
    => DolanSingularType ground pola a
    -> DolanType ground 'Negative b
    -> UnificationSolver ground a b
unifyTypesSTN _ NilDolanType = pure termf
unifyTypesSTN ta (ConsDolanType t1 t2) = do
    f1 <- unifyTypesSS ta t1
    f2 <- unifyTypesSTN ta t2
    return $ meetf f1 f2

unifyTypesSTP ::
       forall (ground :: GroundTypeKind) pola a b. (IsDolanSubtypeGroundType ground, Is PolarityType pola)
    => DolanSingularType ground pola a
    -> DolanType ground 'Positive b
    -> UnificationSolver ground a b
unifyTypesSTP _ NilDolanType = empty
unifyTypesSTP ta (ConsDolanType t1 t2) =
    (fmap (\conv -> join1 . conv) $ unifyTypesSS ta t1) <|> (fmap (\conv -> join2 . conv) $ unifyTypesSTP ta t2)

unifyTypesST1 ::
       forall (ground :: GroundTypeKind) pola polb a b.
       (IsDolanSubtypeGroundType ground, Is PolarityType pola, Is PolarityType polb)
    => DolanSingularType ground pola a
    -> DolanType ground polb b
    -> UnificationSolver ground a b
unifyTypesST1 =
    case polarityType @polb of
        NegativeType -> unifyTypesSTN
        PositiveType -> unifyTypesSTP

unifyTypesST ::
       forall (ground :: GroundTypeKind) pola polb a b.
       (IsDolanSubtypeGroundType ground, Is PolarityType pola, Is PolarityType polb)
    => DolanSingularType ground pola a
    -> DolanType ground polb b
    -> UnificationSolver ground a b
unifyTypesST ta@(VarDolanSingularType na) tb =
    wbind renamerGetNameRigidity $ \isrigid ->
        case isrigid $ witnessToValue na of
            RigidName -> unifyTypesST1 ta tb
            FreeName -> solverLiftExpression $ varExpression $ leUnifierConstraint na tb
unifyTypesST ta@(RecursiveDolanSingularType _ _) tb =
    unifyRecursiveType (singularRecursiveOrPlainType ta) (mkShimWit $ PlainType tb)
unifyTypesST ta tb = unifyTypesST1 ta tb

unifyTypesTNS1 ::
       forall (ground :: GroundTypeKind) polb a b. (IsDolanSubtypeGroundType ground, Is PolarityType polb)
    => DolanType ground 'Negative a
    -> DolanSingularType ground polb b
    -> UnificationSolver ground a b
unifyTypesTNS1 NilDolanType _ = empty
unifyTypesTNS1 (ConsDolanType t1 t2) tb =
    (fmap (\conv -> conv . meet1) $ unifyTypesSS t1 tb) <|> (fmap (\conv -> conv . meet2) $ unifyTypesTNS1 t2 tb)

unifyTypesTNS ::
       forall (ground :: GroundTypeKind) polb a b. (IsDolanSubtypeGroundType ground, Is PolarityType polb)
    => DolanType ground 'Negative a
    -> DolanSingularType ground polb b
    -> UnificationSolver ground a b
unifyTypesTNS ta tb@(VarDolanSingularType nb) =
    wbind renamerGetNameRigidity $ \isrigid ->
        case isrigid $ witnessToValue nb of
            RigidName -> unifyTypesTNS1 ta tb
            FreeName -> solverLiftExpression $ varExpression $ geUnifierConstraint nb ta
unifyTypesTNS ta tb@(RecursiveDolanSingularType _ _) =
    unifyRecursiveType (mkShimWit $ PlainType ta) (singularRecursiveOrPlainType tb)
unifyTypesTNS ta tb = unifyTypesTNS1 ta tb

unifyTypesTPT ::
       forall (ground :: GroundTypeKind) polb a b. (IsDolanSubtypeGroundType ground, Is PolarityType polb)
    => DolanType ground 'Positive a
    -> DolanType ground polb b
    -> UnificationSolver ground a b
unifyTypesTPT NilDolanType _ = pure initf
unifyTypesTPT (ConsDolanType ta1 tar) tb = do
    f1 <- unifyTypesST ta1 tb
    f2 <- unifyTypesTPT tar tb
    return $ joinf f1 f2

unifyTypesTNT ::
       forall (ground :: GroundTypeKind) polb a b. (IsDolanSubtypeGroundType ground, Is PolarityType polb)
    => DolanType ground 'Negative a
    -> DolanType ground polb b
    -> UnificationSolver ground a b
unifyTypesTNT NilDolanType NilDolanType =
    case polarityType @polb of
        PositiveType -> empty
        NegativeType -> pure id
unifyTypesTNT NilDolanType _ = empty
unifyTypesTNT (ConsDolanType ta1 tar) tb =
    (fmap (\conv -> conv . meet1) $ unifyTypesST ta1 tb) <|> (fmap (\conv -> conv . meet2) $ unifyTypesTNT tar tb)

unifyTypesTNTN ::
       forall (ground :: GroundTypeKind) a b. (IsDolanSubtypeGroundType ground)
    => DolanType ground 'Negative a
    -> DolanType ground 'Negative b
    -> UnificationSolver ground a b
unifyTypesTNTN _ NilDolanType = pure termf
unifyTypesTNTN ta (ConsDolanType t1 t2) = do
    f1 <- unifyTypesTNS ta t1
    f2 <- unifyTypesTNTN ta t2
    return $ meetf f1 f2

unifyTypesTT ::
       forall (ground :: GroundTypeKind) pola polb a b.
       (IsDolanSubtypeGroundType ground, Is PolarityType pola, Is PolarityType polb)
    => DolanType ground pola a
    -> DolanType ground polb b
    -> UnificationSolver ground a b
unifyTypesTT ta tb =
    wremonad (remonad $ tackOnTypeConvertError ta tb) $
    (case (polarityType @pola, polarityType @polb) of
         (PositiveType, _) -> unifyTypesTPT
         (NegativeType, NegativeType) -> unifyTypesTNTN
         (NegativeType, PositiveType) -> unifyTypesTNT)
        ta
        tb

unifyRecursiveType ::
       forall (ground :: GroundTypeKind) pola polb a b.
       (IsDolanSubtypeGroundType ground, Is PolarityType pola, Is PolarityType polb)
    => PShimWit (DolanPolyIsoShim ground Type) (RecursiveOrPlainType ground) pola a
    -> PShimWit (DolanPolyIsoShim ground Type) (RecursiveOrPlainType ground) polb b
    -> UnificationSolver ground a b
unifyRecursiveType ta tb = solveRecursiveShimWits unifyTypesTT ta tb

bisubstitutePositiveVar ::
       forall (ground :: GroundTypeKind) name t. IsDolanSubtypeGroundType ground
    => SymbolType name
    -> DolanType ground 'Positive t
    -> DolanUnifier ground (DolanPolyShim ground Type t (UVarT name))
bisubstitutePositiveVar _ NilDolanType = pure initf
bisubstitutePositiveVar vn (ConsDolanType t1 tr) =
    OpenExpression (geSingleUnifierConstraint vn t1) $
    fmap (\fr f1 -> joinf (f1 . join1) fr) $ bisubstitutePositiveVar vn tr

bisubstituteNegativeVar ::
       forall (ground :: GroundTypeKind) name t. IsDolanSubtypeGroundType ground
    => SymbolType name
    -> DolanType ground 'Negative t
    -> DolanUnifier ground (DolanPolyShim ground Type (UVarT name) t)
bisubstituteNegativeVar _ NilDolanType = pure termf
bisubstituteNegativeVar vn (ConsDolanType t1 tr) =
    OpenExpression (leSingleUnifierConstraint vn t1) $
    fmap (\fr f1 -> meetf (meet1 . f1) fr) $ bisubstituteNegativeVar vn tr

bindUnifierMWit ::
       forall (ground :: GroundTypeKind) polarity wit t r. (IsDolanSubtypeGroundType ground)
    => UnifierM ground (DolanShimWit ground polarity t)
    -> (forall t'.
                DolanType ground polarity t' -> PolarMapType (DolanPolyShim ground Type) polarity t t' -> Solver ground wit r)
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
            conv <- unifyTypesTT tp tw
            val' <- bisubstituteUnifier bisub expr
            pure $ val' $ conv . convp
bisubstituteUnifier bisub@(MkBisubstitution _ vsub _ mwq) (OpenExpression (GEUnifierConstraint vwit polwit tw _) expr)
    | Just Refl <- testEquality vsub vwit =
        withRepresentative polwit $
        bindUnifierMWit mwq $ \tq convq -> do
            conv <- unifyTypesTT tw tq
            val' <- bisubstituteUnifier bisub expr
            pure $ val' $ convq . conv
bisubstituteUnifier bisub (OpenExpression (LEUnifierConstraint vn NegativeType tw _) expr) =
    bindUnifierMWit (bisubstituteShimWit bisub $ mkShimWit tw) $ \tp' conv -> do
        val' <- bisubstituteUnifier bisub expr
        pv <- solverLiftExpression $ bisubstituteNegativeVar vn tp'
        pure $ val' $ conv . pv
bisubstituteUnifier bisub (OpenExpression (GEUnifierConstraint vn PositiveType tw _) expr) =
    bindUnifierMWit (bisubstituteShimWit bisub $ mkShimWit tw) $ \tp' conv -> do
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
invertSubstitute sub@(MkInvertSubstitution oldvar PositiveType newvar _) (OpenExpression (LEUnifierConstraint depvar PositiveType vt recv) expr)
    | Just Refl <- testEquality oldvar depvar =
        solverOpenExpression (LEUnifierConstraint newvar PositiveType vt recv) $ do
            fa <- invertSubstitute sub expr
            pure $ \conv -> fa $ conv . meet1
invertSubstitute sub@(MkInvertSubstitution oldvar NegativeType newvar st) (OpenExpression (LEUnifierConstraint depvar PositiveType vt recv) expr)
    | Just Refl <- testEquality oldvar depvar =
        solverOpenExpression (LEUnifierConstraint newvar PositiveType vt recv) $ do
            fa <- invertSubstitute sub expr
            convm <- unifyTypesTT st vt
            pure $ \conv -> fa $ joinf conv convm
invertSubstitute sub@(MkInvertSubstitution oldvar NegativeType newvar _) (OpenExpression (GEUnifierConstraint depvar NegativeType vt recv) expr)
    | Just Refl <- testEquality oldvar depvar =
        solverOpenExpression (GEUnifierConstraint newvar NegativeType vt recv) $ do
            fa <- invertSubstitute sub expr
            pure $ \conv -> fa $ join1 . conv
invertSubstitute sub@(MkInvertSubstitution oldvar PositiveType newvar st) (OpenExpression (GEUnifierConstraint depvar NegativeType vt recv) expr)
    | Just Refl <- testEquality oldvar depvar =
        solverOpenExpression (GEUnifierConstraint newvar NegativeType vt recv) $ do
            fa <- invertSubstitute sub expr
            convm <- unifyTypesTT vt st
            pure $ \conv -> fa $ meetf conv convm
invertSubstitute sub (OpenExpression subwit expr) = solverOpenExpression subwit $ invertSubstitute sub expr

runUnifier ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => DolanUnifier ground a
    -> WriterT [UnifierBisubstitution ground] (DolanTypeCheckM ground) a
runUnifier (ClosedExpression a) = return a
runUnifier (OpenExpression (LEUnifierConstraint oldvar NegativeType (ptw :: _ pt) False) expr) =
    newUVar (uVarName oldvar) $ \(newvar :: SymbolType newname) ->
        assignUVar @Type @(MeetType (UVarT newname) pt) oldvar $ do
            let
                bisub =
                    mkPolarBisubstitution
                        False
                        oldvar
                        (return $ joinMeetShimWit (varDolanShimWit newvar) (mkShimWit ptw))
                        (return $ singleDolanShimWit $ MkShimWit (VarDolanSingularType newvar) $ MkPolarMap meet1)
            expr' <- lift $ runSolver $ bisubstituteUnifier bisub expr
            expr'' <- runUnifier expr'
            tell [bisub]
            return $ expr'' meet2
runUnifier (OpenExpression (LEUnifierConstraint oldvar PositiveType (ptw :: _ pt) False) expr) =
    newUVar (uVarName oldvar) $ \(newvar :: SymbolType newname) ->
        assignUVar @Type @(MeetType (UVarT newname) pt) oldvar $ do
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
            expr'' <- runUnifier expr'
            tell [bisub]
            return $ expr'' meet2
runUnifier (OpenExpression (GEUnifierConstraint oldvar PositiveType (ptw :: _ pt) False) expr) =
    newUVar (uVarName oldvar) $ \(newvar :: SymbolType newname) ->
        assignUVar @Type @(JoinType (UVarT newname) pt) oldvar $ do
            let
                bisub =
                    mkPolarBisubstitution
                        False
                        oldvar
                        (return $ joinMeetShimWit (varDolanShimWit newvar) (mkShimWit ptw))
                        (return $ singleDolanShimWit $ MkShimWit (VarDolanSingularType newvar) $ MkPolarMap join1)
            expr' <- lift $ runSolver $ bisubstituteUnifier bisub expr
            expr'' <- runUnifier expr'
            tell [bisub]
            return $ expr'' join2
runUnifier (OpenExpression (GEUnifierConstraint oldvar NegativeType (ptw :: _ pt) False) expr) =
    newUVar (uVarName oldvar) $ \(newvar :: SymbolType newname) ->
        assignUVar @Type @(JoinType (UVarT newname) pt) oldvar $ do
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
            expr'' <- runUnifier expr'
            tell [bisub]
            return $ expr'' join2
runUnifier (OpenExpression (LEUnifierConstraint (oldvar :: SymbolType oldname) NegativeType (ptw :: _ pt) True) expr) = do
    newvarname <- lift $ renamerGenerate FreeName []
    newUVar newvarname $ \(newvar :: SymbolType newname) ->
        assignUVar @Type @(MeetType (UVarT newname) pt) oldvar $ do
            let
                bisub =
                    mkPolarBisubstitution
                        False
                        oldvar
                        (return $
                         singleDolanShimWit $
                         recursiveDolanShimWit oldvar $ joinMeetShimWit (varDolanShimWit newvar) (mkShimWit ptw))
                        (return $ singleDolanShimWit $ MkShimWit (VarDolanSingularType newvar) $ MkPolarMap meet1)
            expr' <- lift $ runSolver $ bisubstituteUnifier bisub expr
            expr'' <- runUnifier expr'
            tell [bisub]
            return $ expr'' meet2
runUnifier (OpenExpression (LEUnifierConstraint (oldvar :: SymbolType oldname) PositiveType (ptw :: _ pt) True) expr) = do
    newvarname <- lift $ renamerGenerate FreeName []
    newUVar newvarname $ \(newvar :: SymbolType newname) ->
        assignUVar @Type @(MeetType (UVarT newname) pt) oldvar $ do
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
                                 recursiveDolanShimWit oldvar $ joinMeetShimWit (varDolanShimWit newvar) tq)
            expr' <- lift $ runSolver $ invertSubstitute (MkInvertSubstitution oldvar PositiveType newvar ptw) expr
            expr'' <- runUnifier expr'
            tell [bisub]
            return $ expr'' meet2
runUnifier (OpenExpression (GEUnifierConstraint (oldvar :: SymbolType oldname) PositiveType (ptw :: _ pt) True) expr) = do
    newvarname <- lift $ renamerGenerate FreeName []
    newUVar newvarname $ \(newvar :: SymbolType newname) ->
        assignUVar @Type @(JoinType (UVarT newname) pt) oldvar $ do
            let
                bisub =
                    mkPolarBisubstitution
                        False
                        oldvar
                        (return $
                         singleDolanShimWit $
                         recursiveDolanShimWit oldvar $ joinMeetShimWit (varDolanShimWit newvar) (mkShimWit ptw))
                        (return $ singleDolanShimWit $ MkShimWit (VarDolanSingularType newvar) $ MkPolarMap join1)
            expr' <- lift $ runSolver $ bisubstituteUnifier bisub expr
            expr'' <- runUnifier expr'
            tell [bisub]
            return $ expr'' join2
runUnifier (OpenExpression (GEUnifierConstraint (oldvar :: SymbolType oldname) NegativeType (ptw :: _ pt) True) expr) = do
    newvarname <- lift $ renamerGenerate FreeName []
    newUVar newvarname $ \(newvar :: SymbolType newname) ->
        assignUVar @Type @(JoinType (UVarT newname) pt) oldvar $ do
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
                                 recursiveDolanShimWit oldvar $ joinMeetShimWit (varDolanShimWit newvar) tq)
            expr' <- lift $ runSolver $ invertSubstitute (MkInvertSubstitution oldvar NegativeType newvar ptw) expr
            expr'' <- runUnifier expr'
            tell [bisub]
            return $ expr'' join2

instance forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground => UnifyTypeSystem (DolanTypeSystem ground) where
    type Unifier (DolanTypeSystem ground) = DolanUnifier ground
    type UnifierSubstitutions (DolanTypeSystem ground) = [UnifierBisubstitution ground]
    unifyNegWitnesses ta tb = return $ uuLiftNegShimWit $ joinMeetShimWit (mkShimWit ta) (mkShimWit tb)
    unifyPosWitnesses ta tb = return $ uuLiftPosShimWit $ joinMeetShimWit (mkShimWit ta) (mkShimWit tb)
    unifyPosNegWitnesses tq tp = fmap MkUUShim $ runSolver $ unifyTypesTT tq tp
    solveUnifier u = fmap (\(a, subs) -> (a, reverse subs)) $ runWriterT $ runUnifier u
    unifierPosSubstitute bisubs t = lift $ runUnifierM $ bisubstitutesType bisubs t
    unifierNegSubstitute bisubs t = lift $ runUnifierM $ bisubstitutesType bisubs t

instance forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground =>
             SubsumeTypeSystem (DolanTypeSystem ground) where
    type Subsumer (DolanTypeSystem ground) = DolanUnifier ground
    type SubsumerSubstitutions (DolanTypeSystem ground) = [UnifierBisubstitution ground]
    usubSubsumer [] subsumer = return subsumer
    usubSubsumer (s:ss) subsumer = do
        subsumer' <- runSolver $ bisubstituteUnifier s subsumer
        usubSubsumer @(DolanTypeSystem ground) ss subsumer'
    solveSubsumer = solveUnifier @(DolanTypeSystem ground)
    subsumerNegSubstitute subs t = lift $ runUnifierM $ bisubstitutesType subs t
    subsumePosWitnesses tinf tdecl = runSolver $ unifyTypesTT tinf tdecl

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

subsumeSingularTypes ::
       forall (ground :: GroundTypeKind) polarity a b. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity a
    -> DolanSingularType ground polarity b
    -> UnifierSolver ground (DolanPolarMap ground polarity a b)
subsumeSingularTypes ta tb =
    case polarityType @polarity of
        PositiveType -> fmap MkPolarMap $ unifyTypesSS ta tb
        NegativeType -> fmap MkPolarMap $ unifyTypesSS tb ta

-- used for simplification, where all vars are fixed
subtypeSingularType ::
       forall (ground :: GroundTypeKind) polarity a b. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity a
    -> DolanSingularType ground polarity b
    -> DolanTypeCheckM ground (DolanPolarMap ground polarity a b)
subtypeSingularType ta tb = do
    expr <- runSolver $ subsumeSingularTypes ta tb
    solveExpression checkSameVar expr

invertedPolarSubtype ::
       forall (ground :: GroundTypeKind) polarity a b. (Is PolarityType polarity, IsDolanSubtypeGroundType ground)
    => DolanType ground (InvertPolarity polarity) a
    -> DolanType ground polarity b
    -> DolanTypeCheckM ground (DolanUnifier ground (DolanPolarMap ground polarity a b))
invertedPolarSubtype ta tb =
    runSolver $
    case polarityType @polarity of
        PositiveType -> fmap MkPolarMap $ unifyTypesTT @ground ta tb
        NegativeType -> fmap MkPolarMap $ unifyTypesTT tb ta
