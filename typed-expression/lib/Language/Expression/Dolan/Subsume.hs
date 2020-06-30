{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Subsume
    ( DolanSubsumer
    , subtypeSingularType
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Combine
import Language.Expression.Dolan.Inverted
import Language.Expression.Dolan.Solver
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.TypeVariable
import Shapes

minimalPositiveSupertypeSingular ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => DolanSingularType ground 'Negative a
    -> Maybe (DolanShimWit ground 'Positive a)
minimalPositiveSupertypeSingular (VarDolanSingularType v) =
    Just $ singleDolanShimWit $ mkShimWit $ VarDolanSingularType v
minimalPositiveSupertypeSingular (GroundDolanSingularType gt args) = do
    MkShimWit args' conv <-
        mapInvertDolanArgumentsM limitInvertType (groundTypeVarianceType gt) (groundTypeVarianceMap gt) args
    return $ singleDolanShimWit $ MkShimWit (GroundDolanSingularType gt args') conv

minimalPositiveSupertypePlain ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => DolanPlainType ground 'Negative a
    -> Maybe (DolanShimWit ground 'Positive a)
minimalPositiveSupertypePlain (ConsDolanPlainType t NilDolanPlainType) = do
    tf <- minimalPositiveSupertypeSingular t
    return $ ccontramap @_ @_ @(DolanPolyShim ground Type) meet1 tf
minimalPositiveSupertypePlain _ = Nothing

minimalPositiveSupertype ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => DolanType ground 'Negative a
    -> Maybe (DolanShimWit ground 'Positive a)
minimalPositiveSupertype (PlainDolanType pt) = minimalPositiveSupertypePlain pt
minimalPositiveSupertype (RecursiveDolanType n pt) = do
    pt' <- minimalPositiveSupertypePlain pt
    return $ recursiveDolanShimWit (uVarName n) pt'

maximalNegativeSubtypeSingular ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => DolanSingularType ground 'Positive a
    -> Maybe (DolanShimWit ground 'Negative a)
maximalNegativeSubtypeSingular (VarDolanSingularType v) = Just $ singleDolanShimWit $ mkShimWit $ VarDolanSingularType v
maximalNegativeSubtypeSingular (GroundDolanSingularType gt args) = do
    MkShimWit args' conv <-
        mapInvertDolanArgumentsM limitInvertType (groundTypeVarianceType gt) (groundTypeVarianceMap gt) args
    return $ singleDolanShimWit $ MkShimWit (GroundDolanSingularType gt args') conv

maximalNegativeSubtypePlain ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => DolanPlainType ground 'Positive a
    -> Maybe (DolanShimWit ground 'Negative a)
maximalNegativeSubtypePlain (ConsDolanPlainType t NilDolanPlainType) = do
    tf <- maximalNegativeSubtypeSingular t
    return $ cfmap @_ @_ @(DolanPolyShim ground Type) join1 tf
maximalNegativeSubtypePlain _ = Nothing

maximalNegativeSubtype ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => DolanType ground 'Positive a
    -> Maybe (DolanShimWit ground 'Negative a)
maximalNegativeSubtype (PlainDolanType pt) = maximalNegativeSubtypePlain pt
maximalNegativeSubtype (RecursiveDolanType n pt) = do
    pt' <- maximalNegativeSubtypePlain pt
    return $ recursiveDolanShimWit (uVarName n) pt'

limitInvertType ::
       forall (ground :: GroundTypeKind) polarity a. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity a
    -> Maybe (DolanShimWit ground (InvertPolarity polarity) a)
limitInvertType =
    case polarityType @polarity of
        PositiveType -> maximalNegativeSubtype @ground
        NegativeType -> minimalPositiveSupertype @ground

type SubsumerError :: GroundTypeKind -> Type
data SubsumerError ground where
    UninvertibleError
        :: forall (ground :: GroundTypeKind) polarity t. Is PolarityType polarity
        => DolanType ground polarity t
        -> SubsumerError ground

type SubsumerM :: GroundTypeKind -> Type -> Type
type SubsumerM ground = Result (SubsumerError ground)

runSubsumerM ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => SubsumerM ground a
    -> DolanTypeCheckM ground a
runSubsumerM (SuccessResult a) = return a
runSubsumerM (FailureResult (UninvertibleError t)) = liftTypeCheck $ throwTypeNotInvertible t

limitInvertType' ::
       forall (ground :: GroundTypeKind) polarity a. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity a
    -> SubsumerM ground (DolanShimWit ground (InvertPolarity polarity) a)
limitInvertType' t =
    case limitInvertType t of
        Just r -> return r
        Nothing -> FailureResult (UninvertibleError t)

-- Kind of the dual of 'BisubstitutionWitness'.
type SubsumeWitness :: GroundTypeKind -> Type -> Type
data SubsumeWitness ground t where
    MkSubsumeWitness
        :: forall (ground :: GroundTypeKind) polarity name t. Is PolarityType polarity
        => SymbolType name
        -> DolanPlainType ground polarity t
        -> SubsumeWitness ground (DolanPolarMap ground polarity (UVar Type name) t)

type DolanSubsumer :: GroundTypeKind -> Type -> Type
type DolanSubsumer ground = Expression (SubsumeWitness ground)

type FullSubsumer :: GroundTypeKind -> Type -> Type
type FullSubsumer ground = Solver ground (SubsumeWitness ground)

subsumeContext ::
       forall (ground :: GroundTypeKind) polarity. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => SubtypeContext (DolanType ground) (DolanPolyShim ground Type) (FullSubsumer ground) polarity polarity
subsumeContext = let
    subtypeTypes ::
           forall ta tb.
           DolanType ground polarity ta
        -> DolanType ground polarity tb
        -> FullSubsumer ground (DolanPolyShim ground Type ta tb)
    subtypeTypes ta tb =
        case polarityType @polarity of
            PositiveType -> fmap unPolarMap $ subsumeType ta tb
            NegativeType -> fmap unPolarMap $ subsumeType tb ta
    subtypeInverted = invertPolarity @polarity subsumeContext
    in MkSubtypeContext {..}

subsumeGroundSingularType ::
       forall (ground :: GroundTypeKind) polarity dv ginf inf decl.
       (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => ground dv ginf
    -> DolanArguments dv (DolanType ground) ginf polarity inf
    -> DolanSingularType ground polarity decl
    -> FullSubsumer ground (DolanPolarMap ground polarity inf decl)
subsumeGroundSingularType _gtinf _targsinf (VarDolanSingularType _vdecl) = empty
subsumeGroundSingularType gtinf targsinf (GroundDolanSingularType gtdecl targsdecl) =
    case polarityType @polarity of
        PositiveType -> fmap MkPolarMap $ subtypeGroundTypes solverLiftM subsumeContext gtinf targsinf gtdecl targsdecl
        NegativeType -> fmap MkPolarMap $ subtypeGroundTypes solverLiftM subsumeContext gtdecl targsdecl gtinf targsinf

subsumeSingularSingularType ::
       forall (ground :: GroundTypeKind) polarity inf decl. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity inf
    -> DolanSingularType ground polarity decl
    -> FullSubsumer ground (DolanPolarMap ground polarity inf decl)
subsumeSingularSingularType (VarDolanSingularType vinf) tdecl =
    fmap (\conv -> iPolarL1 <.> conv) $
    solverLiftExpression $ varExpression $ MkSubsumeWitness vinf $ ConsDolanPlainType tdecl NilDolanPlainType
subsumeSingularSingularType (GroundDolanSingularType ginf argsinf) tdecl = subsumeGroundSingularType ginf argsinf tdecl

subsumeGroundPlainType ::
       forall (ground :: GroundTypeKind) polarity dv ginf inf decl.
       (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => ground dv ginf
    -> DolanArguments dv (DolanType ground) ginf polarity inf
    -> DolanPlainType ground polarity decl
    -> FullSubsumer ground (DolanPolarMap ground polarity inf decl)
subsumeGroundPlainType _gtinf _targsinf NilDolanPlainType = empty
subsumeGroundPlainType gtinf targsinf (ConsDolanPlainType t1 tr) =
    (fmap (\conv -> polar1 . conv) $ subsumeGroundSingularType gtinf targsinf t1) <|>
    (fmap (\conv -> polar2 . conv) $ subsumeGroundPlainType gtinf targsinf tr)

subsumeSingularPlainType ::
       forall (ground :: GroundTypeKind) polarity inf decl. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity inf
    -> DolanPlainType ground polarity decl
    -> FullSubsumer ground (DolanPolarMap ground polarity inf decl)
subsumeSingularPlainType (VarDolanSingularType vinf) tdecl =
    solverLiftExpression $ varExpression $ MkSubsumeWitness vinf tdecl
subsumeSingularPlainType tinf@(GroundDolanSingularType ginf argsinf) tdecl =
    subsumeGroundPlainType ginf argsinf tdecl <|> (solverLiftM $ throwTypeSubsumeError tinf tdecl)

subsumePlainPlainType ::
       forall (ground :: GroundTypeKind) polarity inf decl. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanPlainType ground polarity inf
    -> DolanPlainType ground polarity decl
    -> FullSubsumer ground (DolanPolarMap ground polarity inf decl)
subsumePlainPlainType NilDolanPlainType _ = pure polarLimit
subsumePlainPlainType (ConsDolanPlainType t1 tr) tdecl =
    liftA2 polarF (subsumeSingularPlainType t1 tdecl) (subsumePlainPlainType tr tdecl)

subsumeType ::
       forall (ground :: GroundTypeKind) polarity inf decl. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity inf
    -> DolanType ground polarity decl
    -> FullSubsumer ground (DolanPolarMap ground polarity inf decl)
subsumeType tinf tdecl =
    case polarityType @polarity of
        PositiveType ->
            fmap MkPolarMap $
            solveRecursiveTypes
                @ground
                @polarity
                @polarity
                (\pinf pdecl -> fmap unPolarMap $ subsumePlainPlainType pinf pdecl)
                tinf
                tdecl
        NegativeType ->
            fmap MkPolarMap $
            solveRecursiveTypes
                @ground
                @polarity
                @polarity
                (\pdecl pinf -> fmap unPolarMap $ subsumePlainPlainType pinf pdecl)
                tdecl
                tinf

-- used for simplification, where all vars are fixed
checkSameVar ::
       forall (ground :: GroundTypeKind) t. IsDolanSubtypeGroundType ground
    => SubsumeWitness ground t
    -> DolanTypeCheckM ground t
checkSameVar (MkSubsumeWitness va (ConsDolanPlainType (VarDolanSingularType vb) NilDolanPlainType))
    | Just Refl <- testEquality va vb = return iPolarR1
checkSameVar _ = empty

-- used for simplification, where all vars are fixed
subtypeSingularType ::
       forall (ground :: GroundTypeKind) polarity a b. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity a
    -> DolanSingularType ground polarity b
    -> DolanTypeCheckM ground (DolanPolarMap ground polarity a b)
subtypeSingularType ta tb = do
    expr <- runSolver $ subsumeSingularSingularType ta tb
    solveExpression checkSameVar expr

type InvertSubstitution :: GroundTypeKind -> Type
data InvertSubstitution ground where
    MkInvertSubstitution
        :: forall (ground :: GroundTypeKind) polarity name name' t. Is PolarityType polarity
        => SymbolType name
        -> SymbolType name'
        -> DolanPlainType ground polarity t
        -> PolarMap (DolanPolyIsoShim ground Type) polarity (JoinMeetType (InvertPolarity polarity) (UVar Type name') t) (UVar Type name)
        -> InvertSubstitution ground

invertSubstitute ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => InvertSubstitution ground
    -> DolanSubsumer ground a
    -> FullSubsumer ground a
invertSubstitute _ (ClosedExpression a) = pure a
invertSubstitute bisub@(MkInvertSubstitution bn n' (st :: DolanPlainType ground spol _) bij) (OpenExpression (MkSubsumeWitness vn (vt :: DolanPlainType ground wpol _)) expr)
    | Just Refl <- testEquality bn vn =
        solverOpenExpression (MkSubsumeWitness n' vt) $
        case samePolarity @spol @wpol of
            Left Refl ->
                invertPolarity @wpol $ do
                    fa <- invertSubstitute bisub expr
                    pure $ \conv -> fa $ conv <.> uninvertPolarMap polar1 <.> polarPolyIsoBackwards bij
            Right Refl ->
                case isInvertInvertPolarity @wpol of
                    Refl -> do
                        fa <- invertSubstitute bisub expr
                        convm <- invertedPolarSubtype st vt
                        pure $ \conv -> fa $ polarF conv convm <.> invertPolarMap (polarPolyIsoForwards bij)
invertSubstitute bisub (OpenExpression subwit expr) = solverOpenExpression subwit $ invertSubstitute bisub expr

instance forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground =>
             SubsumeTypeSystem (DolanTypeSystem ground) where
    type Subsumer (DolanTypeSystem ground) = DolanSubsumer ground
    type SubsumerSubstitutions (DolanTypeSystem ground) = [Bisubstitution ground (SubsumerM ground)]
    solveSubsumer (ClosedExpression a) = return (a, [])
    solveSubsumer (OpenExpression (MkSubsumeWitness oldvar (tp :: DolanPlainType ground polarity t)) expr) =
        invertPolarity @polarity $
        newUVar (uVarName oldvar) $ \(newvar :: SymbolType newname) ->
            assignUVar @Type @(JoinMeetType (InvertPolarity polarity) (UVar Type newname) t) oldvar $ do
                let
                    bisub :: Bisubstitution ground (SubsumerM ground)
                    bisub =
                        mkPolarBisubstitution
                            oldvar
                            (return $
                             singleDolanShimWit $ MkShimWit (VarDolanSingularType newvar) $ uninvertPolarMap polar1)
                            (do
                                 tq <- limitInvertType' $ PlainDolanType tp
                                 return $
                                     joinMeetShimWit (singleDolanShimWit $ mkShimWit $ VarDolanSingularType newvar) tq)
                expr' <- runSolver $ invertSubstitute (MkInvertSubstitution oldvar newvar tp cid) expr
                (expr'', bisubs) <-
                    solveSubsumer @(DolanTypeSystem ground) $ fmap (\fa -> fa $ uninvertPolarMap polar2) expr'
                return (expr'', bisub : bisubs)
    subsumerNegSubstitute subs t = runSubsumerM $ bisubstitutesType subs t
    subsumePosWitnesses tinf tdecl = fmap (fmap unPolarMap) $ runSolver $ subsumeType tinf tdecl
