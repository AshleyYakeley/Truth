{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Subsume
    ( DolanSubsumer
    , subtypeSingularType
    , invertType
    ) where

import Control.Applicative.Wrapped
import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Combine
import Language.Expression.Dolan.Inverted
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Solver
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Unify
import Language.Expression.Dolan.Unroll
import Shapes

minimalPositiveSupertypeSingular ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => DolanSingularType ground 'Negative a
    -> Maybe (DolanShimWit ground 'Positive a)
minimalPositiveSupertypeSingular (VarDolanSingularType v) = Just $ varDolanShimWit v
minimalPositiveSupertypeSingular (GroundDolanSingularType gt args) = do
    MkShimWit args' conv <-
        mapInvertDolanArgumentsM invertTypeMaybe (groundTypeVarianceType gt) (groundTypeVarianceMap gt) args
    return $ singleDolanShimWit $ MkShimWit (GroundDolanSingularType gt args') conv
minimalPositiveSupertypeSingular (RecursiveDolanSingularType var t) = do
    t' <- minimalPositiveSupertype t
    return $ singleDolanShimWit $ recursiveDolanShimWit var t'

minimalPositiveSupertype ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => DolanType ground 'Negative a
    -> Maybe (DolanShimWit ground 'Positive a)
minimalPositiveSupertype (ConsDolanType t NilDolanType) = do
    tf <- minimalPositiveSupertypeSingular t
    return $ ccontramap @_ @_ @(DolanPolyShim ground Type) meet1 tf
minimalPositiveSupertype _ = Nothing

maximalNegativeSubtypeSingular ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => DolanSingularType ground 'Positive a
    -> Maybe (DolanShimWit ground 'Negative a)
maximalNegativeSubtypeSingular (VarDolanSingularType v) = Just $ varDolanShimWit v
maximalNegativeSubtypeSingular (GroundDolanSingularType gt args) = do
    MkShimWit args' conv <-
        mapInvertDolanArgumentsM invertTypeMaybe (groundTypeVarianceType gt) (groundTypeVarianceMap gt) args
    return $ singleDolanShimWit $ MkShimWit (GroundDolanSingularType gt args') conv
maximalNegativeSubtypeSingular (RecursiveDolanSingularType var t) = do
    t' <- maximalNegativeSubtype t
    return $ singleDolanShimWit $ recursiveDolanShimWit var t'

maximalNegativeSubtype ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => DolanType ground 'Positive a
    -> Maybe (DolanShimWit ground 'Negative a)
maximalNegativeSubtype (ConsDolanType t NilDolanType) = do
    tf <- maximalNegativeSubtypeSingular t
    return $ cfmap @_ @_ @(DolanPolyShim ground Type) join1 tf
maximalNegativeSubtype _ = Nothing

invertTypeMaybe ::
       forall (ground :: GroundTypeKind) polarity a. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity a
    -> Maybe (DolanShimWit ground (InvertPolarity polarity) a)
invertTypeMaybe =
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
    -> DolanM ground a
runSubsumerM (SuccessResult a) = return a
runSubsumerM (FailureResult (UninvertibleError t)) = throwTypeNotInvertible t

invertTypeM ::
       forall (ground :: GroundTypeKind) polarity a. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity a
    -> SubsumerM ground (DolanShimWit ground (InvertPolarity polarity) a)
invertTypeM t =
    case invertTypeMaybe t of
        Just r -> return r
        Nothing -> FailureResult (UninvertibleError t)

invertType ::
       forall (ground :: GroundTypeKind) polarity a. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity a
    -> DolanM ground (DolanShimWit ground (InvertPolarity polarity) a)
invertType t = runSubsumerM $ invertTypeM t

-- Kind of the dual of 'BisubstitutionWitness'.
type SubsumeWitness :: GroundTypeKind -> Type -> Type
data SubsumeWitness ground t where
    MkSubsumeWitness
        :: forall (ground :: GroundTypeKind) polarity name t. Is PolarityType polarity
        => SymbolType name -- inf
        -> DolanType ground polarity t -- decl
        -> SubsumeWitness ground (DolanPolarMap ground polarity (UVarT name) t)
    MkReversedSubsumeWitness
        :: forall (ground :: GroundTypeKind) polarity name t. Is PolarityType polarity
        => SymbolType name -- decl
        -> DolanType ground polarity t -- inf
        -> SubsumeWitness ground (DolanPolarMap ground polarity t (UVarT name))

instance forall (ground :: GroundTypeKind) t. IsDolanGroundType ground => Show (SubsumeWitness ground t) where
    show (MkSubsumeWitness name (t :: _ polarity _)) =
        case polarityType @polarity of
            PositiveType -> show name <> " <: " <> showAllWitness t
            NegativeType -> show name <> " :> " <> showAllWitness t
    show (MkReversedSubsumeWitness name (t :: _ polarity _)) =
        case polarityType @polarity of
            PositiveType -> show name <> " :> " <> showAllWitness t
            NegativeType -> show name <> " <: " <> showAllWitness t

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground =>
             AllWitnessConstraint Show (SubsumeWitness ground) where
    allWitnessConstraint = Dict

type DolanSubsumer :: GroundTypeKind -> Type -> Type
type DolanSubsumer ground = Expression (SubsumeWitness ground)

type FullSubsumer :: GroundTypeKind -> Type -> Type
type FullSubsumer ground = Solver ground (SubsumeWitness ground)

subsumeContext ::
       forall (ground :: GroundTypeKind) polarity. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => SubtypeContext (DolanType ground) (DolanPolyShim ground Type) (FullSubsumer ground) polarity polarity
subsumeContext = let
    subtypeConvert ::
           forall ta tb.
           DolanType ground polarity ta
        -> DolanType ground polarity tb
        -> FullSubsumer ground (DolanPolyShim ground Type ta tb)
    subtypeConvert ta tb =
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
subsumeGroundSingularType gtinf targsinf (VarDolanSingularType vdecl) =
    wbind renamerGetRigidNames $ \rigidnames ->
        if elem (witnessToValue vdecl) rigidnames
            then empty
            else fmap (\conv -> conv . polar1) $
                 solverLiftExpression $
                 varExpression $
                 MkReversedSubsumeWitness vdecl $ ConsDolanType (GroundDolanSingularType gtinf targsinf) NilDolanType
subsumeGroundSingularType gtinf targsinf (GroundDolanSingularType gtdecl targsdecl) =
    case polarityType @polarity of
        PositiveType -> fmap MkPolarMap $ subtypeGroundTypes subsumeContext gtinf targsinf gtdecl targsdecl
        NegativeType -> fmap MkPolarMap $ subtypeGroundTypes subsumeContext gtdecl targsdecl gtinf targsinf
subsumeGroundSingularType gtinf targsinf tdecl@(RecursiveDolanSingularType _ _) =
    subsumeRecursiveType
        (singularRecursiveOrPlainType $ GroundDolanSingularType gtinf targsinf)
        (singularRecursiveOrPlainType tdecl)

subsumeSingularSingularType ::
       forall (ground :: GroundTypeKind) polarity inf decl. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity inf
    -> DolanSingularType ground polarity decl
    -> FullSubsumer ground (DolanPolarMap ground polarity inf decl)
subsumeSingularSingularType (VarDolanSingularType vinf) tdecl =
    fmap (\conv -> iPolarL1 <.> conv) $
    solverLiftExpression $ varExpression $ MkSubsumeWitness vinf $ ConsDolanType tdecl NilDolanType
subsumeSingularSingularType (GroundDolanSingularType ginf argsinf) tdecl = subsumeGroundSingularType ginf argsinf tdecl
subsumeSingularSingularType tinf@(RecursiveDolanSingularType _ _) tdecl =
    subsumeRecursiveType (singularRecursiveOrPlainType tinf) (singularRecursiveOrPlainType tdecl)

subsumeGroundType ::
       forall (ground :: GroundTypeKind) polarity dv ginf inf decl.
       (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => ground dv ginf
    -> DolanArguments dv (DolanType ground) ginf polarity inf
    -> DolanType ground polarity decl
    -> FullSubsumer ground (DolanPolarMap ground polarity inf decl)
subsumeGroundType _gtinf _targsinf NilDolanType = empty
subsumeGroundType gtinf targsinf (ConsDolanType t1 tr) =
    (fmap (\conv -> polar1 . conv) $ subsumeGroundSingularType gtinf targsinf t1) <|>
    (fmap (\conv -> polar2 . conv) $ subsumeGroundType gtinf targsinf tr)

subsumeSingularType ::
       forall (ground :: GroundTypeKind) polarity inf decl. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanSingularType ground polarity inf
    -> DolanType ground polarity decl
    -> FullSubsumer ground (DolanPolarMap ground polarity inf decl)
subsumeSingularType (VarDolanSingularType vinf) tdecl =
    solverLiftExpression $ varExpression $ MkSubsumeWitness vinf tdecl
subsumeSingularType tinf@(GroundDolanSingularType ginf argsinf) tdecl =
    subsumeGroundType ginf argsinf tdecl <|> (wlift $ lift $ throwTypeSubsumeError tinf tdecl)
subsumeSingularType tinf@(RecursiveDolanSingularType _ _) tdecl =
    subsumeRecursiveType (singularRecursiveOrPlainType tinf) (mkShimWit $ PlainType tdecl)

subsumeType ::
       forall (ground :: GroundTypeKind) polarity inf decl. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity inf
    -> DolanType ground polarity decl
    -> FullSubsumer ground (DolanPolarMap ground polarity inf decl)
subsumeType NilDolanType _ = pure polarLimit
subsumeType (ConsDolanType t1 tr) tdecl = liftA2 polarF (subsumeSingularType t1 tdecl) (subsumeType tr tdecl)

subsumeRecursiveType ::
       forall (ground :: GroundTypeKind) polarity inf decl. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => PShimWit (DolanPolyIsoShim ground Type) (RecursiveOrPlainType ground) polarity inf
    -> PShimWit (DolanPolyIsoShim ground Type) (RecursiveOrPlainType ground) polarity decl
    -> FullSubsumer ground (DolanPolarMap ground polarity inf decl)
subsumeRecursiveType tinf tdecl =
    case polarityType @polarity of
        PositiveType ->
            fmap MkPolarMap $
            solveRecursiveShimWits
                @ground
                @polarity
                @polarity
                (\pinf pdecl -> fmap unPolarMap $ subsumeType pinf pdecl)
                tinf
                tdecl
        NegativeType ->
            fmap MkPolarMap $
            solveRecursiveShimWits
                @ground
                @polarity
                @polarity
                (\pdecl pinf -> fmap unPolarMap $ subsumeType pinf pdecl)
                tdecl
                tinf

-- used for simplification, where all vars are fixed
checkSameVar ::
       forall (ground :: GroundTypeKind) t. IsDolanSubtypeGroundType ground
    => SubsumeWitness ground t
    -> DolanTypeCheckM ground t
checkSameVar (MkSubsumeWitness va (ConsDolanType (VarDolanSingularType vb) NilDolanType))
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
    MkNormalSubstitution
        :: forall (ground :: GroundTypeKind) polarity name name' t.
           (Is PolarityType polarity, JoinMeetType polarity (UVarT name') t ~ UVarT name)
        => SymbolType name
        -> SymbolType name'
        -> DolanType ground polarity t
        -> InvertSubstitution ground
    MkInvertSubstitution
        :: forall (ground :: GroundTypeKind) polarity name name' t.
           (Is PolarityType polarity, JoinMeetType (InvertPolarity polarity) (UVarT name') t ~ UVarT name)
        => SymbolType name
        -> SymbolType name'
        -> DolanType ground polarity t
        -> InvertSubstitution ground

invertSubstitute ::
       forall (ground :: GroundTypeKind) a. (IsDolanSubtypeGroundType ground)
    => InvertSubstitution ground
    -> DolanSubsumer ground a
    -> FullSubsumer ground a
invertSubstitute _ (ClosedExpression a) = pure a
invertSubstitute bisub@(MkInvertSubstitution oldvar newvar (st :: DolanType ground spol _)) (OpenExpression (MkSubsumeWitness depvar (vt :: DolanType ground wpol _)) expr)
    | Just Refl <- testEquality oldvar depvar =
        solverOpenExpression (MkSubsumeWitness newvar vt) $
        case samePolarity @spol @wpol of
            Left Refl ->
                invertPolarity @wpol $ do
                    fa <- invertSubstitute bisub expr
                    pure $ \conv -> fa $ conv <.> uninvertPolarMap polar1
            Right Refl ->
                case isInvertInvertPolarity @wpol of
                    Refl -> do
                        fa <- invertSubstitute bisub expr
                        convm <- invertedPolarSubtype st vt
                        pure $ \conv -> fa $ polarF conv convm
invertSubstitute bisub@(MkNormalSubstitution oldvar newvar (st :: DolanType ground spol _)) (OpenExpression (MkSubsumeWitness depvar (vt :: DolanType ground wpol _)) expr)
    | Just Refl <- testEquality oldvar depvar =
        solverOpenExpression (MkSubsumeWitness newvar vt) $
        case samePolarity @spol @wpol of
            Right Refl ->
                invertPolarity @wpol $ do
                    fa <- invertSubstitute bisub expr
                    pure $ \conv -> fa $ conv <.> uninvertPolarMap polar1
            Left Refl ->
                case isInvertInvertPolarity @wpol of
                    Refl -> do
                        fa <- invertSubstitute bisub expr
                        convm <- subsumeType st vt
                        pure $ \conv -> fa $ polarF conv convm
invertSubstitute bisub (OpenExpression subwit expr) = solverOpenExpression subwit $ invertSubstitute bisub expr

bisubSubsumer ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => UnifierBisubstitution ground
    -> DolanSubsumer ground a
    -> DolanTypeCheckM ground (DolanSubsumer ground a)
bisubSubsumer _ (ClosedExpression a) = return $ ClosedExpression a
bisubSubsumer (MkBisubstitution _ uname (Identity (MkShimWit tpos convpos)) (Identity (MkShimWit tneg convneg))) (OpenExpression (MkSubsumeWitness sname (stype :: _ polarity _)) expr)
    | Just Refl <- testEquality uname sname =
        case polarityType @polarity of
            PositiveType -> do
                newexpr <- runSolver $ subsumeType tpos stype
                return $ liftA2 (\newconv f -> f $ newconv . convpos) newexpr expr
            NegativeType -> do
                newexpr <- runSolver $ subsumeType tneg stype
                return $ liftA2 (\newconv f -> f $ newconv . convneg) newexpr expr
bisubSubsumer sub (OpenExpression wit expr) = do
    expr' <- bisubSubsumer sub expr
    return $ OpenExpression wit expr'

type SubsumerBisubstitution :: GroundTypeKind -> Type
type SubsumerBisubstitution ground = Bisubstitution ground (DolanPolyShim ground Type) (SubsumerM ground)

instance forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground =>
             SubsumeTypeSystem (DolanTypeSystem ground) where
    type Subsumer (DolanTypeSystem ground) = DolanSubsumer ground
    type SubsumerSubstitutions (DolanTypeSystem ground) = [SubsumerBisubstitution ground]
    usubSubsumer [] subsumer = return subsumer
    usubSubsumer (s:ss) subsumer = do
        subsumer' <- bisubSubsumer s subsumer
        usubSubsumer @(DolanTypeSystem ground) ss subsumer'
    solveSubsumer (ClosedExpression a) = return (a, [])
    solveSubsumer (OpenExpression (MkSubsumeWitness oldvar (tp :: DolanType ground polarity t)) expr) =
        invertPolarity @polarity $
        newUVar (uVarName oldvar) $ \(newvar :: SymbolType newname) ->
            assignUVar @Type @(JoinMeetType (InvertPolarity polarity) (UVarT newname) t) oldvar $ do
                expr' <- runSolver $ invertSubstitute (MkInvertSubstitution oldvar newvar tp) expr
                let
                    bisub :: SubsumerBisubstitution ground
                    bisub =
                        mkPolarBisubstitution
                            False
                            oldvar
                            (return $
                             singleDolanShimWit $ MkShimWit (VarDolanSingularType newvar) $ uninvertPolarMap polar1)
                            (do
                                 tq <- invertTypeM tp
                                 return $ joinMeetShimWit (varDolanShimWit newvar) tq)
                (expr'', bisubs) <-
                    solveSubsumer @(DolanTypeSystem ground) $ fmap (\fa -> fa $ uninvertPolarMap polar2) expr'
                return (expr'', bisub : bisubs)
    solveSubsumer (OpenExpression (MkReversedSubsumeWitness oldvar (tp :: DolanType ground polarity t)) expr) =
        invertPolarity @polarity $
        newUVar (uVarName oldvar) $ \(newvar :: SymbolType newname) ->
            assignUVar @Type @(JoinMeetType polarity (UVarT newname) t) oldvar $ do
                expr' <- runSolver $ invertSubstitute (MkNormalSubstitution oldvar newvar tp) expr
                let
                    bisub :: SubsumerBisubstitution ground
                    bisub =
                        mkPolarBisubstitution
                            @ground
                            @_
                            @polarity
                            False
                            oldvar
                            (return $ joinMeetShimWit (varDolanShimWit newvar) $ mkShimWit tp)
                            (return $
                             singleDolanShimWit $ MkShimWit (VarDolanSingularType newvar) $ invertPolarMap polar1)
                (expr'', bisubs) <- solveSubsumer @(DolanTypeSystem ground) $ fmap (\fa -> fa polar2) expr'
                return (expr'', bisub : bisubs)
    subsumerNegSubstitute subs t = lift $ runSubsumerM $ bisubstitutesType subs t
    subsumePosWitnesses tinf tdecl = fmap (fmap unPolarMap) $ runSolver $ subsumeType tinf tdecl
