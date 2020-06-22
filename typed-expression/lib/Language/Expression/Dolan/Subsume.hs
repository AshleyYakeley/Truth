{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Subsume
    ( DolanSubsumer
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Combine
import Language.Expression.Dolan.Inverted
import Language.Expression.Dolan.Simplify
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
    return $ recursiveDolanShimWit n pt'

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
    return $ recursiveDolanShimWit n pt'

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
        -> SubsumeWitness ground (DolanPolarMap ground polarity (UVar name) t)

type DolanSubsumer :: GroundTypeKind -> Type -> Type
type DolanSubsumer ground = Expression (SubsumeWitness ground)

type SubsumerConstraint :: GroundTypeKind -> Constraint
type SubsumerConstraint ground
     = (IsDolanSubtypeGroundType ground, SubsumerMonad (DolanSubsumer ground) ~ DolanTypeCheckM ground)

type FullSubsumer :: GroundTypeKind -> Type -> Type
type FullSubsumer ground = Solver ground (SubsumeWitness ground)

subsumeContext ::
       forall (ground :: GroundTypeKind) polarity. (SubsumerConstraint ground, Is PolarityType polarity)
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
       (SubsumerConstraint ground, Is PolarityType polarity)
    => ground dv ginf
    -> DolanArguments dv (DolanType ground) ginf polarity inf
    -> DolanSingularType ground polarity decl
    -> FullSubsumer ground (DolanPolarMap ground polarity inf decl)
subsumeGroundSingularType _gtinf _targsinf (VarDolanSingularType _vdecl) = empty
subsumeGroundSingularType gtinf targsinf (GroundDolanSingularType gtdecl targsdecl) =
    case polarityType @polarity of
        PositiveType -> fmap MkPolarMap $ subtypeGroundTypes solverLiftM subsumeContext gtinf targsinf gtdecl targsdecl
        NegativeType -> fmap MkPolarMap $ subtypeGroundTypes solverLiftM subsumeContext gtdecl targsdecl gtinf targsinf

subsumeGroundPlainType ::
       forall (ground :: GroundTypeKind) polarity dv ginf inf decl.
       (SubsumerConstraint ground, Is PolarityType polarity)
    => ground dv ginf
    -> DolanArguments dv (DolanType ground) ginf polarity inf
    -> DolanPlainType ground polarity decl
    -> FullSubsumer ground (DolanPolarMap ground polarity inf decl)
subsumeGroundPlainType _gtinf _targsinf NilDolanPlainType = empty
subsumeGroundPlainType gtinf targsinf (ConsDolanPlainType t1 tr) =
    (fmap (\conv -> polar1 . conv) $ subsumeGroundSingularType gtinf targsinf t1) <|>
    (fmap (\conv -> polar2 . conv) $ subsumeGroundPlainType gtinf targsinf tr)

subsumeSingularType ::
       forall (ground :: GroundTypeKind) polarity inf decl. (SubsumerConstraint ground, Is PolarityType polarity)
    => DolanSingularType ground polarity inf
    -> DolanPlainType ground polarity decl
    -> FullSubsumer ground (DolanPolarMap ground polarity inf decl)
subsumeSingularType (VarDolanSingularType vinf) tdecl =
    solverLiftExpression $ varExpression $ MkSubsumeWitness vinf tdecl
subsumeSingularType tinf@(GroundDolanSingularType ginf argsinf) tdecl =
    subsumeGroundPlainType ginf argsinf tdecl <|> (solverLiftM $ throwTypeSubsumeError tinf tdecl)

subsumePlainType ::
       forall (ground :: GroundTypeKind) polarity inf decl. (SubsumerConstraint ground, Is PolarityType polarity)
    => DolanPlainType ground polarity inf
    -> DolanPlainType ground polarity decl
    -> FullSubsumer ground (DolanPolarMap ground polarity inf decl)
subsumePlainType NilDolanPlainType _ = pure polarLimit
subsumePlainType (ConsDolanPlainType t1 tr) tdecl =
    liftA2 polarF (subsumeSingularType t1 tdecl) (subsumePlainType tr tdecl)

subsumeType ::
       forall (ground :: GroundTypeKind) polarity inf decl. (SubsumerConstraint ground, Is PolarityType polarity)
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
                (\pinf pdecl -> fmap unPolarMap $ subsumePlainType pinf pdecl)
                tinf
                tdecl
        NegativeType ->
            fmap MkPolarMap $
            solveRecursiveTypes
                @ground
                @polarity
                @polarity
                (\pdecl pinf -> fmap unPolarMap $ subsumePlainType pinf pdecl)
                tdecl
                tinf

type InvertSubstitution :: GroundTypeKind -> Type
data InvertSubstitution ground where
    MkInvertSubstitution
        :: forall (ground :: GroundTypeKind) polarity name name' t. Is PolarityType polarity
        => SymbolType name
        -> SymbolType name'
        -> DolanPlainType ground polarity t
        -> PolarMap (DolanPolyIsoShim ground Type) polarity (JoinMeetType (InvertPolarity polarity) (UVar name') t) (UVar name)
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

instance forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground => Subsumer (DolanSubsumer ground) where
    type SubsumerMonad (DolanSubsumer ground) = DolanTypeCheckM ground
    type SubsumerNegWitness (DolanSubsumer ground) = DolanType ground 'Negative
    type SubsumerPosWitness (DolanSubsumer ground) = DolanType ground 'Positive
    type SubsumerSubstitutions (DolanSubsumer ground) = [Bisubstitution ground (SubsumerM ground)]
    type SubsumerShim (DolanSubsumer ground) = DolanPolyShim ground Type
    solveSubsumer (ClosedExpression a) = return (a, [])
    solveSubsumer (OpenExpression (MkSubsumeWitness (vn :: SymbolType name) (tp :: DolanPlainType ground polarity t)) expr) =
        invertPolarity @polarity $ do
            let
                varBij ::
                       PolarMap (DolanPolyIsoShim ground Type) polarity (JoinMeetType (InvertPolarity polarity) (UVar name) t) (UVar name)
                varBij = isoPolyIso unsafeUVarIsomorphism
                bisub :: Bisubstitution ground (SubsumerM ground)
                bisub =
                    mkPolarBisubstitution
                        vn
                        (return $
                         mapShimWit (uninvertPolarMap polar1 <.> polarPolyIsoBackwards varBij) $
                         singleDolanShimWit $ mkShimWit $ VarDolanSingularType vn)
                        (do
                             tq <- limitInvertType' $ PlainDolanType tp
                             return $
                                 mapShimWit (invertPolarMap $ polarPolyIsoForwards varBij) $
                                 joinMeetShimWit (singleDolanShimWit $ mkShimWit $ VarDolanSingularType vn) tq)
            expr' <- runSolver $ invertSubstitute (MkInvertSubstitution vn vn tp varBij) expr
            (expr'', bisubs) <-
                solveSubsumer $ fmap (\fa -> fa $ uninvertPolarMap polar2 <.> polarPolyIsoBackwards varBij) expr'
            return (expr'', bisub : bisubs)
    subsumerNegSubstitute subs t = runSubsumerM $ bisubstitutesType subs t
    subsumePosWitnesses tinf tdecl = fmap (fmap unPolarMap) $ runSolver $ subsumeType tinf tdecl
    simplifyPosType (MkAnyW t) =
        case dolanSimplifyTypes @ground $ mkShimWit @Type @(DolanPolyShim ground Type) @_ @'Positive t of
            MkShimWit t' _ -> MkAnyW t'
