{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Subsume
    ( DolanSubsumer
    ) where

import Data.Shim
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Inverted
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Simplify
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Expression
import Language.Expression.Subsumer
import Language.Expression.UVar
import Shapes

minimalPositiveSupertypeSingular ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => DolanSingularType ground 'Negative a
    -> Maybe (PShimWit (DolanPolyShim ground Type) (DolanType ground) 'Positive a)
minimalPositiveSupertypeSingular (VarDolanSingularType v) =
    Just $ singleDolanShimWit $ mkShimWit $ VarDolanSingularType v
minimalPositiveSupertypeSingular (GroundDolanSingularType gt args) = do
    MkShimWit args' conv <-
        mapInvertDolanArgumentsM limitInvertType (groundTypeVarianceType gt) (groundTypeVarianceMap gt) args
    return $ singleDolanShimWit $ MkShimWit (GroundDolanSingularType gt args') conv

minimalPositiveSupertype ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => DolanType ground 'Negative a
    -> Maybe (PShimWit (DolanPolyShim ground Type) (DolanType ground) 'Positive a)
minimalPositiveSupertype (ConsDolanType t NilDolanType) = do
    tf <- minimalPositiveSupertypeSingular t
    return $ ccontramap @_ @_ @(DolanPolyShim ground Type) meet1 tf
minimalPositiveSupertype _ = Nothing

maximalNegativeSubtypeSingular ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => DolanSingularType ground 'Positive a
    -> Maybe (PShimWit (DolanPolyShim ground Type) (DolanType ground) 'Negative a)
maximalNegativeSubtypeSingular (VarDolanSingularType v) = Just $ singleDolanShimWit $ mkShimWit $ VarDolanSingularType v
maximalNegativeSubtypeSingular (GroundDolanSingularType gt args) = do
    MkShimWit args' conv <-
        mapInvertDolanArgumentsM limitInvertType (groundTypeVarianceType gt) (groundTypeVarianceMap gt) args
    return $ singleDolanShimWit $ MkShimWit (GroundDolanSingularType gt args') conv

maximalNegativeSubtype ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => DolanType ground 'Positive a
    -> Maybe (PShimWit (DolanPolyShim ground Type) (DolanType ground) 'Negative a)
maximalNegativeSubtype (ConsDolanType t NilDolanType) = do
    tf <- maximalNegativeSubtypeSingular t
    return $ cfmap @_ @_ @(DolanPolyShim ground Type) join1 tf
maximalNegativeSubtype _ = Nothing

limitInvertType ::
       forall (ground :: GroundTypeKind) polarity a. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity a
    -> Maybe (PShimWit (DolanPolyShim ground Type) (DolanType ground) (InvertPolarity polarity) a)
limitInvertType =
    case polarityType @polarity of
        PositiveType -> maximalNegativeSubtype @ground
        NegativeType -> minimalPositiveSupertype @ground

limitInvertType' ::
       forall (ground :: GroundTypeKind) polarity a. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => DolanType ground polarity a
    -> DolanTypeCheckM ground (PShimWit (DolanPolyShim ground Type) (DolanType ground) (InvertPolarity polarity) a)
limitInvertType' t =
    case limitInvertType t of
        Just r -> return r
        Nothing -> lift $ throwTypeNoInverseLimitError t

-- Kind of the dual of 'BisubstitutionWitness'.
type SubsumeWitness :: GroundTypeKind -> Type -> Type
data SubsumeWitness ground t where
    PositiveSubsumeWitness
        :: forall (ground :: GroundTypeKind) name p.
           SymbolType name
        -> DolanType ground 'Positive p
        -> SubsumeWitness ground (DolanPolyShim ground Type (UVar name) p)
    NegativeSubsumeWitness
        :: forall (ground :: GroundTypeKind) name q.
           SymbolType name
        -> DolanType ground 'Negative q
        -> SubsumeWitness ground (DolanPolyShim ground Type q (UVar name))

type DolanSubsumer :: GroundTypeKind -> Type -> Type
type DolanSubsumer ground = Expression (SubsumeWitness ground)

type SubsumerConstraint :: GroundTypeKind -> Constraint
type SubsumerConstraint ground
     = (IsDolanSubtypeGroundType ground, SubsumerMonad (DolanSubsumer ground) ~ DolanTypeCheckM ground)

type DolanFullSubsumer :: GroundTypeKind -> Type -> Type
type DolanFullSubsumer ground = Compose (DolanTypeCheckM ground) (DolanSubsumer ground)

subsumerLiftTypeCheck ::
       forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground
    => MFunction (DolanM ground) (DolanFullSubsumer ground)
subsumerLiftTypeCheck tca = Compose $ fmap pure $ lift tca

subsumePositiveContext ::
       forall (ground :: GroundTypeKind). SubsumerConstraint ground
    => SubtypeContext (DolanType ground) (DolanPolyShim ground Type) (DolanFullSubsumer ground) 'Positive 'Positive
subsumePositiveContext = let
    subtypeTypes = subsumePositiveType
    subtypeInverted = subsumeNegativeContext
    in MkSubtypeContext {..}

subsumePositiveGroundSingularType ::
       forall (ground :: GroundTypeKind) dv ginf inf decl. SubsumerConstraint ground
    => ground dv ginf
    -> DolanArguments dv (DolanType ground) ginf 'Positive inf
    -> DolanSingularType ground 'Positive decl
    -> DolanFullSubsumer ground (DolanPolyShim ground Type inf decl)
subsumePositiveGroundSingularType _gtinf _targsinf (VarDolanSingularType _vdecl) = empty
subsumePositiveGroundSingularType gtinf targsinf (GroundDolanSingularType gtdecl targsdecl) =
    subtypeGroundTypes subsumerLiftTypeCheck subsumePositiveContext gtinf targsinf gtdecl targsdecl

subsumePositiveGroundType ::
       forall (ground :: GroundTypeKind) dv ginf inf decl. SubsumerConstraint ground
    => ground dv ginf
    -> DolanArguments dv (DolanType ground) ginf 'Positive inf
    -> DolanType ground 'Positive decl
    -> DolanFullSubsumer ground (DolanPolyShim ground Type inf decl)
subsumePositiveGroundType _gtinf _targsinf NilDolanType = empty
subsumePositiveGroundType gtinf targsinf (ConsDolanType t1 tr) =
    (fmap (\conv -> join1 . conv) $ subsumePositiveGroundSingularType gtinf targsinf t1) <|>
    (fmap (\conv -> join2 . conv) $ subsumePositiveGroundType gtinf targsinf tr)

subsumePositiveType1 ::
       forall (ground :: GroundTypeKind) inf decl. SubsumerConstraint ground
    => DolanSingularType ground 'Positive inf
    -> DolanType ground 'Positive decl
    -> DolanFullSubsumer ground (DolanPolyShim ground Type inf decl)
subsumePositiveType1 (VarDolanSingularType vinf) tdecl =
    liftSubsumer $ varExpression $ PositiveSubsumeWitness vinf tdecl
subsumePositiveType1 tinf@(GroundDolanSingularType ginf argsinf) tdecl =
    subsumePositiveGroundType ginf argsinf tdecl <|> (subsumerLiftTypeCheck $ throwTypeSubsumeError tinf tdecl)

subsumePositiveType ::
       forall (ground :: GroundTypeKind) inf decl. SubsumerConstraint ground
    => DolanType ground 'Positive inf
    -> DolanType ground 'Positive decl
    -> DolanFullSubsumer ground (DolanPolyShim ground Type inf decl)
subsumePositiveType NilDolanType _ = pure initf
subsumePositiveType (ConsDolanType t1 tr) tb = liftA2 joinf (subsumePositiveType1 t1 tb) (subsumePositiveType tr tb)

subsumeNegativeContext ::
       forall (ground :: GroundTypeKind). SubsumerConstraint ground
    => SubtypeContext (DolanType ground) (DolanPolyShim ground Type) (DolanFullSubsumer ground) 'Negative 'Negative
subsumeNegativeContext = let
    subtypeTypes = subsumeNegativeType
    subtypeInverted = subsumePositiveContext
    in MkSubtypeContext {..}

subsumeNegativeGroundSingularType ::
       forall (ground :: GroundTypeKind) dv ginf inf decl. SubsumerConstraint ground
    => DolanSingularType ground 'Negative decl
    -> ground dv ginf
    -> DolanArguments dv (DolanType ground) ginf 'Negative inf
    -> DolanFullSubsumer ground (DolanPolyShim ground Type decl inf)
subsumeNegativeGroundSingularType (VarDolanSingularType _vdecl) _gtinf _targsinf = empty
subsumeNegativeGroundSingularType (GroundDolanSingularType gtdecl targsdecl) gtinf targsinf =
    subtypeGroundTypes subsumerLiftTypeCheck subsumeNegativeContext gtdecl targsdecl gtinf targsinf

subsumeNegativeGroundType ::
       forall (ground :: GroundTypeKind) dv ginf inf decl. SubsumerConstraint ground
    => DolanType ground 'Negative decl
    -> ground dv ginf
    -> DolanArguments dv (DolanType ground) ginf 'Negative inf
    -> DolanFullSubsumer ground (DolanPolyShim ground Type decl inf)
subsumeNegativeGroundType NilDolanType _gtinf _targsinf = empty
subsumeNegativeGroundType (ConsDolanType t1 tr) gtinf targsinf =
    (fmap (\conv -> conv . meet1) $ subsumeNegativeGroundSingularType t1 gtinf targsinf) <|>
    (fmap (\conv -> conv . meet2) $ subsumeNegativeGroundType tr gtinf targsinf)

subsumeNegativeType1 ::
       forall (ground :: GroundTypeKind) inf decl. SubsumerConstraint ground
    => DolanType ground 'Negative decl
    -> DolanSingularType ground 'Negative inf
    -> DolanFullSubsumer ground (DolanPolyShim ground Type decl inf)
subsumeNegativeType1 tdecl (VarDolanSingularType vinf) =
    liftSubsumer $ varExpression $ NegativeSubsumeWitness vinf tdecl
subsumeNegativeType1 tdecl tinf@(GroundDolanSingularType ginf argsinf) =
    subsumeNegativeGroundType tdecl ginf argsinf <|> (subsumerLiftTypeCheck $ throwTypeSubsumeError tinf tdecl)

subsumeNegativeType ::
       forall (ground :: GroundTypeKind) inf decl. SubsumerConstraint ground
    => DolanType ground 'Negative decl
    -> DolanType ground 'Negative inf
    -> DolanFullSubsumer ground (DolanPolyShim ground Type decl inf)
subsumeNegativeType _ NilDolanType = pure termf
subsumeNegativeType ta (ConsDolanType t1 tr) = liftA2 meetf (subsumeNegativeType1 ta t1) (subsumeNegativeType ta tr)

type InvertSubstitution :: GroundTypeKind -> Type
data InvertSubstitution ground where
    NegInvertSubstitution
        :: forall (ground :: GroundTypeKind) name name' t.
           SymbolType name
        -> SymbolType name'
        -> DolanType ground 'Negative t
        -> Isomorphism (DolanPolyShim ground Type) (JoinType (UVar name') t) (UVar name)
        -> InvertSubstitution ground
    PosInvertSubstitution
        :: forall (ground :: GroundTypeKind) name name' t.
           SymbolType name
        -> SymbolType name'
        -> DolanType ground 'Positive t
        -> Isomorphism (DolanPolyShim ground Type) (MeetType (UVar name') t) (UVar name)
        -> InvertSubstitution ground

invertSubstitute ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => InvertSubstitution ground
    -> DolanSubsumer ground a
    -> DolanFullSubsumer ground a
invertSubstitute _ (ClosedExpression a) = pure a
invertSubstitute bisub@(NegInvertSubstitution bn n' _ bij) (OpenExpression (NegativeSubsumeWitness vn vtp) expr)
    | Just Refl <- testEquality bn vn =
        Compose $ do
            expr' <- getCompose $ invertSubstitute bisub expr
            return $
                OpenExpression (NegativeSubsumeWitness n' vtp) $
                fmap (\fa conv -> fa $ isoForwards bij . join1 . conv) expr'
invertSubstitute bisub@(NegInvertSubstitution bn n' tp bij) (OpenExpression (PositiveSubsumeWitness vn vtq) expr)
    | Just Refl <- testEquality bn vn =
        Compose $ do
            convm <- invertedSubtype lift tp vtq
            expr' <- getCompose $ invertSubstitute bisub expr
            return $
                OpenExpression (PositiveSubsumeWitness n' vtq) $
                fmap (\fa conv -> fa $ joinf conv convm . isoBackwards bij) expr'
invertSubstitute bisub@(PosInvertSubstitution bn n' tq bij) (OpenExpression (NegativeSubsumeWitness vn vtp) expr)
    | Just Refl <- testEquality bn vn =
        Compose $ do
            convm <- invertedSubtype lift vtp tq
            expr' <- getCompose $ invertSubstitute bisub expr
            return $
                OpenExpression (NegativeSubsumeWitness n' vtp) $
                fmap (\fa conv -> fa $ isoForwards bij . meetf conv convm) expr'
invertSubstitute bisub@(PosInvertSubstitution bn n' _ bij) (OpenExpression (PositiveSubsumeWitness vn vtq) expr)
    | Just Refl <- testEquality bn vn =
        Compose $ do
            expr' <- getCompose $ invertSubstitute bisub expr
            return $
                OpenExpression (PositiveSubsumeWitness n' vtq) $
                fmap (\fa conv -> fa $ conv . meet1 . isoBackwards bij) expr'
invertSubstitute bisub (OpenExpression subwit expr) =
    Compose $ do
        expr' <- getCompose $ invertSubstitute bisub expr
        return $ OpenExpression subwit expr'

instance forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground => Subsumer (DolanSubsumer ground) where
    type SubsumerMonad (DolanSubsumer ground) = DolanTypeCheckM ground
    type SubsumerNegWitness (DolanSubsumer ground) = DolanType ground 'Negative
    type SubsumerPosWitness (DolanSubsumer ground) = DolanType ground 'Positive
    type SubsumerSubstitutions (DolanSubsumer ground) = [DolanBisubstitution ground (DolanTypeCheckM ground)]
    type SubsumerShim (DolanSubsumer ground) = DolanPolyShim ground Type
    solveSubsumer (ClosedExpression a) = return (a, [])
    solveSubsumer (OpenExpression (NegativeSubsumeWitness (vn :: SymbolType name) (tp :: DolanType ground 'Negative t)) expr) = do
        let
            varBij :: Isomorphism (DolanPolyShim ground Type) (JoinType (UVar name) t) (UVar name)
            varBij = unsafeUVarIsomorphism
            bisub :: DolanBisubstitution ground (DolanTypeCheckM ground)
            bisub =
                MkBisubstitution
                    vn
                    (do
                         tq <- limitInvertType' tp
                         return $
                             ccontramap (isoBackwards varBij) $
                             joinMeetDolanShimWit (singleDolanShimWit $ mkShimWit $ VarDolanSingularType vn) tq)
                    (return $
                     cfmap (isoForwards varBij . join1) $ singleDolanShimWit $ mkShimWit $ VarDolanSingularType vn)
        expr' <- getCompose $ invertSubstitute (NegInvertSubstitution vn vn tp varBij) expr
        (expr'', bisubs) <- solveSubsumer $ fmap (\fa -> fa $ isoForwards varBij . join2) expr'
        return (expr'', bisub : bisubs)
    solveSubsumer (OpenExpression (PositiveSubsumeWitness (vn :: SymbolType name) (tp :: DolanType ground 'Positive t)) expr) = do
        let
            varBij :: Isomorphism (DolanPolyShim ground Type) (MeetType (UVar name) t) (UVar name)
            varBij = unsafeUVarIsomorphism
            bisub :: DolanBisubstitution ground (DolanTypeCheckM ground)
            bisub =
                MkBisubstitution
                    vn
                    (return $
                     ccontramap (meet1 . isoBackwards varBij) $ singleDolanShimWit $ mkShimWit $ VarDolanSingularType vn)
                    (do
                         tq <- limitInvertType' tp
                         return $
                             cfmap (isoForwards varBij) $
                             joinMeetDolanShimWit (singleDolanShimWit $ mkShimWit $ VarDolanSingularType vn) tq)
        expr' <- getCompose $ invertSubstitute (PosInvertSubstitution vn vn tp varBij) expr
        (expr'', bisubs) <- solveSubsumer $ fmap (\fa -> fa $ meet2 . isoBackwards varBij) expr'
        return (expr'', bisub : bisubs)
    subsumerNegSubstitute = bisubstitutesType
    subsumePosWitnesses tinf tdecl = getCompose $ subsumePositiveType tinf tdecl
    simplifyPosType (MkAnyW t) =
        case dolanSimplifyTypes @ground $ mkShimWit @Type @(DolanPolyShim ground Type) @_ @'Positive t of
            MkShimWit t' _ -> MkAnyW t'
