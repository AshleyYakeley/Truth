{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.TypeSystem.Subsume
    ( PinaforeSubsumer
    ) where

import Data.Shim
import Language.Expression.Dolan
import Language.Expression.Expression
import Language.Expression.Subsumer
import Language.Expression.UVar
import Pinafore.Language.Error
import Pinafore.Language.Type.Ground
import Pinafore.Language.TypeSystem.Bisubstitute
import Pinafore.Language.TypeSystem.Inverted
import Pinafore.Language.TypeSystem.Show
import Pinafore.Language.TypeSystem.Simplify
import Pinafore.Language.TypeSystem.Subtype
import Pinafore.Language.TypeSystem.Type
import Shapes

minimalPositiveSupertypeSingular ::
       forall baseupdate a.
       PinaforeSingularType baseupdate 'Negative a
    -> Maybe (PinaforeShimWit baseupdate 'Positive a)
minimalPositiveSupertypeSingular (VarPinaforeSingularType v) =
    Just $ singlePinaforeShimWit $ mkPJMShimWit $ VarPinaforeSingularType v
minimalPositiveSupertypeSingular (GroundPinaforeSingularType gt args) = do
    MkShimWit args' conv <-
        mapInvertDolanArgumentsM
            limitInvertType
            (pinaforeGroundTypeVarianceType gt)
            (pinaforeGroundTypeVarianceMap gt)
            args
    return $ singlePinaforeShimWit $ MkShimWit (GroundPinaforeSingularType gt args') conv

minimalPositiveSupertype :: PinaforeType baseupdate 'Negative a -> Maybe (PinaforeShimWit baseupdate 'Positive a)
minimalPositiveSupertype (ConsPinaforeType t NilPinaforeType) = do
    tf <- minimalPositiveSupertypeSingular t
    return $ ccontramap @_ @_ @JMShim meet1 tf
minimalPositiveSupertype _ = Nothing

maximalNegativeSubtypeSingular ::
       forall baseupdate a.
       PinaforeSingularType baseupdate 'Positive a
    -> Maybe (PinaforeShimWit baseupdate 'Negative a)
maximalNegativeSubtypeSingular (VarPinaforeSingularType v) =
    Just $ singlePinaforeShimWit $ mkPJMShimWit $ VarPinaforeSingularType v
maximalNegativeSubtypeSingular (GroundPinaforeSingularType gt args) = do
    MkShimWit args' conv <-
        mapInvertDolanArgumentsM
            limitInvertType
            (pinaforeGroundTypeVarianceType gt)
            (pinaforeGroundTypeVarianceMap gt)
            args
    return $ singlePinaforeShimWit $ MkShimWit (GroundPinaforeSingularType gt args') conv

maximalNegativeSubtype :: PinaforeType baseupdate 'Positive a -> Maybe (PinaforeShimWit baseupdate 'Negative a)
maximalNegativeSubtype (ConsPinaforeType t NilPinaforeType) = do
    tf <- maximalNegativeSubtypeSingular t
    return $ cfmap @_ @_ @JMShim join1 tf
maximalNegativeSubtype _ = Nothing

limitInvertType ::
       forall baseupdate polarity a. Is PolarityType polarity
    => PinaforeType baseupdate polarity a
    -> Maybe (PinaforeShimWit baseupdate (InvertPolarity polarity) a)
limitInvertType =
    case representative @_ @_ @polarity of
        PositiveType -> maximalNegativeSubtype
        NegativeType -> minimalPositiveSupertype

limitInvertType' ::
       forall baseupdate polarity a. Is PolarityType polarity
    => PinaforeType baseupdate polarity a
    -> PinaforeTypeCheck baseupdate (PinaforeShimWit baseupdate (InvertPolarity polarity) a)
limitInvertType' t =
    case limitInvertType t of
        Just r -> return r
        Nothing -> throwError $ TypeNoInverseLimitError $ exprShow t

-- Kind of the dual of 'BisubstitutionWitness'.
data SubsumeWitness baseupdate t where
    PositiveSubsumeWitness
        :: SymbolType name -> PinaforeType baseupdate 'Positive p -> SubsumeWitness baseupdate (JMShim (UVar name) p)
    NegativeSubsumeWitness
        :: SymbolType name -> PinaforeType baseupdate 'Negative q -> SubsumeWitness baseupdate (JMShim q (UVar name))

type PinaforeSubsumer baseupdate = Expression (SubsumeWitness baseupdate)

type SubsumerConstraint baseupdate = SubsumerMonad (PinaforeSubsumer baseupdate) ~ PinaforeTypeCheck baseupdate

type PinaforeFullSubsumer baseupdate = Compose (PinaforeTypeCheck baseupdate) (PinaforeSubsumer baseupdate)

subsumerLiftTypeCheck :: PinaforeSourceScoped baseupdate a -> PinaforeFullSubsumer baseupdate a
subsumerLiftTypeCheck tca = Compose $ fmap pure $ lift tca

subsumePositiveContext ::
       SubsumerConstraint baseupdate => SubtypeContext baseupdate (PinaforeFullSubsumer baseupdate) 'Positive 'Positive
subsumePositiveContext = let
    subtypeLift = subsumerLiftTypeCheck
    subtypeTypes = subsumePositiveType
    subtypeInverted = subsumeNegativeContext
    in MkSubtypeContext {..}

subsumePositiveGroundSingularType ::
       SubsumerConstraint baseupdate
    => PinaforeGroundType baseupdate dv ginf
    -> DolanArguments dv (PinaforeType baseupdate) ginf 'Positive inf
    -> PinaforeSingularType baseupdate 'Positive decl
    -> PinaforeFullSubsumer baseupdate (JMShim inf decl)
subsumePositiveGroundSingularType _gtinf _targsinf (VarPinaforeSingularType _vdecl) = empty
subsumePositiveGroundSingularType gtinf targsinf (GroundPinaforeSingularType gtdecl targsdecl) =
    subtypeGroundTypes subsumePositiveContext gtinf targsinf gtdecl targsdecl

subsumePositiveGroundType ::
       SubsumerConstraint baseupdate
    => PinaforeGroundType baseupdate dv ginf
    -> DolanArguments dv (PinaforeType baseupdate) ginf 'Positive inf
    -> PinaforeType baseupdate 'Positive decl
    -> PinaforeFullSubsumer baseupdate (JMShim inf decl)
subsumePositiveGroundType _gtinf _targsinf NilPinaforeType = empty
subsumePositiveGroundType gtinf targsinf (ConsPinaforeType t1 tr) =
    (fmap (\conv -> join1 . conv) $ subsumePositiveGroundSingularType gtinf targsinf t1) <|>
    (fmap (\conv -> join2 . conv) $ subsumePositiveGroundType gtinf targsinf tr)

subsumePositiveType1 ::
       SubsumerConstraint baseupdate
    => PinaforeSingularType baseupdate 'Positive inf
    -> PinaforeType baseupdate 'Positive decl
    -> PinaforeFullSubsumer baseupdate (JMShim inf decl)
subsumePositiveType1 (VarPinaforeSingularType vinf) tdecl =
    liftSubsumer $ varExpression $ PositiveSubsumeWitness vinf tdecl
subsumePositiveType1 tinf@(GroundPinaforeSingularType ginf argsinf) tdecl =
    subsumePositiveGroundType ginf argsinf tdecl <|>
    (subsumerLiftTypeCheck $ throwError $ TypeSubsumeError Positive (exprShow tinf) (exprShow tdecl))

subsumePositiveType ::
       SubsumerConstraint baseupdate
    => PinaforeType baseupdate 'Positive inf
    -> PinaforeType baseupdate 'Positive decl
    -> PinaforeFullSubsumer baseupdate (JMShim inf decl)
subsumePositiveType NilPinaforeType _ = pure initf
subsumePositiveType (ConsPinaforeType t1 tr) tb = liftA2 joinf (subsumePositiveType1 t1 tb) (subsumePositiveType tr tb)

subsumeNegativeContext ::
       SubsumerConstraint baseupdate => SubtypeContext baseupdate (PinaforeFullSubsumer baseupdate) 'Negative 'Negative
subsumeNegativeContext = let
    subtypeLift = subsumerLiftTypeCheck
    subtypeTypes = subsumeNegativeType
    subtypeInverted = subsumePositiveContext
    in MkSubtypeContext {..}

subsumeNegativeGroundSingularType ::
       SubsumerConstraint baseupdate
    => PinaforeSingularType baseupdate 'Negative decl
    -> PinaforeGroundType baseupdate dv ginf
    -> DolanArguments dv (PinaforeType baseupdate) ginf 'Negative inf
    -> PinaforeFullSubsumer baseupdate (JMShim decl inf)
subsumeNegativeGroundSingularType (VarPinaforeSingularType _vdecl) _gtinf _targsinf = empty
subsumeNegativeGroundSingularType (GroundPinaforeSingularType gtdecl targsdecl) gtinf targsinf =
    subtypeGroundTypes subsumeNegativeContext gtdecl targsdecl gtinf targsinf

subsumeNegativeGroundType ::
       SubsumerConstraint baseupdate
    => PinaforeType baseupdate 'Negative decl
    -> PinaforeGroundType baseupdate dv ginf
    -> DolanArguments dv (PinaforeType baseupdate) ginf 'Negative inf
    -> PinaforeFullSubsumer baseupdate (JMShim decl inf)
subsumeNegativeGroundType NilPinaforeType _gtinf _targsinf = empty
subsumeNegativeGroundType (ConsPinaforeType t1 tr) gtinf targsinf =
    (fmap (\conv -> conv . meet1) $ subsumeNegativeGroundSingularType t1 gtinf targsinf) <|>
    (fmap (\conv -> conv . meet2) $ subsumeNegativeGroundType tr gtinf targsinf)

subsumeNegativeType1 ::
       SubsumerConstraint baseupdate
    => PinaforeType baseupdate 'Negative decl
    -> PinaforeSingularType baseupdate 'Negative inf
    -> PinaforeFullSubsumer baseupdate (JMShim decl inf)
subsumeNegativeType1 tdecl (VarPinaforeSingularType vinf) =
    liftSubsumer $ varExpression $ NegativeSubsumeWitness vinf tdecl
subsumeNegativeType1 tdecl tinf@(GroundPinaforeSingularType ginf argsinf) =
    subsumeNegativeGroundType tdecl ginf argsinf <|>
    (subsumerLiftTypeCheck $ throwError $ TypeSubsumeError Negative (exprShow tinf) (exprShow tdecl))

subsumeNegativeType ::
       SubsumerConstraint baseupdate
    => PinaforeType baseupdate 'Negative decl
    -> PinaforeType baseupdate 'Negative inf
    -> PinaforeFullSubsumer baseupdate (JMShim decl inf)
subsumeNegativeType _ NilPinaforeType = pure termf
subsumeNegativeType ta (ConsPinaforeType t1 tr) = liftA2 meetf (subsumeNegativeType1 ta t1) (subsumeNegativeType ta tr)

data InvertSubstitution (wit :: Polarity -> Type -> Type) where
    NegInvertSubstitution
        :: SymbolType name
        -> SymbolType name'
        -> wit 'Negative t
        -> Isomorphism JMShim (JoinType (UVar name') t) (UVar name)
        -> InvertSubstitution wit
    PosInvertSubstitution
        :: SymbolType name
        -> SymbolType name'
        -> wit 'Positive t
        -> Isomorphism JMShim (MeetType (UVar name') t) (UVar name)
        -> InvertSubstitution wit

type PinaforeInvertSubstitution baseupdate = InvertSubstitution (PinaforeType baseupdate)

invertSubstitute ::
       PinaforeInvertSubstitution baseupdate -> PinaforeSubsumer baseupdate a -> PinaforeFullSubsumer baseupdate a
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
            convm <- invertedSubtype tp vtq
            expr' <- getCompose $ invertSubstitute bisub expr
            return $
                OpenExpression (PositiveSubsumeWitness n' vtq) $
                fmap (\fa conv -> fa $ joinf conv convm . isoBackwards bij) expr'
invertSubstitute bisub@(PosInvertSubstitution bn n' tq bij) (OpenExpression (NegativeSubsumeWitness vn vtp) expr)
    | Just Refl <- testEquality bn vn =
        Compose $ do
            convm <- invertedSubtype vtp tq
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

instance Subsumer (PinaforeSubsumer baseupdate) where
    type SubsumerMonad (PinaforeSubsumer baseupdate) = PinaforeTypeCheck baseupdate
    type SubsumerNegWitness (PinaforeSubsumer baseupdate) = PinaforeType baseupdate 'Negative
    type SubsumerPosWitness (PinaforeSubsumer baseupdate) = PinaforeType baseupdate 'Positive
    type SubsumerSubstitutions (PinaforeSubsumer baseupdate) = [PinaforeBisubstitution baseupdate]
    type SubsumerShim (PinaforeSubsumer baseupdate) = JMShim
    solveSubsumer (ClosedExpression a) = return (a, [])
    solveSubsumer (OpenExpression (NegativeSubsumeWitness (vn :: SymbolType name) (tp :: PinaforeType baseupdate 'Negative t)) expr) = do
        let
            varBij :: Isomorphism JMShim (JoinType (UVar name) t) (UVar name)
            varBij = unsafeUVarIsomorphism
            bisub :: PinaforeBisubstitution baseupdate
            bisub =
                MkBisubstitution
                    vn
                    (do
                         tq <- limitInvertType' tp
                         return $
                             ccontramap (isoBackwards varBij) $
                             joinPinaforeShimWit (singlePinaforeShimWit $ mkPJMShimWit $ VarPinaforeSingularType vn) tq)
                    (return $
                     cfmap (isoForwards varBij . join1) $
                     singlePinaforeShimWit $ mkPJMShimWit $ VarPinaforeSingularType vn)
        expr' <- getCompose $ invertSubstitute (NegInvertSubstitution vn vn tp varBij) expr
        (expr'', bisubs) <- solveSubsumer $ fmap (\fa -> fa $ isoForwards varBij . join2) expr'
        return (expr'', bisub : bisubs)
    solveSubsumer (OpenExpression (PositiveSubsumeWitness (vn :: SymbolType name) (tp :: PinaforeType baseupdate 'Positive t)) expr) = do
        let
            varBij :: Isomorphism JMShim (MeetType (UVar name) t) (UVar name)
            varBij = unsafeUVarIsomorphism
            bisub :: PinaforeBisubstitution baseupdate
            bisub =
                MkBisubstitution
                    vn
                    (return $
                     ccontramap (meet1 . isoBackwards varBij) $
                     singlePinaforeShimWit $ mkPJMShimWit $ VarPinaforeSingularType vn)
                    (do
                         tq <- limitInvertType' tp
                         return $
                             cfmap (isoForwards varBij) $
                             meetPinaforeShimWit (singlePinaforeShimWit $ mkPJMShimWit $ VarPinaforeSingularType vn) tq)
        expr' <- getCompose $ invertSubstitute (PosInvertSubstitution vn vn tp varBij) expr
        (expr'', bisubs) <- solveSubsumer $ fmap (\fa -> fa $ meet2 . isoBackwards varBij) expr'
        return (expr'', bisub : bisubs)
    subsumerNegSubstitute = bisubstitutesType
    subsumePosWitnesses tinf tdecl = getCompose $ subsumePositiveType tinf tdecl
    simplifyPosType (MkAnyW t) =
        case pinaforeSimplifyTypes @baseupdate $ mkPJMShimWit t of
            MkShimWit t' _ -> MkAnyW t'
