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

minimalPositiveSupertypeSingular :: forall a. PinaforeSingularType 'Negative a -> Maybe (PinaforeShimWit 'Positive a)
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

minimalPositiveSupertype :: PinaforeType 'Negative a -> Maybe (PinaforeShimWit 'Positive a)
minimalPositiveSupertype (ConsPinaforeType t NilPinaforeType) = do
    tf <- minimalPositiveSupertypeSingular t
    return $ ccontramap @_ @_ @JMShim meet1 tf
minimalPositiveSupertype _ = Nothing

maximalNegativeSubtypeSingular :: forall a. PinaforeSingularType 'Positive a -> Maybe (PinaforeShimWit 'Negative a)
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

maximalNegativeSubtype :: PinaforeType 'Positive a -> Maybe (PinaforeShimWit 'Negative a)
maximalNegativeSubtype (ConsPinaforeType t NilPinaforeType) = do
    tf <- maximalNegativeSubtypeSingular t
    return $ cfmap @_ @_ @JMShim join1 tf
maximalNegativeSubtype _ = Nothing

limitInvertType ::
       forall polarity a. Is PolarityType polarity
    => PinaforeType polarity a
    -> Maybe (PinaforeShimWit (InvertPolarity polarity) a)
limitInvertType =
    case representative @_ @_ @polarity of
        PositiveType -> maximalNegativeSubtype
        NegativeType -> minimalPositiveSupertype

limitInvertType' ::
       forall polarity a. Is PolarityType polarity
    => PinaforeType polarity a
    -> PinaforeTypeCheck (PinaforeShimWit (InvertPolarity polarity) a)
limitInvertType' t =
    case limitInvertType t of
        Just r -> return r
        Nothing -> throw $ TypeNoInverseLimitError $ exprShow t

-- Kind of the dual of 'BisubstitutionWitness'.
data SubsumeWitness t where
    PositiveSubsumeWitness :: SymbolType name -> PinaforeType 'Positive p -> SubsumeWitness (JMShim (UVar name) p)
    NegativeSubsumeWitness :: SymbolType name -> PinaforeType 'Negative q -> SubsumeWitness (JMShim q (UVar name))

type PinaforeSubsumer = Expression (SubsumeWitness)

type SubsumerConstraint = SubsumerMonad (PinaforeSubsumer) ~ PinaforeTypeCheck

type PinaforeFullSubsumer = Compose (PinaforeTypeCheck) (PinaforeSubsumer)

subsumerLiftTypeCheck :: PinaforeSourceScoped a -> PinaforeFullSubsumer a
subsumerLiftTypeCheck tca = Compose $ fmap pure $ lift tca

subsumePositiveContext :: SubsumerConstraint => SubtypeContext (PinaforeFullSubsumer) 'Positive 'Positive
subsumePositiveContext = let
    subtypeLift = subsumerLiftTypeCheck
    subtypeTypes = subsumePositiveType
    subtypeInverted = subsumeNegativeContext
    in MkSubtypeContext {..}

subsumePositiveGroundSingularType ::
       SubsumerConstraint
    => PinaforeGroundType dv ginf
    -> DolanArguments dv PinaforeType ginf 'Positive inf
    -> PinaforeSingularType 'Positive decl
    -> PinaforeFullSubsumer (JMShim inf decl)
subsumePositiveGroundSingularType _gtinf _targsinf (VarPinaforeSingularType _vdecl) = empty
subsumePositiveGroundSingularType gtinf targsinf (GroundPinaforeSingularType gtdecl targsdecl) =
    subtypeGroundTypes subsumePositiveContext gtinf targsinf gtdecl targsdecl

subsumePositiveGroundType ::
       SubsumerConstraint
    => PinaforeGroundType dv ginf
    -> DolanArguments dv PinaforeType ginf 'Positive inf
    -> PinaforeType 'Positive decl
    -> PinaforeFullSubsumer (JMShim inf decl)
subsumePositiveGroundType _gtinf _targsinf NilPinaforeType = empty
subsumePositiveGroundType gtinf targsinf (ConsPinaforeType t1 tr) =
    (fmap (\conv -> join1 . conv) $ subsumePositiveGroundSingularType gtinf targsinf t1) <|>
    (fmap (\conv -> join2 . conv) $ subsumePositiveGroundType gtinf targsinf tr)

subsumePositiveType1 ::
       SubsumerConstraint
    => PinaforeSingularType 'Positive inf
    -> PinaforeType 'Positive decl
    -> PinaforeFullSubsumer (JMShim inf decl)
subsumePositiveType1 (VarPinaforeSingularType vinf) tdecl =
    liftSubsumer $ varExpression $ PositiveSubsumeWitness vinf tdecl
subsumePositiveType1 tinf@(GroundPinaforeSingularType ginf argsinf) tdecl =
    subsumePositiveGroundType ginf argsinf tdecl <|>
    (subsumerLiftTypeCheck $ throw $ TypeSubsumeError Positive (exprShow tinf) (exprShow tdecl))

subsumePositiveType ::
       SubsumerConstraint
    => PinaforeType 'Positive inf
    -> PinaforeType 'Positive decl
    -> PinaforeFullSubsumer (JMShim inf decl)
subsumePositiveType NilPinaforeType _ = pure initf
subsumePositiveType (ConsPinaforeType t1 tr) tb = liftA2 joinf (subsumePositiveType1 t1 tb) (subsumePositiveType tr tb)

subsumeNegativeContext :: SubsumerConstraint => SubtypeContext (PinaforeFullSubsumer) 'Negative 'Negative
subsumeNegativeContext = let
    subtypeLift = subsumerLiftTypeCheck
    subtypeTypes = subsumeNegativeType
    subtypeInverted = subsumePositiveContext
    in MkSubtypeContext {..}

subsumeNegativeGroundSingularType ::
       SubsumerConstraint
    => PinaforeSingularType 'Negative decl
    -> PinaforeGroundType dv ginf
    -> DolanArguments dv PinaforeType ginf 'Negative inf
    -> PinaforeFullSubsumer (JMShim decl inf)
subsumeNegativeGroundSingularType (VarPinaforeSingularType _vdecl) _gtinf _targsinf = empty
subsumeNegativeGroundSingularType (GroundPinaforeSingularType gtdecl targsdecl) gtinf targsinf =
    subtypeGroundTypes subsumeNegativeContext gtdecl targsdecl gtinf targsinf

subsumeNegativeGroundType ::
       SubsumerConstraint
    => PinaforeType 'Negative decl
    -> PinaforeGroundType dv ginf
    -> DolanArguments dv PinaforeType ginf 'Negative inf
    -> PinaforeFullSubsumer (JMShim decl inf)
subsumeNegativeGroundType NilPinaforeType _gtinf _targsinf = empty
subsumeNegativeGroundType (ConsPinaforeType t1 tr) gtinf targsinf =
    (fmap (\conv -> conv . meet1) $ subsumeNegativeGroundSingularType t1 gtinf targsinf) <|>
    (fmap (\conv -> conv . meet2) $ subsumeNegativeGroundType tr gtinf targsinf)

subsumeNegativeType1 ::
       SubsumerConstraint
    => PinaforeType 'Negative decl
    -> PinaforeSingularType 'Negative inf
    -> PinaforeFullSubsumer (JMShim decl inf)
subsumeNegativeType1 tdecl (VarPinaforeSingularType vinf) =
    liftSubsumer $ varExpression $ NegativeSubsumeWitness vinf tdecl
subsumeNegativeType1 tdecl tinf@(GroundPinaforeSingularType ginf argsinf) =
    subsumeNegativeGroundType tdecl ginf argsinf <|>
    (subsumerLiftTypeCheck $ throw $ TypeSubsumeError Negative (exprShow tinf) (exprShow tdecl))

subsumeNegativeType ::
       SubsumerConstraint
    => PinaforeType 'Negative decl
    -> PinaforeType 'Negative inf
    -> PinaforeFullSubsumer (JMShim decl inf)
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

type PinaforeInvertSubstitution = InvertSubstitution PinaforeType

invertSubstitute :: PinaforeInvertSubstitution -> PinaforeSubsumer a -> PinaforeFullSubsumer a
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

instance Subsumer (PinaforeSubsumer) where
    type SubsumerMonad (PinaforeSubsumer) = PinaforeTypeCheck
    type SubsumerNegWitness (PinaforeSubsumer) = PinaforeType 'Negative
    type SubsumerPosWitness (PinaforeSubsumer) = PinaforeType 'Positive
    type SubsumerSubstitutions (PinaforeSubsumer) = [PinaforeBisubstitution]
    type SubsumerShim (PinaforeSubsumer) = JMShim
    solveSubsumer (ClosedExpression a) = return (a, [])
    solveSubsumer (OpenExpression (NegativeSubsumeWitness (vn :: SymbolType name) (tp :: PinaforeType 'Negative t)) expr) = do
        let
            varBij :: Isomorphism JMShim (JoinType (UVar name) t) (UVar name)
            varBij = unsafeUVarIsomorphism
            bisub :: PinaforeBisubstitution
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
    solveSubsumer (OpenExpression (PositiveSubsumeWitness (vn :: SymbolType name) (tp :: PinaforeType 'Positive t)) expr) = do
        let
            varBij :: Isomorphism JMShim (MeetType (UVar name) t) (UVar name)
            varBij = unsafeUVarIsomorphism
            bisub :: PinaforeBisubstitution
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
        case pinaforeSimplifyTypes $ mkPJMShimWit t of
            MkShimWit t' _ -> MkAnyW t'
