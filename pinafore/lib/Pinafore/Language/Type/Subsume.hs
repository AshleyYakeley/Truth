{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Type.Subsume
    ( PinaforeSubsumer
    ) where

import Language.Expression.Dolan
import Language.Expression.Expression
import Language.Expression.Polarity
import Language.Expression.Subsumer
import Language.Expression.TypeF
import Language.Expression.UVar
import Pinafore.Language.GroundType
import Pinafore.Language.Show
import Pinafore.Language.Type.Bisubstitute
import Pinafore.Language.Type.Inverted
import Pinafore.Language.Type.Simplify
import Pinafore.Language.Type.Subtype
import Pinafore.Language.Type.Type
import Shapes

minimalPositiveSupertypeSingular ::
       forall baseedit a. PinaforeSingularType baseedit 'Negative a -> Maybe (PinaforeTypeF baseedit 'Positive a)
minimalPositiveSupertypeSingular (VarPinaforeSingularType v) =
    Just $ singlePinaforeTypeF $ mkPTypeF $ VarPinaforeSingularType v
minimalPositiveSupertypeSingular (GroundPinaforeSingularType gt args) = do
    gt' <- pinaforeGroundTypeInvertPolarity gt
    MkTypeF args' conv <-
        mapInvertDolanArgumentsM
            limitInvertType
            (pinaforeGroundTypeVarianceType gt)
            (pinaforeGroundTypeVarianceMap gt)
            args
    return $ singlePinaforeTypeF $ MkTypeF (GroundPinaforeSingularType gt' args') conv

minimalPositiveSupertype :: PinaforeType baseedit 'Negative a -> Maybe (PinaforeTypeF baseedit 'Positive a)
minimalPositiveSupertype (ConsPinaforeType t NilPinaforeType) = do
    tf <- minimalPositiveSupertypeSingular t
    return $ contramap meet1 tf
minimalPositiveSupertype _ = Nothing

maximalNegativeSubtypeSingular ::
       forall baseedit a. PinaforeSingularType baseedit 'Positive a -> Maybe (PinaforeTypeF baseedit 'Negative a)
maximalNegativeSubtypeSingular (VarPinaforeSingularType v) =
    Just $ singlePinaforeTypeF $ mkPTypeF $ VarPinaforeSingularType v
maximalNegativeSubtypeSingular (GroundPinaforeSingularType gt args) = do
    gt' <- pinaforeGroundTypeInvertPolarity gt
    MkTypeF args' conv <-
        mapInvertDolanArgumentsM
            limitInvertType
            (pinaforeGroundTypeVarianceType gt)
            (pinaforeGroundTypeVarianceMap gt)
            args
    return $ singlePinaforeTypeF $ MkTypeF (GroundPinaforeSingularType gt' args') conv

maximalNegativeSubtype :: PinaforeType baseedit 'Positive a -> Maybe (PinaforeTypeF baseedit 'Negative a)
maximalNegativeSubtype (ConsPinaforeType t NilPinaforeType) = do
    tf <- maximalNegativeSubtypeSingular t
    return $ fmap join1 tf
maximalNegativeSubtype _ = Nothing

limitInvertType ::
       forall baseedit polarity a. Is PolarityType polarity
    => PinaforeType baseedit polarity a
    -> Maybe (PinaforeTypeF baseedit (InvertPolarity polarity) a)
limitInvertType =
    case representative @_ @_ @polarity of
        PositiveType -> maximalNegativeSubtype
        NegativeType -> minimalPositiveSupertype

limitInvertType' ::
       forall baseedit polarity a. Is PolarityType polarity
    => PinaforeType baseedit polarity a
    -> PinaforeTypeCheck baseedit (PinaforeTypeF baseedit (InvertPolarity polarity) a)
limitInvertType' t =
    case limitInvertType t of
        Just r -> return r
        Nothing -> fail $ "no inverse limit for " <> show t

-- Kind of the dual of 'BisubstitutionWitness'.
data SubsumeWitness baseedit t where
    PositiveSubsumeWitness
        :: SymbolType name -> PinaforeType baseedit 'Positive p -> SubsumeWitness baseedit (UVar name -> p)
    NegativeSubsumeWitness
        :: SymbolType name -> PinaforeType baseedit 'Negative q -> SubsumeWitness baseedit (q -> UVar name)

type PinaforeSubsumer baseedit = Expression (SubsumeWitness baseedit)

type SubsumerConstraint baseedit = SubsumerMonad (PinaforeSubsumer baseedit) ~ PinaforeTypeCheck baseedit

type PinaforeFullSubsumer baseedit = Compose (PinaforeTypeCheck baseedit) (PinaforeSubsumer baseedit)

subsumerLiftTypeCheck :: PinaforeSourceScoped baseedit a -> PinaforeFullSubsumer baseedit a
subsumerLiftTypeCheck tca = Compose $ fmap pure $ lift tca

subsumePositiveContext ::
       SubsumerConstraint baseedit => SubtypeContext baseedit (PinaforeFullSubsumer baseedit) 'Positive 'Positive
subsumePositiveContext = let
    subtypeLift = subsumerLiftTypeCheck
    subtypeTypes = subsumePositiveType
    subtypeInverted = subsumeNegativeContext
    in MkSubtypeContext {..}

subsumePositiveGroundSingularType ::
       SubsumerConstraint baseedit
    => PinaforeGroundType baseedit 'Positive dv ginf
    -> DolanArguments dv (PinaforeType baseedit) ginf 'Positive inf
    -> PinaforeSingularType baseedit 'Positive decl
    -> PinaforeFullSubsumer baseedit (inf -> decl)
subsumePositiveGroundSingularType _gtinf _targsinf (VarPinaforeSingularType _vdecl) = empty
subsumePositiveGroundSingularType gtinf targsinf (GroundPinaforeSingularType gtdecl targsdecl) =
    subtypeGroundTypes subsumePositiveContext gtinf targsinf gtdecl targsdecl

subsumePositiveGroundType ::
       SubsumerConstraint baseedit
    => PinaforeGroundType baseedit 'Positive dv ginf
    -> DolanArguments dv (PinaforeType baseedit) ginf 'Positive inf
    -> PinaforeType baseedit 'Positive decl
    -> PinaforeFullSubsumer baseedit (inf -> decl)
subsumePositiveGroundType _gtinf _targsinf NilPinaforeType = empty
subsumePositiveGroundType gtinf targsinf (ConsPinaforeType t1 tr) =
    (fmap (\conv -> join1 . conv) $ subsumePositiveGroundSingularType gtinf targsinf t1) <|>
    (fmap (\conv -> join2 . conv) $ subsumePositiveGroundType gtinf targsinf tr)

subsumePositiveType1 ::
       SubsumerConstraint baseedit
    => PinaforeSingularType baseedit 'Positive inf
    -> PinaforeType baseedit 'Positive decl
    -> PinaforeFullSubsumer baseedit (inf -> decl)
subsumePositiveType1 (VarPinaforeSingularType vinf) tdecl =
    liftSubsumer $ varExpression $ PositiveSubsumeWitness vinf tdecl
subsumePositiveType1 tinf@(GroundPinaforeSingularType ginf argsinf) tdecl =
    subsumePositiveGroundType ginf argsinf tdecl <|>
    (subsumerLiftTypeCheck $ fail $ "cannot subsume " <> (unpack $ exprShow tinf) <> " to " <> (unpack $ exprShow tdecl))

subsumePositiveType ::
       SubsumerConstraint baseedit
    => PinaforeType baseedit 'Positive inf
    -> PinaforeType baseedit 'Positive decl
    -> PinaforeFullSubsumer baseedit (inf -> decl)
subsumePositiveType NilPinaforeType _ = pure never
subsumePositiveType (ConsPinaforeType t1 tr) tb = liftA2 joinf (subsumePositiveType1 t1 tb) (subsumePositiveType tr tb)

subsumeNegativeContext ::
       SubsumerConstraint baseedit => SubtypeContext baseedit (PinaforeFullSubsumer baseedit) 'Negative 'Negative
subsumeNegativeContext = let
    subtypeLift = subsumerLiftTypeCheck
    subtypeTypes = subsumeNegativeType
    subtypeInverted = subsumePositiveContext
    in MkSubtypeContext {..}

subsumeNegativeGroundSingularType ::
       SubsumerConstraint baseedit
    => PinaforeSingularType baseedit 'Negative decl
    -> PinaforeGroundType baseedit 'Negative dv ginf
    -> DolanArguments dv (PinaforeType baseedit) ginf 'Negative inf
    -> PinaforeFullSubsumer baseedit (decl -> inf)
subsumeNegativeGroundSingularType (VarPinaforeSingularType _vdecl) _gtinf _targsinf = empty
subsumeNegativeGroundSingularType (GroundPinaforeSingularType gtdecl targsdecl) gtinf targsinf =
    subtypeGroundTypes subsumeNegativeContext gtdecl targsdecl gtinf targsinf

subsumeNegativeGroundType ::
       SubsumerConstraint baseedit
    => PinaforeType baseedit 'Negative decl
    -> PinaforeGroundType baseedit 'Negative dv ginf
    -> DolanArguments dv (PinaforeType baseedit) ginf 'Negative inf
    -> PinaforeFullSubsumer baseedit (decl -> inf)
subsumeNegativeGroundType NilPinaforeType _gtinf _targsinf = empty
subsumeNegativeGroundType (ConsPinaforeType t1 tr) gtinf targsinf =
    (fmap (\conv -> conv . meet1) $ subsumeNegativeGroundSingularType t1 gtinf targsinf) <|>
    (fmap (\conv -> conv . meet2) $ subsumeNegativeGroundType tr gtinf targsinf)

subsumeNegativeType1 ::
       SubsumerConstraint baseedit
    => PinaforeType baseedit 'Negative decl
    -> PinaforeSingularType baseedit 'Negative inf
    -> PinaforeFullSubsumer baseedit (decl -> inf)
subsumeNegativeType1 tdecl (VarPinaforeSingularType vinf) =
    liftSubsumer $ varExpression $ NegativeSubsumeWitness vinf tdecl
subsumeNegativeType1 tdecl tinf@(GroundPinaforeSingularType ginf argsinf) =
    subsumeNegativeGroundType tdecl ginf argsinf <|>
    (subsumerLiftTypeCheck $
     fail $ "cannot subsume " <> (unpack $ exprShow tinf) <> " to >= " <> (unpack $ exprShow tdecl))

subsumeNegativeType ::
       SubsumerConstraint baseedit
    => PinaforeType baseedit 'Negative decl
    -> PinaforeType baseedit 'Negative inf
    -> PinaforeFullSubsumer baseedit (decl -> inf)
subsumeNegativeType _ NilPinaforeType = pure alwaysTop
subsumeNegativeType ta (ConsPinaforeType t1 tr) = liftA2 meetf (subsumeNegativeType1 ta t1) (subsumeNegativeType ta tr)

data InvertSubstitution (wit :: Polarity -> Type -> Type) where
    NegInvertSubstitution
        :: SymbolType name
        -> SymbolType name'
        -> wit 'Negative t
        -> Bijection (JoinType (UVar name') t) (UVar name)
        -> InvertSubstitution wit
    PosInvertSubstitution
        :: SymbolType name
        -> SymbolType name'
        -> wit 'Positive t
        -> Bijection (MeetType (UVar name') t) (UVar name)
        -> InvertSubstitution wit

type PinaforeInvertSubstitution baseedit = InvertSubstitution (PinaforeType baseedit)

invertSubstitute ::
       PinaforeInvertSubstitution baseedit -> PinaforeSubsumer baseedit a -> PinaforeFullSubsumer baseedit a
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

instance Subsumer (PinaforeSubsumer baseedit) where
    type SubsumerMonad (PinaforeSubsumer baseedit) = PinaforeTypeCheck baseedit
    type SubsumerNegWitness (PinaforeSubsumer baseedit) = PinaforeType baseedit 'Negative
    type SubsumerPosWitness (PinaforeSubsumer baseedit) = PinaforeType baseedit 'Positive
    type SubsumerSubstitutions (PinaforeSubsumer baseedit) = [PinaforeBisubstitution baseedit]
    solveSubsumer (ClosedExpression a) = return (a, [])
    solveSubsumer (OpenExpression (NegativeSubsumeWitness (vn :: SymbolType name) (tp :: PinaforeType baseedit 'Negative t)) expr) = do
        let
            varBij :: Bijection (JoinType (UVar name) t) (UVar name)
            varBij = unsafeUVarBijection
            bisub :: PinaforeBisubstitution baseedit
            bisub =
                MkBisubstitution
                    vn
                    (do
                         tq <- limitInvertType' tp
                         return $
                             contramap (isoBackwards varBij) $
                             joinPinaforeTypeF (singlePinaforeTypeF $ mkPTypeF $ VarPinaforeSingularType vn) tq)
                    (return $
                     fmap (isoForwards varBij . join1) $ singlePinaforeTypeF $ mkPTypeF $ VarPinaforeSingularType vn)
        expr' <- getCompose $ invertSubstitute (NegInvertSubstitution vn vn tp varBij) expr
        (expr'', bisubs) <- solveSubsumer $ fmap (\fa -> fa $ isoForwards varBij . join2) expr'
        return (expr'', bisub : bisubs)
    solveSubsumer (OpenExpression (PositiveSubsumeWitness (vn :: SymbolType name) (tp :: PinaforeType baseedit 'Positive t)) expr) = do
        let
            varBij :: Bijection (MeetType (UVar name) t) (UVar name)
            varBij = unsafeUVarBijection
            bisub :: PinaforeBisubstitution baseedit
            bisub =
                MkBisubstitution
                    vn
                    (return $
                     contramap (meet1 . isoBackwards varBij) $
                     singlePinaforeTypeF $ mkPTypeF $ VarPinaforeSingularType vn)
                    (do
                         tq <- limitInvertType' tp
                         return $
                             fmap (isoForwards varBij) $
                             meetPinaforeTypeF (singlePinaforeTypeF $ mkPTypeF $ VarPinaforeSingularType vn) tq)
        expr' <- getCompose $ invertSubstitute (PosInvertSubstitution vn vn tp varBij) expr
        (expr'', bisubs) <- solveSubsumer $ fmap (\fa -> fa $ meet2 . isoBackwards varBij) expr'
        return (expr'', bisub : bisubs)
    subsumerNegSubstitute = bisubstitutesType
    subsumePosWitnesses tinf tdecl = getCompose $ subsumePositiveType tinf tdecl
    simplifyPosType (MkAnyW t) =
        case pinaforeSimplifyTypes @baseedit $ mkPTypeF t of
            MkTypeF t' _ -> MkAnyW t'
