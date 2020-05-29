{-# LANGUAGE ApplicativeDo #-}

module Pinafore.Language.TypeSystem.Unify where

import Data.Shim
import Language.Expression.Dolan
import Language.Expression.Expression
import Language.Expression.UVar
import Language.Expression.Unifier
import Pinafore.Language.Error
import Pinafore.Language.Name
import Pinafore.Language.Type.Ground
import Pinafore.Language.TypeSystem.Bisubstitute
import Pinafore.Language.TypeSystem.Show
import Pinafore.Language.TypeSystem.Subtype
import Pinafore.Language.TypeSystem.Type
import Shapes

getRangeTypeVars :: PinaforeRangeType polarity t -> [String]
getRangeTypeVars (MkRangeType tp tq) = getTypeVars tp <> getTypeVars tq

getArgTypeVars :: forall polarity v a. VarianceType v -> SingleArgument v PinaforeType polarity a -> [String]
getArgTypeVars CovarianceType ft = getTypeVars ft
getArgTypeVars ContravarianceType ft = getTypeVars ft
getArgTypeVars RangevarianceType (MkRangeType ta tb) = getTypeVars ta <> getTypeVars tb

getArgsTypeVars ::
       forall polarity dv t ta. DolanVarianceType dv -> DolanArguments dv PinaforeType t polarity ta -> [String]
getArgsTypeVars NilListType NilDolanArguments = []
getArgsTypeVars (ConsListType tv targs) (ConsDolanArguments arg args) =
    getArgTypeVars @polarity tv arg <> getArgsTypeVars targs args

getTypeVars :: PinaforeType polarity t -> [String]
getTypeVars NilPinaforeType = mempty
getTypeVars (ConsPinaforeType (GroundPinaforeSingularType gt args) tr) =
    getArgsTypeVars (pinaforeGroundTypeVarianceType gt) args <> getTypeVars tr
getTypeVars (ConsPinaforeType (VarPinaforeSingularType swit) tr) = witnessToValue swit : getTypeVars tr

data BisubstitutionWitness t where
    PositiveBisubstitutionWitness
        :: SymbolType name -> PinaforeSingularType 'Positive p -> BisubstitutionWitness (JMShim p (UVar name))
    NegativeBisubstitutionWitness
        :: SymbolType name -> PinaforeSingularType 'Negative q -> BisubstitutionWitness (JMShim (UVar name) q)

type PinaforeUnifier = Expression (BisubstitutionWitness)

type UnifierConstraint = UnifierMonad PinaforeUnifier ~ PinaforeTypeCheck

type PinaforeFullUnifier = Compose (PinaforeTypeCheck) PinaforeUnifier

fullUnifierLiftTypeCheck :: PinaforeTypeCheck a -> PinaforeFullUnifier a
fullUnifierLiftTypeCheck tca = Compose $ fmap pure tca

fullUnifierLiftSourceScoped :: PinaforeSourceScoped a -> PinaforeFullUnifier a
fullUnifierLiftSourceScoped tca = fullUnifierLiftTypeCheck $ lift tca

unifySubtypeContext :: UnifierConstraint => SubtypeContext (PinaforeFullUnifier) 'Positive 'Negative
unifySubtypeContext = let
    subtypeTypes = unifyPosNegPinaforeTypes
    subtypeLift = fullUnifierLiftSourceScoped
    subtypeInverted = unifySubtypeContext
    in MkSubtypeContext {..}

unifyPosNegGroundTypes ::
       UnifierConstraint
    => PinaforeGroundType dva gta
    -> DolanArguments dva PinaforeType gta 'Positive ta
    -> PinaforeGroundType dvb gtb
    -> DolanArguments dvb PinaforeType gtb 'Negative tb
    -> PinaforeFullUnifier (JMShim ta tb)
unifyPosNegGroundTypes = subtypeGroundTypes unifySubtypeContext

unifyPosNegPinaforeSingularTypes ::
       UnifierConstraint
    => PinaforeSingularType 'Positive a
    -> PinaforeSingularType 'Negative b
    -> PinaforeFullUnifier (JMShim a b)
unifyPosNegPinaforeSingularTypes (VarPinaforeSingularType na) (VarPinaforeSingularType nb)
    | Just Refl <- testEquality na nb = pure id
unifyPosNegPinaforeSingularTypes (VarPinaforeSingularType na) tb =
    liftUnifier $ varExpression $ NegativeBisubstitutionWitness na tb
unifyPosNegPinaforeSingularTypes ta (VarPinaforeSingularType nb) =
    liftUnifier $ varExpression $ PositiveBisubstitutionWitness nb ta
unifyPosNegPinaforeSingularTypes (GroundPinaforeSingularType gta argsa) (GroundPinaforeSingularType gtb argsb) =
    unifyPosNegGroundTypes gta argsa gtb argsb

unifyPosNegPinaforeTypes1 ::
       UnifierConstraint
    => PinaforeSingularType 'Positive a
    -> PinaforeType 'Negative b
    -> PinaforeFullUnifier (JMShim a b)
unifyPosNegPinaforeTypes1 _ NilPinaforeType = pure termf
unifyPosNegPinaforeTypes1 ta (ConsPinaforeType t1 t2) = do
    f1 <- unifyPosNegPinaforeSingularTypes ta t1
    f2 <- unifyPosNegPinaforeTypes1 ta t2
    return $ meetf f1 f2

unifyPosNegPinaforeTypes ::
       UnifierConstraint => PinaforeType 'Positive a -> PinaforeType 'Negative b -> PinaforeFullUnifier (JMShim a b)
unifyPosNegPinaforeTypes NilPinaforeType _ = pure initf
unifyPosNegPinaforeTypes (ConsPinaforeType ta1 tar) tb = do
    f1 <- unifyPosNegPinaforeTypes1 ta1 tb
    f2 <- unifyPosNegPinaforeTypes tar tb
    return $ joinf f1 f2

unifyPosNegPinaforeShimWit ::
       UnifierConstraint
    => PinaforeShimWit 'Positive a
    -> PinaforeShimWit 'Negative b
    -> PinaforeFullUnifier (JMShim a b)
unifyPosNegPinaforeShimWit wa wb =
    unPosShimWit wa $ \ta conva ->
        unNegShimWit wb $ \tb convb -> do
            conv <- unifyPosNegPinaforeTypes ta tb
            return $ convb . conv . conva

occursInArg ::
       forall polarity n sv a. VarianceType sv -> SymbolType n -> SingleArgument sv PinaforeType polarity a -> Bool
occursInArg CovarianceType n t = occursInType n t
occursInArg ContravarianceType n t = occursInType n t
occursInArg RangevarianceType n (MkRangeType tp tq) = occursInType n tp || occursInType n tq

occursInArgs ::
       forall polarity n dv t a.
       DolanVarianceType dv
    -> SymbolType n
    -> DolanArguments dv PinaforeType t polarity a
    -> Bool
occursInArgs NilListType _ NilDolanArguments = False
occursInArgs (ConsListType svt dvt) n (ConsDolanArguments arg args) =
    occursInArg @polarity svt n arg || occursInArgs dvt n args

occursInSingularType :: SymbolType n -> PinaforeSingularType polarity a -> Bool
occursInSingularType n (VarPinaforeSingularType nt)
    | Just Refl <- testEquality n nt = True
occursInSingularType _ (VarPinaforeSingularType _) = False
occursInSingularType n (GroundPinaforeSingularType gt args) = occursInArgs (pinaforeGroundTypeVarianceType gt) n args

occursInType :: SymbolType n -> PinaforeType polarity a -> Bool
occursInType _ NilPinaforeType = False
occursInType n (ConsPinaforeType t1 t2) = occursInSingularType n t1 || occursInType n t2

bisubstitutePositiveVar :: SymbolType name -> PinaforeType 'Positive t -> PinaforeUnifier (JMShim t (UVar name))
bisubstitutePositiveVar _ NilPinaforeType = pure initf
bisubstitutePositiveVar vn (ConsPinaforeType t1 tr) =
    OpenExpression (PositiveBisubstitutionWitness vn t1) $ fmap (\fr f1 -> joinf f1 fr) $ bisubstitutePositiveVar vn tr

bisubstituteNegativeVar :: SymbolType name -> PinaforeType 'Negative t -> PinaforeUnifier (JMShim (UVar name) t)
bisubstituteNegativeVar _ NilPinaforeType = pure termf
bisubstituteNegativeVar vn (ConsPinaforeType t1 tr) =
    OpenExpression (NegativeBisubstitutionWitness vn t1) $ fmap (\fr f1 -> meetf f1 fr) $ bisubstituteNegativeVar vn tr

bisubstituteUnifier :: UnifierConstraint => PinaforeBisubstitution -> PinaforeUnifier a -> PinaforeFullUnifier a
bisubstituteUnifier _ (ClosedExpression a) = pure a
bisubstituteUnifier bisub@(MkBisubstitution bn _ mtq) (OpenExpression (PositiveBisubstitutionWitness vn tp) uval)
    | Just Refl <- testEquality bn vn = do
        conv <-
            Compose $ do
                tq <- mtq
                getCompose $ unifyPosNegPinaforeShimWit (singlePinaforeShimWit $ mkPJMShimWit tp) tq
        val' <- bisubstituteUnifier bisub uval
        pure $ val' conv
bisubstituteUnifier bisub@(MkBisubstitution bn mtp _) (OpenExpression (NegativeBisubstitutionWitness vn tq) uval)
    | Just Refl <- testEquality bn vn = do
        conv <-
            Compose $ do
                tp <- mtp
                getCompose $ unifyPosNegPinaforeShimWit tp (singlePinaforeShimWit $ mkPJMShimWit tq)
        val' <- bisubstituteUnifier bisub uval
        pure $ val' conv
bisubstituteUnifier bisub (OpenExpression (PositiveBisubstitutionWitness vn tp) uval) =
    Compose $ do
        wp' <- bisubstitutePositiveSingularType bisub tp
        unPosShimWit wp' $ \tp' conv -> do
            uval' <- getCompose $ bisubstituteUnifier bisub uval
            return $ do
                val' <- uval'
                pv <- bisubstitutePositiveVar vn tp'
                pure $ val' $ pv . conv
bisubstituteUnifier bisub (OpenExpression (NegativeBisubstitutionWitness vn tp) uval) =
    Compose $ do
        wp' <- bisubstituteNegativeSingularType bisub tp
        unNegShimWit wp' $ \tp' conv -> do
            uval' <- getCompose $ bisubstituteUnifier bisub uval
            return $ do
                val' <- uval'
                pv <- bisubstituteNegativeVar vn tp'
                pure $ val' $ conv . pv

runUnifier ::
       forall a. UnifierConstraint
    => PinaforeUnifier a
    -> PinaforeTypeCheck (a, [PinaforeBisubstitution])
runUnifier (ClosedExpression a) = return (a, [])
runUnifier (OpenExpression (PositiveBisubstitutionWitness vn tp) _)
    | occursInSingularType vn tp = throw $ TypeRecursiveError (symbolTypeToName vn) (exprShow tp)
runUnifier (OpenExpression (NegativeBisubstitutionWitness vn tp) _)
    | occursInSingularType vn tp = throw $ TypeRecursiveError (symbolTypeToName vn) (exprShow tp)
runUnifier (OpenExpression (PositiveBisubstitutionWitness (vn :: SymbolType name) (tp :: PinaforeSingularType 'Positive vw)) expr) = let
    varBij :: Isomorphism JMShim (JoinType (UVar name) vw) (UVar name)
    varBij = unsafeUVarIsomorphism
    bisub =
        MkBisubstitution
            vn
            (return $
             ccontramap (isoBackwards varBij) $
             joinMeetPinaforeShimWit
                 (singlePinaforeShimWit $ mkPJMShimWit $ VarPinaforeSingularType vn)
                 (singlePinaforeShimWit $ mkPJMShimWit tp))
            (return $
             cfmap (isoForwards varBij . join1) $ singlePinaforeShimWit $ mkPJMShimWit $ VarPinaforeSingularType vn)
    in do
           expr' <- getCompose $ bisubstituteUnifier bisub expr
           (ca, subs) <- runUnifier expr'
           return (ca $ isoForwards varBij . join2, bisub : subs)
runUnifier (OpenExpression (NegativeBisubstitutionWitness (vn :: SymbolType name) (tq :: PinaforeSingularType 'Negative vw)) expr) = let
    varBij :: Isomorphism JMShim (MeetType (UVar name) vw) (UVar name)
    varBij = unsafeUVarIsomorphism
    bisub =
        MkBisubstitution
            vn
            (return $
             ccontramap (meet1 . isoBackwards varBij) $
             singlePinaforeShimWit $ mkPJMShimWit $ VarPinaforeSingularType vn)
            (return $
             cfmap (isoForwards varBij) $
             joinMeetPinaforeShimWit
                 (singlePinaforeShimWit $ mkPJMShimWit $ VarPinaforeSingularType vn)
                 (singlePinaforeShimWit $ mkPJMShimWit tq))
    in do
           expr' <- getCompose $ bisubstituteUnifier bisub expr
           (ca, subs) <- runUnifier expr'
           return (ca $ meet2 . isoBackwards varBij, bisub : subs)
