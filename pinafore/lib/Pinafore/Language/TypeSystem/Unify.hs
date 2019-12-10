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

getRangeTypeVars :: PinaforeRangeType baseupdate polarity t -> [String]
getRangeTypeVars (MkRangeType tp tq) = getTypeVars tp <> getTypeVars tq

getArgTypeVars ::
       forall baseupdate polarity v a.
       VarianceType v
    -> SingleArgument v (PinaforeType baseupdate) polarity a
    -> [String]
getArgTypeVars CovarianceType ft = getTypeVars ft
getArgTypeVars ContravarianceType ft = getTypeVars ft
getArgTypeVars RangevarianceType (MkRangeType ta tb) = getTypeVars ta <> getTypeVars tb

getArgsTypeVars ::
       forall baseupdate polarity dv t ta.
       DolanVarianceType dv
    -> DolanArguments dv (PinaforeType baseupdate) t polarity ta
    -> [String]
getArgsTypeVars NilListType NilDolanArguments = []
getArgsTypeVars (ConsListType tv targs) (ConsDolanArguments arg args) =
    getArgTypeVars @baseupdate @polarity tv arg <> getArgsTypeVars targs args

getTypeVars :: PinaforeType baseupdate polarity t -> [String]
getTypeVars NilPinaforeType = mempty
getTypeVars (ConsPinaforeType (GroundPinaforeSingularType gt args) tr) =
    getArgsTypeVars (pinaforeGroundTypeVarianceType gt) args <> getTypeVars tr
getTypeVars (ConsPinaforeType (VarPinaforeSingularType swit) tr) = witnessToValue swit : getTypeVars tr

data BisubstitutionWitness baseupdate t where
    PositiveBisubstitutionWitness
        :: SymbolType name
        -> PinaforeSingularType baseupdate 'Positive p
        -> BisubstitutionWitness baseupdate (JMShim p (UVar name))
    NegativeBisubstitutionWitness
        :: SymbolType name
        -> PinaforeSingularType baseupdate 'Negative q
        -> BisubstitutionWitness baseupdate (JMShim (UVar name) q)

type PinaforeUnifier baseupdate = Expression (BisubstitutionWitness baseupdate)

type UnifierConstraint baseupdate = UnifierMonad (PinaforeUnifier baseupdate) ~ PinaforeTypeCheck baseupdate

type PinaforeFullUnifier baseupdate = Compose (PinaforeTypeCheck baseupdate) (PinaforeUnifier baseupdate)

fullUnifierLiftTypeCheck :: PinaforeTypeCheck baseupdate a -> PinaforeFullUnifier baseupdate a
fullUnifierLiftTypeCheck tca = Compose $ fmap pure tca

fullUnifierLiftSourceScoped :: PinaforeSourceScoped baseupdate a -> PinaforeFullUnifier baseupdate a
fullUnifierLiftSourceScoped tca = fullUnifierLiftTypeCheck $ lift tca

unifySubtypeContext ::
       UnifierConstraint baseupdate => SubtypeContext baseupdate (PinaforeFullUnifier baseupdate) 'Positive 'Negative
unifySubtypeContext = let
    subtypeTypes = unifyPosNegPinaforeTypes
    subtypeLift = fullUnifierLiftSourceScoped
    subtypeInverted = unifySubtypeContext
    in MkSubtypeContext {..}

unifyPosNegGroundTypes ::
       UnifierConstraint baseupdate
    => PinaforeGroundType baseupdate 'Positive dva gta
    -> DolanArguments dva (PinaforeType baseupdate) gta 'Positive ta
    -> PinaforeGroundType baseupdate 'Negative dvb gtb
    -> DolanArguments dvb (PinaforeType baseupdate) gtb 'Negative tb
    -> PinaforeFullUnifier baseupdate (JMShim ta tb)
unifyPosNegGroundTypes = subtypeGroundTypes unifySubtypeContext

unifyPosNegPinaforeSingularTypes ::
       UnifierConstraint baseupdate
    => PinaforeSingularType baseupdate 'Positive a
    -> PinaforeSingularType baseupdate 'Negative b
    -> PinaforeFullUnifier baseupdate (JMShim a b)
unifyPosNegPinaforeSingularTypes (VarPinaforeSingularType na) (VarPinaforeSingularType nb)
    | Just Refl <- testEquality na nb = pure id
unifyPosNegPinaforeSingularTypes (VarPinaforeSingularType na) tb =
    liftUnifier $ varExpression $ NegativeBisubstitutionWitness na tb
unifyPosNegPinaforeSingularTypes ta (VarPinaforeSingularType nb) =
    liftUnifier $ varExpression $ PositiveBisubstitutionWitness nb ta
unifyPosNegPinaforeSingularTypes (GroundPinaforeSingularType gta argsa) (GroundPinaforeSingularType gtb argsb) =
    unifyPosNegGroundTypes gta argsa gtb argsb

unifyPosNegPinaforeTypes1 ::
       UnifierConstraint baseupdate
    => PinaforeSingularType baseupdate 'Positive a
    -> PinaforeType baseupdate 'Negative b
    -> PinaforeFullUnifier baseupdate (JMShim a b)
unifyPosNegPinaforeTypes1 _ NilPinaforeType = pure termf
unifyPosNegPinaforeTypes1 ta (ConsPinaforeType t1 t2) = do
    f1 <- unifyPosNegPinaforeSingularTypes ta t1
    f2 <- unifyPosNegPinaforeTypes1 ta t2
    return $ meetf f1 f2

unifyPosNegPinaforeTypes ::
       UnifierConstraint baseupdate
    => PinaforeType baseupdate 'Positive a
    -> PinaforeType baseupdate 'Negative b
    -> PinaforeFullUnifier baseupdate (JMShim a b)
unifyPosNegPinaforeTypes NilPinaforeType _ = pure initf
unifyPosNegPinaforeTypes (ConsPinaforeType ta1 tar) tb = do
    f1 <- unifyPosNegPinaforeTypes1 ta1 tb
    f2 <- unifyPosNegPinaforeTypes tar tb
    return $ joinf f1 f2

unifyPosNegPinaforeShimWit ::
       UnifierConstraint baseupdate
    => PinaforeShimWit baseupdate 'Positive a
    -> PinaforeShimWit baseupdate 'Negative b
    -> PinaforeFullUnifier baseupdate (JMShim a b)
unifyPosNegPinaforeShimWit (MkShimWit ta conva) (MkShimWit tb convb) = do
    conv <- unifyPosNegPinaforeTypes ta tb
    return $ convb . conv . conva

occursInArg ::
       forall baseupdate polarity n sv a.
       VarianceType sv
    -> SymbolType n
    -> SingleArgument sv (PinaforeType baseupdate) polarity a
    -> Bool
occursInArg CovarianceType n t = occursInType n t
occursInArg ContravarianceType n t = occursInType n t
occursInArg RangevarianceType n (MkRangeType tp tq) = occursInType n tp || occursInType n tq

occursInArgs ::
       forall baseupdate polarity n dv t a.
       DolanVarianceType dv
    -> SymbolType n
    -> DolanArguments dv (PinaforeType baseupdate) t polarity a
    -> Bool
occursInArgs NilListType _ NilDolanArguments = False
occursInArgs (ConsListType svt dvt) n (ConsDolanArguments arg args) =
    occursInArg @baseupdate @polarity svt n arg || occursInArgs dvt n args

occursInSingularType :: SymbolType n -> PinaforeSingularType baseupdate polarity a -> Bool
occursInSingularType n (VarPinaforeSingularType nt)
    | Just Refl <- testEquality n nt = True
occursInSingularType _ (VarPinaforeSingularType _) = False
occursInSingularType n (GroundPinaforeSingularType gt args) = occursInArgs (pinaforeGroundTypeVarianceType gt) n args

occursInType :: SymbolType n -> PinaforeType baseupdate polarity a -> Bool
occursInType _ NilPinaforeType = False
occursInType n (ConsPinaforeType t1 t2) = occursInSingularType n t1 || occursInType n t2

bisubstitutePositiveVar ::
       SymbolType name -> PinaforeType baseupdate 'Positive t -> PinaforeUnifier baseupdate (JMShim t (UVar name))
bisubstitutePositiveVar _ NilPinaforeType = pure initf
bisubstitutePositiveVar vn (ConsPinaforeType t1 tr) =
    OpenExpression (PositiveBisubstitutionWitness vn t1) $ fmap (\fr f1 -> joinf f1 fr) $ bisubstitutePositiveVar vn tr

bisubstituteNegativeVar ::
       SymbolType name -> PinaforeType baseupdate 'Negative t -> PinaforeUnifier baseupdate (JMShim (UVar name) t)
bisubstituteNegativeVar _ NilPinaforeType = pure termf
bisubstituteNegativeVar vn (ConsPinaforeType t1 tr) =
    OpenExpression (NegativeBisubstitutionWitness vn t1) $ fmap (\fr f1 -> meetf f1 fr) $ bisubstituteNegativeVar vn tr

bisubstituteUnifier ::
       UnifierConstraint baseupdate
    => PinaforeBisubstitution baseupdate
    -> PinaforeUnifier baseupdate a
    -> PinaforeFullUnifier baseupdate a
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
        MkShimWit tp' conv <- bisubstitutePositiveSingularType bisub tp
        uval' <- getCompose $ bisubstituteUnifier bisub uval
        return $ do
            val' <- uval'
            pv <- bisubstitutePositiveVar vn tp'
            pure $ val' $ pv . conv
bisubstituteUnifier bisub (OpenExpression (NegativeBisubstitutionWitness vn tp) uval) =
    Compose $ do
        MkShimWit tp' conv <- bisubstituteNegativeSingularType bisub tp
        uval' <- getCompose $ bisubstituteUnifier bisub uval
        return $ do
            val' <- uval'
            pv <- bisubstituteNegativeVar vn tp'
            pure $ val' $ conv . pv

runUnifier ::
       forall baseupdate a. UnifierConstraint baseupdate
    => PinaforeUnifier baseupdate a
    -> PinaforeTypeCheck baseupdate (a, [PinaforeBisubstitution baseupdate])
runUnifier (ClosedExpression a) = return (a, [])
runUnifier (OpenExpression (PositiveBisubstitutionWitness vn tp) _)
    | occursInSingularType vn tp = throwError $ TypeRecursiveError (symbolTypeToName vn) (exprShow tp)
runUnifier (OpenExpression (NegativeBisubstitutionWitness vn tp) _)
    | occursInSingularType vn tp = throwError $ TypeRecursiveError (symbolTypeToName vn) (exprShow tp)
runUnifier (OpenExpression (PositiveBisubstitutionWitness (vn :: SymbolType name) (tp :: PinaforeSingularType baseupdate 'Positive vw)) expr) = let
    varBij :: Isomorphism JMShim (JoinType (UVar name) vw) (UVar name)
    varBij = unsafeUVarIsomorphism
    bisub =
        MkBisubstitution
            vn
            (return $
             ccontramap (isoBackwards varBij) $
             joinPinaforeShimWit
                 (singlePinaforeShimWit $ mkPJMShimWit $ VarPinaforeSingularType vn)
                 (singlePinaforeShimWit $ mkPJMShimWit tp))
            (return $
             cfmap (isoForwards varBij . join1) $ singlePinaforeShimWit $ mkPJMShimWit $ VarPinaforeSingularType vn)
    in do
           expr' <- getCompose $ bisubstituteUnifier bisub expr
           (ca, subs) <- runUnifier expr'
           return (ca $ isoForwards varBij . join2, bisub : subs)
runUnifier (OpenExpression (NegativeBisubstitutionWitness (vn :: SymbolType name) (tq :: PinaforeSingularType baseupdate 'Negative vw)) expr) = let
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
             meetPinaforeShimWit
                 (singlePinaforeShimWit $ mkPJMShimWit $ VarPinaforeSingularType vn)
                 (singlePinaforeShimWit $ mkPJMShimWit tq))
    in do
           expr' <- getCompose $ bisubstituteUnifier bisub expr
           (ca, subs) <- runUnifier expr'
           return (ca $ meet2 . isoBackwards varBij, bisub : subs)
