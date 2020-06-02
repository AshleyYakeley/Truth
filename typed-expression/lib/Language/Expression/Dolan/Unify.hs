{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Dolan.Unify
    ( DolanUnifier
    ) where

import Data.Shim
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Simplify
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Variance
import Language.Expression.Expression
import Language.Expression.UVar
import Language.Expression.Unifier
import Shapes

type UnifierBisubstitution :: GroundTypeKind -> Type
type UnifierBisubstitution ground = DolanBisubstitution ground (DolanTypeCheckM ground)

type BisubstitutionWitness :: GroundTypeKind -> Type -> Type
data BisubstitutionWitness ground t where
    PositiveBisubstitutionWitness
        :: forall (ground :: GroundTypeKind) name p.
           SymbolType name
        -> (DolanSingularType ground) 'Positive p
        -> BisubstitutionWitness ground (DolanPolyShim ground Type p (UVar name))
    NegativeBisubstitutionWitness
        :: forall (ground :: GroundTypeKind) name q.
           SymbolType name
        -> (DolanSingularType ground) 'Negative q
        -> BisubstitutionWitness ground (DolanPolyShim ground Type (UVar name) q)

type DolanUnifier :: GroundTypeKind -> Type -> Type
type DolanUnifier ground = Expression (BisubstitutionWitness ground)

type DolanFullUnifier :: GroundTypeKind -> Type -> Type
type DolanFullUnifier ground = Compose (DolanTypeCheckM ground) (DolanUnifier ground)

fullUnifierLiftTypeCheck ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => DolanTypeCheckM ground a
    -> DolanFullUnifier ground a
fullUnifierLiftTypeCheck tca = Compose $ fmap pure tca

fullUnifierLiftSourceScoped ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => DolanM ground a
    -> DolanFullUnifier ground a
fullUnifierLiftSourceScoped tca = fullUnifierLiftTypeCheck $ lift tca

unifySubtypeContext ::
       forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground
    => SubtypeContext (DolanType ground) (DolanPolyShim ground Type) (DolanFullUnifier ground) 'Positive 'Negative
unifySubtypeContext = let
    subtypeTypes = unifyPosNegPinaforeTypes
    subtypeInverted = unifySubtypeContext
    in MkSubtypeContext {..}

unifyPosNegGroundTypes ::
       forall (ground :: GroundTypeKind) dva gta ta dvb gtb tb. IsDolanSubtypeGroundType ground
    => ground dva gta
    -> DolanArguments dva (DolanType ground) gta 'Positive ta
    -> ground dvb gtb
    -> DolanArguments dvb (DolanType ground) gtb 'Negative tb
    -> DolanFullUnifier ground (DolanPolyShim ground Type ta tb)
unifyPosNegGroundTypes = subtypeGroundTypes fullUnifierLiftSourceScoped unifySubtypeContext

unifyPosNegPinaforeSingularTypes ::
       forall (ground :: GroundTypeKind) a b. IsDolanSubtypeGroundType ground
    => (DolanSingularType ground) 'Positive a
    -> (DolanSingularType ground) 'Negative b
    -> DolanFullUnifier ground (DolanPolyShim ground Type a b)
unifyPosNegPinaforeSingularTypes (VarDolanSingularType na) (VarDolanSingularType nb)
    | Just Refl <- testEquality na nb = pure id
unifyPosNegPinaforeSingularTypes (VarDolanSingularType na) tb =
    liftUnifier $ varExpression $ NegativeBisubstitutionWitness na tb
unifyPosNegPinaforeSingularTypes ta (VarDolanSingularType nb) =
    liftUnifier $ varExpression $ PositiveBisubstitutionWitness nb ta
unifyPosNegPinaforeSingularTypes (GroundDolanSingularType gta argsa) (GroundDolanSingularType gtb argsb) =
    unifyPosNegGroundTypes gta argsa gtb argsb

unifyPosNegPinaforeTypes1 ::
       forall (ground :: GroundTypeKind) a b. IsDolanSubtypeGroundType ground
    => (DolanSingularType ground) 'Positive a
    -> (DolanType ground) 'Negative b
    -> DolanFullUnifier ground (DolanPolyShim ground Type a b)
unifyPosNegPinaforeTypes1 _ NilDolanType = pure termf
unifyPosNegPinaforeTypes1 ta (ConsDolanType t1 t2) = do
    f1 <- unifyPosNegPinaforeSingularTypes ta t1
    f2 <- unifyPosNegPinaforeTypes1 ta t2
    return $ meetf f1 f2

unifyPosNegPinaforeTypes ::
       forall (ground :: GroundTypeKind) a b. IsDolanSubtypeGroundType ground
    => (DolanType ground) 'Positive a
    -> (DolanType ground) 'Negative b
    -> DolanFullUnifier ground (DolanPolyShim ground Type a b)
unifyPosNegPinaforeTypes NilDolanType _ = pure initf
unifyPosNegPinaforeTypes (ConsDolanType ta1 tar) tb = do
    f1 <- unifyPosNegPinaforeTypes1 ta1 tb
    f2 <- unifyPosNegPinaforeTypes tar tb
    return $ joinf f1 f2

unifyPosNegPinaforeShimWit ::
       forall (ground :: GroundTypeKind) a b. IsDolanSubtypeGroundType ground
    => PShimWit (DolanPolyShim ground Type) (DolanType ground) 'Positive a
    -> PShimWit (DolanPolyShim ground Type) (DolanType ground) 'Negative b
    -> DolanFullUnifier ground (DolanPolyShim ground Type a b)
unifyPosNegPinaforeShimWit wa wb =
    unPosShimWit wa $ \ta conva ->
        unNegShimWit wb $ \tb convb -> do
            conv <- unifyPosNegPinaforeTypes ta tb
            return $ convb . conv . conva

occursInArg ::
       forall (ground :: GroundTypeKind) polarity n sv a. IsDolanSubtypeGroundType ground
    => VarianceType sv
    -> SymbolType n
    -> SingleArgument sv (DolanType ground) polarity a
    -> Bool
occursInArg CovarianceType n t = occursInType n t
occursInArg ContravarianceType n t = occursInType n t
occursInArg RangevarianceType n (MkRangeType tp tq) = occursInType n tp || occursInType n tq

occursInArgs ::
       forall (ground :: GroundTypeKind) polarity n dv t a. IsDolanSubtypeGroundType ground
    => DolanVarianceType dv
    -> SymbolType n
    -> DolanArguments dv (DolanType ground) t polarity a
    -> Bool
occursInArgs NilListType _ NilDolanArguments = False
occursInArgs (ConsListType svt dvt) n (ConsDolanArguments arg args) =
    occursInArg @ground @polarity svt n arg || occursInArgs dvt n args

occursInSingularType ::
       forall (ground :: GroundTypeKind) polarity name a. IsDolanSubtypeGroundType ground
    => SymbolType name
    -> DolanSingularType ground polarity a
    -> Bool
occursInSingularType n (VarDolanSingularType nt)
    | Just Refl <- testEquality n nt = True
occursInSingularType _ (VarDolanSingularType _) = False
occursInSingularType n (GroundDolanSingularType gt args) = occursInArgs (groundTypeVarianceType gt) n args

occursInType ::
       forall (ground :: GroundTypeKind) polarity name a. IsDolanSubtypeGroundType ground
    => SymbolType name
    -> DolanType ground polarity a
    -> Bool
occursInType _ NilDolanType = False
occursInType n (ConsDolanType t1 t2) = occursInSingularType n t1 || occursInType n t2

bisubstitutePositiveVar ::
       forall (ground :: GroundTypeKind) name t. IsDolanSubtypeGroundType ground
    => SymbolType name
    -> (DolanType ground) 'Positive t
    -> (DolanUnifier ground) (DolanPolyShim ground Type t (UVar name))
bisubstitutePositiveVar _ NilDolanType = pure initf
bisubstitutePositiveVar vn (ConsDolanType t1 tr) =
    OpenExpression (PositiveBisubstitutionWitness vn t1) $ fmap (\fr f1 -> joinf f1 fr) $ bisubstitutePositiveVar vn tr

bisubstituteNegativeVar ::
       forall (ground :: GroundTypeKind) name t. IsDolanSubtypeGroundType ground
    => SymbolType name
    -> (DolanType ground) 'Negative t
    -> (DolanUnifier ground) (DolanPolyShim ground Type (UVar name) t)
bisubstituteNegativeVar _ NilDolanType = pure termf
bisubstituteNegativeVar vn (ConsDolanType t1 tr) =
    OpenExpression (NegativeBisubstitutionWitness vn t1) $ fmap (\fr f1 -> meetf f1 fr) $ bisubstituteNegativeVar vn tr

bisubstituteUnifier ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => UnifierBisubstitution ground
    -> DolanUnifier ground a
    -> DolanFullUnifier ground a
bisubstituteUnifier _ (ClosedExpression a) = pure a
bisubstituteUnifier bisub@(MkBisubstitution bn _ mtq) (OpenExpression (PositiveBisubstitutionWitness vn tp) uval)
    | Just Refl <- testEquality bn vn = do
        conv <-
            Compose $ do
                tq <- mtq
                getCompose $ unifyPosNegPinaforeShimWit (singleDolanShimWit $ mkShimWit tp) tq
        val' <- bisubstituteUnifier bisub uval
        pure $ val' conv
bisubstituteUnifier bisub@(MkBisubstitution bn mtp _) (OpenExpression (NegativeBisubstitutionWitness vn tq) uval)
    | Just Refl <- testEquality bn vn = do
        conv <-
            Compose $ do
                tp <- mtp
                getCompose $ unifyPosNegPinaforeShimWit tp (singleDolanShimWit $ mkShimWit tq)
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
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => DolanUnifier ground a
    -> DolanTypeCheckM ground (a, [UnifierBisubstitution ground])
runUnifier (ClosedExpression a) = return (a, [])
runUnifier (OpenExpression (PositiveBisubstitutionWitness vn tp) _)
    | occursInSingularType vn tp = lift $ throwTypeRecursiveError vn tp
runUnifier (OpenExpression (NegativeBisubstitutionWitness vn tp) _)
    | occursInSingularType vn tp = lift $ throwTypeRecursiveError vn tp
runUnifier (OpenExpression (PositiveBisubstitutionWitness (vn :: SymbolType name) (tp :: (DolanSingularType ground) 'Positive vw)) expr) = let
    varBij :: Isomorphism (DolanPolyShim ground Type) (JoinType (UVar name) vw) (UVar name)
    varBij = unsafeUVarIsomorphism
    bisub =
        MkBisubstitution
            vn
            (return $
             ccontramap (isoBackwards varBij) $
             joinMeetDolanShimWit
                 (singleDolanShimWit $ mkShimWit $ VarDolanSingularType vn)
                 (singleDolanShimWit $ mkShimWit tp))
            (return $ cfmap (isoForwards varBij . join1) $ singleDolanShimWit $ mkShimWit $ VarDolanSingularType vn)
    in do
           expr' <- getCompose $ bisubstituteUnifier bisub expr
           (ca, subs) <- runUnifier expr'
           return (ca $ isoForwards varBij . join2, bisub : subs)
runUnifier (OpenExpression (NegativeBisubstitutionWitness (vn :: SymbolType name) (tq :: (DolanSingularType ground) 'Negative vw)) expr) = let
    varBij :: Isomorphism (DolanPolyShim ground Type) (MeetType (UVar name) vw) (UVar name)
    varBij = unsafeUVarIsomorphism
    bisub =
        MkBisubstitution
            vn
            (return $
             ccontramap (meet1 . isoBackwards varBij) $ singleDolanShimWit $ mkShimWit $ VarDolanSingularType vn)
            (return $
             cfmap (isoForwards varBij) $
             joinMeetDolanShimWit
                 (singleDolanShimWit $ mkShimWit $ VarDolanSingularType vn)
                 (singleDolanShimWit $ mkShimWit tq))
    in do
           expr' <- getCompose $ bisubstituteUnifier bisub expr
           (ca, subs) <- runUnifier expr'
           return (ca $ meet2 . isoBackwards varBij, bisub : subs)

instance forall (ground :: GroundTypeKind). (Eq (DolanName ground), IsDolanSubtypeGroundType ground) =>
             Unifier (DolanUnifier ground) where
    type UnifierName (DolanUnifier ground) = DolanName ground
    type UnifierMonad (DolanUnifier ground) = DolanTypeCheckM ground
    type UnifierNegWitness (DolanUnifier ground) = DolanType ground 'Negative
    type UnifierPosWitness (DolanUnifier ground) = DolanType ground 'Positive
    type UnifierSubstitutions (DolanUnifier ground) = [UnifierBisubstitution ground]
    type UnifierShim (DolanUnifier ground) = DolanPolyShim ground Type
    unifyNegWitnesses ta tb = return $ uuLiftNegShimWit $ joinMeetDolanShimWit (mkShimWit ta) (mkShimWit tb)
    unifyPosWitnesses ta tb = return $ uuLiftPosShimWit $ joinMeetDolanShimWit (mkShimWit ta) (mkShimWit tb)
    unifyPosNegWitnesses tq tp = fmap MkUUShim $ getCompose $ unifyPosNegPinaforeTypes tq tp
    solveUnifier = runUnifier
    unifierPosSubstitute = bisubstitutesType
    unifierNegSubstitute = bisubstitutesType
    simplify = return . dolanSimplifyTypes @ground
