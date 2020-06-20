{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Dolan.Unify
    ( DolanUnifier
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Combine
import Language.Expression.Dolan.Simplify
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Variance
import Language.Expression.TypeVariable
import Shapes

type UnifierBisubstitution :: GroundTypeKind -> Type
type UnifierBisubstitution ground = Bisubstitution ground Identity

type BisubstitutionWitness :: GroundTypeKind -> Type -> Type
data BisubstitutionWitness ground t where
    PositiveBisubstitutionWitness
        :: forall (ground :: GroundTypeKind) name p.
           SymbolType name
        -> DolanShimWit ground 'Positive p
        -> BisubstitutionWitness ground (DolanPolyShim ground Type p (UVar name))
    NegativeBisubstitutionWitness
        :: forall (ground :: GroundTypeKind) name q.
           SymbolType name
        -> DolanShimWit ground 'Negative q
        -> BisubstitutionWitness ground (DolanPolyShim ground Type (UVar name) q)

mkPositiveBisubstitutionWitness ::
       forall (ground :: GroundTypeKind) name p. IsDolanSubtypeGroundType ground
    => SymbolType name
    -> DolanSingularType ground 'Positive p
    -> BisubstitutionWitness ground (DolanPolyShim ground Type p (UVar name))
mkPositiveBisubstitutionWitness n t =
    PositiveBisubstitutionWitness n $
    if occursInSingularType n t
        then plainRecursiveDolanShimWit n $ singleDolanPlainShimWit $ mkShimWit t
        else singleDolanShimWit $ mkShimWit t

mkNegativeBisubstitutionWitness ::
       forall (ground :: GroundTypeKind) name q. IsDolanSubtypeGroundType ground
    => SymbolType name
    -> DolanSingularType ground 'Negative q
    -> BisubstitutionWitness ground (DolanPolyShim ground Type (UVar name) q)
mkNegativeBisubstitutionWitness n t =
    NegativeBisubstitutionWitness n $
    if occursInSingularType n t
        then plainRecursiveDolanShimWit n $ singleDolanPlainShimWit $ mkShimWit t
        else singleDolanShimWit $ mkShimWit t

type DolanUnifier :: GroundTypeKind -> Type -> Type
type DolanUnifier ground = Expression (BisubstitutionWitness ground)

type DolanFullUnifier :: GroundTypeKind -> Type -> Type
type DolanFullUnifier ground = Compose (ReaderT () (DolanTypeCheckM ground)) (DolanUnifier ground)

fullUnifierLift ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => DolanUnifier ground a
    -> DolanFullUnifier ground a
fullUnifierLift ua = Compose $ pure ua

fullUnifierLiftTypeCheck ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => DolanTypeCheckM ground a
    -> DolanFullUnifier ground a
fullUnifierLiftTypeCheck tca = Compose $ lift $ fmap pure tca

fullUnifierLiftSourceScoped ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => DolanM ground a
    -> DolanFullUnifier ground a
fullUnifierLiftSourceScoped tca = fullUnifierLiftTypeCheck $ liftTypeCheck tca

runFullUnifier ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => DolanFullUnifier ground a
    -> DolanTypeCheckM ground (DolanUnifier ground a)
runFullUnifier (Compose rma) = runReaderT rma ()

unifySubtypeContext ::
       forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground
    => SubtypeContext (DolanType ground) (DolanPolyShim ground Type) (DolanFullUnifier ground) 'Positive 'Negative
unifySubtypeContext = let
    subtypeTypes = unifyPosNegDolanTypes
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

unifyPosNegDolanSingularTypes ::
       forall (ground :: GroundTypeKind) a b. IsDolanSubtypeGroundType ground
    => DolanSingularType ground 'Positive a
    -> DolanSingularType ground 'Negative b
    -> DolanFullUnifier ground (DolanPolyShim ground Type a b)
unifyPosNegDolanSingularTypes (VarDolanSingularType na) (VarDolanSingularType nb)
    | Just Refl <- testEquality na nb = pure id
unifyPosNegDolanSingularTypes (VarDolanSingularType na) tb =
    fullUnifierLift $ varExpression $ mkNegativeBisubstitutionWitness na tb
unifyPosNegDolanSingularTypes ta (VarDolanSingularType nb) =
    fullUnifierLift $ varExpression $ mkPositiveBisubstitutionWitness nb ta
unifyPosNegDolanSingularTypes (GroundDolanSingularType gta argsa) (GroundDolanSingularType gtb argsb) =
    unifyPosNegGroundTypes gta argsa gtb argsb

unifyPosNegDolanPlainTypes1 ::
       forall (ground :: GroundTypeKind) a b. IsDolanSubtypeGroundType ground
    => DolanSingularType ground 'Positive a
    -> DolanPlainType ground 'Negative b
    -> DolanFullUnifier ground (DolanPolyShim ground Type a b)
unifyPosNegDolanPlainTypes1 _ NilDolanPlainType = pure termf
unifyPosNegDolanPlainTypes1 ta (ConsDolanPlainType t1 t2) = do
    f1 <- unifyPosNegDolanSingularTypes ta t1
    f2 <- unifyPosNegDolanPlainTypes1 ta t2
    return $ meetf f1 f2

unifyPosNegDolanPlainTypes ::
       forall (ground :: GroundTypeKind) a b. IsDolanSubtypeGroundType ground
    => DolanPlainType ground 'Positive a
    -> DolanPlainType ground 'Negative b
    -> DolanFullUnifier ground (DolanPolyShim ground Type a b)
unifyPosNegDolanPlainTypes NilDolanPlainType _ = pure initf
unifyPosNegDolanPlainTypes (ConsDolanPlainType ta1 tar) tb = do
    f1 <- unifyPosNegDolanPlainTypes1 ta1 tb
    f2 <- unifyPosNegDolanPlainTypes tar tb
    return $ joinf f1 f2

unifyPosNegDolanTypes ::
       forall (ground :: GroundTypeKind) a b. IsDolanSubtypeGroundType ground
    => DolanType ground 'Positive a
    -> DolanType ground 'Negative b
    -> DolanFullUnifier ground (DolanPolyShim ground Type a b)
unifyPosNegDolanTypes (PlainDolanType pta) (PlainDolanType ptb) = unifyPosNegDolanPlainTypes pta ptb
unifyPosNegDolanTypes (RecursiveDolanType n pt) _ = fullUnifierLiftSourceScoped $ throwTypeRecursiveError n pt
unifyPosNegDolanTypes _ (RecursiveDolanType n pt) = fullUnifierLiftSourceScoped $ throwTypeRecursiveError n pt

unifyPosNegDolanShimWit ::
       forall (ground :: GroundTypeKind) a b. IsDolanSubtypeGroundType ground
    => DolanShimWit ground 'Positive a
    -> DolanShimWit ground 'Negative b
    -> DolanFullUnifier ground (DolanPolyShim ground Type a b)
unifyPosNegDolanShimWit wa wb =
    unPosShimWit wa $ \ta conva ->
        unNegShimWit wb $ \tb convb -> do
            conv <- unifyPosNegDolanTypes ta tb
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

occursInPlainType ::
       forall (ground :: GroundTypeKind) polarity name a. IsDolanSubtypeGroundType ground
    => SymbolType name
    -> DolanPlainType ground polarity a
    -> Bool
occursInPlainType _ NilDolanPlainType = False
occursInPlainType n (ConsDolanPlainType t1 t2) = occursInSingularType n t1 || occursInPlainType n t2

occursInType ::
       forall (ground :: GroundTypeKind) polarity name a. IsDolanSubtypeGroundType ground
    => SymbolType name
    -> DolanType ground polarity a
    -> Bool
occursInType n (PlainDolanType pt) = occursInPlainType n pt
occursInType n (RecursiveDolanType n' _)
    | Just Refl <- testEquality n n' = False
occursInType n (RecursiveDolanType _ pt) = occursInPlainType n pt

bisubstitutePlainPositiveVar ::
       forall (ground :: GroundTypeKind) name t. IsDolanSubtypeGroundType ground
    => SymbolType name
    -> DolanPlainType ground 'Positive t
    -> DolanUnifier ground (DolanPolyShim ground Type t (UVar name))
bisubstitutePlainPositiveVar _ NilDolanPlainType = pure initf
bisubstitutePlainPositiveVar vn (ConsDolanPlainType t1 tr) =
    OpenExpression (mkPositiveBisubstitutionWitness vn t1) $
    fmap (\fr f1 -> joinf f1 fr) $ bisubstitutePlainPositiveVar vn tr

bisubstitutePositiveVar ::
       forall (ground :: GroundTypeKind) name t. IsDolanSubtypeGroundType ground
    => SymbolType name
    -> DolanType ground 'Positive t
    -> DolanUnifier ground (DolanPolyShim ground Type t (UVar name))
bisubstitutePositiveVar n t =
    case polarPolyIsoShimWit $ dolanTypeToPlainUnroll t of
        MkShimWit pt (MkPolarMap tconv) -> fmap (\conv -> conv <.> tconv) $ bisubstitutePlainPositiveVar n pt

bisubstitutePlainNegativeVar ::
       forall (ground :: GroundTypeKind) name t. IsDolanSubtypeGroundType ground
    => SymbolType name
    -> DolanPlainType ground 'Negative t
    -> DolanUnifier ground (DolanPolyShim ground Type (UVar name) t)
bisubstitutePlainNegativeVar _ NilDolanPlainType = pure termf
bisubstitutePlainNegativeVar vn (ConsDolanPlainType t1 tr) =
    OpenExpression (mkNegativeBisubstitutionWitness vn t1) $
    fmap (\fr f1 -> meetf f1 fr) $ bisubstitutePlainNegativeVar vn tr

bisubstituteNegativeVar ::
       forall (ground :: GroundTypeKind) name t. IsDolanSubtypeGroundType ground
    => SymbolType name
    -> DolanType ground 'Negative t
    -> DolanUnifier ground (DolanPolyShim ground Type (UVar name) t)
bisubstituteNegativeVar n t =
    case polarPolyIsoShimWit $ dolanTypeToPlainUnroll t of
        MkShimWit pt (MkPolarMap tconv) -> fmap (\conv -> tconv <.> conv) $ bisubstitutePlainNegativeVar n pt

bisubstituteUnifier ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => UnifierBisubstitution ground
    -> DolanUnifier ground a
    -> DolanFullUnifier ground a
bisubstituteUnifier _ (ClosedExpression a) = pure a
bisubstituteUnifier bisub@(MkBisubstitution bn _ (Identity tq)) (OpenExpression (PositiveBisubstitutionWitness vn tp) uval)
    | Just Refl <- testEquality bn vn = do
        conv <- unifyPosNegDolanShimWit tp tq
        val' <- bisubstituteUnifier bisub uval
        pure $ val' conv
bisubstituteUnifier bisub@(MkBisubstitution bn (Identity tp) _) (OpenExpression (NegativeBisubstitutionWitness vn tq) uval)
    | Just Refl <- testEquality bn vn = do
        conv <- unifyPosNegDolanShimWit tp tq
        val' <- bisubstituteUnifier bisub uval
        pure $ val' conv
bisubstituteUnifier bisub (OpenExpression (PositiveBisubstitutionWitness vn tp) uval) = let
    wp' = runIdentity $ bisubstituteShimWit bisub tp
    in unPosShimWit wp' $ \tp' conv ->
           Compose $ do
               uval' <- getCompose $ bisubstituteUnifier bisub uval
               return $ do
                   val' <- uval'
                   pv <- bisubstitutePositiveVar vn tp'
                   pure $ val' $ pv . conv
bisubstituteUnifier bisub (OpenExpression (NegativeBisubstitutionWitness vn tp) uval) = let
    wp' = runIdentity $ bisubstituteShimWit bisub tp
    in unNegShimWit wp' $ \tp' conv ->
           Compose $ do
               uval' <- getCompose $ bisubstituteUnifier bisub uval
               return $ do
                   val' <- uval'
                   pv <- bisubstituteNegativeVar vn tp'
                   pure $ val' $ conv . pv

runUnifier ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => DolanUnifier ground a
    -> WriterT [UnifierBisubstitution ground] (DolanTypeCheckM ground) a
runUnifier (ClosedExpression a) = return a
runUnifier (OpenExpression (PositiveBisubstitutionWitness (vn :: SymbolType name) (tp :: DolanShimWit ground 'Positive vw)) expr) = let
    varBij :: Isomorphism (DolanPolyShim ground Type) (JoinType (UVar name) vw) (UVar name)
    varBij = unsafeUVarIsomorphism
    bisub =
        MkBisubstitution
            vn
            (return $
             ccontramap (isoBackwards varBij) $
             joinMeetShimWit (singleDolanShimWit $ mkShimWit $ VarDolanSingularType vn) tp)
            (return $ cfmap (isoForwards varBij . join1) $ singleDolanShimWit $ mkShimWit $ VarDolanSingularType vn)
    in do
           expr' <- lift $ runFullUnifier $ bisubstituteUnifier bisub expr
           ca <- runUnifier expr'
           tell [bisub]
           return $ ca $ isoForwards varBij . join2
runUnifier (OpenExpression (NegativeBisubstitutionWitness (vn :: SymbolType name) (tq :: DolanShimWit ground 'Negative vw)) expr) = let
    varBij :: Isomorphism (DolanPolyShim ground Type) (MeetType (UVar name) vw) (UVar name)
    varBij = unsafeUVarIsomorphism
    bisub =
        MkBisubstitution
            vn
            (return $
             ccontramap (meet1 . isoBackwards varBij) $ singleDolanShimWit $ mkShimWit $ VarDolanSingularType vn)
            (return $
             cfmap (isoForwards varBij) $ joinMeetShimWit (singleDolanShimWit $ mkShimWit $ VarDolanSingularType vn) tq)
    in do
           expr' <- lift $ runFullUnifier $ bisubstituteUnifier bisub expr
           ca <- runUnifier expr'
           tell [bisub]
           return $ ca $ meet2 . isoBackwards varBij

instance forall (ground :: GroundTypeKind). (Eq (DolanName ground), IsDolanSubtypeGroundType ground) =>
             Unifier (DolanUnifier ground) where
    type UnifierName (DolanUnifier ground) = DolanName ground
    type UnifierMonad (DolanUnifier ground) = DolanTypeCheckM ground
    type UnifierNegWitness (DolanUnifier ground) = DolanType ground 'Negative
    type UnifierPosWitness (DolanUnifier ground) = DolanType ground 'Positive
    type UnifierSubstitutions (DolanUnifier ground) = [UnifierBisubstitution ground]
    type UnifierShim (DolanUnifier ground) = DolanPolyShim ground Type
    unifyNegWitnesses ta tb = return $ uuLiftNegShimWit $ joinMeetShimWit (mkShimWit ta) (mkShimWit tb)
    unifyPosWitnesses ta tb = return $ uuLiftPosShimWit $ joinMeetShimWit (mkShimWit ta) (mkShimWit tb)
    unifyPosNegWitnesses tq tp = fmap MkUUShim $ runFullUnifier $ unifyPosNegDolanTypes tq tp
    solveUnifier u = fmap (\(a, subs) -> (a, reverse subs)) $ runWriterT $ runUnifier u
    unifierPosSubstitute bisubs t = return $ runIdentity $ bisubstitutesType bisubs t
    unifierNegSubstitute bisubs t = return $ runIdentity $ bisubstitutesType bisubs t
    simplify = return . dolanSimplifyTypes @ground
