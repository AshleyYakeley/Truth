{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Unify
    (
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Combine
import Language.Expression.Dolan.Solver
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
        -> BisubstitutionWitness ground (DolanPolyShim ground Type p (UVar Type name))
    NegativeBisubstitutionWitness
        :: forall (ground :: GroundTypeKind) name q.
           SymbolType name
        -> DolanShimWit ground 'Negative q
        -> BisubstitutionWitness ground (DolanPolyShim ground Type (UVar Type name) q)

mkPositiveBisubstitutionWitness ::
       forall (ground :: GroundTypeKind) name p. IsDolanSubtypeGroundType ground
    => SymbolType name
    -> DolanSingularType ground 'Positive p
    -> BisubstitutionWitness ground (DolanPolyShim ground Type p (UVar Type name))
mkPositiveBisubstitutionWitness n t =
    PositiveBisubstitutionWitness n $
    if occursInSingularType n t
        then plainRecursiveDolanShimWit (uVarName n) $ singleDolanPlainShimWit $ mkShimWit t
        else singleDolanShimWit $ mkShimWit t

mkNegativeBisubstitutionWitness ::
       forall (ground :: GroundTypeKind) name q. IsDolanSubtypeGroundType ground
    => SymbolType name
    -> DolanSingularType ground 'Negative q
    -> BisubstitutionWitness ground (DolanPolyShim ground Type (UVar Type name) q)
mkNegativeBisubstitutionWitness n t =
    NegativeBisubstitutionWitness n $
    if occursInSingularType n t
        then plainRecursiveDolanShimWit (uVarName n) $ singleDolanPlainShimWit $ mkShimWit t
        else singleDolanShimWit $ mkShimWit t

type DolanUnifier :: GroundTypeKind -> Type -> Type
type DolanUnifier ground = Expression (BisubstitutionWitness ground)

type FullUnifier :: GroundTypeKind -> Type -> Type
type FullUnifier ground = Solver ground (BisubstitutionWitness ground)

type DolanUnification :: GroundTypeKind -> Type -> Type -> Type
type DolanUnification ground a b = FullUnifier ground (DolanPolyShim ground Type a b)

unifySubtypeContext ::
       forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground
    => SubtypeContext (DolanType ground) (DolanPolyShim ground Type) (FullUnifier ground) 'Positive 'Negative
unifySubtypeContext = let
    subtypeTypes = unifyTypes
    subtypeInverted = unifySubtypeContext
    in MkSubtypeContext {..}

unifyGroundTypes ::
       forall (ground :: GroundTypeKind) dva gta ta dvb gtb tb. IsDolanSubtypeGroundType ground
    => ground dva gta
    -> DolanArguments dva (DolanType ground) gta 'Positive ta
    -> ground dvb gtb
    -> DolanArguments dvb (DolanType ground) gtb 'Negative tb
    -> DolanUnification ground ta tb
unifyGroundTypes gta argsa gtb argsb = subtypeGroundTypes solverLiftM unifySubtypeContext gta argsa gtb argsb

unifySingularTypes ::
       forall (ground :: GroundTypeKind) a b. IsDolanSubtypeGroundType ground
    => DolanSingularType ground 'Positive a
    -> DolanSingularType ground 'Negative b
    -> DolanUnification ground a b
unifySingularTypes (VarDolanSingularType na) (VarDolanSingularType nb)
    | Just Refl <- testEquality na nb = pure id
unifySingularTypes (VarDolanSingularType na) tb =
    solverLiftExpression $ varExpression $ mkNegativeBisubstitutionWitness na tb
unifySingularTypes ta (VarDolanSingularType nb) =
    solverLiftExpression $ varExpression $ mkPositiveBisubstitutionWitness nb ta
unifySingularTypes (GroundDolanSingularType gta argsa) (GroundDolanSingularType gtb argsb) =
    unifyGroundTypes gta argsa gtb argsb

unifyPlainTypes1 ::
       forall (ground :: GroundTypeKind) a b. IsDolanSubtypeGroundType ground
    => DolanSingularType ground 'Positive a
    -> DolanPlainType ground 'Negative b
    -> DolanUnification ground a b
unifyPlainTypes1 _ NilDolanPlainType = pure termf
unifyPlainTypes1 ta (ConsDolanPlainType t1 t2) = do
    f1 <- unifySingularTypes ta t1
    f2 <- unifyPlainTypes1 ta t2
    return $ meetf f1 f2

unifyPlainTypes ::
       forall (ground :: GroundTypeKind) a b. IsDolanSubtypeGroundType ground
    => DolanPlainType ground 'Positive a
    -> DolanPlainType ground 'Negative b
    -> DolanUnification ground a b
unifyPlainTypes NilDolanPlainType _ = pure initf
unifyPlainTypes (ConsDolanPlainType ta1 tar) tb = do
    f1 <- unifyPlainTypes1 ta1 tb
    f2 <- unifyPlainTypes tar tb
    return $ joinf f1 f2

unifyTypes ::
       forall (ground :: GroundTypeKind) a b. IsDolanSubtypeGroundType ground
    => DolanType ground 'Positive a
    -> DolanType ground 'Negative b
    -> DolanUnification ground a b
unifyTypes = solveRecursiveTypes unifyPlainTypes

unifyShimWits ::
       forall (ground :: GroundTypeKind) a b. IsDolanSubtypeGroundType ground
    => DolanShimWit ground 'Positive a
    -> DolanShimWit ground 'Negative b
    -> DolanUnification ground a b
unifyShimWits wa wb =
    unPosShimWit wa $ \ta conva ->
        unNegShimWit wb $ \tb convb -> do
            conv <- unifyTypes ta tb
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
    -> DolanUnifier ground (DolanPolyShim ground Type t (UVar Type name))
bisubstitutePlainPositiveVar _ NilDolanPlainType = pure initf
bisubstitutePlainPositiveVar vn (ConsDolanPlainType t1 tr) =
    OpenExpression (mkPositiveBisubstitutionWitness vn t1) $
    fmap (\fr f1 -> joinf f1 fr) $ bisubstitutePlainPositiveVar vn tr

bisubstitutePositiveVar ::
       forall (ground :: GroundTypeKind) name t. IsDolanSubtypeGroundType ground
    => SymbolType name
    -> DolanType ground 'Positive t
    -> DolanUnifier ground (DolanPolyShim ground Type t (UVar Type name))
bisubstitutePositiveVar n t =
    case polarPolySemiIsoShimWit $ dolanTypeToPlainUnroll t of
        MkShimWit pt (MkPolarMap tconv) -> fmap (\conv -> conv . tconv) $ bisubstitutePlainPositiveVar n pt

bisubstitutePlainNegativeVar ::
       forall (ground :: GroundTypeKind) name t. IsDolanSubtypeGroundType ground
    => SymbolType name
    -> DolanPlainType ground 'Negative t
    -> DolanUnifier ground (DolanPolyShim ground Type (UVar Type name) t)
bisubstitutePlainNegativeVar _ NilDolanPlainType = pure termf
bisubstitutePlainNegativeVar vn (ConsDolanPlainType t1 tr) =
    OpenExpression (mkNegativeBisubstitutionWitness vn t1) $
    fmap (\fr f1 -> meetf f1 fr) $ bisubstitutePlainNegativeVar vn tr

bisubstituteNegativeVar ::
       forall (ground :: GroundTypeKind) name t. IsDolanSubtypeGroundType ground
    => SymbolType name
    -> DolanType ground 'Negative t
    -> DolanUnifier ground (DolanPolyShim ground Type (UVar Type name) t)
bisubstituteNegativeVar n t =
    case polarPolySemiIsoShimWit $ dolanTypeToPlainUnroll t of
        MkShimWit pt (MkPolarMap tconv) -> fmap (\conv -> tconv . conv) $ bisubstitutePlainNegativeVar n pt

bisubstituteUnifier ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => UnifierBisubstitution ground
    -> DolanUnifier ground a
    -> FullUnifier ground a
bisubstituteUnifier _ (ClosedExpression a) = pure a
bisubstituteUnifier bisub@(MkBisubstitution bn _ (Identity tq)) (OpenExpression (PositiveBisubstitutionWitness vn tp) uval)
    | Just Refl <- testEquality bn vn = do
        conv <- unifyShimWits tp tq
        val' <- bisubstituteUnifier bisub uval
        pure $ val' conv
bisubstituteUnifier bisub@(MkBisubstitution bn (Identity tp) _) (OpenExpression (NegativeBisubstitutionWitness vn tq) uval)
    | Just Refl <- testEquality bn vn = do
        conv <- unifyShimWits tp tq
        val' <- bisubstituteUnifier bisub uval
        pure $ val' conv
bisubstituteUnifier bisub (OpenExpression (PositiveBisubstitutionWitness vn tp) uval) = let
    wp' = runIdentity $ bisubstituteType bisub tp
    in unPosShimWit wp' $ \tp' conv -> do
           val' <- bisubstituteUnifier bisub uval
           pv <- solverLiftExpression $ bisubstitutePositiveVar vn tp'
           pure $ val' $ pv <.> conv
bisubstituteUnifier bisub (OpenExpression (NegativeBisubstitutionWitness vn tp) uval) = let
    wp' = runIdentity $ bisubstituteType bisub tp
    in unNegShimWit wp' $ \tp' conv -> do
           val' <- bisubstituteUnifier bisub uval
           pv <- solverLiftExpression $ bisubstituteNegativeVar vn tp'
           pure $ val' $ conv <.> pv

runUnifier ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => DolanUnifier ground a
    -> WriterT [UnifierBisubstitution ground] (DolanTypeCheckM ground) a
runUnifier (ClosedExpression a) = return a
runUnifier (OpenExpression (PositiveBisubstitutionWitness oldvn (tp :: DolanShimWit ground 'Positive vw)) expr) =
    newUVar (uVarName oldvn) $ \(newvn :: SymbolType newname) ->
        assignUVar @Type @(JoinType (UVar Type newname) vw) oldvn $ let
            bisub =
                MkBisubstitution
                    oldvn
                    (return $ joinMeetShimWit (singleDolanShimWit $ mkShimWit $ VarDolanSingularType newvn) tp)
                    (return $ singleDolanShimWit $ MkShimWit (VarDolanSingularType newvn) $ invertPolarMap polar1)
            in do
                   expr' <- lift $ runSolver $ bisubstituteUnifier bisub expr
                   ca <- runUnifier expr'
                   tell [bisub]
                   return $ ca join2
runUnifier (OpenExpression (NegativeBisubstitutionWitness oldvn (tq :: DolanShimWit ground 'Negative vw)) expr) =
    newUVar (uVarName oldvn) $ \(newvn :: SymbolType newname) ->
        assignUVar @Type @(MeetType (UVar Type newname) vw) oldvn $ let
            bisub =
                MkBisubstitution
                    oldvn
                    (return $ singleDolanShimWit $ MkShimWit (VarDolanSingularType newvn) $ invertPolarMap polar1)
                    (return $ joinMeetShimWit (singleDolanShimWit $ mkShimWit $ VarDolanSingularType newvn) tq)
            in do
                   expr' <- lift $ runSolver $ bisubstituteUnifier bisub expr
                   ca <- runUnifier expr'
                   tell [bisub]
                   return $ ca meet2

instance forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground => UnifyTypeSystem (DolanTypeSystem ground) where
    type Unifier (DolanTypeSystem ground) = DolanUnifier ground
    type UnifierSubstitutions (DolanTypeSystem ground) = [UnifierBisubstitution ground]
    unifyNegWitnesses ta tb = return $ uuLiftNegShimWit $ joinMeetShimWit (mkShimWit ta) (mkShimWit tb)
    unifyPosWitnesses ta tb = return $ uuLiftPosShimWit $ joinMeetShimWit (mkShimWit ta) (mkShimWit tb)
    unifyPosNegWitnesses tq tp = fmap MkUUShim $ runSolver $ unifyTypes tq tp
    solveUnifier u = fmap (\(a, subs) -> (a, reverse subs)) $ runWriterT $ runUnifier u
    unifierPosSubstitute bisubs t = return $ runIdentity $ bisubstitutesType bisubs t
    unifierNegSubstitute bisubs t = return $ runIdentity $ bisubstitutesType bisubs t
