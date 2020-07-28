{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS -fno-warn-orphans #-}

module Language.Expression.Dolan.Unify
    ( bisubstituteWitnessForTest
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.Combine
import Language.Expression.Dolan.MapType
import Language.Expression.Dolan.Occur
import Language.Expression.Dolan.Recursive
import Language.Expression.Dolan.Solver
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

type UnifierBisubstitution :: GroundTypeKind -> Type
type UnifierBisubstitution ground = Bisubstitution ground (DolanPolyShim ground) Identity

type BisubstitutionWitness :: GroundTypeKind -> Type -> Type
data BisubstitutionWitness ground t where
    MkBisubstitutionWitness
        :: forall (ground :: GroundTypeKind) polarity name p. Is PolarityType polarity
        => SymbolType name
        -> DolanShimWit ground polarity p
        -> BisubstitutionWitness ground (PolarMapType (DolanPolyShim ground Type) polarity p (UVarT name))

mkBisubstitutionWitness ::
       forall (ground :: GroundTypeKind) polarity name t. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => SymbolType name
    -> DolanSingularType ground polarity t
    -> BisubstitutionWitness ground (PolarMapType (DolanPolyShim ground Type) polarity t (UVarT name))
mkBisubstitutionWitness var pt =
    MkBisubstitutionWitness var $
    singleDolanShimWit $
    if occursInSingularType var pt
        then withRefl (usubIdentity @t var) $ let
                 af :: ApplyFunctor (USub name t)
                 af = substituteApplyFunctor var pt
                 bijRoll :: Bijection (Recursive (USub name t)) (Apply (USub name t) (Recursive (USub name t)))
                 bijRoll = rollRecursiveBijection @(USub name t) af
                 magic :: Bijection (Recursive (USub name t)) (UVar Type name)
                 magic = assignUVar @Type @t var $ recursiveForceIso var
                 fmagic :: Bijection (Apply (USub name t) (Recursive (USub name t))) (Apply (USub name t) (UVarT name))
                 fmagic = bijApplyFunctor @(USub name t) @(Recursive (USub name t)) @(UVarT name) af magic
                 iconv :: PolarMap (DolanPolyShim ground Type) polarity t (Recursive (USub name t))
                 iconv = isoPolarBackwards $ isoFunctionToShim "unroll" $ fmagic . bijRoll
                 in mapShimWit iconv $ recursiveDolanShimWit var $ singleDolanShimWit $ mkShimWit pt
        else mkShimWit pt

bisubstituteWitnessForTest ::
       forall (ground :: GroundTypeKind) polarity name t. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => SymbolType name
    -> DolanSingularType ground polarity t
    -> DolanShimWit ground polarity t
bisubstituteWitnessForTest var st = let
    bisub = mkBisubstitutionWitness var st
    in case bisub of
           MkBisubstitutionWitness _ (t :: DolanShimWit ground polarity' _) ->
               case (polarityType @polarity, polarityType @polarity') of
                   (PositiveType, PositiveType) -> t
                   (NegativeType, NegativeType) -> t
                   _ -> error "bisubstituteWitnessForTest"

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
unifySingularTypes (VarDolanSingularType na) tb = solverLiftExpression $ varExpression $ mkBisubstitutionWitness na tb
unifySingularTypes ta (VarDolanSingularType nb) = solverLiftExpression $ varExpression $ mkBisubstitutionWitness nb ta
unifySingularTypes (GroundDolanSingularType gta argsa) (GroundDolanSingularType gtb argsb) =
    unifyGroundTypes gta argsa gtb argsb
unifySingularTypes sta@(RecursiveDolanSingularType _ _) stb = solveRecursiveSingularTypes unifyTypes sta stb
unifySingularTypes sta stb@(RecursiveDolanSingularType _ _) = solveRecursiveSingularTypes unifyTypes sta stb

unifyTypes1 ::
       forall (ground :: GroundTypeKind) a b. IsDolanSubtypeGroundType ground
    => DolanSingularType ground 'Positive a
    -> DolanType ground 'Negative b
    -> DolanUnification ground a b
unifyTypes1 _ NilDolanType = pure termf
unifyTypes1 ta (ConsDolanType t1 t2) = do
    f1 <- unifySingularTypes ta t1
    f2 <- unifyTypes1 ta t2
    return $ meetf f1 f2

unifyTypes ::
       forall (ground :: GroundTypeKind) a b. IsDolanSubtypeGroundType ground
    => DolanType ground 'Positive a
    -> DolanType ground 'Negative b
    -> DolanUnification ground a b
unifyTypes NilDolanType _ = pure initf
unifyTypes (ConsDolanType ta1 tar) tb = do
    f1 <- unifyTypes1 ta1 tb
    f2 <- unifyTypes tar tb
    return $ joinf f1 f2

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

bisubstitutePositiveVar ::
       forall (ground :: GroundTypeKind) name t. IsDolanSubtypeGroundType ground
    => SymbolType name
    -> DolanType ground 'Positive t
    -> DolanUnifier ground (DolanPolyShim ground Type t (UVarT name))
bisubstitutePositiveVar _ NilDolanType = pure initf
bisubstitutePositiveVar vn (ConsDolanType t1 tr) =
    OpenExpression (mkBisubstitutionWitness vn t1) $ fmap (\fr f1 -> joinf f1 fr) $ bisubstitutePositiveVar vn tr

bisubstituteNegativeVar ::
       forall (ground :: GroundTypeKind) name t. IsDolanSubtypeGroundType ground
    => SymbolType name
    -> DolanType ground 'Negative t
    -> DolanUnifier ground (DolanPolyShim ground Type (UVarT name) t)
bisubstituteNegativeVar _ NilDolanType = pure termf
bisubstituteNegativeVar vn (ConsDolanType t1 tr) =
    OpenExpression (mkBisubstitutionWitness vn t1) $ fmap (\fr f1 -> meetf f1 fr) $ bisubstituteNegativeVar vn tr

bisubstituteUnifier ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => UnifierBisubstitution ground
    -> DolanUnifier ground a
    -> FullUnifier ground a
bisubstituteUnifier _ (ClosedExpression a) = pure a
bisubstituteUnifier bisub@(MkBisubstitution vsub (Identity tp) (Identity tq)) (OpenExpression (MkBisubstitutionWitness vwit (tw :: DolanShimWit ground polarity _)) uval)
    | Just Refl <- testEquality vsub vwit = do
        conv <-
            case polarityType @polarity of
                PositiveType -> unifyShimWits tw tq
                NegativeType -> unifyShimWits tp tw
        val' <- bisubstituteUnifier bisub uval
        pure $ val' conv
bisubstituteUnifier bisub (OpenExpression (MkBisubstitutionWitness vn (tw :: DolanShimWit ground polarity _)) uval) = let
    wp' = runIdentity $ bisubstituteShimWit bisub tw
    in case polarityType @polarity of
           PositiveType ->
               unPosShimWit wp' $ \tp' conv -> do
                   val' <- bisubstituteUnifier bisub uval
                   pv <- solverLiftExpression $ bisubstitutePositiveVar vn tp'
                   pure $ val' $ pv <.> conv
           NegativeType ->
               unNegShimWit wp' $ \tp' conv -> do
                   val' <- bisubstituteUnifier bisub uval
                   pv <- solverLiftExpression $ bisubstituteNegativeVar vn tp'
                   pure $ val' $ conv <.> pv

runUnifier ::
       forall (ground :: GroundTypeKind) a. IsDolanSubtypeGroundType ground
    => DolanUnifier ground a
    -> WriterT [UnifierBisubstitution ground] (DolanTypeCheckM ground) a
runUnifier (ClosedExpression a) = return a
runUnifier (OpenExpression (MkBisubstitutionWitness oldvn (tw :: DolanShimWit ground polarity vw)) expr) =
    invertPolarity @polarity $
    newUVar (uVarName oldvn) $ \(newvn :: SymbolType newname) ->
        assignUVar @Type @(JoinMeetType polarity (UVarT newname) vw) oldvn $ let
            bisub =
                mkPolarBisubstitution
                    oldvn
                    (return $ joinMeetShimWit (varDolanShimWit newvn) tw)
                    (return $ singleDolanShimWit $ MkShimWit (VarDolanSingularType newvn) $ invertPolarMap polar1)
            in do
                   expr' <- lift $ runSolver $ bisubstituteUnifier bisub expr
                   ca <- runUnifier expr'
                   tell [bisub]
                   return $
                       case polarityType @polarity of
                           PositiveType -> ca join2
                           NegativeType -> ca meet2

instance forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground => UnifyTypeSystem (DolanTypeSystem ground) where
    type Unifier (DolanTypeSystem ground) = DolanUnifier ground
    type UnifierSubstitutions (DolanTypeSystem ground) = [UnifierBisubstitution ground]
    unifyNegWitnesses ta tb = return $ uuLiftNegShimWit $ joinMeetShimWit (mkShimWit ta) (mkShimWit tb)
    unifyPosWitnesses ta tb = return $ uuLiftPosShimWit $ joinMeetShimWit (mkShimWit ta) (mkShimWit tb)
    unifyPosNegWitnesses tq tp = fmap MkUUShim $ runSolver $ unifyTypes tq tp
    solveUnifier u = fmap (\(a, subs) -> (a, reverse subs)) $ runWriterT $ runUnifier u
    unifierPosSubstitute bisubs t = return $ runIdentity $ bisubstitutesType bisubs t
    unifierNegSubstitute bisubs t = return $ runIdentity $ bisubstitutesType bisubs t
