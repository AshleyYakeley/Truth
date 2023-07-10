module Language.Expression.Dolan.Simplify.FullyConstrainedTypeVars
    ( fullyConstrainedTypeVars
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Bisubstitute
import Language.Expression.Dolan.InvertedCombine
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Simplify.VarUsage
import Language.Expression.Dolan.Simplify.VarUses
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Unifier
import Shapes

type UsageWitness :: GroundTypeKind -> Type -> Type -> Type
data UsageWitness ground tv a where
    MkUsageWitness
        :: forall (ground :: GroundTypeKind) polarity tv a.
           PolarityType polarity
        -> DolanType ground polarity a
        -> UsageWitness ground tv (DolanPolarMap ground polarity tv a)

instance forall (ground :: GroundTypeKind) tv a. IsDolanGroundType ground => Show (UsageWitness ground tv a) where
    show (MkUsageWitness p t) = show p <> withDict (getRepWitness p) (showDolanType t)

instance forall (ground :: GroundTypeKind) tv. IsDolanGroundType ground => AllConstraint Show (UsageWitness ground tv) where
    allConstraint = Dict

type ExpressionPolyShim :: (Type -> Type) -> PolyShimKind -> PolyShimKind
type ExpressionPolyShim w = PolyComposeShim (Expression w)

type ExpressionShimWit :: (GroundTypeKind -> Polarity -> Type -> Type) -> GroundTypeKind -> Polarity -> Type -> Type -> Type
type ExpressionShimWit w ground polarity tv
     = PShimWit (ExpressionPolyShim (UsageWitness ground tv) (DolanPolyShim ground) Type) (w ground) polarity

nilUsageBox ::
       forall (ground :: GroundTypeKind) polarity tv. (IsDolanGroundType ground, Is PolarityType polarity)
    => ExpressionShimWit DolanType ground polarity tv (LimitType polarity)
nilUsageBox = nilDolanShimWit

consMeetUsageBox ::
       forall (ground :: GroundTypeKind) polarity tv a b. (IsDolanGroundType ground, Is PolarityType polarity)
    => ExpressionShimWit DolanSingularType ground polarity tv a
    -> ExpressionShimWit DolanType ground polarity tv b
    -> ExpressionShimWit DolanType ground polarity tv (JoinMeetType polarity a b)
consMeetUsageBox (MkShimWit ta expra) (MkShimWit tb exprb) = MkShimWit (ConsDolanType ta tb) $ iPolarPair expra exprb

buildUsageThis ::
       forall (ground :: GroundTypeKind) polarity tv a. (IsDolanGroundType ground, Is PolarityType polarity)
    => TypeVarT tv
    -> DolanType ground polarity a
    -> ExpressionShimWit DolanType ground polarity tv a
buildUsageThis n t =
    case getTVarUsage n t of
        Just (MkTVarUsage (MkShimWit tw convw) convr) ->
            MkShimWit tw $
            polarF
                (polarMkPolyComposeShim $ varExpression $ MkUsageWitness representative tw)
                (polarMkPolyComposeShim $ pure convw) .
            (polarMkPolyComposeShim $ pure convr)
        Nothing -> mkPolarShimWit t

buildUsageSingular ::
       forall (ground :: GroundTypeKind) polarity tv a. (IsDolanGroundType ground, Is PolarityType polarity)
    => TypeVarT tv
    -> DolanSingularType ground polarity a
    -> ExpressionShimWit DolanSingularType ground polarity tv a
buildUsageSingular n t@(RecursiveDolanSingularType n' _)
    | Just Refl <- testEquality n n' = mkPolarShimWit t
buildUsageSingular n t = mapDolanSingularType (buildUsage n) t

buildUsageInside ::
       forall (ground :: GroundTypeKind) polarity tv a. (IsDolanGroundType ground, Is PolarityType polarity)
    => TypeVarT tv
    -> DolanType ground polarity a
    -> ExpressionShimWit DolanType ground polarity tv a
buildUsageInside _ NilDolanType = nilUsageBox
buildUsageInside n (ConsDolanType t1 tr) = consMeetUsageBox (buildUsageSingular n t1) (buildUsageInside n tr)

buildUsage ::
       forall (ground :: GroundTypeKind) polarity tv a. (IsDolanGroundType ground, Is PolarityType polarity)
    => TypeVarT tv
    -> DolanType ground polarity a
    -> ExpressionShimWit DolanType ground polarity tv a
buildUsage n t = chainPolarShimWit (buildUsageInside n) $ buildUsageThis n t

type UsageSolution :: GroundTypeKind -> Type -> Type -> Type
data UsageSolution ground tv t =
    forall a b. MkUsageSolution (InvertedCombinedDolanType ground 'Negative a)
                                (InvertedCombinedDolanType ground 'Positive b)
                                (DolanShim ground a tv -> DolanShim ground tv b -> t)

instance forall (ground :: GroundTypeKind) tv. Functor (UsageSolution ground tv) where
    fmap c (MkUsageSolution n p f) = MkUsageSolution n p $ \nconv pconv -> c $ f nconv pconv

instance forall (ground :: GroundTypeKind) tv. IsDolanGroundType ground => Applicative (UsageSolution ground tv) where
    pure a = MkUsageSolution NilInvertedCombinedDolanType NilInvertedCombinedDolanType $ \_ _ -> a
    liftA2 c (MkUsageSolution na pa fa) (MkUsageSolution nb pb fb) =
        case joinMeetInvertedCombinedType na nb of
            MkShimWit nab (MkPolarMap convn) ->
                case joinMeetInvertedCombinedType pa pb of
                    MkShimWit pab (MkPolarMap convp) ->
                        MkUsageSolution nab pab $ \nconv pconv ->
                            c
                                (fa (nconv . convn . join1) (meet1 . convp . pconv))
                                (fb (nconv . convn . join2) (meet2 . convp . pconv))

solveUsageWitness ::
       forall (ground :: GroundTypeKind) tv t. IsDolanGroundType ground
    => UsageWitness ground tv t
    -> UsageSolution ground tv t
solveUsageWitness (MkUsageWitness PositiveType tw) =
    MkUsageSolution NilInvertedCombinedDolanType (ConsInvertedCombinedDolanType tw NilInvertedCombinedDolanType) $ \_ conv ->
        MkPolarMap $ meet1 . conv
solveUsageWitness (MkUsageWitness NegativeType tw) =
    MkUsageSolution (ConsInvertedCombinedDolanType tw NilInvertedCombinedDolanType) NilInvertedCombinedDolanType $ \conv _ ->
        MkPolarMap $ conv . join1

solveUsageExpression ::
       forall (ground :: GroundTypeKind) tv t. IsDolanGroundType ground
    => Expression (UsageWitness ground tv) t
    -> UsageSolution ground tv t
solveUsageExpression expr = solveExpression solveUsageWitness expr

getUsageSolution ::
       forall (ground :: GroundTypeKind) polarity tv a. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => TypeVarT tv
    -> DolanType ground polarity a
    -> UsageSolution ground tv (DolanShimWit ground polarity a)
getUsageSolution var t =
    case buildUsage var t of
        MkShimWit t' expr -> fmap (MkShimWit t') $ solveUsageExpression @ground $ polarUnPolyComposeShim expr

invertedSubtype ::
       forall (ground :: GroundTypeKind) a b. IsDolanSubtypeGroundType ground
    => DolanType ground 'Negative a
    -> DolanType ground 'Positive b
    -> Compose (DolanTypeCheckM ground) (Unifier (DolanTypeSystem ground)) (DolanShim ground a b)
invertedSubtype ta tb = fmap unPolarMap $ Compose $ invertedPolarSubtype ta tb

invertedCombinedSubtype1 ::
       forall (ground :: GroundTypeKind) a b. IsDolanSubtypeGroundType ground
    => DolanType ground 'Negative a
    -> InvertedCombinedDolanType ground 'Positive b
    -> Compose (DolanTypeCheckM ground) (Unifier (DolanTypeSystem ground)) (DolanShim ground a b)
invertedCombinedSubtype1 _ NilInvertedCombinedDolanType = pure termf
invertedCombinedSubtype1 ta (ConsInvertedCombinedDolanType t1 tr) =
    liftA2 meetf (invertedSubtype ta t1) (invertedCombinedSubtype1 ta tr)

invertedCombinedSubtype ::
       forall (ground :: GroundTypeKind) a b. IsDolanSubtypeGroundType ground
    => InvertedCombinedDolanType ground 'Negative a
    -> InvertedCombinedDolanType ground 'Positive b
    -> Compose (DolanTypeCheckM ground) (Unifier (DolanTypeSystem ground)) (DolanShim ground a b)
invertedCombinedSubtype NilInvertedCombinedDolanType _ = pure initf
invertedCombinedSubtype (ConsInvertedCombinedDolanType t1 tr) tb =
    liftA2 joinf (invertedCombinedSubtype1 t1 tb) (invertedCombinedSubtype tr tb)

testInvertedCombinedSubtype ::
       forall (ground :: GroundTypeKind) a b. IsDolanSubtypeGroundType ground
    => InvertedCombinedDolanType ground 'Negative a
    -> InvertedCombinedDolanType ground 'Positive b
    -> DolanM ground (Maybe (DolanShim ground a b))
testInvertedCombinedSubtype negtype postype =
    fmap exec $
    mcatch $ do
        expr <- runVarRenamerT [] [] $ getCompose $ invertedCombinedSubtype @ground negtype postype
        return $ resultToMaybe $ evalExpressionResult expr

reduceUsageSolution ::
       forall (ground :: GroundTypeKind) tv t. IsDolanSubtypeGroundType ground
    => TypeVarT tv
    -> UsageSolution ground tv t
    -> DolanM ground (Maybe t)
reduceUsageSolution var (MkUsageSolution (n :: _ a) p f) = do
    mconv <- testInvertedCombinedSubtype n p
    return $ fmap (\conv -> assignTypeVarT @a var $ f id conv) mconv

eliminateVariable ::
       forall (ground :: GroundTypeKind) a.
       (IsDolanSubtypeGroundType ground, PShimWitMappable (DolanShim ground) (DolanType ground) a)
    => SomeTypeVarT
    -> a
    -> DolanM ground (a, Bool)
eliminateVariable (MkSomeTypeVarT var) a = do
    ma' <-
        reduceUsageSolution var $
        unEndoM (mapPShimWitsM (getUsageSolution @ground var) (getUsageSolution @ground var)) a
    return $
        case ma' of
            Just a' -> (a', True)
            Nothing -> (a, False)

eliminateVariables ::
       forall (ground :: GroundTypeKind) a.
       (IsDolanSubtypeGroundType ground, PShimWitMappable (DolanShim ground) (DolanType ground) a)
    => [SomeTypeVarT]
    -> a
    -> DolanM ground (a, Bool)
eliminateVariables [] t = return (t, False)
eliminateVariables (v:vv) t = do
    (t1, elimflag1) <- eliminateVariable @ground v t
    (t2, elimflag2) <- eliminateVariables @ground vv t1
    return (t2, elimflag1 || elimflag2)

-- | Iterate until no more variables get eliminated
keepEliminatingVariables ::
       forall (ground :: GroundTypeKind) a.
       (IsDolanSubtypeGroundType ground, PShimWitMappable (DolanShim ground) (DolanType ground) a)
    => a
    -> DolanM ground a
keepEliminatingVariables a = do
    let
        (setFromList @(FiniteSet SomeTypeVarT) -> posvars, setFromList -> negvars) = mappableGetVars @ground a
        allvars = toList $ union posvars negvars
    (a', elimflag) <- eliminateVariables @ground allvars a
    if elimflag
        then keepEliminatingVariables @ground a'
        else return a'

fullyConstrainedTypeVars ::
       forall (ground :: GroundTypeKind) a.
       (IsDolanSubtypeGroundType ground, PShimWitMappable (DolanShim ground) (DolanType ground) a)
    => EndoM (DolanTypeCheckM ground) a
fullyConstrainedTypeVars = liftEndoM $ MkEndoM $ keepEliminatingVariables @ground
