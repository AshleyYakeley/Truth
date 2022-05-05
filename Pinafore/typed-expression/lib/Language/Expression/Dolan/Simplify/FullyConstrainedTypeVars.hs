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

type UsageWitness :: GroundTypeKind -> Symbol -> Type -> Type
data UsageWitness ground name a where
    MkUsageWitness
        :: forall (ground :: GroundTypeKind) polarity name a.
           PolarityType polarity
        -> DolanType ground polarity a
        -> UsageWitness ground name (DolanPolarMap ground polarity (UVarT name) a)

instance forall (ground :: GroundTypeKind) name a. IsDolanGroundType ground => Show (UsageWitness ground name a) where
    show (MkUsageWitness p t) = show p <> withDict (getRepWitness p) (showDolanType t)

instance forall (ground :: GroundTypeKind) name. IsDolanGroundType ground =>
             AllWitnessConstraint Show (UsageWitness ground name) where
    allWitnessConstraint = Dict

type ExpressionPolyShim :: (Type -> Type) -> PolyShimKind -> PolyShimKind
type ExpressionPolyShim w = PolyComposeShim (Expression w)

type ExpressionShimWit :: (GroundTypeKind -> Polarity -> Type -> Type) -> GroundTypeKind -> Polarity -> Symbol -> Type -> Type
type ExpressionShimWit w ground polarity name
     = PShimWit (ExpressionPolyShim (UsageWitness ground name) (DolanPolyShim ground) Type) (w ground) polarity

nilUsageBox ::
       forall (ground :: GroundTypeKind) polarity name. (IsDolanGroundType ground, Is PolarityType polarity)
    => ExpressionShimWit DolanType ground polarity name (LimitType polarity)
nilUsageBox = nilDolanShimWit

consMeetUsageBox ::
       forall (ground :: GroundTypeKind) polarity name a b. (IsDolanGroundType ground, Is PolarityType polarity)
    => ExpressionShimWit DolanSingularType ground polarity name a
    -> ExpressionShimWit DolanType ground polarity name b
    -> ExpressionShimWit DolanType ground polarity name (JoinMeetType polarity a b)
consMeetUsageBox (MkShimWit ta expra) (MkShimWit tb exprb) = MkShimWit (ConsDolanType ta tb) $ iPolarPair expra exprb

buildUsageThis ::
       forall (ground :: GroundTypeKind) polarity name a. (IsDolanGroundType ground, Is PolarityType polarity)
    => SymbolType name
    -> DolanType ground polarity a
    -> ExpressionShimWit DolanType ground polarity name a
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
       forall (ground :: GroundTypeKind) polarity name a. (IsDolanGroundType ground, Is PolarityType polarity)
    => SymbolType name
    -> DolanSingularType ground polarity a
    -> ExpressionShimWit DolanSingularType ground polarity name a
buildUsageSingular n t@(RecursiveDolanSingularType n' _)
    | Just Refl <- testEquality n n' = mkPolarShimWit t
buildUsageSingular n t = mapDolanSingularType (buildUsage n) t

buildUsageInside ::
       forall (ground :: GroundTypeKind) polarity name a. (IsDolanGroundType ground, Is PolarityType polarity)
    => SymbolType name
    -> DolanType ground polarity a
    -> ExpressionShimWit DolanType ground polarity name a
buildUsageInside _ NilDolanType = nilUsageBox
buildUsageInside n (ConsDolanType t1 tr) = consMeetUsageBox (buildUsageSingular n t1) (buildUsageInside n tr)

buildUsage ::
       forall (ground :: GroundTypeKind) polarity name a. (IsDolanGroundType ground, Is PolarityType polarity)
    => SymbolType name
    -> DolanType ground polarity a
    -> ExpressionShimWit DolanType ground polarity name a
buildUsage n t = chainPolarShimWit (buildUsageInside n) $ buildUsageThis n t

type UsageSolution :: GroundTypeKind -> Symbol -> Type -> Type
data UsageSolution ground name t =
    forall a b. MkUsageSolution (InvertedCombinedDolanType ground 'Negative a)
                                (InvertedCombinedDolanType ground 'Positive b)
                                (DolanShim ground a (UVarT name) -> DolanShim ground (UVarT name) b -> t)

instance forall (ground :: GroundTypeKind) name. Functor (UsageSolution ground name) where
    fmap c (MkUsageSolution n p f) = MkUsageSolution n p $ \nconv pconv -> c $ f nconv pconv

instance forall (ground :: GroundTypeKind) name. IsDolanGroundType ground => Applicative (UsageSolution ground name) where
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
       forall (ground :: GroundTypeKind) name t. IsDolanGroundType ground
    => UsageWitness ground name t
    -> UsageSolution ground name t
solveUsageWitness (MkUsageWitness PositiveType tw) =
    MkUsageSolution NilInvertedCombinedDolanType (ConsInvertedCombinedDolanType tw NilInvertedCombinedDolanType) $ \_ conv ->
        MkPolarMap $ meet1 . conv
solveUsageWitness (MkUsageWitness NegativeType tw) =
    MkUsageSolution (ConsInvertedCombinedDolanType tw NilInvertedCombinedDolanType) NilInvertedCombinedDolanType $ \conv _ ->
        MkPolarMap $ conv . join1

solveUsageExpression ::
       forall (ground :: GroundTypeKind) name t. IsDolanGroundType ground
    => Expression (UsageWitness ground name) t
    -> UsageSolution ground name t
solveUsageExpression expr = solveExpression solveUsageWitness expr

getUsageSolution ::
       forall (ground :: GroundTypeKind) polarity name a. (IsDolanSubtypeGroundType ground, Is PolarityType polarity)
    => SymbolType name
    -> DolanType ground polarity a
    -> UsageSolution ground name (DolanShimWit ground polarity a)
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
    mcatch $ do
        expr <- runVarRenamerT $ getCompose $ invertedCombinedSubtype @ground negtype postype
        evalExpression expr

reduceUsageSolution ::
       forall (ground :: GroundTypeKind) name t. IsDolanSubtypeGroundType ground
    => SymbolType name
    -> UsageSolution ground name t
    -> DolanM ground (Maybe t)
reduceUsageSolution var (MkUsageSolution (n :: _ a) p f) = do
    mconv <- testInvertedCombinedSubtype n p
    return $ fmap (\conv -> assignUVarT @a var $ f id conv) mconv

eliminateVariable ::
       forall (ground :: GroundTypeKind) a.
       (IsDolanSubtypeGroundType ground, PShimWitMappable (DolanShim ground) (DolanType ground) a)
    => Some SymbolType
    -> a
    -> DolanM ground (a, Bool)
eliminateVariable (MkSome var) a = do
    ma' <- reduceUsageSolution var $ mapPShimWitsM (getUsageSolution @ground var) (getUsageSolution @ground var) a
    return $
        case ma' of
            Just a' -> (a', True)
            Nothing -> (a, False)

eliminateVariables ::
       forall (ground :: GroundTypeKind) a.
       (IsDolanSubtypeGroundType ground, PShimWitMappable (DolanShim ground) (DolanType ground) a)
    => [Some SymbolType]
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
        (setFromList @(FiniteSet (Some SymbolType)) -> posvars, setFromList -> negvars) = mappableGetVars @ground a
        allvars = toList $ union posvars negvars
    (a', elimflag) <- eliminateVariables @ground allvars a
    if elimflag
        then keepEliminatingVariables @ground a'
        else return a'

fullyConstrainedTypeVars ::
       forall (ground :: GroundTypeKind) a.
       (IsDolanSubtypeGroundType ground, PShimWitMappable (DolanShim ground) (DolanType ground) a)
    => a
    -> DolanTypeCheckM ground a
fullyConstrainedTypeVars expr = lift $ keepEliminatingVariables @ground expr
