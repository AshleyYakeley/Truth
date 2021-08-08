module Language.Expression.Dolan.Nonpolar
    ( NonpolarGroundDolanType(..)
    , NonpolarDolanType(..)
    , coApplyNonpolarGroundType
    , contraApplyNonpolarGroundType
    , rangeApplyNonpolarGroundType
    , NonpolarGroundShimWit
    , NonpolarShimWit
    , coApplyNonpolarGroundShimWit
    , contraApplyNonpolarGroundShimWit
    , rangeApplyNonpolarGroundShimWit
    , nonPolarGroundCodec
    , nonpolarToDolanType
    , dolanTypeToNonpolar
    , nonpolarTypeFreeVariables
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.PShimWit
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Variance
import Shapes

newtype AnyPolarity (w :: k -> Type) (polarity :: Polarity) (a :: k) =
    MkAnyPolarity (w a)

instance TestEquality w => TestEquality (AnyPolarity w polarity) where
    testEquality (MkAnyPolarity ta) (MkAnyPolarity tb) = testEquality ta tb

type NonpolarArgument (w :: Type -> Type) (sv :: Variance) = SingleArgument sv (AnyPolarity w) 'Positive

type NonpolarGroundDolanType :: GroundTypeKind -> forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type
data NonpolarGroundDolanType ground dv t where
    GroundNonpolarGroundType
        :: forall (ground :: GroundTypeKind) dv t. ground dv t -> NonpolarGroundDolanType ground dv t
    ApplyNonpolarGroundType
        :: forall (ground :: GroundTypeKind) sv dv f a.
           VarianceType sv
        -> NonpolarGroundDolanType ground (sv ': dv) f
        -> NonpolarArgument (NonpolarDolanType ground '[]) sv a
        -> NonpolarGroundDolanType ground dv (f a)

nonpolarGroundDolanTypeDolanVarianceMap ::
       forall (ground :: GroundTypeKind) dv t. IsDolanGroundType ground
    => NonpolarGroundDolanType ground dv t
    -> DolanVarianceMap dv t
nonpolarGroundDolanTypeDolanVarianceMap (GroundNonpolarGroundType t) = groundTypeVarianceMap t
nonpolarGroundDolanTypeDolanVarianceMap (ApplyNonpolarGroundType _ t _) =
    case nonpolarGroundDolanTypeDolanVarianceMap t of
        ConsDolanVarianceMap dvm -> dvm

type NonpolarDolanType :: GroundTypeKind -> forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type
data NonpolarDolanType ground dv t where
    GroundNonpolarType
        :: forall (ground :: GroundTypeKind) dv t. NonpolarGroundDolanType ground dv t -> NonpolarDolanType ground dv t
    VarNonpolarType
        :: forall (ground :: GroundTypeKind) name. SymbolType name -> NonpolarDolanType ground '[] (UVarT name)

nonPolarGroundCodec ::
       forall (ground :: GroundTypeKind) dv t.
       Codec (NonpolarDolanType ground dv t) (NonpolarGroundDolanType ground dv t)
nonPolarGroundCodec = let
    encode = GroundNonpolarType
    decode =
        \case
            GroundNonpolarType t -> Just t
            _ -> Nothing
    in MkCodec {..}

coApplyNonpolarGroundType ::
       forall (ground :: GroundTypeKind) dv f a. HasVariance 'Covariance f
    => NonpolarGroundDolanType ground ('Covariance ': dv) f
    -> NonpolarDolanType ground '[] a
    -> NonpolarGroundDolanType ground dv (f a)
coApplyNonpolarGroundType tf ta = ApplyNonpolarGroundType CovarianceType tf (MkAnyPolarity ta)

contraApplyNonpolarGroundType ::
       forall (ground :: GroundTypeKind) dv f a.
       NonpolarGroundDolanType ground ('Contravariance ': dv) f
    -> NonpolarDolanType ground '[] a
    -> NonpolarGroundDolanType ground dv (f a)
contraApplyNonpolarGroundType tf ta = ApplyNonpolarGroundType ContravarianceType tf (MkAnyPolarity ta)

rangeApplyNonpolarGroundType ::
       forall (ground :: GroundTypeKind) dv f a b.
       NonpolarGroundDolanType ground ('Rangevariance ': dv) f
    -> NonpolarDolanType ground '[] a
    -> NonpolarDolanType ground '[] b
    -> NonpolarGroundDolanType ground dv (f '( a, b))
rangeApplyNonpolarGroundType tf ta tb =
    ApplyNonpolarGroundType RangevarianceType tf (MkRangeType (MkAnyPolarity ta) (MkAnyPolarity tb))

type NonpolarShimWit :: GroundTypeKind -> forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type
type NonpolarShimWit ground dv = ShimWit (DolanPolyIsoShim ground (DolanVarianceKind dv)) (NonpolarDolanType ground dv)

type NonpolarGroundShimWit :: GroundTypeKind -> forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type
type NonpolarGroundShimWit ground dv
     = ShimWit (DolanPolyIsoShim ground (DolanVarianceKind dv)) (NonpolarGroundDolanType ground dv)

coApplyNonpolarGroundShimWit ::
       forall (ground :: GroundTypeKind) dv f a. (IsDolanGroundType ground, HasVariance 'Covariance f)
    => NonpolarGroundShimWit ground ('Covariance ': dv) f
    -> NonpolarShimWit ground '[] a
    -> NonpolarGroundShimWit ground dv (f a)
coApplyNonpolarGroundShimWit (MkShimWit (tf :: _ f') convf) (MkShimWit ta conva) =
    case applyFunctionKindWitness (inKind @_ @f') ta of
        Dict ->
            case nonpolarGroundDolanTypeDolanVarianceMap tf of
                ConsDolanVarianceMap _ -> MkShimWit (coApplyNonpolarGroundType tf ta) (applyCoPolyShim convf conva)

contraApplyNonpolarGroundShimWit ::
       forall (ground :: GroundTypeKind) dv f a. (IsDolanGroundType ground, HasVariance 'Contravariance f)
    => NonpolarGroundShimWit ground ('Contravariance ': dv) f
    -> NonpolarShimWit ground '[] a
    -> NonpolarGroundShimWit ground dv (f a)
contraApplyNonpolarGroundShimWit (MkShimWit (tf :: _ f') convf) (MkShimWit ta conva) =
    case applyFunctionKindWitness (inKind @_ @f') ta of
        Dict ->
            case nonpolarGroundDolanTypeDolanVarianceMap tf of
                ConsDolanVarianceMap _ ->
                    MkShimWit (contraApplyNonpolarGroundType tf ta) (applyContraPolyShim convf $ invert conva)

rangeApplyNonpolarGroundShimWit ::
       forall (ground :: GroundTypeKind) dv f ap aq. (IsDolanGroundType ground, HasVariance 'Rangevariance f)
    => NonpolarGroundShimWit ground ('Rangevariance ': dv) f
    -> NonpolarShimWit ground '[] ap
    -> NonpolarShimWit ground '[] aq
    -> NonpolarGroundShimWit ground dv (f '( ap, aq))
rangeApplyNonpolarGroundShimWit (MkShimWit (tf :: _ f') convf) (MkShimWit (tap :: _ ap') convap) (MkShimWit (taq :: _ aq') convaq) =
    case applyFunctionKindWitness (inKind @_ @f') $ Proxy @('( ap', aq')) of
        Dict ->
            case nonpolarGroundDolanTypeDolanVarianceMap tf of
                ConsDolanVarianceMap _ ->
                    MkShimWit
                        (rangeApplyNonpolarGroundType tf tap taq)
                        (applyRangePolyShim convf (invert convap) convaq)

argFreeVariables ::
       forall (ground :: GroundTypeKind) sv t.
       VarianceType sv
    -> NonpolarArgument (NonpolarDolanType ground '[]) sv t
    -> [AnyW SymbolType]
argFreeVariables CovarianceType (MkAnyPolarity arg) = nonpolarTypeFreeVariables arg
argFreeVariables ContravarianceType (MkAnyPolarity arg) = nonpolarTypeFreeVariables arg
argFreeVariables RangevarianceType (MkRangeType (MkAnyPolarity argp) (MkAnyPolarity argq)) =
    nonpolarTypeFreeVariables argp <> nonpolarTypeFreeVariables argq

nonpolarGroundTypeFreeVariables ::
       forall (ground :: GroundTypeKind) dv t. NonpolarGroundDolanType ground dv t -> [AnyW SymbolType]
nonpolarGroundTypeFreeVariables (GroundNonpolarGroundType _) = []
nonpolarGroundTypeFreeVariables (ApplyNonpolarGroundType sv tf targ) =
    nonpolarGroundTypeFreeVariables @ground tf <> argFreeVariables @ground sv targ

nonpolarTypeFreeVariables :: forall (ground :: GroundTypeKind) dv t. NonpolarDolanType ground dv t -> [AnyW SymbolType]
nonpolarTypeFreeVariables (VarNonpolarType n) = [MkAnyW n]
nonpolarTypeFreeVariables (GroundNonpolarType t) = nonpolarGroundTypeFreeVariables t

fromApplyArg ::
       forall (ground :: GroundTypeKind) polarity sv dv f t a r.
       (IsDolanGroundType ground, Is PolarityType polarity, HasVariance sv f)
    => VarianceType sv
    -> DolanVarianceType dv
    -> (forall x. DolanVarianceMap dv (f x))
    -> NonpolarArgument (NonpolarDolanType ground '[]) sv a
    -> DolanArguments dv (DolanType ground) (f a) polarity t
    -> (forall b.
            InKind b =>
                    SingleArgument sv (DolanType ground) polarity b -> PShimWit (DolanShim ground) (DolanArguments dv (DolanType ground) (f b)) polarity t -> r)
    -> r
fromApplyArg CovarianceType dvt dvm (MkAnyPolarity ta) args call =
    case dolanVarianceInCategory @(DolanPolyShim ground) (ConsListType CovarianceType dvt) of
        Dict ->
            case nonpolarToDolanType ta of
                MkShimWit (arg :: _ b) aconv ->
                    call arg $ mapDolanArgumentsType dvt dvm dvm args $ polarMapTypeApply CovarianceType cid aconv
fromApplyArg ContravarianceType dvt dvm (MkAnyPolarity ta) args call =
    case dolanVarianceInCategory @(DolanPolyShim ground) (ConsListType ContravarianceType dvt) of
        Dict ->
            invertPolarity @polarity $
            case nonpolarToDolanType ta of
                MkShimWit (arg :: _ b) aconv ->
                    call arg $
                    mapDolanArgumentsType dvt dvm dvm args $
                    polarMapTypeApply ContravarianceType cid $ MkCatDual $ uninvertPolarMap aconv
fromApplyArg RangevarianceType dvt dvm (MkRangeType (MkAnyPolarity pa) (MkAnyPolarity qa)) args call =
    case dolanVarianceInCategory @(DolanPolyShim ground) (ConsListType RangevarianceType dvt) of
        Dict ->
            invertPolarity @polarity $
            case nonpolarToDolanType pa of
                MkShimWit (parg :: _ pb) pconv ->
                    case nonpolarToDolanType qa of
                        MkShimWit (qarg :: _ qb) qconv ->
                            call (MkRangeType parg qarg) $
                            mapDolanArgumentsType dvt dvm dvm args $
                            polarMapTypeApply RangevarianceType cid $ MkCatRange (uninvertPolarMap pconv) qconv

type ArgWit :: GroundTypeKind -> Polarity -> forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type
newtype ArgWit ground polarity dv f = MkArgWit
    { unArgWit :: forall (t :: Type).
                          DolanArguments dv (DolanType ground) f polarity t -> DolanSingularShimWit ground polarity t
    }

nonpolarGroundToDolanSingularType ::
       forall (ground :: GroundTypeKind) (polarity :: Polarity) (dv :: DolanVariance) (f :: DolanVarianceKind dv).
       (IsDolanGroundType ground, Is PolarityType polarity)
    => NonpolarGroundDolanType ground dv f
    -> DolanVarianceType dv
    -> (DolanVarianceMap dv f, ArgWit ground polarity dv f)
nonpolarGroundToDolanSingularType (GroundNonpolarGroundType ground) _ =
    (groundTypeVarianceMap ground, MkArgWit $ \args -> mkPolarShimWit $ GroundedDolanSingularType ground args)
nonpolarGroundToDolanSingularType (ApplyNonpolarGroundType svt tf ta) dvt =
    case nonpolarGroundToDolanSingularType tf (ConsListType svt dvt) of
        (ConsDolanVarianceMap dvm, MkArgWit swit) ->
            ( dvm
            , MkArgWit $ \args ->
                  fromApplyArg svt dvt dvm ta args $ \arg (MkShimWit args' aaconv) ->
                      mapPolarShimWit aaconv $ swit $ ConsDolanArguments arg args')

nonpolarToDolanSingularType ::
       forall (ground :: GroundTypeKind) (polarity :: Polarity) (dv :: DolanVariance) (f :: DolanVarianceKind dv).
       (IsDolanGroundType ground, Is PolarityType polarity)
    => NonpolarDolanType ground dv f
    -> DolanVarianceType dv
    -> (DolanVarianceMap dv f, ArgWit ground polarity dv f)
nonpolarToDolanSingularType (VarNonpolarType n) NilListType =
    (NilDolanVarianceMap, MkArgWit $ \NilDolanArguments -> mkPolarShimWit $ VarDolanSingularType n)
nonpolarToDolanSingularType (GroundNonpolarType t) dv = nonpolarGroundToDolanSingularType t dv

nonpolarToDolanType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => NonpolarDolanType ground '[] t
    -> DolanShimWit ground polarity t
nonpolarToDolanType t =
    singleDolanShimWit $ unArgWit (snd (nonpolarToDolanSingularType t NilListType)) NilDolanArguments

applyArg ::
       forall (ground :: GroundTypeKind) polarity sv t. IsDolanGroundType ground
    => VarianceType sv
    -> SingleArgument sv (DolanType ground) polarity t
    -> Maybe (AnyW (NonpolarArgument (NonpolarDolanType ground '[]) sv))
applyArg CovarianceType t = do
    ana <- dolanTypeToNonpolar t
    case ana of
        MkAnyW na -> return $ MkAnyW $ MkAnyPolarity na
applyArg ContravarianceType t = do
    ana <- dolanTypeToNonpolar t
    case ana of
        MkAnyW na -> return $ MkAnyW $ MkAnyPolarity na
applyArg RangevarianceType (MkRangeType p q) = do
    anp <- dolanTypeToNonpolar p
    anq <- dolanTypeToNonpolar q
    case (anp, anq) of
        (MkAnyW np, MkAnyW nq) -> return $ MkAnyW $ MkRangeType (MkAnyPolarity np) (MkAnyPolarity nq)

applyArgs ::
       forall (ground :: GroundTypeKind) polarity dv gt gt' t. IsDolanGroundType ground
    => DolanVarianceType dv
    -> NonpolarGroundDolanType ground dv gt
    -> DolanArguments dv (DolanType ground) gt' polarity t
    -> Maybe (AnyW (NonpolarGroundDolanType ground '[]))
applyArgs NilListType ft NilDolanArguments = Just $ MkAnyW ft
applyArgs (ConsListType sv dv) ft (ConsDolanArguments a1 ar) = do
    ana1 <- applyArg @ground @polarity sv a1
    case ana1 of
        MkAnyW na1 -> applyArgs dv (ApplyNonpolarGroundType sv ft na1) ar

dolanSingularTypeToNonpolar ::
       forall (ground :: GroundTypeKind) polarity t. IsDolanGroundType ground
    => DolanSingularType ground polarity t
    -> Maybe (AnyW (NonpolarDolanType ground '[]))
dolanSingularTypeToNonpolar (VarDolanSingularType n) = Just $ MkAnyW $ VarNonpolarType n
dolanSingularTypeToNonpolar (GroundedDolanSingularType ground args) =
    fmap (mapAnyW GroundNonpolarType) $
    applyArgs @ground (groundTypeVarianceType ground) (GroundNonpolarGroundType ground) args
dolanSingularTypeToNonpolar (RecursiveDolanSingularType _ _) = empty

dolanTypeToNonpolar ::
       forall (ground :: GroundTypeKind) polarity t. IsDolanGroundType ground
    => DolanType ground polarity t
    -> Maybe (AnyW (NonpolarDolanType ground '[]))
dolanTypeToNonpolar t = do
    MkAnyW st <- dolanTypeToSingular t
    dolanSingularTypeToNonpolar st

nonpolarArgTypeTestEquality ::
       forall (ground :: GroundTypeKind) sv a b. IsDolanGroundType ground
    => VarianceType sv
    -> NonpolarArgument (NonpolarDolanType ground '[]) sv a
    -> NonpolarArgument (NonpolarDolanType ground '[]) sv b
    -> Maybe (a :~: b)
nonpolarArgTypeTestEquality CovarianceType = testEquality
nonpolarArgTypeTestEquality ContravarianceType = testEquality
nonpolarArgTypeTestEquality RangevarianceType = testEquality

nonpolarGroundTypeTestEquality ::
       forall (ground :: GroundTypeKind) dva ta dvb tb. IsDolanGroundType ground
    => NonpolarGroundDolanType ground dva ta
    -> NonpolarGroundDolanType ground dvb tb
    -> Maybe (dva :~: dvb, ta :~~: tb)
nonpolarGroundTypeTestEquality (GroundNonpolarGroundType ta) (GroundNonpolarGroundType tb) = do
    (Refl, HRefl) <- groundTypeTestEquality ta tb
    return (Refl, HRefl)
nonpolarGroundTypeTestEquality (ApplyNonpolarGroundType sva fa ta) (ApplyNonpolarGroundType svb fb tb) = do
    Refl <- testEquality sva svb
    (Refl, HRefl) <- nonpolarGroundTypeTestEquality @ground fa fb
    Refl <- nonpolarArgTypeTestEquality @ground sva ta tb
    return (Refl, HRefl)
nonpolarGroundTypeTestEquality _ _ = Nothing

nonpolarTypeTestEquality ::
       forall (ground :: GroundTypeKind) dva ta dvb tb. IsDolanGroundType ground
    => NonpolarDolanType ground dva ta
    -> NonpolarDolanType ground dvb tb
    -> Maybe (dva :~: dvb, ta :~~: tb)
nonpolarTypeTestEquality (GroundNonpolarType ta) (GroundNonpolarType tb) = do
    (Refl, HRefl) <- nonpolarGroundTypeTestEquality ta tb
    return (Refl, HRefl)
nonpolarTypeTestEquality (VarNonpolarType na) (VarNonpolarType nb) = do
    Refl <- testEquality na nb
    return (Refl, HRefl)
nonpolarTypeTestEquality _ _ = Nothing

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground =>
             TestEquality (NonpolarGroundDolanType ground '[]) where
    testEquality ta tb = do
        (Refl, HRefl) <- nonpolarGroundTypeTestEquality ta tb
        return Refl

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => TestEquality (NonpolarDolanType ground '[]) where
    testEquality ta tb = do
        (Refl, HRefl) <- nonpolarTypeTestEquality ta tb
        return Refl
