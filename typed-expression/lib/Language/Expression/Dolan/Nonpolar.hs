module Language.Expression.Dolan.Nonpolar
    ( NonpolarDolanType
    , NonpolarGroundedShimWit
    , groundNonpolarGroundShimWit
    , NonpolarShimWit
    , groundNonpolarShimWit
    , varNonpolarShimWit
    , NonpolarArgumentDolanShimWit(..)
    , applyNonpolarGroundShimWit
    , nonPolarGroundedCodec
    , nonpolarToDolanType
    , nonpolarToDolanShimWit
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

type NonpolarGroundedDolanType :: GroundTypeKind -> forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type
data NonpolarGroundedDolanType ground dv t where
    GroundNonpolarGroundedType
        :: forall (ground :: GroundTypeKind) dv t. ground dv t -> NonpolarGroundedDolanType ground dv t
    ApplyNonpolarGroundedType
        :: forall (ground :: GroundTypeKind) sv dv f a.
           VarianceType sv
        -> NonpolarGroundedDolanType ground (sv ': dv) f
        -> NonpolarArgument (NonpolarDolanType ground '[]) sv a
        -> NonpolarGroundedDolanType ground dv (f a)

nonpolarGroundDolanTypeDolanVarianceMap ::
       forall (ground :: GroundTypeKind) dv t. IsDolanGroundType ground
    => NonpolarGroundedDolanType ground dv t
    -> DolanVarianceMap dv t
nonpolarGroundDolanTypeDolanVarianceMap (GroundNonpolarGroundedType t) = groundTypeVarianceMap t
nonpolarGroundDolanTypeDolanVarianceMap (ApplyNonpolarGroundedType _ t _) =
    case nonpolarGroundDolanTypeDolanVarianceMap t of
        ConsDolanVarianceMap dvm -> dvm

type NonpolarDolanType :: GroundTypeKind -> forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type
data NonpolarDolanType ground dv t where
    GroundedNonpolarType
        :: forall (ground :: GroundTypeKind) dv t.
           NonpolarGroundedDolanType ground dv t
        -> NonpolarDolanType ground dv t
    VarNonpolarType
        :: forall (ground :: GroundTypeKind) name. SymbolType name -> NonpolarDolanType ground '[] (UVarT name)

nonPolarGroundedCodec ::
       forall (ground :: GroundTypeKind) dv t.
       Codec (NonpolarDolanType ground dv t) (NonpolarGroundedDolanType ground dv t)
nonPolarGroundedCodec = let
    encode = GroundedNonpolarType
    decode =
        \case
            GroundedNonpolarType t -> Just t
            _ -> Nothing
    in MkCodec {..}

coApplyNonpolarGroundType ::
       forall (ground :: GroundTypeKind) dv f a. HasVariance 'Covariance f
    => NonpolarGroundedDolanType ground ('Covariance ': dv) f
    -> NonpolarDolanType ground '[] a
    -> NonpolarGroundedDolanType ground dv (f a)
coApplyNonpolarGroundType tf ta = ApplyNonpolarGroundedType CovarianceType tf (MkAnyPolarity ta)

contraApplyNonpolarGroundType ::
       forall (ground :: GroundTypeKind) dv f a.
       NonpolarGroundedDolanType ground ('Contravariance ': dv) f
    -> NonpolarDolanType ground '[] a
    -> NonpolarGroundedDolanType ground dv (f a)
contraApplyNonpolarGroundType tf ta = ApplyNonpolarGroundedType ContravarianceType tf (MkAnyPolarity ta)

rangeApplyNonpolarGroundType ::
       forall (ground :: GroundTypeKind) dv f a b.
       NonpolarGroundedDolanType ground ('Rangevariance ': dv) f
    -> NonpolarDolanType ground '[] a
    -> NonpolarDolanType ground '[] b
    -> NonpolarGroundedDolanType ground dv (f '( a, b))
rangeApplyNonpolarGroundType tf ta tb =
    ApplyNonpolarGroundedType RangevarianceType tf (MkRangeType (MkAnyPolarity ta) (MkAnyPolarity tb))

type NonpolarShimWit :: GroundTypeKind -> forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type
type NonpolarShimWit ground dv = ShimWit (DolanPolyIsoShim ground (DolanVarianceKind dv)) (NonpolarDolanType ground dv)

type NonpolarGroundedShimWit :: GroundTypeKind -> forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type
type NonpolarGroundedShimWit ground dv
     = ShimWit (DolanPolyIsoShim ground (DolanVarianceKind dv)) (NonpolarGroundedDolanType ground dv)

groundNonpolarGroundShimWit ::
       forall (ground :: GroundTypeKind) dv t. (InCategory (DolanPolyIsoShim ground (DolanVarianceKind dv)), InKind t)
    => ground dv t
    -> NonpolarGroundedShimWit ground dv t
groundNonpolarGroundShimWit t = mkShimWit $ GroundNonpolarGroundedType t

groundNonpolarShimWit ::
       forall (ground :: GroundTypeKind) dv t. NonpolarGroundedShimWit ground dv t -> NonpolarShimWit ground dv t
groundNonpolarShimWit (MkShimWit t conv) = MkShimWit (GroundedNonpolarType t) conv

varNonpolarShimWit ::
       forall (ground :: GroundTypeKind) name t.
       (Category (DolanPolyShim ground Type), CoerceShim (DolanPolyShim ground Type), Coercible t (UVarT name))
    => SymbolType name
    -> NonpolarShimWit ground '[] t
varNonpolarShimWit var = MkShimWit (VarNonpolarType var) $ coerceEnhanced "var"

coApplyNonpolarGroundShimWit ::
       forall (ground :: GroundTypeKind) dv f a. (IsDolanGroundType ground, HasVariance 'Covariance f)
    => NonpolarGroundedShimWit ground ('Covariance ': dv) f
    -> NonpolarShimWit ground '[] a
    -> NonpolarGroundedShimWit ground dv (f a)
coApplyNonpolarGroundShimWit (MkShimWit (tf :: _ f') convf) (MkShimWit ta conva) =
    case applyFunctionKindWitness (inKind @_ @f') ta of
        Dict ->
            case nonpolarGroundDolanTypeDolanVarianceMap tf of
                ConsDolanVarianceMap _ -> MkShimWit (coApplyNonpolarGroundType tf ta) (applyCoPolyShim convf conva)

contraApplyNonpolarGroundShimWit ::
       forall (ground :: GroundTypeKind) dv f a. (IsDolanGroundType ground, HasVariance 'Contravariance f)
    => NonpolarGroundedShimWit ground ('Contravariance ': dv) f
    -> NonpolarShimWit ground '[] a
    -> NonpolarGroundedShimWit ground dv (f a)
contraApplyNonpolarGroundShimWit (MkShimWit (tf :: _ f') convf) (MkShimWit ta conva) =
    case applyFunctionKindWitness (inKind @_ @f') ta of
        Dict ->
            case nonpolarGroundDolanTypeDolanVarianceMap tf of
                ConsDolanVarianceMap _ ->
                    MkShimWit (contraApplyNonpolarGroundType tf ta) (applyContraPolyShim convf $ invert conva)

rangeApplyNonpolarGroundShimWit ::
       forall (ground :: GroundTypeKind) dv f ap aq. (IsDolanGroundType ground, HasVariance 'Rangevariance f)
    => NonpolarGroundedShimWit ground ('Rangevariance ': dv) f
    -> NonpolarShimWit ground '[] ap
    -> NonpolarShimWit ground '[] aq
    -> NonpolarGroundedShimWit ground dv (f '( ap, aq))
rangeApplyNonpolarGroundShimWit (MkShimWit (tf :: _ f') convf) (MkShimWit (tap :: _ ap') convap) (MkShimWit (taq :: _ aq') convaq) =
    case applyFunctionKindWitness (inKind @_ @f') $ Proxy @('( ap', aq')) of
        Dict ->
            case nonpolarGroundDolanTypeDolanVarianceMap tf of
                ConsDolanVarianceMap _ ->
                    MkShimWit
                        (rangeApplyNonpolarGroundType tf tap taq)
                        (applyRangePolyShim convf (invert convap) convaq)

type NonpolarArgumentDolanShimWit :: GroundTypeKind -> forall k. k -> Type
data NonpolarArgumentDolanShimWit ground t where
    SingleNonpolarArgumentShimWit
        :: forall (ground :: GroundTypeKind) (t :: Type).
           NonpolarShimWit ground '[] t
        -> NonpolarArgumentDolanShimWit ground t
    PairNonpolarArgumentShimWit
        :: forall (ground :: GroundTypeKind) (ta :: Type) (tb :: Type).
           NonpolarShimWit ground '[] ta
        -> NonpolarShimWit ground '[] tb
        -> NonpolarArgumentDolanShimWit ground '( ta, tb)

applyNonpolarGroundShimWit ::
       forall (ground :: GroundTypeKind) sv dv (f :: DolanVarianceKind (sv ': dv)) (a :: VarianceKind sv).
       (IsDolanGroundType ground, HasVariance sv f)
    => NonpolarGroundedShimWit ground (sv ': dv) f
    -> NonpolarArgumentDolanShimWit ground a
    -> NonpolarGroundedShimWit ground dv (f a)
applyNonpolarGroundShimWit tf =
    case representative @_ @VarianceType @sv of
        CovarianceType -> \(SingleNonpolarArgumentShimWit ta) -> coApplyNonpolarGroundShimWit tf ta
        ContravarianceType -> \(SingleNonpolarArgumentShimWit ta) -> contraApplyNonpolarGroundShimWit tf ta
        RangevarianceType -> \(PairNonpolarArgumentShimWit tap taq) -> rangeApplyNonpolarGroundShimWit tf tap taq

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
       forall (ground :: GroundTypeKind) dv t. NonpolarGroundedDolanType ground dv t -> [AnyW SymbolType]
nonpolarGroundTypeFreeVariables (GroundNonpolarGroundedType _) = []
nonpolarGroundTypeFreeVariables (ApplyNonpolarGroundedType sv tf targ) =
    nonpolarGroundTypeFreeVariables @ground tf <> argFreeVariables @ground sv targ

nonpolarTypeFreeVariables :: forall (ground :: GroundTypeKind) dv t. NonpolarDolanType ground dv t -> [AnyW SymbolType]
nonpolarTypeFreeVariables (VarNonpolarType n) = [MkAnyW n]
nonpolarTypeFreeVariables (GroundedNonpolarType t) = nonpolarGroundTypeFreeVariables t

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
    => NonpolarGroundedDolanType ground dv f
    -> DolanVarianceType dv
    -> (DolanVarianceMap dv f, ArgWit ground polarity dv f)
nonpolarGroundToDolanSingularType (GroundNonpolarGroundedType ground) _ =
    (groundTypeVarianceMap ground, MkArgWit $ \args -> mkPolarShimWit $ GroundedDolanSingularType ground args)
nonpolarGroundToDolanSingularType (ApplyNonpolarGroundedType svt tf ta) dvt =
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
nonpolarToDolanSingularType (GroundedNonpolarType t) dv = nonpolarGroundToDolanSingularType t dv

{-
nonpolarToDolanSingularShimWit ::
       forall (ground :: GroundTypeKind) (polarity :: Polarity) (dv :: DolanVariance) (f :: DolanVarianceKind dv) (t :: Type).
       (IsDolanGroundType ground, Is PolarityType polarity)
    => NonpolarShimWit ground dv f
    -> DolanVarianceType dv
    -> DolanVarianceMap dv f
    -> DolanArguments dv (DolanType ground) f polarity t -> DolanSingularShimWit ground polarity t
nonpolarToDolanSingularShimWit (MkShimWit tf conv) dv dvmap args = case nonpolarToDolanSingularType @ground @polarity tf dv of
    (dvmap',argwit) -> case mapDolanArgumentsType dv dvmap dvmap' args (polyIsoPolar conv) of
        MkShimWit args' argsconv ->  mapPolarShimWit argsconv $ unArgWit argwit args'
-}
nonpolarToDolanType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => NonpolarDolanType ground '[] t
    -> DolanShimWit ground polarity t
nonpolarToDolanType t =
    singleDolanShimWit $ unArgWit (snd (nonpolarToDolanSingularType t NilListType)) NilDolanArguments

nonpolarToDolanShimWit ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => NonpolarShimWit ground '[] t
    -> DolanShimWit ground polarity t
nonpolarToDolanShimWit (MkShimWit t conv) =
    case nonpolarToDolanType t of
        MkShimWit t' conv' -> MkShimWit t' $ conv' . polyIsoPolar conv

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
    -> NonpolarGroundedDolanType ground dv gt
    -> DolanArguments dv (DolanType ground) gt' polarity t
    -> Maybe (AnyW (NonpolarGroundedDolanType ground '[]))
applyArgs NilListType ft NilDolanArguments = Just $ MkAnyW ft
applyArgs (ConsListType sv dv) ft (ConsDolanArguments a1 ar) = do
    ana1 <- applyArg @ground @polarity sv a1
    case ana1 of
        MkAnyW na1 -> applyArgs dv (ApplyNonpolarGroundedType sv ft na1) ar

dolanSingularTypeToNonpolar ::
       forall (ground :: GroundTypeKind) polarity t. IsDolanGroundType ground
    => DolanSingularType ground polarity t
    -> Maybe (AnyW (NonpolarDolanType ground '[]))
dolanSingularTypeToNonpolar (VarDolanSingularType n) = Just $ MkAnyW $ VarNonpolarType n
dolanSingularTypeToNonpolar (GroundedDolanSingularType ground args) =
    fmap (mapAnyW GroundedNonpolarType) $
    applyArgs @ground (groundTypeVarianceType ground) (GroundNonpolarGroundedType ground) args
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
    => NonpolarGroundedDolanType ground dva ta
    -> NonpolarGroundedDolanType ground dvb tb
    -> Maybe (dva :~: dvb, ta :~~: tb)
nonpolarGroundTypeTestEquality (GroundNonpolarGroundedType ta) (GroundNonpolarGroundedType tb) = do
    (Refl, HRefl) <- groundTypeTestEquality ta tb
    return (Refl, HRefl)
nonpolarGroundTypeTestEquality (ApplyNonpolarGroundedType sva fa ta) (ApplyNonpolarGroundedType svb fb tb) = do
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
nonpolarTypeTestEquality (GroundedNonpolarType ta) (GroundedNonpolarType tb) = do
    (Refl, HRefl) <- nonpolarGroundTypeTestEquality ta tb
    return (Refl, HRefl)
nonpolarTypeTestEquality (VarNonpolarType na) (VarNonpolarType nb) = do
    Refl <- testEquality na nb
    return (Refl, HRefl)
nonpolarTypeTestEquality _ _ = Nothing

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground =>
             TestEquality (NonpolarGroundedDolanType ground '[]) where
    testEquality ta tb = do
        (Refl, HRefl) <- nonpolarGroundTypeTestEquality ta tb
        return Refl

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => TestEquality (NonpolarDolanType ground '[]) where
    testEquality ta tb = do
        (Refl, HRefl) <- nonpolarTypeTestEquality ta tb
        return Refl
