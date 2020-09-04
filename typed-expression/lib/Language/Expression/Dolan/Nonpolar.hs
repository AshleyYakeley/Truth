module Language.Expression.Dolan.Nonpolar
    ( NonpolarDolanType
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

type NonpolarDolanType :: GroundTypeKind -> forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type
data NonpolarDolanType ground dv t where
    GroundNonpolarType :: forall (ground :: GroundTypeKind) dv t. ground dv t -> NonpolarDolanType ground dv t
    ApplyNonpolarType
        :: forall (ground :: GroundTypeKind) sv dv f a.
           VarianceType sv
        -> NonpolarDolanType ground (sv ': dv) f
        -> NonpolarArgument (NonpolarDolanType ground '[]) sv a
        -> NonpolarDolanType ground dv (f a)
    VarNonpolarType
        :: forall (ground :: GroundTypeKind) name. SymbolType name -> NonpolarDolanType ground '[] (UVarT name)

argFreeVariables ::
       forall (ground :: GroundTypeKind) sv t.
       VarianceType sv
    -> NonpolarArgument (NonpolarDolanType ground '[]) sv t
    -> [AnyW SymbolType]
argFreeVariables CovarianceType (MkAnyPolarity arg) = nonpolarTypeFreeVariables arg
argFreeVariables ContravarianceType (MkAnyPolarity arg) = nonpolarTypeFreeVariables arg
argFreeVariables RangevarianceType (MkRangeType (MkAnyPolarity argp) (MkAnyPolarity argq)) =
    nonpolarTypeFreeVariables argp <> nonpolarTypeFreeVariables argq

nonpolarTypeFreeVariables :: forall (ground :: GroundTypeKind) dv t. NonpolarDolanType ground dv t -> [AnyW SymbolType]
nonpolarTypeFreeVariables (VarNonpolarType n) = [MkAnyW n]
nonpolarTypeFreeVariables (GroundNonpolarType _) = []
nonpolarTypeFreeVariables (ApplyNonpolarType sv tf targ) =
    nonpolarTypeFreeVariables @ground tf <> argFreeVariables @ground sv targ

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
                    SingleArgument sv (DolanType ground) polarity b -> PShimWit (DolanPolyShim ground Type) (DolanArguments dv (DolanType ground) (f b)) polarity t -> r)
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

nonpolarToPinaforeSingularType ::
       forall (ground :: GroundTypeKind) (polarity :: Polarity) (dv :: DolanVariance) (f :: DolanVarianceKind dv).
       (IsDolanGroundType ground, Is PolarityType polarity)
    => NonpolarDolanType ground dv f
    -> DolanVarianceType dv
    -> (DolanVarianceMap dv f, ArgWit ground polarity dv f)
nonpolarToPinaforeSingularType (VarNonpolarType n) NilListType =
    (NilDolanVarianceMap, MkArgWit $ \NilDolanArguments -> mkShimWit $ VarDolanSingularType n)
nonpolarToPinaforeSingularType (GroundNonpolarType ground) _ =
    (groundTypeVarianceMap ground, MkArgWit $ \args -> mkShimWit $ GroundDolanSingularType ground args)
nonpolarToPinaforeSingularType (ApplyNonpolarType svt tf ta) dvt =
    case nonpolarToPinaforeSingularType tf (ConsListType svt dvt) of
        (ConsDolanVarianceMap dvm, MkArgWit swit) ->
            ( dvm
            , MkArgWit $ \args ->
                  fromApplyArg svt dvt dvm ta args $ \arg (MkShimWit args' aaconv) ->
                      mapShimWit aaconv $ swit $ ConsDolanArguments arg args')

nonpolarToDolanType ::
       forall (ground :: GroundTypeKind) polarity t. (IsDolanGroundType ground, Is PolarityType polarity)
    => NonpolarDolanType ground '[] t
    -> DolanShimWit ground polarity t
nonpolarToDolanType t =
    singleDolanShimWit $ unArgWit (snd (nonpolarToPinaforeSingularType t NilListType)) NilDolanArguments

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
    -> NonpolarDolanType ground dv gt
    -> DolanArguments dv (DolanType ground) gt' polarity t
    -> Maybe (AnyW (NonpolarDolanType ground '[]))
applyArgs NilListType ft NilDolanArguments = Just $ MkAnyW ft
applyArgs (ConsListType sv dv) ft (ConsDolanArguments a1 ar) = do
    ana1 <- applyArg @ground @polarity sv a1
    case ana1 of
        MkAnyW na1 -> applyArgs dv (ApplyNonpolarType sv ft na1) ar

pinaforeSinglularTypeToNonpolar ::
       forall (ground :: GroundTypeKind) polarity t. IsDolanGroundType ground
    => DolanSingularType ground polarity t
    -> Maybe (AnyW (NonpolarDolanType ground '[]))
pinaforeSinglularTypeToNonpolar (VarDolanSingularType n) = Just $ MkAnyW $ VarNonpolarType n
pinaforeSinglularTypeToNonpolar (GroundDolanSingularType ground args) =
    applyArgs @ground (groundTypeVarianceType ground) (GroundNonpolarType ground) args
pinaforeSinglularTypeToNonpolar (RecursiveDolanSingularType _ _) = empty

dolanTypeToNonpolar ::
       forall (ground :: GroundTypeKind) polarity t. IsDolanGroundType ground
    => DolanType ground polarity t
    -> Maybe (AnyW (NonpolarDolanType ground '[]))
dolanTypeToNonpolar (ConsDolanType t NilDolanType) = pinaforeSinglularTypeToNonpolar t
dolanTypeToNonpolar _ = Nothing

pinaforeNonpolarArgTypeTestEquality ::
       forall (ground :: GroundTypeKind) sv a b. IsDolanGroundType ground
    => VarianceType sv
    -> NonpolarArgument (NonpolarDolanType ground '[]) sv a
    -> NonpolarArgument (NonpolarDolanType ground '[]) sv b
    -> Maybe (a :~: b)
pinaforeNonpolarArgTypeTestEquality CovarianceType = testEquality
pinaforeNonpolarArgTypeTestEquality ContravarianceType = testEquality
pinaforeNonpolarArgTypeTestEquality RangevarianceType = testEquality

pinaforeNonpolarTypeTestEquality ::
       forall (ground :: GroundTypeKind) dva ta dvb tb. IsDolanGroundType ground
    => NonpolarDolanType ground dva ta
    -> NonpolarDolanType ground dvb tb
    -> Maybe (dva :~: dvb, ta :~~: tb)
pinaforeNonpolarTypeTestEquality (GroundNonpolarType ta) (GroundNonpolarType tb) = do
    (Refl, HRefl) <- groundTypeTestEquality ta tb
    return (Refl, HRefl)
pinaforeNonpolarTypeTestEquality (ApplyNonpolarType sva fa ta) (ApplyNonpolarType svb fb tb) = do
    Refl <- testEquality sva svb
    (Refl, HRefl) <- pinaforeNonpolarTypeTestEquality @ground fa fb
    Refl <- pinaforeNonpolarArgTypeTestEquality @ground sva ta tb
    return (Refl, HRefl)
pinaforeNonpolarTypeTestEquality (VarNonpolarType na) (VarNonpolarType nb) = do
    Refl <- testEquality na nb
    return (Refl, HRefl)
pinaforeNonpolarTypeTestEquality _ _ = Nothing

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => TestEquality (NonpolarDolanType ground '[]) where
    testEquality ta tb = do
        (Refl, HRefl) <- pinaforeNonpolarTypeTestEquality ta tb
        return Refl
