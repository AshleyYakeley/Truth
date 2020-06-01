module Pinafore.Language.TypeSystem.Nonpolar
    ( PinaforeNonpolarType
    , nonpolarToPinaforeType
    , pinaforeTypeToNonpolar
    , nonPolarTypeFreeVariables
    ) where

import Data.Shim
import Language.Expression.Dolan
import Language.Expression.UVar
import Pinafore.Language.Shim
import Pinafore.Language.Type.Ground
import Pinafore.Language.TypeSystem.Type
import Shapes

newtype AnyPolarity (w :: k -> Type) (polarity :: Polarity) (a :: k) =
    MkAnyPolarity (w a)

instance TestEquality w => TestEquality (AnyPolarity w polarity) where
    testEquality (MkAnyPolarity ta) (MkAnyPolarity tb) = testEquality ta tb

type NonpolarArgument (w :: Type -> Type) (sv :: Variance) = SingleArgument sv (AnyPolarity w) 'Positive

type PinaforeNonpolarType :: forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type
data PinaforeNonpolarType dv t where
    GroundPinaforeNonpolarType :: PinaforeGroundType dv t -> PinaforeNonpolarType dv t
    ApplyPinaforeNonpolarType
        :: VarianceType sv
        -> PinaforeNonpolarType (sv ': dv) f
        -> NonpolarArgument (PinaforeNonpolarType '[]) sv a
        -> PinaforeNonpolarType dv (f a)
    VarPinaforeNonpolarType :: SymbolType name -> PinaforeNonpolarType '[] (UVar name)

argFreeVariables ::
       forall sv t. VarianceType sv -> NonpolarArgument (PinaforeNonpolarType '[]) sv t -> [AnyW SymbolType]
argFreeVariables CovarianceType (MkAnyPolarity arg) = nonPolarTypeFreeVariables arg
argFreeVariables ContravarianceType (MkAnyPolarity arg) = nonPolarTypeFreeVariables arg
argFreeVariables RangevarianceType (MkRangeType (MkAnyPolarity argp) (MkAnyPolarity argq)) =
    nonPolarTypeFreeVariables argp <> nonPolarTypeFreeVariables argq

nonPolarTypeFreeVariables :: forall dv t. PinaforeNonpolarType dv t -> [AnyW SymbolType]
nonPolarTypeFreeVariables (VarPinaforeNonpolarType n) = [MkAnyW n]
nonPolarTypeFreeVariables (GroundPinaforeNonpolarType _) = []
nonPolarTypeFreeVariables (ApplyPinaforeNonpolarType sv tf targ) =
    nonPolarTypeFreeVariables tf <> argFreeVariables sv targ

fromApplyArg ::
       forall polarity sv dv f t a r. (Is PolarityType polarity, HasVariance sv f)
    => VarianceType sv
    -> DolanVarianceType dv
    -> (forall x. DolanVarianceMap dv (f x))
    -> NonpolarArgument (PinaforeNonpolarType '[]) sv a
    -> DolanArguments dv PinaforeType (f a) polarity t
    -> (forall b.
            InKind b =>
                    SingleArgument sv PinaforeType polarity b -> PShimWit (PinaforeShim Type) (DolanArguments dv PinaforeType (f b)) polarity t -> r)
    -> r
fromApplyArg CovarianceType dvt dvm (MkAnyPolarity ta) args call =
    case dolanVarianceInCategory @PinaforeShim dvt of
        Dict ->
            case nonpolarToPinaforeType ta of
                MkShimWit (arg :: _ b) aconv ->
                    call arg $ mapDolanArgumentsType dvt dvm dvm args $ polarMapTypeApply CovarianceType cid aconv
fromApplyArg ContravarianceType dvt dvm (MkAnyPolarity ta) args call =
    case dolanVarianceInCategory @PinaforeShim dvt of
        Dict ->
            invertPolarity @polarity $
            case nonpolarToPinaforeType ta of
                MkShimWit (arg :: _ b) aconv ->
                    call arg $
                    mapDolanArgumentsType dvt dvm dvm args $
                    polarMapTypeApply ContravarianceType cid $ MkCatDual $ uninvertPolarMap aconv
fromApplyArg RangevarianceType dvt dvm (MkRangeType (MkAnyPolarity pa) (MkAnyPolarity qa)) args call =
    case dolanVarianceInCategory @PinaforeShim dvt of
        Dict ->
            invertPolarity @polarity $
            case nonpolarToPinaforeType pa of
                MkShimWit (parg :: _ pb) pconv ->
                    case nonpolarToPinaforeType qa of
                        MkShimWit (qarg :: _ qb) qconv ->
                            call (MkRangeType parg qarg) $
                            mapDolanArgumentsType dvt dvm dvm args $
                            polarMapTypeApply RangevarianceType cid $ MkCatRange (uninvertPolarMap pconv) qconv

newtype ArgWit polarity dv f = MkArgWit
    { unArgWit :: forall (t :: Type).
                          DolanArguments dv PinaforeType f polarity t -> PShimWit (PinaforeShim Type) PinaforeSingularType polarity t
    }

nonpolarToPinaforeSingularType ::
       forall (polarity :: Polarity) (dv :: DolanVariance) (f :: DolanVarianceKind dv). Is PolarityType polarity
    => PinaforeNonpolarType dv f
    -> DolanVarianceType dv
    -> (DolanVarianceMap dv f, ArgWit polarity dv f)
nonpolarToPinaforeSingularType (VarPinaforeNonpolarType n) NilListType =
    (NilDolanVarianceMap, MkArgWit $ \NilDolanArguments -> mkShimWit $ VarPinaforeSingularType n)
nonpolarToPinaforeSingularType (GroundPinaforeNonpolarType gt) _ =
    (pinaforeGroundTypeVarianceMap gt, MkArgWit $ \args -> mkShimWit $ GroundPinaforeSingularType gt args)
nonpolarToPinaforeSingularType (ApplyPinaforeNonpolarType svt tf ta) dvt =
    case nonpolarToPinaforeSingularType tf (ConsListType svt dvt) of
        (ConsDolanVarianceMap dvm, MkArgWit swit) ->
            ( dvm
            , MkArgWit $ \args ->
                  fromApplyArg svt dvt dvm ta args $ \arg (MkShimWit args' aaconv) ->
                      mapShimWit aaconv $ swit $ ConsDolanArguments arg args')

nonpolarToPinaforeType ::
       forall polarity t. Is PolarityType polarity
    => PinaforeNonpolarType '[] t
    -> PinaforeTypeShimWit polarity t
nonpolarToPinaforeType t =
    singlePinaforeShimWit $ unArgWit (snd (nonpolarToPinaforeSingularType t NilListType)) NilDolanArguments

applyArg ::
       forall polarity sv t.
       VarianceType sv
    -> SingleArgument sv PinaforeType polarity t
    -> Maybe (AnyW (NonpolarArgument (PinaforeNonpolarType '[]) sv))
applyArg CovarianceType t = do
    ana <- pinaforeTypeToNonpolar t
    case ana of
        MkAnyW na -> return $ MkAnyW $ MkAnyPolarity na
applyArg ContravarianceType t = do
    ana <- pinaforeTypeToNonpolar t
    case ana of
        MkAnyW na -> return $ MkAnyW $ MkAnyPolarity na
applyArg RangevarianceType (MkRangeType p q) = do
    anp <- pinaforeTypeToNonpolar p
    anq <- pinaforeTypeToNonpolar q
    case (anp, anq) of
        (MkAnyW np, MkAnyW nq) -> return $ MkAnyW $ MkRangeType (MkAnyPolarity np) (MkAnyPolarity nq)

applyArgs ::
       forall polarity dv gt gt' t.
       DolanVarianceType dv
    -> PinaforeNonpolarType dv gt
    -> DolanArguments dv PinaforeType gt' polarity t
    -> Maybe (AnyW (PinaforeNonpolarType '[]))
applyArgs NilListType ft NilDolanArguments = Just $ MkAnyW ft
applyArgs (ConsListType sv dv) ft (ConsDolanArguments a1 ar) = do
    ana1 <- applyArg @polarity sv a1
    case ana1 of
        MkAnyW na1 -> applyArgs dv (ApplyPinaforeNonpolarType sv ft na1) ar

pinaforeSinglularTypeToNonpolar :: PinaforeSingularType polarity t -> Maybe (AnyW (PinaforeNonpolarType '[]))
pinaforeSinglularTypeToNonpolar (VarPinaforeSingularType n) = Just $ MkAnyW $ VarPinaforeNonpolarType n
pinaforeSinglularTypeToNonpolar (GroundPinaforeSingularType gt args) =
    applyArgs (pinaforeGroundTypeVarianceType gt) (GroundPinaforeNonpolarType gt) args

pinaforeTypeToNonpolar :: PinaforeType polarity t -> Maybe (AnyW (PinaforeNonpolarType '[]))
pinaforeTypeToNonpolar (ConsPinaforeType t NilPinaforeType) = pinaforeSinglularTypeToNonpolar t
pinaforeTypeToNonpolar _ = Nothing

pinaforeNonpolarArgTypeTestEquality ::
       forall sv a b.
       VarianceType sv
    -> NonpolarArgument (PinaforeNonpolarType '[]) sv a
    -> NonpolarArgument (PinaforeNonpolarType '[]) sv b
    -> Maybe (a :~: b)
pinaforeNonpolarArgTypeTestEquality CovarianceType = testEquality
pinaforeNonpolarArgTypeTestEquality ContravarianceType = testEquality
pinaforeNonpolarArgTypeTestEquality RangevarianceType = testEquality

pinaforeNonpolarTypeTestEquality ::
       forall dva ta dvb tb.
       PinaforeNonpolarType dva ta
    -> PinaforeNonpolarType dvb tb
    -> Maybe (dva :~: dvb, ta :~~: tb)
pinaforeNonpolarTypeTestEquality (GroundPinaforeNonpolarType ta) (GroundPinaforeNonpolarType tb) = do
    (Refl, HRefl) <- pinaforeGroundTypeTestEquality ta tb
    return (Refl, HRefl)
pinaforeNonpolarTypeTestEquality (ApplyPinaforeNonpolarType sva fa ta) (ApplyPinaforeNonpolarType svb fb tb) = do
    Refl <- testEquality sva svb
    (Refl, HRefl) <- pinaforeNonpolarTypeTestEquality fa fb
    Refl <- pinaforeNonpolarArgTypeTestEquality sva ta tb
    return (Refl, HRefl)
pinaforeNonpolarTypeTestEquality (VarPinaforeNonpolarType na) (VarPinaforeNonpolarType nb) = do
    Refl <- testEquality na nb
    return (Refl, HRefl)
pinaforeNonpolarTypeTestEquality _ _ = Nothing

instance TestEquality (PinaforeNonpolarType '[]) where
    testEquality ta tb = do
        (Refl, HRefl) <- pinaforeNonpolarTypeTestEquality ta tb
        return Refl
