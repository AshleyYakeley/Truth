module Pinafore.Language.TypeSystem.Nonpolar
    ( PinaforeNonpolarType
    , nonpolarToPinaforeType
    , pinaforeTypeToNonpolar
    , nonPolarTypeFreeVariables
    ) where

import Data.Shim
import Language.Expression.Dolan
import Language.Expression.UVar
import Pinafore.Language.Type.Ground
import Pinafore.Language.TypeSystem.Type
import Shapes

newtype AnyPolarity (w :: k -> Type) (polarity :: Polarity) (a :: k) =
    MkAnyPolarity (w a)

instance TestEquality w => TestEquality (AnyPolarity w polarity) where
    testEquality (MkAnyPolarity ta) (MkAnyPolarity tb) = testEquality ta tb

type NonpolarArgument (w :: Type -> Type) (sv :: Variance) = SingleArgument sv (AnyPolarity w) 'Positive

data PinaforeNonpolarType (baseupdate :: Type) (dv :: DolanVariance) (t :: DolanVarianceKind dv) where
    GroundPinaforeNonpolarType :: PinaforeGroundType baseupdate dv t -> PinaforeNonpolarType baseupdate dv t
    ApplyPinaforeNonpolarType
        :: VarianceType sv
        -> PinaforeNonpolarType baseupdate (sv ': dv) f
        -> NonpolarArgument (PinaforeNonpolarType baseupdate '[]) sv a
        -> PinaforeNonpolarType baseupdate dv (f a)
    VarPinaforeNonpolarType :: SymbolType name -> PinaforeNonpolarType baseupdate '[] (UVar name)

argFreeVariables ::
       forall baseupdate sv t.
       VarianceType sv
    -> NonpolarArgument (PinaforeNonpolarType baseupdate '[]) sv t
    -> [AnyW SymbolType]
argFreeVariables CovarianceType (MkAnyPolarity arg) = nonPolarTypeFreeVariables arg
argFreeVariables ContravarianceType (MkAnyPolarity arg) = nonPolarTypeFreeVariables arg
argFreeVariables RangevarianceType (MkRangeType (MkAnyPolarity argp) (MkAnyPolarity argq)) =
    nonPolarTypeFreeVariables argp <> nonPolarTypeFreeVariables argq

nonPolarTypeFreeVariables :: forall baseupdate dv t. PinaforeNonpolarType baseupdate dv t -> [AnyW SymbolType]
nonPolarTypeFreeVariables (VarPinaforeNonpolarType n) = [MkAnyW n]
nonPolarTypeFreeVariables (GroundPinaforeNonpolarType _) = []
nonPolarTypeFreeVariables (ApplyPinaforeNonpolarType sv tf targ) =
    nonPolarTypeFreeVariables tf <> argFreeVariables @baseupdate sv targ

invertPolarMap ::
       forall polarity a b. Is PolarityType polarity
    => PolarMapType JMShim (InvertPolarity polarity) a b
    -> PolarMapType (CatDual JMShim) polarity a b
invertPolarMap =
    case representative @_ @_ @polarity of
        PositiveType -> MkCatDual
        NegativeType -> MkCatDual

rangePolarMap ::
       forall (polarity :: Polarity) (a :: (Type, Type)) (b :: (Type, Type)). Is PolarityType polarity
    => PolarMapType JMShim (InvertPolarity polarity) (Contra a) (Contra b)
    -> PolarMapType JMShim polarity (Co a) (Co b)
    -> PolarMapType (CatRange JMShim) polarity '( Contra a, Co a) '( Contra b, Co b)
rangePolarMap =
    case representative @_ @_ @polarity of
        PositiveType -> MkCatRange
        NegativeType -> MkCatRange

liftConv ::
       forall (polarity :: Polarity) (v :: Variance) k (f :: VarianceKind v -> k) (a :: VarianceKind v) (b :: VarianceKind v).
       (CoercibleKind k, Is PolarityType polarity, HasVariance v f, InKind a, InKind b)
    => VarianceType v
    -> PolarMapType (VarianceCategory JMShim v) polarity a b
    -> PolarMapType JMShim polarity (f a) (f b)
liftConv svt conv =
    case representative @_ @_ @polarity of
        PositiveType -> consShimFunc @JMShim @v @k @f @f svt cid conv
        NegativeType -> consShimFunc @JMShim @v @k @f @f svt cid conv

fromApplyArg ::
       forall baseupdate polarity sv dv f t a r. (Is PolarityType polarity, HasVariance sv f)
    => VarianceType sv
    -> DolanVarianceType dv
    -> (forall x. DolanVarianceMap dv (f x))
    -> NonpolarArgument (PinaforeNonpolarType baseupdate '[]) sv a
    -> DolanArguments dv (PinaforeType baseupdate) (f a) polarity t
    -> (forall b.
            InKind b =>
                    SingleArgument sv (PinaforeType baseupdate) polarity b -> PJMShimWit (DolanArguments dv (PinaforeType baseupdate) (f b)) polarity t -> r)
    -> r
fromApplyArg CovarianceType dvt dvm (MkAnyPolarity ta) args call =
    case dolanVarianceInCategory @JMShim dvt of
        Dict ->
            case nonpolarToPinaforeType ta of
                MkShimWit (arg :: _ b) aconv ->
                    call arg $
                    mapDolanArgumentsType dvt dvm dvm args $ liftConv @polarity @sv @_ @f @a @b CovarianceType aconv
fromApplyArg ContravarianceType dvt dvm (MkAnyPolarity ta) args call =
    case dolanVarianceInCategory @JMShim dvt of
        Dict ->
            invertPolarity @polarity $
            case nonpolarToPinaforeType ta of
                MkShimWit (arg :: _ b) aconv ->
                    call arg $
                    mapDolanArgumentsType dvt dvm dvm args $
                    liftConv @polarity @sv @_ @f @a @b ContravarianceType $ invertPolarMap @polarity @a @b aconv
fromApplyArg RangevarianceType dvt dvm (MkRangeType (MkAnyPolarity pa) (MkAnyPolarity qa)) args call =
    case dolanVarianceInCategory @JMShim dvt of
        Dict ->
            invertPolarity @polarity $
            case nonpolarToPinaforeType pa of
                MkShimWit (parg :: _ pb) pconv ->
                    case nonpolarToPinaforeType qa of
                        MkShimWit (qarg :: _ qb) qconv ->
                            call (MkRangeType parg qarg) $
                            mapDolanArgumentsType dvt dvm dvm args $
                            liftConv @polarity @sv @_ @f @a @'( pb, qb) RangevarianceType $
                            rangePolarMap @polarity @a @'( pb, qb) pconv qconv

newtype ArgWit baseupdate polarity dv f = MkArgWit
    { unArgWit :: forall (t :: Type).
                          DolanArguments dv (PinaforeType baseupdate) f polarity t -> PJMShimWit (PinaforeSingularType baseupdate) polarity t
    }

nonpolarToPinaforeSingularType ::
       forall (baseupdate :: Type) (polarity :: Polarity) (dv :: DolanVariance) (f :: DolanVarianceKind dv).
       Is PolarityType polarity
    => PinaforeNonpolarType baseupdate dv f
    -> DolanVarianceType dv
    -> (DolanVarianceMap dv f, ArgWit baseupdate polarity dv f)
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
       forall baseupdate polarity t. Is PolarityType polarity
    => PinaforeNonpolarType baseupdate '[] t
    -> PinaforeShimWit baseupdate polarity t
nonpolarToPinaforeType t =
    singlePinaforeShimWit $ unArgWit (snd (nonpolarToPinaforeSingularType t NilListType)) NilDolanArguments

applyArg ::
       forall baseupdate polarity sv t.
       VarianceType sv
    -> SingleArgument sv (PinaforeType baseupdate) polarity t
    -> Maybe (AnyW (NonpolarArgument (PinaforeNonpolarType baseupdate '[]) sv))
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
       forall baseupdate polarity dv gt gt' t.
       DolanVarianceType dv
    -> PinaforeNonpolarType baseupdate dv gt
    -> DolanArguments dv (PinaforeType baseupdate) gt' polarity t
    -> Maybe (AnyW (PinaforeNonpolarType baseupdate '[]))
applyArgs NilListType ft NilDolanArguments = Just $ MkAnyW ft
applyArgs (ConsListType sv dv) ft (ConsDolanArguments a1 ar) = do
    ana1 <- applyArg @baseupdate @polarity sv a1
    case ana1 of
        MkAnyW na1 -> applyArgs dv (ApplyPinaforeNonpolarType sv ft na1) ar

pinaforeSinglularTypeToNonpolar ::
       PinaforeSingularType baseupdate polarity t -> Maybe (AnyW (PinaforeNonpolarType baseupdate '[]))
pinaforeSinglularTypeToNonpolar (VarPinaforeSingularType n) = Just $ MkAnyW $ VarPinaforeNonpolarType n
pinaforeSinglularTypeToNonpolar (GroundPinaforeSingularType gt args) =
    applyArgs (pinaforeGroundTypeVarianceType gt) (GroundPinaforeNonpolarType gt) args

pinaforeTypeToNonpolar :: PinaforeType baseupdate polarity t -> Maybe (AnyW (PinaforeNonpolarType baseupdate '[]))
pinaforeTypeToNonpolar (ConsPinaforeType t NilPinaforeType) = pinaforeSinglularTypeToNonpolar t
pinaforeTypeToNonpolar _ = Nothing

pinaforeNonpolarArgTypeTestEquality ::
       forall baseupdate sv a b.
       VarianceType sv
    -> NonpolarArgument (PinaforeNonpolarType baseupdate '[]) sv a
    -> NonpolarArgument (PinaforeNonpolarType baseupdate '[]) sv b
    -> Maybe (a :~: b)
pinaforeNonpolarArgTypeTestEquality CovarianceType = testEquality
pinaforeNonpolarArgTypeTestEquality ContravarianceType = testEquality
pinaforeNonpolarArgTypeTestEquality RangevarianceType = testEquality

pinaforeNonpolarTypeTestEquality ::
       forall baseupdate dva ta dvb tb.
       PinaforeNonpolarType baseupdate dva ta
    -> PinaforeNonpolarType baseupdate dvb tb
    -> Maybe (dva :~: dvb, ta :~~: tb)
pinaforeNonpolarTypeTestEquality (GroundPinaforeNonpolarType ta) (GroundPinaforeNonpolarType tb) = do
    (Refl, HRefl) <- pinaforeGroundTypeTestEquality ta tb
    return (Refl, HRefl)
pinaforeNonpolarTypeTestEquality (ApplyPinaforeNonpolarType sva fa ta) (ApplyPinaforeNonpolarType svb fb tb) = do
    Refl <- testEquality sva svb
    (Refl, HRefl) <- pinaforeNonpolarTypeTestEquality fa fb
    Refl <- pinaforeNonpolarArgTypeTestEquality @baseupdate sva ta tb
    return (Refl, HRefl)
pinaforeNonpolarTypeTestEquality (VarPinaforeNonpolarType na) (VarPinaforeNonpolarType nb) = do
    Refl <- testEquality na nb
    return (Refl, HRefl)
pinaforeNonpolarTypeTestEquality _ _ = Nothing

instance TestEquality (PinaforeNonpolarType baseupdate '[]) where
    testEquality ta tb = do
        (Refl, HRefl) <- pinaforeNonpolarTypeTestEquality ta tb
        return Refl
