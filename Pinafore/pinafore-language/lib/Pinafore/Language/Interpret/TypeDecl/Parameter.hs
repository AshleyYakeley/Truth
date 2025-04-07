module Pinafore.Language.Interpret.TypeDecl.Parameter
    ( CCRTypeParam (..)
    , CCRTypeParams
    , tParamsVars
    , GenCCRTypeParams
    , AnyCCRTypeParams (..)
    , getAnyCCRTypeParams
    , tParamToPolarArgument
    , tParamToNonpolarArgument
    , getCCRVariancesMap
    )
where

import Import
import Pinafore.Language.Error
import Pinafore.Language.Interpret.TypeDecl.DoubleParams
import Pinafore.Language.Interpreter
import Pinafore.Language.Type

type CCRTypeParam :: CCRArgumentKind
data CCRTypeParam (sv :: CCRVariance) (t :: CCRVarianceKind sv) where
    CoCCRTypeParam :: TypeVarT tv -> CCRTypeParam CoCCRVariance tv
    ContraCCRTypeParam :: TypeVarT tv -> CCRTypeParam ContraCCRVariance tv
    RangeCCRTypeParam :: TypeVarT tvp -> TypeVarT tvq -> CCRTypeParam 'RangeCCRVariance '(tvp, tvq) -- contra, co

instance IsCCRArg CCRTypeParam where
    ccrArgumentType (CoCCRTypeParam _) = CoCCRVarianceType
    ccrArgumentType (ContraCCRTypeParam _) = ContraCCRVarianceType
    ccrArgumentType (RangeCCRTypeParam _ _) = RangeCCRVarianceType
    ccrArgumentTestEquality (CoCCRTypeParam arg1) (CoCCRTypeParam arg2) = do
        Refl <- testEquality arg1 arg2
        return Refl
    ccrArgumentTestEquality (ContraCCRTypeParam arg1) (ContraCCRTypeParam arg2) = do
        Refl <- testEquality arg1 arg2
        return Refl
    ccrArgumentTestEquality (RangeCCRTypeParam p1 q1) (RangeCCRTypeParam p2 q2) = do
        Refl <- testEquality p1 p2
        Refl <- testEquality q1 q2
        return Refl

assignCCRTypeParam ::
    forall (sv :: CCRVariance) (a :: CCRVarianceKind sv) (t :: CCRVarianceKind sv) r.
    CCRTypeParam sv t ->
    (t ~ a => r) ->
    r
assignCCRTypeParam (CoCCRTypeParam v) call = assignTypeVarT @a v call
assignCCRTypeParam (ContraCCRTypeParam v) call = assignTypeVarT @a v call
assignCCRTypeParam (RangeCCRTypeParam vp vq) call =
    case unsafeTypeIsPair @_ @_ @a of
        Refl -> assignTypeVarT @(Contra a) vp $ assignTypeVarT @(Co a) vq call

tParamVars :: CCRTypeParam sv t -> [SomeTypeVarT]
tParamVars (CoCCRTypeParam t) = [MkSomeTypeVarT t]
tParamVars (ContraCCRTypeParam t) = [MkSomeTypeVarT t]
tParamVars (RangeCCRTypeParam p q) = [MkSomeTypeVarT p, MkSomeTypeVarT q]

type CCRTypeParams :: forall (dv :: CCRVariances) -> CCRVariancesKind dv -> Type -> Type
type CCRTypeParams = CCRArguments CCRTypeParam

tParamsVars :: CCRTypeParams dv gt t -> [SomeTypeVarT]
tParamsVars NilCCRArguments = []
tParamsVars (ConsCCRArguments tp tps) = tParamVars tp ++ tParamsVars tps

withCCRTypeParam :: SyntaxTypeParameter -> (forall sv t. [Name] -> CCRTypeParam sv t -> r) -> r
withCCRTypeParam (PositiveSyntaxTypeParameter n) cont = nameToTypeVarT n $ \v -> cont [] $ CoCCRTypeParam v
withCCRTypeParam (NegativeSyntaxTypeParameter n) cont = nameToTypeVarT n $ \v -> cont [] $ ContraCCRTypeParam v
withCCRTypeParam (RangeSyntaxTypeParameter np nq) cont =
    nameToTypeVarT np $ \vp -> nameToTypeVarT nq $ \vq -> cont [] $ RangeCCRTypeParam vp vq
withCCRTypeParam (DoubleRangeSyntaxTypeParameter n) cont = let
    (np, nq) = doubleParameterNames n
    in nameToTypeVarT np $ \vp -> nameToTypeVarT nq $ \vq -> cont [n] $ RangeCCRTypeParam vp vq

getDataTypeMappingOrError ::
    forall v tv t. FullName -> VarianceType v -> TypeVarT tv -> VarMapping t -> QInterpreter (Mapping tv t)
getDataTypeMappingOrError tname vt var vm =
    case runVarMapping vm vt var of
        Nothing -> throw $ InterpretTypeDeclTypeVariableWrongPolarityError tname $ typeVarToName var
        Just vmap -> return vmap

getCCRVariation ::
    FullName ->
    CCRTypeParam sv a ->
    VarMapping t ->
    QInterpreter (CCRVarianceCategory KindFunction sv a a -> (t -> t))
getCCRVariation tname (CoCCRTypeParam v) vm = do
    f <- getDataTypeMappingOrError tname CoVarianceType v vm
    return $ runMapping f
getCCRVariation tname (ContraCCRTypeParam v) vm = do
    f <- getDataTypeMappingOrError tname ContraVarianceType v vm
    return $ \(MkCatDual tt) -> runMapping f tt
getCCRVariation tname (RangeCCRTypeParam vp vq) vm = do
    fp <- getDataTypeMappingOrError tname ContraVarianceType vp vm
    fq <- getDataTypeMappingOrError tname CoVarianceType vq vm
    return $ \(MkCatRange pp qq) -> runMapping fp pp . runMapping fq qq

type GenCCRTypeParams dv = forall (gt :: CCRVariancesKind dv). Some (CCRTypeParams dv gt)

data AnyCCRTypeParams where
    MkAnyCCRTypeParams :: forall (dv :: CCRVariances). GenCCRTypeParams dv -> AnyCCRTypeParams

getAnyCCRTypeParams :: [SyntaxTypeParameter] -> ([Name], AnyCCRTypeParams)
getAnyCCRTypeParams [] = ([], MkAnyCCRTypeParams nilAnyCCRArguments)
getAnyCCRTypeParams (sp : spp) =
    withCCRTypeParam sp $ \nn1 p ->
        case getAnyCCRTypeParams spp of
            (nn2, MkAnyCCRTypeParams f) -> (nn1 <> nn2, MkAnyCCRTypeParams $ consAnyCCRArguments p f)

tParamToPolarArgument ::
    forall sv (t :: CCRVarianceKind sv) polarity.
    Is PolarityType polarity =>
    CCRTypeParam sv t ->
    CCRPolarArgumentShimWit QShim QType polarity sv t
tParamToPolarArgument (CoCCRTypeParam var) =
    case shimWitToDolan $ mkShimWit $ VarDolanSingularType var of
        MkShimWit arg conv -> MkShimWit (CoCCRPolarArgument arg) conv
tParamToPolarArgument (ContraCCRTypeParam var) =
    withInvertPolarity @polarity
        $ case shimWitToDolan $ mkShimWit $ VarDolanSingularType var of
            MkShimWit arg conv -> MkShimWit (ContraCCRPolarArgument arg) $ MkCatDual $ uninvertPolarShim conv
tParamToPolarArgument (RangeCCRTypeParam varp varq) =
    withInvertPolarity @polarity
        $ case ( shimWitToDolan $ mkShimWit $ VarDolanSingularType varp
               , shimWitToDolan $ mkShimWit $ VarDolanSingularType varq
               ) of
            (MkShimWit argp convp, MkShimWit argq convq) ->
                MkShimWit (RangeCCRPolarArgument argp argq) $ MkCatRange (uninvertPolarShim convp) convq

tParamToNonpolarArgument ::
    forall sv (t :: CCRVarianceKind sv).
    CCRTypeParam sv t ->
    NonpolarArgument QGroundType sv t
tParamToNonpolarArgument (CoCCRTypeParam var) = CoNonpolarArgument $ VarNonpolarType var
tParamToNonpolarArgument (ContraCCRTypeParam var) = ContraNonpolarArgument $ VarNonpolarType var
tParamToNonpolarArgument (RangeCCRTypeParam varp varq) = RangeNonpolarArgument (VarNonpolarType varp) (VarNonpolarType varq)

paramsUnEndo ::
    forall (t :: Type) (dv :: CCRVariances) (f :: CCRVariancesKind dv).
    CCRTypeParams dv f t ->
    (t -> t) ->
    KindMorphism (->) f f
paramsUnEndo NilCCRArguments tt = tt
paramsUnEndo (ConsCCRArguments p pp) tt = let
    ff :: forall x. KindFunction (f x) (f x)
    ff = assignCCRTypeParam @_ @x p $ paramsUnEndo pp tt
    in MkNestedMorphism ff

paramsCCRVMap ::
    forall (sv :: CCRVariance) (x :: CCRVarianceKind sv) (t :: Type) (dv :: CCRVariances) (f :: CCRVarianceKind sv -> CCRVariancesKind dv) (a :: CCRVarianceKind sv) (b :: CCRVarianceKind sv).
    CCRTypeParam sv x ->
    (CCRVarianceCategory (->) sv x x -> t -> t) ->
    CCRArguments CCRTypeParam dv (f x) t ->
    CCRVarianceCategory (->) sv a b ->
    KindMorphism (->) (f a) (f b)
paramsCCRVMap p ff pp ab = assignCCRTypeParam @sv @a p $ assignCCRTypeParam @sv @b p $ paramsUnEndo pp $ ff ab

assignDolanArgVars :: forall sv dv gt t a. CCRTypeParam sv t -> CCRVariancesMap dv (gt t) -> CCRVariancesMap dv (gt a)
assignDolanArgVars p dvm = assignCCRTypeParam @sv @a p dvm

getCCRVariancesMap :: FullName -> CCRTypeParams dv gt t -> VarMapping t -> QInterpreter (CCRVariancesMap dv gt)
getCCRVariancesMap _ NilCCRArguments _ = return NilCCRVariancesMap
getCCRVariancesMap tname (ConsCCRArguments p pp) vm = do
    ff <- getCCRVariation tname p vm
    args <- getCCRVariancesMap tname pp vm
    return $ ConsCCRVariancesMap (MkCCRVariation Nothing $ paramsCCRVMap p ff pp) $ assignDolanArgVars p args
