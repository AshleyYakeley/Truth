module Pinafore.Language.Grammar.Interpret.TypeDecl.Data
    ( makeDataTypeBox
    ) where

import qualified Data.List as List
import Pinafore.Language.Error
import Pinafore.Language.ExprShow
import Pinafore.Language.Expression
import Pinafore.Language.Grammar.Interpret.Type
import Pinafore.Language.Grammar.Interpret.TypeDecl.TypeBox
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Shim
import Pinafore.Language.Type
import Pinafore.Markdown
import Shapes
import Shapes.Unsafe (unsafeGetRefl)

-- | Structure of a datatype
data PinaforeDataType :: forall (k :: Type). k -> Type where
    NilDataType :: PinaforeDataType Void
    ConsDataType :: ListType PinaforeNonpolarType tl -> PinaforeDataType tt -> PinaforeDataType (Either (HList tl) tt)

-- | Structural equality
instance TestHetEquality PinaforeDataType where
    testHetEquality NilDataType NilDataType = return HRefl
    testHetEquality (ConsDataType a1 ar) (ConsDataType b1 br) = do
        Refl <- testEquality a1 b1
        HRefl <- testHetEquality ar br
        return HRefl
    testHetEquality _ _ = Nothing

data DataBox =
    forall t. MkDataBox (PinaforeDataType t)
                        [Constructor PinaforeNonpolarType t]

assembleDataType :: [(Name, AnyW (ListType PinaforeNonpolarType))] -> DataBox
assembleDataType [] = MkDataBox NilDataType []
assembleDataType ((n, MkAnyW el):cc) =
    case assembleDataType cc of
        MkDataBox ct conss ->
            MkDataBox (ConsDataType el ct) $ (MkConstructor n el Left eitherLeft) : fmap extendConstructor conss

data DataTypeFamily :: FamilyKind where
    MkDataTypeFamily :: forall (tid :: BigNat). TypeIDType tid -> DataTypeFamily (Identified tid)

instance TestHetEquality DataTypeFamily where
    testHetEquality (MkDataTypeFamily ia) (MkDataTypeFamily ib) = do
        Refl <- testEquality ia ib
        return HRefl

datatypeIOWitness :: IOWitness ('MkWitKind DataTypeFamily)
datatypeIOWitness = $(iowitness [t|'MkWitKind DataTypeFamily|])

interpretDataTypeConstructor ::
       SyntaxDatatypeConstructor -> PinaforeInterpreter (Name, AnyW (ListType PinaforeNonpolarType))
interpretDataTypeConstructor (MkSyntaxDatatypeConstructor consName stypes) = do
    etypes <- for stypes interpretNonpolarType
    return (consName, assembleListType etypes)

type TParam :: CCRArgumentKind
data TParam (sv :: CCRVariance) (t :: CCRVarianceKind sv) where
    CoTParam :: SymbolType n -> TParam CoCCRVariance (UVarT n)
    ContraTParam :: SymbolType n -> TParam ContraCCRVariance (UVarT n)
    RangeTParam :: SymbolType np -> SymbolType nq -> TParam 'RangeCCRVariance '( UVarT np, UVarT nq) -- contra, co

instance IsCCRArg TParam where
    ccrArgumentType (CoTParam _) = CoCCRVarianceType
    ccrArgumentType (ContraTParam _) = ContraCCRVarianceType
    ccrArgumentType (RangeTParam _ _) = RangeCCRVarianceType
    ccrArgumentTestEquality (CoTParam arg1) (CoTParam arg2) = do
        Refl <- testEquality arg1 arg2
        return Refl
    ccrArgumentTestEquality (ContraTParam arg1) (ContraTParam arg2) = do
        Refl <- testEquality arg1 arg2
        return Refl
    ccrArgumentTestEquality (RangeTParam p1 q1) (RangeTParam p2 q2) = do
        Refl <- testEquality p1 p2
        Refl <- testEquality q1 q2
        return Refl

tParamVars :: TParam sv t -> [AnyW SymbolType]
tParamVars (CoTParam t) = [MkAnyW t]
tParamVars (ContraTParam t) = [MkAnyW t]
tParamVars (RangeTParam p q) = [MkAnyW p, MkAnyW q]

withTParam :: SyntaxDatatypeParameter -> (forall sv t. TParam sv t -> r) -> r
withTParam (PositiveSyntaxDatatypeParameter n) cont = nameToSymbolType n $ \v -> cont $ CoTParam v
withTParam (NegativeSyntaxDatatypeParameter n) cont = nameToSymbolType n $ \v -> cont $ ContraTParam v
withTParam (RangeSyntaxDatatypeParameter np nq) cont =
    nameToSymbolType np $ \vp -> nameToSymbolType nq $ \vq -> cont $ RangeTParam vp vq

type TParams :: forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type -> Type
type TParams = CCRArguments TParam

tParamToPolarArgument ::
       forall sv (t :: CCRVarianceKind sv) polarity. Is PolarityType polarity
    => TParam sv t
    -> CCRPolarArgumentShimWit (PinaforePolyShim Type) PinaforeType polarity sv t
tParamToPolarArgument (CoTParam var) =
    case singleDolanShimWit $ mkShimWit $ VarDolanSingularType var of
        MkShimWit arg conv -> MkShimWit (CoCCRPolarArgument arg) conv
tParamToPolarArgument (ContraTParam var) =
    invertPolarity @polarity $
    case singleDolanShimWit $ mkShimWit $ VarDolanSingularType var of
        MkShimWit arg conv -> MkShimWit (ContraCCRPolarArgument arg) $ MkCatDual $ uninvertPolarMap conv
tParamToPolarArgument (RangeTParam varp varq) =
    invertPolarity @polarity $
    case ( singleDolanShimWit $ mkShimWit $ VarDolanSingularType varp
         , singleDolanShimWit $ mkShimWit $ VarDolanSingularType varq) of
        (MkShimWit argp convp, MkShimWit argq convq) ->
            MkShimWit (RangeCCRPolarArgument argp argq) $ MkCatRange (uninvertPolarMap convp) convq

type GenTParams dv = forall (gt :: DolanVarianceKind dv). AnyW (TParams dv gt)

data AnyTParams where
    MkAnyTParams :: forall (dv :: DolanVariance). GenTParams dv -> AnyTParams

tParamsVars :: TParams dv gt t -> [AnyW SymbolType]
tParamsVars NilCCRArguments = []
tParamsVars (ConsCCRArguments tp tps) = tParamVars tp ++ tParamsVars tps

addTParam ::
       forall sv (t :: CCRVarianceKind sv) (dv :: DolanVariance).
       TParam sv t
    -> (forall (gt :: DolanVarianceKind dv). AnyW (TParams dv gt))
    -> (forall (gt :: CCRVarianceKind sv -> DolanVarianceKind dv). AnyW (TParams (sv ': dv) gt))
addTParam p atp = let
    atp' :: forall (gt :: CCRVarianceKind sv -> DolanVarianceKind dv). AnyW (TParams (sv ': dv) gt)
    atp' =
        case atp @(gt t) of
            MkAnyW pp -> MkAnyW $ ConsCCRArguments p pp
    in atp'

getAnyTParams :: [SyntaxDatatypeParameter] -> AnyTParams
getAnyTParams [] = MkAnyTParams $ MkAnyW NilCCRArguments
getAnyTParams (sp:spp) =
    withTParam sp $ \p ->
        case getAnyTParams spp of
            MkAnyTParams f -> MkAnyTParams $ addTParam p f

type Mapping :: Symbol -> Type -> Type
newtype Mapping n t =
    MkMapping (Kleisli Endo (UVarT n -> UVarT n) t)
    deriving (Semigroup, Monoid, IsoVariant, Summish, Productish)

varMapping :: forall (n :: Symbol). Mapping n (UVarT n)
varMapping = MkMapping $ Kleisli $ \ab -> Endo ab

runMapping :: Mapping n t -> (UVarT n -> UVarT n) -> t -> t
runMapping (MkMapping (Kleisli f)) ab = appEndo $ f ab

mapMapping :: ((p -> p) -> (q -> q)) -> Mapping n p -> Mapping n q
mapMapping ff (MkMapping (Kleisli f)) = MkMapping $ Kleisli $ \tt -> Endo $ ff $ appEndo $ f tt

joinMapping :: ((p -> p) -> (q -> q) -> (t -> t)) -> Mapping n p -> Mapping n q -> Mapping n t
joinMapping ff (MkMapping (Kleisli fp)) (MkMapping (Kleisli fq)) =
    MkMapping $ Kleisli $ \tt -> Endo $ ff (appEndo $ fp tt) (appEndo $ fq tt)

getArgumentVariation ::
       forall v n (t :: Type) (sv :: CCRVariance) dv (f :: CCRVarianceKind sv -> DolanVarianceKind dv) (a :: CCRVarianceKind sv).
       Name
    -> VarianceType v
    -> SymbolType n
    -> CCRVarianceType sv
    -> CCRVariation sv f
    -> NonpolarArgument PinaforeGroundType sv a
    -> NonpolarArguments PinaforeGroundType dv (f a) t
    -> PinaforeInterpreter (Mapping n t)
getArgumentVariation tname v var _ svm (CoNonpolarArgument t) args = do
    mapping <- getNonpolarVariation tname v var t
    return $ mapMapping (\aa -> ccrArgumentsEndo args (ccrvMap svm aa)) mapping
getArgumentVariation tname v var _ svm (ContraNonpolarArgument t) args = do
    mapping <- invertVarianceType v $ \v' -> getNonpolarVariation tname v' var t
    return $ mapMapping (\aa -> ccrArgumentsEndo args (ccrvMap svm $ MkCatDual aa)) mapping
getArgumentVariation tname v var _ svm (RangeNonpolarArgument tp tq) args = do
    mappingp <- invertVarianceType v $ \v' -> getNonpolarVariation tname v' var tp
    mappingq <- getNonpolarVariation tname v var tq
    return $ joinMapping (\pp qq -> ccrArgumentsEndo args (ccrvMap svm $ MkCatRange pp qq)) mappingp mappingq

getArgumentsVariation ::
       forall v n (t :: Type) dv gt.
       Name
    -> VarianceType v
    -> SymbolType n
    -> DolanVarianceType dv
    -> DolanVarianceMap dv gt
    -> NonpolarArguments PinaforeGroundType dv gt t
    -> PinaforeInterpreter (Mapping n t)
getArgumentsVariation _ _ _ NilListType NilDolanVarianceMap NilCCRArguments = return mempty
getArgumentsVariation tname v var (ConsListType svt dvt) (ConsDolanVarianceMap ccrv dvm) (ConsCCRArguments arg args) = do
    vmap1 <- getArgumentVariation tname v var svt ccrv arg args
    vmapr <- getArgumentsVariation tname v var dvt dvm args
    return $ vmap1 <> vmapr

getNonpolarVariation ::
       forall v n t.
       Name
    -> VarianceType v
    -> SymbolType n
    -> PinaforeNonpolarType t
    -> PinaforeInterpreter (Mapping n t)
getNonpolarVariation _ CoVarianceType var (VarNonpolarType var')
    | Just Refl <- testEquality var var' = return varMapping
getNonpolarVariation tname ContraVarianceType var (VarNonpolarType var')
    | Just Refl <- testEquality var var' =
        throw $ InterpretTypeDeclTypeVariableWrongPolarityError tname $ symbolTypeToName var
getNonpolarVariation _ _ _ (VarNonpolarType _) = return mempty
getNonpolarVariation tname v var (GroundedNonpolarType gt args) =
    getArgumentsVariation tname v var (pgtVarianceType gt) (pgtVarianceMap gt) args

getConstructorVariation ::
       forall v n tl.
       Name
    -> VarianceType v
    -> SymbolType n
    -> ListType PinaforeNonpolarType tl
    -> PinaforeInterpreter (Mapping n (HList tl))
getConstructorVariation _ _ _ NilListType = return pUnit
getConstructorVariation tname v var (ConsListType t1 tr) = do
    vmap1 <- getNonpolarVariation tname v var t1
    vmapr <- getConstructorVariation tname v var tr
    return $ vmap1 <***> vmapr

getDataTypeVariation ::
       forall v n t. Name -> VarianceType v -> SymbolType n -> PinaforeDataType t -> PinaforeInterpreter (Mapping n t)
getDataTypeVariation _ _ _ NilDataType = return pNone
getDataTypeVariation tname vt var (ConsDataType c dt) = do
    vmap1 <- getConstructorVariation tname vt var c
    vmapr <- getDataTypeVariation tname vt var dt
    return $ vmap1 <+++> vmapr

getCCRVariation ::
       Name
    -> TParam sv a
    -> PinaforeDataType t
    -> PinaforeInterpreter (CCRVarianceCategory KindFunction sv a a -> (t -> t))
getCCRVariation tname (CoTParam v) dt = do
    f <- getDataTypeVariation tname CoVarianceType v dt
    return $ runMapping f
getCCRVariation tname (ContraTParam v) dt = do
    f <- getDataTypeVariation tname ContraVarianceType v dt
    return $ \(MkCatDual tt) -> runMapping f tt
getCCRVariation tname (RangeTParam vp vq) dt = do
    fp <- getDataTypeVariation tname ContraVarianceType vp dt
    fq <- getDataTypeVariation tname CoVarianceType vq dt
    return $ \(MkCatRange pp qq) -> runMapping fp pp . runMapping fq qq

assignTParam ::
       forall (sv :: CCRVariance) (a :: CCRVarianceKind sv) (t :: CCRVarianceKind sv) r.
       TParam sv t
    -> (t ~ a => r)
    -> r
assignTParam (CoTParam v) call = assignUVarT @a v call
assignTParam (ContraTParam v) call = assignUVarT @a v call
assignTParam (RangeTParam vp vq) call =
    case unsafeTypeIsPair @_ @_ @a of
        Refl -> assignUVarT @(Contra a) vp $ assignUVarT @(Co a) vq call

paramsUnEndo ::
       forall (t :: Type) (dv :: DolanVariance) (f :: DolanVarianceKind dv).
       TParams dv f t
    -> (t -> t)
    -> KindMorphism (->) f f
paramsUnEndo NilCCRArguments tt = tt
paramsUnEndo (ConsCCRArguments p pp) tt = let
    ff :: forall x. KindFunction (f x) (f x)
    ff = assignTParam @_ @x p $ paramsUnEndo pp tt
    in MkNestedMorphism ff

paramsCCRVMap ::
       forall (sv :: CCRVariance) (x :: CCRVarianceKind sv) (t :: Type) (dv :: DolanVariance) (f :: CCRVarianceKind sv -> DolanVarianceKind dv) (a :: CCRVarianceKind sv) (b :: CCRVarianceKind sv).
       TParam sv x
    -> (CCRVarianceCategory (->) sv x x -> t -> t)
    -> CCRArguments TParam dv (f x) t
    -> CCRVarianceCategory (->) sv a b
    -> KindMorphism (->) (f a) (f b)
paramsCCRVMap p ff pp ab = assignTParam @sv @a p $ assignTParam @sv @b p $ paramsUnEndo pp $ ff ab

assignDolanArgVars :: forall sv dv gt t a. TParam sv t -> DolanVarianceMap dv (gt t) -> DolanVarianceMap dv (gt a)
assignDolanArgVars = assignTParam @sv @a

getDolanVarianceMap :: Name -> TParams dv gt t -> PinaforeDataType t -> PinaforeInterpreter (DolanVarianceMap dv gt)
getDolanVarianceMap _ NilCCRArguments _ = return NilDolanVarianceMap
getDolanVarianceMap tname (ConsCCRArguments p pp) dt = do
    ff <- getCCRVariation tname p dt
    args <- getDolanVarianceMap tname pp dt
    return $ ConsDolanVarianceMap (MkCCRVariation Nothing $ paramsCCRVMap p ff pp) $ assignDolanArgVars p args

lazyKindMorphism ::
       forall dv (a :: DolanVarianceKind dv) (b :: DolanVarianceKind dv).
       DolanVarianceType dv
    -> KindMorphism (->) a b
    -> KindMorphism (->) a b
lazyKindMorphism NilListType ab = ab
lazyKindMorphism (ConsListType _ dvt) ~(MkNestedMorphism ab) = MkNestedMorphism $ lazyKindMorphism dvt ab

lazyCCRVariation ::
       forall (sv :: CCRVariance) dv (f :: CCRVarianceKind sv -> DolanVarianceKind dv).
       CCRVarianceType sv
    -> DolanVarianceType dv
    -> CCRVariation sv f
    -> CCRVariation sv f
lazyCCRVariation _ dvt ~(MkCCRVariation _mr mp) = MkCCRVariation Nothing $ \ab -> lazyKindMorphism dvt $ mp ab

lazyDolanVarianceMap :: DolanVarianceType dv -> DolanVarianceMap dv t -> DolanVarianceMap dv t
lazyDolanVarianceMap NilListType _cdvm = NilDolanVarianceMap
lazyDolanVarianceMap (ConsListType svt dvt) cdvm =
    ConsDolanVarianceMap
        (lazyCCRVariation svt dvt $
         case cdvm of
             ConsDolanVarianceMap ccrv _ -> ccrv) $
    lazyDolanVarianceMap
        dvt
        (case cdvm of
             ConsDolanVarianceMap _ dv -> dv)

makeDataTypeBox ::
       Name
    -> Markdown
    -> [SyntaxDatatypeParameter]
    -> [SyntaxDatatypeConstructor]
    -> PinaforeInterpreter PinaforeTypeBox
makeDataTypeBox name doc params sconss =
    case getAnyTParams params of
        MkAnyTParams (gtparams :: GenTParams dv) ->
            newTypeID $ \(tidsym :: _ tid) ->
                case unsafeIdentifyKind @_ @(DolanVarianceKind dv) tidsym of
                    Identity Refl ->
                        case gtparams @(Identified tid) of
                            MkAnyW (tparams :: TParams dv (Identified tid) decltype) ->
                                withRepresentative (ccrArgumentsType tparams) $ let
                                    dvt :: DolanVarianceType dv
                                    dvt = ccrArgumentsType tparams
                                    mkgt ::
                                           DolanVarianceMap dv (Identified tid)
                                        -> PinaforeGroundType dv (Identified tid)
                                    mkgt dvm =
                                        MkPinaforeGroundType
                                            { pgtVarianceType = dvt
                                            , pgtVarianceMap = lazyDolanVarianceMap dvt dvm
                                            , pgtShowType = standardListTypeExprShow @dv $ exprShow name
                                            , pgtFamilyType = MkFamilyType datatypeIOWitness $ MkDataTypeFamily tidsym
                                            , pgtGreatestDynamicSupertype = \_ -> Nothing
                                            }
                                    mktype :: DolanVarianceMap dv (Identified tid) -> PinaforeBoundType
                                    mktype dvm = MkBoundType $ mkgt dvm
                                    in mkTypeFixBox name doc mktype $ do
                                           tconss <- for sconss interpretDataTypeConstructor
                                           MkDataBox (pdt :: _ structtype) conss <- return $ assembleDataType tconss
                                           let
                                               freevars :: [AnyW SymbolType]
                                               freevars = nub $ mconcat $ fmap constructorFreeVariables conss
                                               declaredvars :: [AnyW SymbolType]
                                               declaredvars = tParamsVars tparams
                                               unboundvars :: [AnyW SymbolType]
                                               unboundvars = freevars List.\\ declaredvars
                                           case nonEmpty $ duplicates declaredvars of
                                               Nothing -> return ()
                                               Just vv ->
                                                   throw $
                                                   InterpretTypeDeclDuplicateTypeVariablesError name $
                                                   fmap (\(MkAnyW s) -> symbolTypeToName s) vv
                                           case nonEmpty unboundvars of
                                               Nothing -> return ()
                                               Just vv ->
                                                   throw $
                                                   InterpretTypeDeclUnboundTypeVariablesError name $
                                                   fmap (\(MkAnyW s) -> symbolTypeToName s) vv
                                           Refl <- unsafeGetRefl @Type @structtype @decltype
                                           dvm :: DolanVarianceMap dv (Identified tid) <-
                                               getDolanVarianceMap @dv name tparams pdt
                                           let
                                               getargs ::
                                                      forall polarity. Is PolarityType polarity
                                                   => DolanArgumentsShimWit PinaforePolyShim dv PinaforeType (Identified tid) polarity decltype
                                               getargs =
                                                   mapCCRArguments
                                                       @PinaforePolyShim
                                                       @TParam
                                                       @(CCRPolarArgument PinaforeType polarity)
                                                       @dv
                                                       @polarity
                                                       @(Identified tid)
                                                       tParamToPolarArgument
                                                       dvm
                                                       tparams
                                           case (getargs @'Positive, getargs @'Negative) of
                                               (MkShimWit posargs posconv, MkShimWit negargs negconv) -> do
                                                   let
                                                       gt = mkgt dvm
                                                       ctfpos :: PinaforeShimWit 'Positive decltype
                                                       ctfpos =
                                                           mapShimWit posconv $
                                                           singleDolanShimWit $
                                                           mkPolarShimWit $ GroundedDolanSingularType gt posargs
                                                       ctfneg :: PinaforeShimWit 'Negative decltype
                                                       ctfneg =
                                                           mapShimWit negconv $
                                                           singleDolanShimWit $
                                                           mkPolarShimWit $ GroundedDolanSingularType gt negargs
                                                   patts <-
                                                       for conss $ \(MkConstructor cname lt at tma) -> do
                                                           ltp <- return $ mapListType nonpolarToDolanType lt
                                                           ltn <- return $ mapListType nonpolarToDolanType lt
                                                           let
                                                               expr =
                                                                   qConstExprAny $
                                                                   MkAnyValue (qFunctionPosWitnesses ltn ctfpos) at
                                                               pc = toPatternConstructor ctfneg ltp tma
                                                           withNewPatternConstructor cname doc expr pc
                                                   return (dvm, compAll patts)
