module Pinafore.Language.Grammar.Interpret.TypeDecl.Data
    ( makeDataTypeBox
    ) where

import qualified Data.List as List
import Pinafore.Language.Error
import Pinafore.Language.ExprShow
import Pinafore.Language.Expression
import Pinafore.Language.Grammar.Interpret.Type
import Pinafore.Language.Grammar.Interpret.TypeDecl.Mapping
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
       SyntaxDatatypeConstructorOrSubtype -> PinaforeInterpreter (Name, AnyW (ListType PinaforeNonpolarType))
interpretDataTypeConstructor (ConstructorSyntaxDatatypeConstructorOrSubtype consName stypes) = do
    etypes <- for stypes interpretNonpolarType
    return (consName, assembleListType etypes)
interpretDataTypeConstructor (SubtypeSyntaxDatatypeConstructorOrSubtype _subtypeName _stypes) =
    throw $ KnownIssueError 132 "Subtypes not supported in datatype definitions"

type CCRTypeParam :: CCRArgumentKind
data CCRTypeParam (sv :: CCRVariance) (t :: CCRVarianceKind sv) where
    CoCCRTypeParam :: SymbolType n -> CCRTypeParam CoCCRVariance (UVarT n)
    ContraCCRTypeParam :: SymbolType n -> CCRTypeParam ContraCCRVariance (UVarT n)
    RangeCCRTypeParam :: SymbolType np -> SymbolType nq -> CCRTypeParam 'RangeCCRVariance '( UVarT np, UVarT nq) -- contra, co

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

tParamVars :: CCRTypeParam sv t -> [AnyW SymbolType]
tParamVars (CoCCRTypeParam t) = [MkAnyW t]
tParamVars (ContraCCRTypeParam t) = [MkAnyW t]
tParamVars (RangeCCRTypeParam p q) = [MkAnyW p, MkAnyW q]

withCCRTypeParam :: SyntaxDatatypeParameter -> (forall sv t. CCRTypeParam sv t -> r) -> r
withCCRTypeParam (PositiveSyntaxDatatypeParameter n) cont = nameToSymbolType n $ \v -> cont $ CoCCRTypeParam v
withCCRTypeParam (NegativeSyntaxDatatypeParameter n) cont = nameToSymbolType n $ \v -> cont $ ContraCCRTypeParam v
withCCRTypeParam (RangeSyntaxDatatypeParameter np nq) cont =
    nameToSymbolType np $ \vp -> nameToSymbolType nq $ \vq -> cont $ RangeCCRTypeParam vp vq

assignCCRTypeParam ::
       forall (sv :: CCRVariance) (a :: CCRVarianceKind sv) (t :: CCRVarianceKind sv) r.
       CCRTypeParam sv t
    -> (t ~ a => r)
    -> r
assignCCRTypeParam (CoCCRTypeParam v) call = assignUVarT @a v call
assignCCRTypeParam (ContraCCRTypeParam v) call = assignUVarT @a v call
assignCCRTypeParam (RangeCCRTypeParam vp vq) call =
    case unsafeTypeIsPair @_ @_ @a of
        Refl -> assignUVarT @(Contra a) vp $ assignUVarT @(Co a) vq call

type CCRTypeParams :: forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type -> Type
type CCRTypeParams = CCRArguments CCRTypeParam

tParamToPolarArgument ::
       forall sv (t :: CCRVarianceKind sv) polarity. Is PolarityType polarity
    => CCRTypeParam sv t
    -> CCRPolarArgumentShimWit (PinaforePolyShim Type) PinaforeType polarity sv t
tParamToPolarArgument (CoCCRTypeParam var) =
    case singleDolanShimWit $ mkShimWit $ VarDolanSingularType var of
        MkShimWit arg conv -> MkShimWit (CoCCRPolarArgument arg) conv
tParamToPolarArgument (ContraCCRTypeParam var) =
    invertPolarity @polarity $
    case singleDolanShimWit $ mkShimWit $ VarDolanSingularType var of
        MkShimWit arg conv -> MkShimWit (ContraCCRPolarArgument arg) $ MkCatDual $ uninvertPolarMap conv
tParamToPolarArgument (RangeCCRTypeParam varp varq) =
    invertPolarity @polarity $
    case ( singleDolanShimWit $ mkShimWit $ VarDolanSingularType varp
         , singleDolanShimWit $ mkShimWit $ VarDolanSingularType varq) of
        (MkShimWit argp convp, MkShimWit argq convq) ->
            MkShimWit (RangeCCRPolarArgument argp argq) $ MkCatRange (uninvertPolarMap convp) convq

type GenCCRTypeParams dv = forall (gt :: DolanVarianceKind dv). AnyW (CCRTypeParams dv gt)

data AnyCCRTypeParams where
    MkAnyCCRTypeParams :: forall (dv :: DolanVariance). GenCCRTypeParams dv -> AnyCCRTypeParams

tParamsVars :: CCRTypeParams dv gt t -> [AnyW SymbolType]
tParamsVars NilCCRArguments = []
tParamsVars (ConsCCRArguments tp tps) = tParamVars tp ++ tParamsVars tps

addCCRTypeParam ::
       forall sv (t :: CCRVarianceKind sv) (dv :: DolanVariance).
       CCRTypeParam sv t
    -> (forall (gt :: DolanVarianceKind dv). AnyW (CCRTypeParams dv gt))
    -> (forall (gt :: CCRVarianceKind sv -> DolanVarianceKind dv). AnyW (CCRTypeParams (sv ': dv) gt))
addCCRTypeParam p atp = let
    atp' :: forall (gt :: CCRVarianceKind sv -> DolanVarianceKind dv). AnyW (CCRTypeParams (sv ': dv) gt)
    atp' =
        case atp @(gt t) of
            MkAnyW pp -> MkAnyW $ ConsCCRArguments p pp
    in atp'

getAnyCCRTypeParams :: [SyntaxDatatypeParameter] -> AnyCCRTypeParams
getAnyCCRTypeParams [] = MkAnyCCRTypeParams $ MkAnyW NilCCRArguments
getAnyCCRTypeParams (sp:spp) =
    withCCRTypeParam sp $ \p ->
        case getAnyCCRTypeParams spp of
            MkAnyCCRTypeParams f -> MkAnyCCRTypeParams $ addCCRTypeParam p f

paramsUnEndo ::
       forall (t :: Type) (dv :: DolanVariance) (f :: DolanVarianceKind dv).
       CCRTypeParams dv f t
    -> (t -> t)
    -> KindMorphism (->) f f
paramsUnEndo NilCCRArguments tt = tt
paramsUnEndo (ConsCCRArguments p pp) tt = let
    ff :: forall x. KindFunction (f x) (f x)
    ff = assignCCRTypeParam @_ @x p $ paramsUnEndo pp tt
    in MkNestedMorphism ff

getDataTypeMapping ::
       forall v n t. Name -> VarianceType v -> SymbolType n -> PinaforeDataType t -> PinaforeInterpreter (Mapping n t)
getDataTypeMapping _ _ _ NilDataType = return pNone
getDataTypeMapping tname vt var (ConsDataType c dt) = do
    vmap1 <- getConstructorMapping tname vt var c
    vmapr <- getDataTypeMapping tname vt var dt
    return $ vmap1 <+++> vmapr

getCCRVariation ::
       Name
    -> CCRTypeParam sv a
    -> PinaforeDataType t
    -> PinaforeInterpreter (CCRVarianceCategory KindFunction sv a a -> (t -> t))
getCCRVariation tname (CoCCRTypeParam v) dt = do
    f <- getDataTypeMapping tname CoVarianceType v dt
    return $ runMapping f
getCCRVariation tname (ContraCCRTypeParam v) dt = do
    f <- getDataTypeMapping tname ContraVarianceType v dt
    return $ \(MkCatDual tt) -> runMapping f tt
getCCRVariation tname (RangeCCRTypeParam vp vq) dt = do
    fp <- getDataTypeMapping tname ContraVarianceType vp dt
    fq <- getDataTypeMapping tname CoVarianceType vq dt
    return $ \(MkCatRange pp qq) -> runMapping fp pp . runMapping fq qq

paramsCCRVMap ::
       forall (sv :: CCRVariance) (x :: CCRVarianceKind sv) (t :: Type) (dv :: DolanVariance) (f :: CCRVarianceKind sv -> DolanVarianceKind dv) (a :: CCRVarianceKind sv) (b :: CCRVarianceKind sv).
       CCRTypeParam sv x
    -> (CCRVarianceCategory (->) sv x x -> t -> t)
    -> CCRArguments CCRTypeParam dv (f x) t
    -> CCRVarianceCategory (->) sv a b
    -> KindMorphism (->) (f a) (f b)
paramsCCRVMap p ff pp ab = assignCCRTypeParam @sv @a p $ assignCCRTypeParam @sv @b p $ paramsUnEndo pp $ ff ab

assignDolanArgVars :: forall sv dv gt t a. CCRTypeParam sv t -> DolanVarianceMap dv (gt t) -> DolanVarianceMap dv (gt a)
assignDolanArgVars = assignCCRTypeParam @sv @a

getDolanVarianceMap ::
       Name -> CCRTypeParams dv gt t -> PinaforeDataType t -> PinaforeInterpreter (DolanVarianceMap dv gt)
getDolanVarianceMap _ NilCCRArguments _ = return NilDolanVarianceMap
getDolanVarianceMap tname (ConsCCRArguments p pp) dt = do
    ff <- getCCRVariation tname p dt
    args <- getDolanVarianceMap tname pp dt
    return $ ConsDolanVarianceMap (MkCCRVariation Nothing $ paramsCCRVMap p ff pp) $ assignDolanArgVars p args

makeDataTypeBox ::
       Name
    -> Markdown
    -> [SyntaxDatatypeParameter]
    -> [SyntaxDatatypeConstructorOrSubtype]
    -> PinaforeInterpreter PinaforeTypeBox
makeDataTypeBox name doc params sconss =
    case getAnyCCRTypeParams params of
        MkAnyCCRTypeParams (gtparams :: GenCCRTypeParams dv) ->
            newTypeID $ \(tidsym :: _ tid) ->
                case unsafeIdentifyKind @_ @(DolanVarianceKind dv) tidsym of
                    Identity Refl ->
                        case gtparams @(Identified tid) of
                            MkAnyW (tparams :: CCRTypeParams dv (Identified tid) decltype) ->
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
                                                       @CCRTypeParam
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
                                                   return (dvm, mconcat patts)
