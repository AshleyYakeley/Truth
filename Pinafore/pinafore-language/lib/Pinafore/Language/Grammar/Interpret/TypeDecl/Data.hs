module Pinafore.Language.Grammar.Interpret.TypeDecl.Data
    ( Stages(..)
    , GroundTypeFromTypeID(..)
    , GroundTypeMaker
    , makeDeclTypeBox
    , makeDataTypeBox
    ) where

import qualified Data.List as List
import Pinafore.Language.Error
import Pinafore.Language.ExprShow
import Pinafore.Language.Expression
import Pinafore.Language.Grammar.Interpret.Type
import Pinafore.Language.Grammar.Interpret.TypeDecl.Constructor
import Pinafore.Language.Grammar.Interpret.TypeDecl.Mapping
import Pinafore.Language.Grammar.Interpret.TypeDecl.Parameter
import Pinafore.Language.Grammar.Interpret.TypeDecl.Representation
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Shim
import Pinafore.Language.Type
import Pinafore.Markdown
import Shapes
import Shapes.Unsafe (unsafeGetRefl)

assembleDataType ::
       [(n, Some (ListType PinaforeNonpolarType))]
    -> forall r. (forall t. [Constructor n (ListProductType PinaforeNonpolarType) t] -> VarMapping t -> r) -> r
assembleDataType tconss call =
    case assembleListType $ fmap (\(_, (MkSome lt)) -> MkSome $ MkListProductType lt) tconss of
        MkSome lcons ->
            getPreferredTypeRepresentation lcons $ \(MkTypeRepresentation ccons vmap) _ -> let
                mkConss :: [n -> Constructor n (ListProductType PinaforeNonpolarType) _]
                mkConss =
                    listTypeToList getConst $
                    joinListType (\codec el -> Const $ \name -> MkConstructor name el codec) ccons lcons
                in call (fmap (\(f, (n, _)) -> f n) $ zip mkConss tconss) vmap

data DataTypeFamily :: FamilyKind where
    MkDataTypeFamily :: forall (tid :: Nat). TypeIDType tid -> DataTypeFamily (Identified tid)

instance TestHetEquality DataTypeFamily where
    testHetEquality (MkDataTypeFamily ia) (MkDataTypeFamily ib) = do
        Refl <- testEquality ia ib
        return HRefl

datatypeIOWitness :: IOWitness ('MkWitKind DataTypeFamily)
datatypeIOWitness = $(iowitness [t|'MkWitKind DataTypeFamily|])

interpretDataTypeConstructor ::
       Maybe Name
    -> SyntaxConstructorOrSubtype extra
    -> PinaforeInterpreter ([((Name, extra, Maybe Name), Some (ListType PinaforeNonpolarType))], [(Name, Maybe Name)])
interpretDataTypeConstructor mtype (ConstructorSyntaxConstructorOrSubtype consName stypes extra) = do
    etypes <- for stypes interpretNonpolarType
    return $ (pure ((consName, extra, mtype), assembleListType etypes), mempty)
interpretDataTypeConstructor _ (SubtypeSyntaxConstructorOrSubtype subtypeName stypes) =
    interpretDataTypeConstructors (Just subtypeName) stypes

interpretDataTypeConstructors ::
       Maybe Name
    -> [SyntaxConstructorOrSubtype extra]
    -> PinaforeInterpreter ([((Name, extra, Maybe Name), Some (ListType PinaforeNonpolarType))], [(Name, Maybe Name)])
interpretDataTypeConstructors mtype sconss = fmap mconcat $ for sconss $ interpretDataTypeConstructor mtype

withCCRTypeParam :: SyntaxTypeParameter -> (forall sv t. CCRTypeParam sv t -> r) -> r
withCCRTypeParam (PositiveSyntaxTypeParameter n) cont = nameToSymbolType n $ \v -> cont $ CoCCRTypeParam v
withCCRTypeParam (NegativeSyntaxTypeParameter n) cont = nameToSymbolType n $ \v -> cont $ ContraCCRTypeParam v
withCCRTypeParam (RangeSyntaxTypeParameter np nq) cont =
    nameToSymbolType np $ \vp -> nameToSymbolType nq $ \vq -> cont $ RangeCCRTypeParam vp vq

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

type GenCCRTypeParams dv = forall (gt :: DolanVarianceKind dv). Some (CCRTypeParams dv gt)

data AnyCCRTypeParams where
    MkAnyCCRTypeParams :: forall (dv :: DolanVariance). GenCCRTypeParams dv -> AnyCCRTypeParams

tParamsVars :: CCRTypeParams dv gt t -> [Some SymbolType]
tParamsVars NilCCRArguments = []
tParamsVars (ConsCCRArguments tp tps) = tParamVars tp ++ tParamsVars tps

getAnyCCRTypeParams :: [SyntaxTypeParameter] -> AnyCCRTypeParams
getAnyCCRTypeParams [] = MkAnyCCRTypeParams nilAnyCCRArguments
getAnyCCRTypeParams (sp:spp) =
    withCCRTypeParam sp $ \p ->
        case getAnyCCRTypeParams spp of
            MkAnyCCRTypeParams f -> MkAnyCCRTypeParams $ consAnyCCRArguments p f

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

getDataTypeMappingOrError ::
       forall v n t. Name -> VarianceType v -> SymbolType n -> VarMapping t -> PinaforeInterpreter (Mapping n t)
getDataTypeMappingOrError tname vt var vm =
    case runVarMapping vm vt var of
        Nothing -> throw $ InterpretTypeDeclTypeVariableWrongPolarityError tname $ symbolTypeToName var
        Just vmap -> return vmap

getCCRVariation ::
       Name
    -> CCRTypeParam sv a
    -> VarMapping t
    -> PinaforeInterpreter (CCRVarianceCategory KindFunction sv a a -> (t -> t))
getCCRVariation tname (CoCCRTypeParam v) dt = do
    f <- getDataTypeMappingOrError tname CoVarianceType v dt
    return $ runMapping f
getCCRVariation tname (ContraCCRTypeParam v) dt = do
    f <- getDataTypeMappingOrError tname ContraVarianceType v dt
    return $ \(MkCatDual tt) -> runMapping f tt
getCCRVariation tname (RangeCCRTypeParam vp vq) dt = do
    fp <- getDataTypeMappingOrError tname ContraVarianceType vp dt
    fq <- getDataTypeMappingOrError tname CoVarianceType vq dt
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
assignDolanArgVars p dvm = assignCCRTypeParam @sv @a p dvm

getDolanVarianceMap :: Name -> CCRTypeParams dv gt t -> VarMapping t -> PinaforeInterpreter (DolanVarianceMap dv gt)
getDolanVarianceMap _ NilCCRArguments _ = return NilDolanVarianceMap
getDolanVarianceMap tname (ConsCCRArguments p pp) dt = do
    ff <- getCCRVariation tname p dt
    args <- getDolanVarianceMap tname pp dt
    return $ ConsDolanVarianceMap (MkCCRVariation Nothing $ paramsCCRVMap p ff pp) $ assignDolanArgVars p args

{-
data ConsOrBox t =

data SyntaxDatatypeConstructorOrSubtype
    = ConstructorSyntaxDatatypeConstructorOrSubtype Name
                                                    [SyntaxType]
    | SubtypeSyntaxDatatypeConstructorOrSubtype Name
                                                [SyntaxDatatypeConstructorOrSubtype]
    deriving (Eq)
-}
data Stages a b =
    forall x. MkStages (a -> PinaforeInterpreter x)
                       (x -> PinaforeInterpreter b)

type GroundTypeFromTypeID :: forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type
newtype GroundTypeFromTypeID dv gt = MkGroundTypeFromTypeID
    { unGroundTypeFromTypeID :: forall (tid :: Nat).
                                    (IdentifiedKind tid ~ DolanVarianceKind dv, gt ~~ Identified tid) =>
                                            Name -> TypeIDType tid -> PinaforeGroundType dv gt
    }

type GroundTypeMaker extra
     = forall (dv :: DolanVariance) (gt :: DolanVarianceKind dv) (decltype :: Type).
           Is DolanVarianceType dv =>
                   Name -> CCRTypeParams dv gt decltype -> Stages ( DolanVarianceMap dv gt
                                                                  , [Constructor (Name, extra, Maybe Name) (ListProductType PinaforeNonpolarType) decltype]) (GroundTypeFromTypeID dv gt)

getConsSubtypeNames :: SyntaxConstructorOrSubtype extra -> [Name]
getConsSubtypeNames (ConstructorSyntaxConstructorOrSubtype _ _ _) = []
getConsSubtypeNames (SubtypeSyntaxConstructorOrSubtype tname conss) = tname : getConssSubtypeNames conss

getConssSubtypeNames :: [SyntaxConstructorOrSubtype extra] -> [Name]
getConssSubtypeNames conss = mconcat $ fmap getConsSubtypeNames conss

makeBox ::
       forall extra dv.
       GroundTypeMaker extra
    -> Name
    -> Markdown
    -> [SyntaxConstructorOrSubtype extra]
    -> GenCCRTypeParams dv
    -> PinaforeInterpreter (PinaforeFixBox () ())
makeBox gmaker mainTypeName mainTypeDoc sconss gtparams = do
    let subtypenames = getConssSubtypeNames sconss
    newTypeID $ \(mainTypeID :: _ maintid) ->
        case unsafeIdentifyKind @_ @(DolanVarianceKind dv) mainTypeID of
            Identity Refl ->
                case gtparams @(Identified maintid) of
                    MkSome (tparams :: CCRTypeParams dv (Identified maintid) decltype) ->
                        withRepresentative (ccrArgumentsType tparams) $
                        case gmaker mainTypeName tparams of
                            MkStages mkx (mkgt :: x -> _) -> let
                                mainRegister :: x -> PinaforeScopeInterpreter ()
                                mainRegister x = do
                                    MkGroundTypeFromTypeID gttid <- lift $ mkgt x
                                    registerType mainTypeName mainTypeDoc $ gttid mainTypeName mainTypeID
                                mainConstruct ::
                                       ()
                                    -> PinaforeScopeInterpreter ( x
                                                                , ( x
                                                                  , PinaforeGroundType dv (Identified maintid)
                                                                  , Some VarMapping))
                                mainConstruct () = do
                                    (tconss, _moretypes) <- lift $ interpretDataTypeConstructors Nothing sconss
                                    assembleDataType tconss $ \conss (vmap :: _ structtype) -> do
                                        let
                                            freevars :: [Some SymbolType]
                                            freevars = nub $ mconcat $ fmap constructorFreeVariables conss
                                            declaredvars :: [Some SymbolType]
                                            declaredvars = tParamsVars tparams
                                            unboundvars :: [Some SymbolType]
                                            unboundvars = freevars List.\\ declaredvars
                                        case nonEmpty $ duplicates declaredvars of
                                            Nothing -> return ()
                                            Just vv ->
                                                lift $
                                                throw $
                                                InterpretTypeDeclDuplicateTypeVariablesError mainTypeName $
                                                fmap (\(MkSome s) -> symbolTypeToName s) vv
                                        case nonEmpty unboundvars of
                                            Nothing -> return ()
                                            Just vv ->
                                                lift $
                                                throw $
                                                InterpretTypeDeclUnboundTypeVariablesError mainTypeName $
                                                fmap (\(MkSome s) -> symbolTypeToName s) vv
                                        Refl <- unsafeGetRefl @Type @structtype @decltype
                                        dvm :: DolanVarianceMap dv (Identified maintid) <-
                                            lift $ getDolanVarianceMap @dv mainTypeName tparams vmap
                                        x <- lift $ mkx (dvm, conss)
                                        gtft <- lift $ mkgt x
                                        let
                                            mainGroundType :: PinaforeGroundType dv (Identified maintid)
                                            mainGroundType = unGroundTypeFromTypeID gtft mainTypeName mainTypeID
                                        let
                                            registerCons ::
                                                   PinaforeGroundType dv (Identified maintid)
                                                -> Constructor (Name, extra, Maybe Name) (ListProductType PinaforeNonpolarType) decltype
                                                -> PinaforeScopeInterpreter ()
                                            registerCons groundType (MkConstructor (cname, _, _) (MkListProductType lt) codec) = let
                                                getargs ::
                                                       forall polarity. Is PolarityType polarity
                                                    => DolanArgumentsShimWit PinaforePolyShim dv PinaforeType (Identified maintid) polarity decltype
                                                getargs =
                                                    mapCCRArguments
                                                        @PinaforePolyShim
                                                        @CCRTypeParam
                                                        @(CCRPolarArgument PinaforeType polarity)
                                                        @dv
                                                        @polarity
                                                        @(Identified maintid)
                                                        tParamToPolarArgument
                                                        dvm
                                                        tparams
                                                in case (getargs @'Positive, getargs @'Negative) of
                                                       (MkShimWit posargs posconv, MkShimWit negargs negconv) -> do
                                                           let
                                                               ctfpos :: PinaforeShimWit 'Positive decltype
                                                               ctfpos =
                                                                   mapShimWit posconv $
                                                                   singleDolanShimWit $
                                                                   mkPolarShimWit $
                                                                   GroundedDolanSingularType groundType posargs
                                                               ctfneg :: PinaforeShimWit 'Negative decltype
                                                               ctfneg =
                                                                   mapShimWit negconv $
                                                                   singleDolanShimWit $
                                                                   mkPolarShimWit $
                                                                   GroundedDolanSingularType groundType negargs
                                                           ltp <- return $ mapListType nonpolarToDolanType lt
                                                           ltn <- return $ mapListType nonpolarToDolanType lt
                                                           let
                                                               expr =
                                                                   qConstExprAny $
                                                                   MkSomeOf (qFunctionPosWitnesses ltn ctfpos) $
                                                                   encode codec
                                                               pc = toPatternConstructor ctfneg ltp $ decode codec
                                                           registerPatternConstructor cname mainTypeDoc expr pc
                                        for_ conss $ registerCons mainGroundType
                                        return (x, (x, mainGroundType, MkSome vmap))
                                mainBox ::
                                       PinaforeFixBox () ( x
                                                         , PinaforeGroundType dv (Identified maintid)
                                                         , Some VarMapping)
                                mainBox = mkFixBox mainRegister mainConstruct
                                subRegister ::
                                       Name
                                    -> Markdown
                                    -> (PinaforeGroundType dv (Identified maintid), x)
                                    -> PinaforeScopeInterpreter ()
                                subRegister subTypeName subTypeDoc ~(mainGroundType, x) = do
                                    MkGroundTypeFromTypeID gttid <- lift $ mkgt x
                                    stid <- lift $ newTypeID $ return . MkSome
                                    case stid of
                                        MkSome (subTypeID :: _ subtid) -> do
                                            Refl <- unsafeIdentifyKind @_ @(DolanVarianceKind dv) subTypeID
                                            Refl <- unsafeIdentify @subtid @(Identified maintid) subTypeID
                                            let subGroundType = gttid subTypeName subTypeID
                                            registerType subTypeName subTypeDoc subGroundType
                                            registerSubtypeConversion $
                                                simpleSubtypeConversionEntry
                                                    subGroundType
                                                    mainGroundType
                                                    idSubtypeConversion
                                subBox :: Name -> PinaforeFixBox (PinaforeGroundType dv (Identified maintid), x) ()
                                subBox subTypeName = mkRegisterFixBox (subRegister subTypeName mainTypeDoc)
                                in return $
                                   proc () -> do
                                       (x, mainGroundType, _) <- mainBox -< ()
                                       mconcat (fmap subBox subtypenames) -< (mainGroundType, x)

makeDeclTypeBox ::
       forall extra.
       GroundTypeMaker extra
    -> Name
    -> Markdown
    -> [SyntaxTypeParameter]
    -> [SyntaxConstructorOrSubtype extra]
    -> PinaforeInterpreter (PinaforeFixBox () ())
makeDeclTypeBox gmaker name doc params sconss =
    case getAnyCCRTypeParams params of
        MkAnyCCRTypeParams gtparams -> makeBox gmaker name doc sconss gtparams

makeDataGroundType ::
       forall (dv :: DolanVariance) (gt :: DolanVarianceKind dv) (decltype :: Type). Is DolanVarianceType dv
    => Name
    -> CCRTypeParams dv gt decltype
    -> Stages ( DolanVarianceMap dv gt
              , [Constructor (Name, (), Maybe Name) (ListProductType PinaforeNonpolarType) decltype]) (GroundTypeFromTypeID dv gt)
makeDataGroundType _ tparams = let
    dvt :: DolanVarianceType dv
    dvt = ccrArgumentsType tparams
    mkx :: ( DolanVarianceMap dv gt
           , [Constructor (Name, (), Maybe Name) (ListProductType PinaforeNonpolarType) decltype])
        -> PinaforeInterpreter (DolanVarianceMap dv gt)
    mkx (dvm, _) = return dvm
    mkgt :: DolanVarianceMap dv gt -> PinaforeInterpreter (GroundTypeFromTypeID dv gt)
    mkgt dvm =
        return $
        MkGroundTypeFromTypeID $ \name mainTypeID ->
            MkPinaforeGroundType
                { pgtVarianceType = dvt
                , pgtVarianceMap = lazyDolanVarianceMap dvt dvm
                , pgtShowType = standardListTypeExprShow @dv $ exprShow name
                , pgtFamilyType = MkFamilialType datatypeIOWitness $ MkDataTypeFamily mainTypeID
                , pgtGreatestDynamicSupertype = nullPolyGreatestDynamicSupertype
                }
    in MkStages mkx mkgt

makeDataTypeBox ::
       Name
    -> Markdown
    -> [SyntaxTypeParameter]
    -> [SyntaxDatatypeConstructorOrSubtype]
    -> PinaforeInterpreter (PinaforeFixBox () ())
makeDataTypeBox = makeDeclTypeBox makeDataGroundType
