module Pinafore.Language.Grammar.Interpret.TypeDecl.Data
    ( ConstructorCodec
    , TypeConstruction(..)
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
import Shapes.Unsafe (unsafeGetRefl, unsafeRefl)

type ConstructorCodec t = SomeFor (Codec t) (ListProductType PinaforeNonpolarType)

constructorFreeVariables :: ConstructorCodec t -> [Some SymbolType]
constructorFreeVariables (MkSomeFor (MkListProductType lt) _) = mconcat $ listTypeToList nonpolarTypeFreeVariables lt

assembleDataType ::
       forall n.
       FixedList n (Some (ListType PinaforeNonpolarType))
    -> forall r.
               (forall t. FixedList n (ConstructorCodec t) -> VarMapping t -> (t -> (forall a. FixedList n a -> a)) -> r) -> r
assembleDataType tconss call = let
    nconss :: FixedList n (Some (ListProductType PinaforeNonpolarType))
    nconss = fmap (\(MkSome lt) -> MkSome $ MkListProductType lt) tconss
    in listTypeFromFixedList nconss $ \lcons ->
           getPreferredTypeRepresentation lcons $ \(MkTypeRepresentation ccons vmap) pickcons -> let
               conss :: FixedList n (SomeFor (Codec _) (ListProductType PinaforeNonpolarType))
               conss = listTypeToFixedList getConst $ joinListType (\codec el -> Const $ MkSomeFor el codec) ccons lcons
               in call conss vmap $ \t -> fixedListElement $ listElementTypeIndex $ someForToSome $ pickcons t

data DataTypeFamily :: FamilyKind where
    MkDataTypeFamily :: forall (tid :: Nat). TypeIDType tid -> DataTypeFamily (Identified tid)

instance TestHetEquality DataTypeFamily where
    testHetEquality (MkDataTypeFamily ia) (MkDataTypeFamily ib) = do
        Refl <- testEquality ia ib
        return HRefl

datatypeIOWitness :: IOWitness ('MkWitKind DataTypeFamily)
datatypeIOWitness = $(iowitness [t|'MkWitKind DataTypeFamily|])

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
    case shimWitToDolan $ mkShimWit $ VarDolanSingularType var of
        MkShimWit arg conv -> MkShimWit (CoCCRPolarArgument arg) conv
tParamToPolarArgument (ContraCCRTypeParam var) =
    invertPolarity @polarity $
    case shimWitToDolan $ mkShimWit $ VarDolanSingularType var of
        MkShimWit arg conv -> MkShimWit (ContraCCRPolarArgument arg) $ MkCatDual $ uninvertPolarMap conv
tParamToPolarArgument (RangeCCRTypeParam varp varq) =
    invertPolarity @polarity $
    case ( shimWitToDolan $ mkShimWit $ VarDolanSingularType varp
         , shimWitToDolan $ mkShimWit $ VarDolanSingularType varq) of
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
getDolanVarianceMap tname (ConsCCRArguments p pp) vm = do
    ff <- getCCRVariation tname p vm
    args <- getDolanVarianceMap tname pp vm
    return $ ConsDolanVarianceMap (MkCCRVariation Nothing $ paramsCCRVMap p ff pp) $ assignDolanArgVars p args

data TypeConstruction dv gt extra =
    forall x y. MkTypeConstruction (DolanVarianceMap dv gt -> extra -> PinaforeInterpreter x)
                                   (x -> PinaforeInterpreter (GroundTypeFromTypeID dv gt y))
                                   (PinaforeGroundType dv gt -> y -> PinaforeScopeInterpreter ())

type GroundTypeFromTypeID :: forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type -> Type
newtype GroundTypeFromTypeID dv gt y = MkGroundTypeFromTypeID
    { unGroundTypeFromTypeID :: forall (tid :: Nat).
                                    (IdentifiedKind tid ~ DolanVarianceKind dv, gt ~~ Identified tid) =>
                                            Name -> TypeIDType tid -> (PinaforeGroundType dv gt, y)
    }

type GroundTypeMaker extra
     = forall (dv :: DolanVariance) (gt :: DolanVarianceKind dv) (decltype :: Type).
           Is DolanVarianceType dv =>
                   Name -> CCRTypeParams dv gt decltype -> TypeConstruction dv gt [(ConstructorCodec decltype, extra)]

type MatchingTypeID :: forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type
data MatchingTypeID dv t where
    MkMatchingTypeID
        :: forall tid (dv :: DolanVariance) (t :: DolanVarianceKind dv).
           (IdentifiedKind tid ~ DolanVarianceKind dv, Identified tid ~~ t)
        => TypeIDType tid
        -> MatchingTypeID dv t

instance forall (dv :: DolanVariance) (t :: DolanVarianceKind dv). Eq (MatchingTypeID dv t) where
    MkMatchingTypeID ta == MkMatchingTypeID tb = isJust $ testEquality ta tb

newMatchingTypeID :: forall (dv :: DolanVariance). PinaforeInterpreter (Some (MatchingTypeID dv))
newMatchingTypeID =
    withNewTypeID $ \(typeID :: _ tid) ->
        case unsafeIdentifyKind @_ @(DolanVarianceKind dv) typeID of
            Identity Refl -> return $ MkSome $ MkMatchingTypeID typeID

data TypeData dv t = MkTypeData
    { tdID :: MatchingTypeID dv t
    , tdSupertype :: Maybe (TypeData dv t)
    , tdSubtypes :: [MatchingTypeID dv t] -- includes self
    , tdName :: Name
    , tdDoc :: Markdown
    }

getConsSubtypeData ::
       forall (dv :: DolanVariance) (t :: DolanVarianceKind dv) extra.
       TypeData dv t
    -> SyntaxWithDoc (SyntaxConstructorOrSubtype extra)
    -> PinaforeInterpreter [TypeData dv t]
getConsSubtypeData _ (MkSyntaxWithDoc _ (ConstructorSyntaxConstructorOrSubtype _ _ _)) = return mempty
getConsSubtypeData superTD (MkSyntaxWithDoc doc (SubtypeSyntaxConstructorOrSubtype tname conss)) = do
    ssubtid <- newMatchingTypeID @dv
    case (tdID superTD, ssubtid) of
        (superMTID :: _ supertype, MkSome subMTID@(MkMatchingTypeID subTypeID)) -> do
            Refl <- unsafeIdentify @_ @supertype subTypeID
            rec
                let
                    subtypes = superMTID : mconcat (fmap tdSubtypes subtdata)
                    subTD :: TypeData dv t
                    subTD = MkTypeData subMTID (Just superTD) subtypes tname doc
                subtdata <- getConssSubtypeData subTD conss
            return $ subTD : subtdata

getConssSubtypeData ::
       TypeData dv t -> [SyntaxWithDoc (SyntaxConstructorOrSubtype extra)] -> PinaforeInterpreter [TypeData dv t]
getConssSubtypeData superTD conss = do
    tdatas <- for conss $ getConsSubtypeData superTD
    return $ mconcat tdatas

data Constructor dv t extra = MkConstructor
    { ctName :: Name
    , ctDoc :: Markdown
    , ctOuterType :: TypeData dv t
    , ctInnerTypes :: [SyntaxType]
    , ctExtra :: extra
    }

typeDataLookup :: [TypeData dv t] -> Name -> PinaforeInterpreter (TypeData dv t)
typeDataLookup [] _ = liftIO $ fail "type name not found"
typeDataLookup (t:_) name
    | tdName t == name = return t
typeDataLookup (_:tt) name = typeDataLookup tt name

getConstructor ::
       [TypeData dv t]
    -> TypeData dv t
    -> SyntaxWithDoc (SyntaxConstructorOrSubtype extra)
    -> PinaforeInterpreter [Constructor dv t extra]
getConstructor _ typeNM (MkSyntaxWithDoc doc (ConstructorSyntaxConstructorOrSubtype consName stypes extra)) =
    return $ pure $ MkConstructor consName doc typeNM stypes extra
getConstructor tdata _ (MkSyntaxWithDoc _ (SubtypeSyntaxConstructorOrSubtype subtypeName stypes)) = do
    subtypeTD <- typeDataLookup tdata subtypeName
    getConstructors tdata subtypeTD stypes

getConstructors ::
       [TypeData dv t]
    -> TypeData dv t
    -> [SyntaxWithDoc (SyntaxConstructorOrSubtype extra)]
    -> PinaforeInterpreter [Constructor dv t extra]
getConstructors tdata typeNM syntaxConstructorList =
    fmap mconcat $ for syntaxConstructorList $ getConstructor tdata typeNM

interpretConstructorTypes :: Constructor dv t extra -> PinaforeInterpreter (Some (ListType PinaforeNonpolarType))
interpretConstructorTypes c = do
    etypes <- for (ctInnerTypes c) interpretNonpolarType
    return $ assembleListType etypes

makeBox ::
       forall extra (dv :: DolanVariance).
       GroundTypeMaker extra
    -> Name
    -> Markdown
    -> [SyntaxWithDoc (SyntaxConstructorOrSubtype extra)]
    -> GenCCRTypeParams dv
    -> PinaforeInterpreter (PinaforeFixBox () ())
makeBox gmaker mainTypeName mainTypeDoc syntaxConstructorList gtparams = do
    stid <- newMatchingTypeID @dv
    case stid of
        MkSome (mainMTID@(MkMatchingTypeID mainTypeID) :: _ maintype) -> do
            rec
                let
                    mainTypeData :: TypeData dv maintype
                    mainTypeData =
                        MkTypeData
                            { tdID = mainMTID
                            , tdSupertype = Nothing
                            , tdSubtypes = mainMTID : mconcat (fmap tdSubtypes subtypeDatas)
                            , tdName = mainTypeName
                            , tdDoc = mainTypeDoc
                            }
                subtypeDatas <- getConssSubtypeData mainTypeData syntaxConstructorList
            constructorList <- getConstructors subtypeDatas mainTypeData syntaxConstructorList
            fixedFromList constructorList $ \(constructorCount :: _ conscount) constructorFixedList ->
                withRepresentative constructorCount $
                case gtparams @maintype of
                    MkSome (tparams :: CCRTypeParams dv maintype decltype) ->
                        withRepresentative (ccrArgumentsType tparams) $
                        case gmaker mainTypeName tparams of
                            MkTypeConstruction mkx (mkgt :: x -> _ (_ y)) postregister -> let
                                mainRegister :: x -> PinaforeScopeInterpreter ()
                                mainRegister x = do
                                    MkGroundTypeFromTypeID gttid <- lift $ mkgt x
                                    let (gt, y) = gttid mainTypeName mainTypeID
                                    registerType mainTypeName mainTypeDoc gt
                                    postregister gt y
                                mainConstruct ::
                                       ()
                                    -> PinaforeScopeInterpreter ( x
                                                                , ( x
                                                                  , DolanVarianceMap dv maintype
                                                                  , FixedList conscount (ConstructorCodec decltype)
                                                                  , decltype -> TypeData dv maintype))
                                mainConstruct () = do
                                    constructorInnerTypes <- lift $ for constructorFixedList interpretConstructorTypes
                                    assembleDataType constructorInnerTypes $ \codecs (vmap :: VarMapping structtype) pickn -> do
                                        let
                                            freevars :: [Some SymbolType]
                                            freevars = nub $ mconcat $ fmap constructorFreeVariables $ toList codecs
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
                                        dvm :: DolanVarianceMap dv maintype <-
                                            lift $ getDolanVarianceMap @dv mainTypeName tparams vmap
                                        x <-
                                            lift $
                                            mkx dvm $ toList $ liftA2 (,) codecs $ fmap ctExtra constructorFixedList
                                        return $ (x, (x, dvm, codecs, \t -> ctOuterType $ pickn t constructorFixedList))
                                mainBox ::
                                       PinaforeFixBox () ( x
                                                         , DolanVarianceMap dv maintype
                                                         , FixedList conscount (ConstructorCodec decltype)
                                                         , decltype -> TypeData dv maintype)
                                mainBox = mkFixBox mainRegister mainConstruct
                                mainTypeBox :: PinaforeFixBox x (PinaforeGroundType dv maintype)
                                mainTypeBox =
                                    mkConstructFixBox $ \x -> do
                                        gtft <- lift $ mkgt x
                                        return $ fst $ unGroundTypeFromTypeID gtft mainTypeName mainTypeID
                                getGroundType ::
                                       PinaforeGroundType dv maintype
                                    -> (decltype -> TypeData dv maintype)
                                    -> GroundTypeFromTypeID dv maintype y
                                    -> TypeData dv maintype
                                    -> PinaforeGroundType dv maintype
                                getGroundType mainGroundType picktype (MkGroundTypeFromTypeID gttid) tdata =
                                    case tdID tdata of
                                        MkMatchingTypeID (typeID :: _ subtid) -> let
                                            baseGroundType :: PinaforeGroundType dv maintype
                                            (baseGroundType, _) = gttid (tdName tdata) typeID
                                            gds :: PinaforePolyGreatestDynamicSupertype dv maintype
                                            gds =
                                                GeneralPolyGreatestDynamicSupertype $ \(args :: _ argstype) ->
                                                    case unsafeRefl @Type @decltype @argstype of
                                                        Refl ->
                                                            Just $
                                                            MkShimWit (MkDolanGroundedType mainGroundType args) $
                                                            MkPolarMap $
                                                            functionToShim "supertype" $ \t ->
                                                                if elem (tdID $ picktype t) $ tdSubtypes tdata
                                                                    then Just t
                                                                    else Nothing
                                            in baseGroundType
                                                   { pgtGreatestDynamicSupertype =
                                                         if tdID tdata == mainMTID
                                                             then nullPolyGreatestDynamicSupertype
                                                             else gds
                                                   }
                                registerConstructor ::
                                       Constructor dv maintype extra
                                    -> ( PinaforeGroundType dv maintype
                                       , decltype -> TypeData dv maintype
                                       , x
                                       , DolanVarianceMap dv maintype
                                       , ConstructorCodec decltype)
                                    -> PinaforeScopeInterpreter ()
                                registerConstructor constructor (mainGroundType, picktype, x, dvm, MkSomeFor (MkListProductType lt) codec) = do
                                    gttid <- lift $ mkgt x
                                    let
                                        groundType :: PinaforeGroundType dv maintype
                                        groundType =
                                            getGroundType mainGroundType picktype gttid $ ctOuterType constructor
                                        getargs ::
                                               forall polarity. Is PolarityType polarity
                                            => DolanArgumentsShimWit PinaforePolyShim dv PinaforeType maintype polarity decltype
                                        getargs =
                                            mapCCRArguments
                                                @PinaforePolyShim
                                                @CCRTypeParam
                                                @(CCRPolarArgument PinaforeType polarity)
                                                @dv
                                                @polarity
                                                @maintype
                                                tParamToPolarArgument
                                                dvm
                                                tparams
                                    case (getargs @'Positive, getargs @'Negative) of
                                        (MkShimWit posargs posconv, MkShimWit negargs negconv) -> do
                                            let
                                                ctfpos :: PinaforeShimWit 'Positive decltype
                                                ctfpos =
                                                    mapShimWit posconv $
                                                    typeToDolan $ MkDolanGroundedType groundType posargs
                                                ctfneg :: PinaforeShimWit 'Negative decltype
                                                ctfneg =
                                                    mapShimWit negconv $
                                                    typeToDolan $ MkDolanGroundedType groundType negargs
                                            ltp <- return $ mapListType (nonpolarToPositive @PinaforeTypeSystem) lt
                                            ltn <- return $ mapListType (nonpolarToNegative @PinaforeTypeSystem) lt
                                            let
                                                expr =
                                                    qConstExprAny $
                                                    MkSomeOf (qFunctionPosWitnesses ltn ctfpos) $ encode codec
                                                pc = toPatternConstructor ctfneg ltp $ ImpureFunction $ decode codec
                                            registerPatternConstructor (ctName constructor) (ctDoc constructor) expr $
                                                toExpressionPatternConstructor pc
                                constructorBox ::
                                       Constructor dv maintype extra
                                    -> PinaforeFixBox ( PinaforeGroundType dv maintype
                                                      , decltype -> TypeData dv maintype
                                                      , x
                                                      , DolanVarianceMap dv maintype
                                                      , ConstructorCodec decltype) ()
                                constructorBox constructor = mkConstructFixBox $ registerConstructor constructor
                                subtypeRegister ::
                                       TypeData dv maintype
                                    -> (PinaforeGroundType dv maintype, decltype -> TypeData dv maintype, x)
                                    -> PinaforeScopeInterpreter ()
                                subtypeRegister tdata (~(mainGroundType, picktype, x)) = do
                                    gttid <- lift $ mkgt x
                                    let
                                        subGroundType :: PinaforeGroundType dv maintype
                                        subGroundType = getGroundType mainGroundType picktype gttid tdata
                                    registerType (tdName tdata) (tdDoc tdata) subGroundType
                                    for_ (tdSupertype tdata) $ \supertdata -> let
                                        superGroundType :: PinaforeGroundType dv maintype
                                        superGroundType = getGroundType mainGroundType picktype gttid supertdata
                                        in registerSubtypeConversion $
                                           MkSubtypeConversionEntry
                                               Verify
                                               subGroundType
                                               superGroundType
                                               IdentitySubtypeConversion
                                subtypeBox ::
                                       TypeData dv maintype
                                    -> PinaforeFixBox ( PinaforeGroundType dv maintype
                                                      , decltype -> TypeData dv maintype
                                                      , x) ()
                                subtypeBox tdata = mkRegisterFixBox $ subtypeRegister tdata
                                in return $
                                   proc () -> do
                                       (x, dvm, codecs, picktype) <- mainBox -< ()
                                       mainGroundType <- mainTypeBox -< x
                                       mconcat (fmap subtypeBox subtypeDatas) -< (mainGroundType, picktype, x)
                                       fixedListArrowSequence_ (fmap constructorBox constructorFixedList) -<
                                           fmap (\codec -> (mainGroundType, picktype, x, dvm, codec)) codecs

makeDeclTypeBox ::
       forall extra.
       GroundTypeMaker extra
    -> Name
    -> Markdown
    -> [SyntaxTypeParameter]
    -> [SyntaxWithDoc (SyntaxConstructorOrSubtype extra)]
    -> PinaforeInterpreter (PinaforeFixBox () ())
makeDeclTypeBox gmaker name doc params syntaxConstructorList =
    case getAnyCCRTypeParams params of
        MkAnyCCRTypeParams gtparams -> makeBox gmaker name doc syntaxConstructorList gtparams

makeDataGroundType ::
       forall (dv :: DolanVariance) (gt :: DolanVarianceKind dv) (decltype :: Type). Is DolanVarianceType dv
    => Name
    -> CCRTypeParams dv gt decltype
    -> TypeConstruction dv gt [(ConstructorCodec decltype, ())]
makeDataGroundType _ tparams = let
    dvt :: DolanVarianceType dv
    dvt = ccrArgumentsType tparams
    mkx :: DolanVarianceMap dv gt -> [(ConstructorCodec decltype, ())] -> PinaforeInterpreter (DolanVarianceMap dv gt)
    mkx dvm _ = return dvm
    mkgt :: DolanVarianceMap dv gt -> PinaforeInterpreter (GroundTypeFromTypeID dv gt ())
    mkgt dvm =
        return $
        MkGroundTypeFromTypeID $ \name mainTypeID -> let
            gt =
                MkPinaforeGroundType
                    { pgtVarianceType = dvt
                    , pgtVarianceMap = lazyDolanVarianceMap dvt dvm
                    , pgtShowType = standardListTypeExprShow @dv $ exprShow name
                    , pgtFamilyType = MkFamilialType datatypeIOWitness $ MkDataTypeFamily mainTypeID
                    , pgtSubtypeGroup = Nothing
                    , pgtGreatestDynamicSupertype = nullPolyGreatestDynamicSupertype
                    }
            in (gt, ())
    in MkTypeConstruction mkx mkgt $ \_ _ -> return ()

makeDataTypeBox ::
       Name
    -> Markdown
    -> [SyntaxTypeParameter]
    -> [SyntaxWithDoc SyntaxDatatypeConstructorOrSubtype]
    -> PinaforeInterpreter (PinaforeFixBox () ())
makeDataTypeBox = makeDeclTypeBox makeDataGroundType
