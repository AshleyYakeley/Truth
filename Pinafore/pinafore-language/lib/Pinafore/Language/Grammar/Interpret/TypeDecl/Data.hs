module Pinafore.Language.Grammar.Interpret.TypeDecl.Data
    ( ConstructorFlavour(..)
    , ConstructorType(..)
    , ConstructorCodec
    , TypeConstruction(..)
    , GroundTypeFromTypeID(..)
    , GroundTypeMaker
    , makeDataTypeBox
    , makePlainDataTypeBox
    ) where

import Pinafore.Language.Error
import Pinafore.Language.Expression
import Pinafore.Language.Grammar.Interpret.Type
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

data ConstructorFlavour (w :: Type -> Type) where
    PositionalCF :: ConstructorFlavour QNonpolarType
    RecordCF :: ConstructorFlavour (QSignature 'Positive)

data ConstructorType (t :: Type) where
    MkConstructorType :: ConstructorFlavour w -> ListVType w tt -> ConstructorType (ListVProduct tt)

instance MaybeUnitWitness ConstructorType where
    maybeUnitWitness (MkConstructorType _ (MkListVType t :: ListVType w tt)) = do
        Refl <- listVectorIsEmpty t
        case emptyMapTypeRefl @Type @Type @w @tt of
            Refl -> return Dict

instance HasVarMapping ConstructorType where
    getVarMapping (MkConstructorType PositionalCF tt) = getVarMapping $ MkListVProductType tt
    getVarMapping (MkConstructorType RecordCF tt) = getVarMapping $ MkListVProductType tt

type ConstructorCodec t = SomeFor (Codec t) ConstructorType

constructorTypeFreeVariables :: ConstructorFlavour w -> w t -> FiniteSet (Some SymbolType)
constructorTypeFreeVariables PositionalCF wt = freeTypeVariables wt
constructorTypeFreeVariables RecordCF _ = mempty

instance FreeTypeVariables (ConstructorCodec t) where
    freeTypeVariables (MkSomeFor (MkConstructorType cf w) _) =
        mconcat $ toList $ listVTypeToVector (constructorTypeFreeVariables cf) w

assembleDataType ::
       forall n.
       FixedList n (Some ConstructorType)
    -> forall r.
               (forall t. FixedList n (ConstructorCodec t) -> VarMapping t -> (t -> (forall a. FixedList n a -> a)) -> r) -> r
assembleDataType tconss call =
    listTypeFromFixedList tconss $ \lcons ->
        getPreferredTypeRepresentation lcons $ \(MkTypeRepresentation ccons vmap) pickcons -> let
            conss :: FixedList n (SomeFor (Codec _) ConstructorType)
            conss = listTypeToFixedList getConst $ joinListType (\codec el -> Const $ MkSomeFor el codec) ccons lcons
            in call conss vmap $ \t -> fixedListElement $ listElementTypeIndex $ someForToSome $ pickcons t

withCCRTypeParam :: SyntaxTypeParameter -> (forall sv t. CCRTypeParam sv t -> r) -> r
withCCRTypeParam (PositiveSyntaxTypeParameter n) cont = nameToSymbolType n $ \v -> cont $ CoCCRTypeParam v
withCCRTypeParam (NegativeSyntaxTypeParameter n) cont = nameToSymbolType n $ \v -> cont $ ContraCCRTypeParam v
withCCRTypeParam (RangeSyntaxTypeParameter np nq) cont =
    nameToSymbolType np $ \vp -> nameToSymbolType nq $ \vq -> cont $ RangeCCRTypeParam vp vq

tParamToPolarArgument ::
       forall sv (t :: CCRVarianceKind sv) polarity. Is PolarityType polarity
    => CCRTypeParam sv t
    -> CCRPolarArgumentShimWit (QPolyShim Type) QType polarity sv t
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
       forall v n t. FullName -> VarianceType v -> SymbolType n -> VarMapping t -> QInterpreter (Mapping n t)
getDataTypeMappingOrError tname vt var vm =
    case runVarMapping vm vt var of
        Nothing -> throw $ InterpretTypeDeclTypeVariableWrongPolarityError tname $ symbolTypeToName var
        Just vmap -> return vmap

getCCRVariation ::
       FullName
    -> CCRTypeParam sv a
    -> VarMapping t
    -> QInterpreter (CCRVarianceCategory KindFunction sv a a -> (t -> t))
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

getDolanVarianceMap :: FullName -> CCRTypeParams dv gt t -> VarMapping t -> QInterpreter (DolanVarianceMap dv gt)
getDolanVarianceMap _ NilCCRArguments _ = return NilDolanVarianceMap
getDolanVarianceMap tname (ConsCCRArguments p pp) vm = do
    ff <- getCCRVariation tname p vm
    args <- getDolanVarianceMap tname pp vm
    return $ ConsDolanVarianceMap (MkCCRVariation Nothing $ paramsCCRVMap p ff pp) $ assignDolanArgVars p args

data TypeConstruction dv gt extra =
    forall x y. MkTypeConstruction (DolanVarianceMap dv gt -> extra -> QInterpreter x)
                                   (x -> QInterpreter (GroundTypeFromTypeID dv gt y))
                                   (QGroundType dv gt -> y -> QScopeInterpreter ())

type GroundTypeFromTypeID :: forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type -> Type
newtype GroundTypeFromTypeID dv gt y = MkGroundTypeFromTypeID
    { unGroundTypeFromTypeID :: forall (tid :: Nat).
                                    (IdentifiedKind tid ~ DolanVarianceKind dv, gt ~~ Identified tid) =>
                                            FullName -> TypeIDType tid -> (QGroundType dv gt, y)
    }

type GroundTypeMaker extra
     = forall (dv :: DolanVariance) (gt :: DolanVarianceKind dv) (decltype :: Type).
           Is DolanVarianceType dv =>
                   FullName -> CCRTypeParams dv gt decltype -> TypeConstruction dv gt [( ConstructorCodec decltype
                                                                                       , extra)]

type MatchingTypeID :: forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type
data MatchingTypeID dv t where
    MkMatchingTypeID
        :: forall tid (dv :: DolanVariance) (t :: DolanVarianceKind dv).
           (IdentifiedKind tid ~ DolanVarianceKind dv, Identified tid ~~ t)
        => TypeIDType tid
        -> MatchingTypeID dv t

instance forall (dv :: DolanVariance) (t :: DolanVarianceKind dv). Eq (MatchingTypeID dv t) where
    MkMatchingTypeID ta == MkMatchingTypeID tb = isJust $ testEquality ta tb

newMatchingTypeID :: forall (dv :: DolanVariance). QInterpreter (Some (MatchingTypeID dv))
newMatchingTypeID =
    withNewTypeID $ \(typeID :: _ tid) ->
        case unsafeIdentifyKind @_ @(DolanVarianceKind dv) typeID of
            Identity Refl -> return $ MkSome $ MkMatchingTypeID typeID

data TypeData dv t = MkTypeData
    { tdID :: MatchingTypeID dv t
    , tdSupertype :: Maybe (TypeData dv t)
    , tdSubtypes :: [MatchingTypeID dv t] -- includes self
    , tdName :: FullName
    , tdDoc :: RawMarkdown
    }

getConsSubtypeData ::
       forall (dv :: DolanVariance) (t :: DolanVarianceKind dv) extra.
       TypeData dv t
    -> SyntaxWithDoc (SyntaxConstructorOrSubtype extra)
    -> QInterpreter [TypeData dv t]
getConsSubtypeData superTD (MkSyntaxWithDoc doc (SubtypeSyntaxConstructorOrSubtype tname conss)) = do
    ssubtid <- newMatchingTypeID @dv
    case ssubtid of
        MkSome subMTID@(MkMatchingTypeID subTypeID) -> do
            Refl <- unsafeIdentify @_ @t subTypeID
            rec
                let
                    subtypes = tdID superTD : mconcat (fmap tdSubtypes subtdata)
                    subTD :: TypeData dv t
                    subTD = MkTypeData subMTID (Just superTD) subtypes tname doc
                subtdata <- getConssSubtypeData subTD conss
            return $ subTD : subtdata
getConsSubtypeData _ _ = return mempty

getConssSubtypeData ::
       TypeData dv t -> [SyntaxWithDoc (SyntaxConstructorOrSubtype extra)] -> QInterpreter [TypeData dv t]
getConssSubtypeData superTD conss = do
    tdatas <- for conss $ getConsSubtypeData superTD
    return $ mconcat tdatas

data Constructor dv t extra = MkConstructor
    { ctName :: FullName
    , ctDoc :: RawMarkdown
    , ctOuterType :: TypeData dv t
    , ctContents :: Either (Vector SyntaxType) (Vector SyntaxSignature)
    , ctExtra :: extra
    }

typeDataLookup :: [TypeData dv t] -> FullName -> QInterpreter (TypeData dv t)
typeDataLookup [] _ = liftIO $ fail "type name not found"
typeDataLookup (t:_) name
    | tdName t == name = return t
typeDataLookup (_:tt) name = typeDataLookup tt name

getConstructor ::
       [TypeData dv t]
    -> TypeData dv t
    -> SyntaxWithDoc (SyntaxConstructorOrSubtype extra)
    -> QInterpreter [Constructor dv t extra]
getConstructor _ typeNM (MkSyntaxWithDoc doc (ConstructorSyntaxConstructorOrSubtype consName stypes extra)) =
    return $ pure $ MkConstructor consName doc typeNM (Left $ fromList stypes) extra
getConstructor _ typeNM (MkSyntaxWithDoc doc (RecordSyntaxConstructorOrSubtype consName sigs)) =
    return $ pure $ MkConstructor consName doc typeNM (Right $ fromList sigs) $ error "record extra data"
getConstructor tdata _ (MkSyntaxWithDoc _ (SubtypeSyntaxConstructorOrSubtype subtypeName stypes)) = do
    subtypeTD <- typeDataLookup tdata subtypeName
    getConstructors tdata subtypeTD stypes

getConstructors ::
       [TypeData dv t]
    -> TypeData dv t
    -> [SyntaxWithDoc (SyntaxConstructorOrSubtype extra)]
    -> QInterpreter [Constructor dv t extra]
getConstructors tdata typeNM syntaxConstructorList =
    fmap mconcat $ for syntaxConstructorList $ getConstructor tdata typeNM

interpretSignature' :: SyntaxSignature' -> QInterpreter (Some (QSignature 'Positive))
interpretSignature' (ValueSyntaxSignature name stype) = do
    qtype <- interpretType stype
    return $ mapSome (ValueSignature name) qtype

interpretSignature :: SyntaxSignature -> QInterpreter (Some (QSignature 'Positive))
interpretSignature (MkSyntaxWithDoc _ (MkWithSourcePos _ ssig)) = interpretSignature' ssig

interpretConstructorTypes :: Constructor dv t extra -> QInterpreter (Some ConstructorType)
interpretConstructorTypes c =
    case ctContents c of
        Left innerTypes -> do
            etypes <- for innerTypes interpretNonpolarType
            case assembleListVType etypes of
                MkSome npts -> return $ MkSome $ MkConstructorType PositionalCF npts
        Right sigs -> do
            qsigs <- for sigs interpretSignature
            case assembleListVType qsigs of
                MkSome qsiglist -> return $ MkSome $ MkConstructorType RecordCF qsiglist

makeBox ::
       forall extra (dv :: DolanVariance).
       GroundTypeMaker extra
    -> FullName
    -> RawMarkdown
    -> [SyntaxWithDoc (SyntaxConstructorOrSubtype extra)]
    -> GenCCRTypeParams dv
    -> QInterpreter (QFixBox () ())
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
                                mainRegister :: x -> QScopeInterpreter ()
                                mainRegister x = do
                                    MkGroundTypeFromTypeID gttid <- lift $ mkgt x
                                    let (gt, y) = gttid mainTypeName mainTypeID
                                    registerType mainTypeName mainTypeDoc gt
                                    postregister gt y
                                mainConstruct ::
                                       ()
                                    -> QScopeInterpreter ( x
                                                         , ( x
                                                           , DolanVarianceMap dv maintype
                                                           , FixedList conscount (ConstructorCodec decltype)
                                                           , decltype -> TypeData dv maintype))
                                mainConstruct () = do
                                    constructorInnerTypes <- lift $ for constructorFixedList interpretConstructorTypes
                                    assembleDataType constructorInnerTypes $ \codecs (vmap :: VarMapping structtype) pickn -> do
                                        let
                                            freevars :: FiniteSet (Some SymbolType)
                                            freevars = mconcat $ fmap freeTypeVariables $ toList codecs
                                            declaredvars :: [Some SymbolType]
                                            declaredvars = tParamsVars tparams
                                            unboundvars :: [Some SymbolType]
                                            unboundvars = toList freevars \\ declaredvars
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
                                       QFixBox () ( x
                                                  , DolanVarianceMap dv maintype
                                                  , FixedList conscount (ConstructorCodec decltype)
                                                  , decltype -> TypeData dv maintype)
                                mainBox = mkFixBox mainRegister mainConstruct
                                mainTypeBox :: QFixBox x (QGroundType dv maintype)
                                mainTypeBox =
                                    mkConstructFixBox $ \x -> do
                                        gtft <- lift $ mkgt x
                                        return $ fst $ unGroundTypeFromTypeID gtft mainTypeName mainTypeID
                                getGroundType ::
                                       QGroundType dv maintype
                                    -> (decltype -> TypeData dv maintype)
                                    -> GroundTypeFromTypeID dv maintype y
                                    -> TypeData dv maintype
                                    -> QGroundType dv maintype
                                getGroundType mainGroundType picktype (MkGroundTypeFromTypeID gttid) tdata =
                                    case tdID tdata of
                                        MkMatchingTypeID (typeID :: _ subtid) -> let
                                            baseGroundType :: QGroundType dv maintype
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
                                                   { qgtGreatestDynamicSupertype =
                                                         if tdID tdata == mainMTID
                                                             then nullPolyGreatestDynamicSupertype
                                                             else gds
                                                   }
                                registerConstructor ::
                                       Constructor dv maintype extra
                                    -> ( QGroundType dv maintype
                                       , decltype -> TypeData dv maintype
                                       , x
                                       , DolanVarianceMap dv maintype
                                       , ConstructorCodec decltype)
                                    -> QScopeInterpreter ()
                                registerConstructor constructor (mainGroundType, picktype, x, dvm, MkSomeFor ctype codec) = do
                                    gttid <- lift $ mkgt x
                                    let
                                        groundType :: QGroundType dv maintype
                                        groundType =
                                            getGroundType mainGroundType picktype gttid $ ctOuterType constructor
                                        getargs ::
                                               forall polarity. Is PolarityType polarity
                                            => DolanArgumentsShimWit QPolyShim dv QType maintype polarity decltype
                                        getargs =
                                            mapCCRArguments
                                                @QPolyShim
                                                @CCRTypeParam
                                                @(CCRPolarArgument QType polarity)
                                                @dv
                                                @polarity
                                                @maintype
                                                tParamToPolarArgument
                                                dvm
                                                tparams
                                    case (getargs @'Positive, getargs @'Negative) of
                                        (MkShimWit posargs posconv, MkShimWit negargs negconv) -> let
                                            ctfullname = ctName constructor
                                            ctfpos :: QShimWit 'Positive decltype
                                            ctfpos =
                                                mapShimWit posconv $
                                                typeToDolan $ MkDolanGroundedType groundType posargs
                                            ctfneg :: QShimWit 'Negative decltype
                                            ctfneg =
                                                mapShimWit negconv $
                                                typeToDolan $ MkDolanGroundedType groundType negargs
                                            in case ctype of
                                                   MkConstructorType PositionalCF lt -> let
                                                       ltp =
                                                           listVTypeToType $
                                                           mapListVType (nonpolarToPositive @QTypeSystem) lt
                                                       ltn =
                                                           listVTypeToType $
                                                           mapListVType (nonpolarToNegative @QTypeSystem) lt
                                                       expr =
                                                           qConstExprAny $
                                                           MkSomeOf (qFunctionPosWitnesses ltn ctfpos) $
                                                           encode codec . listProductToVProduct lt
                                                       pc =
                                                           toPatternConstructor ctfneg ltp $
                                                           ImpureFunction $ fmap listVProductToProduct . decode codec
                                                       in registerPatternConstructor ctfullname (ctDoc constructor) expr $
                                                          toExpressionPatternConstructor pc
                                                   MkConstructorType RecordCF lt -> let
                                                       ltp = listVTypeToType lt
                                                       recordcons = MkRecordConstructor ltp ctfpos $ encode codec
                                                       recordpat = MkRecordPattern ltp ctfneg $ decode codec
                                                       in registerRecord
                                                              ctfullname
                                                              (ctDoc constructor)
                                                              recordcons
                                                              recordpat
                                constructorBox ::
                                       Constructor dv maintype extra
                                    -> QFixBox ( QGroundType dv maintype
                                               , decltype -> TypeData dv maintype
                                               , x
                                               , DolanVarianceMap dv maintype
                                               , ConstructorCodec decltype) ()
                                constructorBox constructor = mkConstructFixBox $ registerConstructor constructor
                                subtypeRegister ::
                                       TypeData dv maintype
                                    -> (QGroundType dv maintype, decltype -> TypeData dv maintype, x)
                                    -> QScopeInterpreter ()
                                subtypeRegister tdata (~(mainGroundType, picktype, x)) = do
                                    gttid <- lift $ mkgt x
                                    let
                                        subGroundType :: QGroundType dv maintype
                                        subGroundType = getGroundType mainGroundType picktype gttid tdata
                                    registerType (tdName tdata) (tdDoc tdata) subGroundType
                                    for_ (tdSupertype tdata) $ \supertdata -> let
                                        superGroundType :: QGroundType dv maintype
                                        superGroundType = getGroundType mainGroundType picktype gttid supertdata
                                        in registerSubtypeConversion $
                                           MkSubtypeConversionEntry
                                               Verify
                                               subGroundType
                                               superGroundType
                                               identitySubtypeConversion
                                subtypeBox ::
                                       TypeData dv maintype
                                    -> QFixBox (QGroundType dv maintype, decltype -> TypeData dv maintype, x) ()
                                subtypeBox tdata = mkRegisterFixBox $ subtypeRegister tdata
                                in return $
                                   proc () -> do
                                       (x, dvm, codecs, picktype) <- mainBox -< ()
                                       mainGroundType <- mainTypeBox -< x
                                       mconcat (fmap subtypeBox subtypeDatas) -< (mainGroundType, picktype, x)
                                       fixedListArrowSequence_ (fmap constructorBox constructorFixedList) -<
                                           fmap (\codec -> (mainGroundType, picktype, x, dvm, codec)) codecs

makeDataTypeBox ::
       forall extra.
       GroundTypeMaker extra
    -> FullName
    -> RawMarkdown
    -> [SyntaxTypeParameter]
    -> [SyntaxWithDoc (SyntaxConstructorOrSubtype extra)]
    -> QInterpreter (QFixBox () ())
makeDataTypeBox gmaker name doc params syntaxConstructorList =
    case getAnyCCRTypeParams params of
        MkAnyCCRTypeParams gtparams -> makeBox gmaker name doc syntaxConstructorList gtparams

makePlainGroundType ::
       forall (dv :: DolanVariance) (gt :: DolanVarianceKind dv) (decltype :: Type). Is DolanVarianceType dv
    => FullName
    -> CCRTypeParams dv gt decltype
    -> TypeConstruction dv gt [(ConstructorCodec decltype, ())]
makePlainGroundType _ tparams = let
    dvt :: DolanVarianceType dv
    dvt = ccrArgumentsType tparams
    mkx :: DolanVarianceMap dv gt -> [(ConstructorCodec decltype, ())] -> QInterpreter (DolanVarianceMap dv gt)
    mkx dvm _ = return dvm
    mkgt :: DolanVarianceMap dv gt -> QInterpreter (GroundTypeFromTypeID dv gt ())
    mkgt dvm =
        return $
        MkGroundTypeFromTypeID $ \name mainTypeID -> let
            gt =
                MkQGroundType
                    { qgtVarianceType = dvt
                    , qgtVarianceMap = lazyDolanVarianceMap dvt dvm
                    , qgtShowType = standardListTypeExprShow @dv $ toNamedText name
                    , qgtFamilyType = MkFamilialType identifiedFamilyWitness $ MkIdentifiedTypeFamily mainTypeID
                    , qgtSubtypeGroup = Nothing
                    , qgtProperties = mempty
                    , qgtGreatestDynamicSupertype = nullPolyGreatestDynamicSupertype
                    }
            in (gt, ())
    in MkTypeConstruction mkx mkgt $ \_ _ -> return ()

makePlainDataTypeBox ::
       FullName
    -> RawMarkdown
    -> [SyntaxTypeParameter]
    -> [SyntaxWithDoc SyntaxPlainDatatypeConstructorOrSubtype]
    -> QInterpreter (QFixBox () ())
makePlainDataTypeBox = makeDataTypeBox makePlainGroundType
