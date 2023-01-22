module Pinafore.Language.Grammar.Interpret.TypeDecl.Synonym
    ( makeSynonymTypeBox
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
       Some QNonpolarType
    -> forall r.
               (forall t. VarMapping t -> r) -> r
assembleDataType (MkSome t) call = call $ getVarMapping t

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
                   FullName -> CCRTypeParams dv gt decltype -> TypeConstruction dv gt extra

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
    -> SyntaxType
    -> GenCCRTypeParams dv
    -> QInterpreter (QFixBox () ())
makeBox gmaker mainTypeName mainTypeDoc sBodyType gtparams = do
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
                            , tdSubtypes = [mainMTID]
                            , tdName = mainTypeName
                            , tdDoc = mainTypeDoc
                            }
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
                                                           , QNonpolarType decltype))
                                mainConstruct () = do
                                    MkSome (bodyType :: _ structtype) <- lift $ interpretNonpolarType sBodyType
                                    let
                                        vmap = getVarMapping bodyType
                                    do
                                        let
                                            freevars :: FiniteSet (Some SymbolType)
                                            freevars = freeTypeVariables bodyType
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
                                            -- mkx dvm $ toList $ liftA2 (,) codecs $ fmap ctExtra constructorFixedList
                                            mkx dvm ()
                                        return $ (x, (x, dvm,  bodyType))
                                registerSubtypes :: QFixBox (QGroundType dv maintype, QNonpolarType decltype) ()
                                registerSubtypes = mkRegisterFixBox $ \ ~(declType,bodyType) -> do
                                        registerSubtypeConversion $
                                            MkSubtypeConversionEntry
                                                Verify
                                                declType
                                                bodyType
                                                identitySubtypeConversion
                                        registerSubtypeConversion $
                                            MkSubtypeConversionEntry
                                                Verify
                                                bodyType
                                                declType
                                                identitySubtypeConversion
                                mainBox ::
                                       QFixBox () ( x
                                                  , DolanVarianceMap dv maintype
                                                  , QNonpolarType decltype)
                                mainBox = mkFixBox mainRegister mainConstruct
                                mainTypeBox :: QFixBox x (QGroundType dv maintype)
                                mainTypeBox =
                                    mkConstructFixBox $ \x -> do
                                        gtft <- lift $ mkgt x
                                        return $ fst $ unGroundTypeFromTypeID gtft mainTypeName mainTypeID
                                in return $
                                   proc () -> do
                                       (x, dvm, bodyType) <- mainBox -< ()
                                       declType <- mainTypeBox -< x
                                       registerSubtypes -< (declType,bodyType)

makeDataTypeBox ::
       forall extra.
       GroundTypeMaker extra
    -> FullName
    -> RawMarkdown
    -> [SyntaxTypeParameter]
    -> SyntaxType
    -> QInterpreter (QFixBox () ())
makeDataTypeBox gmaker name doc params bodytype =
    case getAnyCCRTypeParams params of
        MkAnyCCRTypeParams gtparams -> makeBox gmaker name doc bodytype gtparams

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
                MkPinaforeGroundType
                    { pgtVarianceType = dvt
                    , pgtVarianceMap = lazyDolanVarianceMap dvt dvm
                    , pgtShowType = standardListTypeExprShow @dv $ toNamedText name
                    , pgtFamilyType = MkFamilialType datatypeIOWitness $ MkDataTypeFamily mainTypeID
                    , pgtSubtypeGroup = Nothing
                    , pgtGreatestDynamicSupertype = nullPolyGreatestDynamicSupertype
                    }
            in (gt, ())
    in MkTypeConstruction mkx mkgt $ \_ _ -> return ()


makeSynonymTypeBox ::
       FullName -> RawMarkdown -> Bool -> [SyntaxTypeParameter] -> SyntaxType -> QInterpreter (QFixBox () ())
makeSynonymTypeBox name doc False params bodytype = makeDataTypeBox makePlainGroundType name doc params bodytype
makeSynonymTypeBox _name _doc True _params _bodytype = throw $ KnownIssueError 47 "type synonyms NYI"
