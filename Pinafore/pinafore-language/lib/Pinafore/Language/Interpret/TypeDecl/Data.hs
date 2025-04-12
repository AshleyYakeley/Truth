module Pinafore.Language.Interpret.TypeDecl.Data
    ( ConstructorFlavour (..)
    , ConstructorType (..)
    , ConstructorCodec
    , TypeConstruction (..)
    , GroundTypeFromTypeID (..)
    , GroundTypeMaker
    , makeDataTypeBox
    , makePlainDataTypeBox
    )
where

import Shapes.Unsafe (unsafeCoercion)

import Import
import Pinafore.Language.Error
import Pinafore.Language.Expression
import Pinafore.Language.Interpret.Type
import Pinafore.Language.Interpret.TypeDecl.DoubleParams
import Pinafore.Language.Interpret.TypeDecl.Parameter
import Pinafore.Language.Interpret.TypeDecl.Representation
import Pinafore.Language.Interpreter
import Pinafore.Language.Type

data ConstructorFlavour (w :: Type -> Type) where
    PositionalCF :: ConstructorFlavour QNonpolarType
    RecordCF :: [RecordConstructorData] -> ConstructorFlavour (QSignature 'Positive)

data ConstructorType (t :: Type) where
    MkConstructorType :: Name -> ConstructorFlavour w -> ListVType w tt -> ConstructorType (ListVProduct tt)

instance DoubleParams (ConstructorType t) where
    rangeMapParams nn (MkConstructorType n PositionalCF tt) = MkConstructorType n PositionalCF $ rangeMapParams nn tt
    rangeMapParams nn (MkConstructorType n cf@RecordCF{} tt) = MkConstructorType n cf $ rangeMapParams nn tt

instance MaybeUnitWitness ConstructorType where
    maybeUnitWitness (MkConstructorType _ _ (MkListVType t :: ListVType w tt)) = do
        Refl <- listVectorIsEmpty t
        case emptyMapTypeRefl @Type @Type @w @tt of
            Refl -> return Dict

instance HasVarMapping ConstructorType where
    getVarMapping (MkConstructorType _ PositionalCF tt) = getVarMapping $ MkListVProductType tt
    getVarMapping (MkConstructorType _ (RecordCF _) tt) = getVarMapping $ MkListVProductType tt

type ConstructorCodec t = SomeFor (Codec t) ConstructorType

constructorTypeFreeVariables :: ConstructorFlavour w -> w t -> ListSet SomeTypeVarT
constructorTypeFreeVariables PositionalCF wt = freeTypeVariables wt
constructorTypeFreeVariables (RecordCF _) _ = mempty

instance FreeTypeVariables (ConstructorCodec t) where
    freeTypeVariables (MkSomeFor (MkConstructorType _ cf w) _) =
        mconcat $ toList $ listVTypeToVector (constructorTypeFreeVariables cf) w

assembleDataType ::
    forall n.
    FixedList n (Some ConstructorType) ->
    forall r.
    (forall t. FixedList n (ConstructorCodec t) -> VarMapping t -> (t -> (forall a. FixedList n a -> a)) -> r) -> r
assembleDataType tconss call =
    listTypeFromFixedList tconss $ \lcons ->
        getPreferredTypeRepresentation lcons $ \(MkTypeRepresentation ccons vmap) pickcons -> let
            conss :: FixedList n (SomeFor (Codec _) ConstructorType)
            conss = listTypeToFixedList getConst $ joinListType (\codec el -> Const $ MkSomeFor el codec) ccons lcons
            in call conss vmap $ \t -> fixedListElement $ listElementTypeIndex $ someForToSome $ pickcons t

data TypeConstruction dv gt extra
    = forall x y. MkTypeConstruction
        (CCRVariancesMap dv gt -> extra -> QInterpreter x)
        (x -> QInterpreter (GroundTypeFromTypeID dv gt y))
        (QGroundType dv gt -> y -> QScopeBuilder ())

type GroundTypeFromTypeID :: forall (dv :: CCRVariances) -> CCRVariancesKind dv -> Type -> Type
newtype GroundTypeFromTypeID dv gt y = MkGroundTypeFromTypeID
    { unGroundTypeFromTypeID :: FullName -> FamilialType gt -> (QGroundType dv gt, y)
    }

type GroundTypeMaker extra =
    forall (dv :: CCRVariances) (gt :: CCRVariancesKind dv) (decltype :: Type).
    Is CCRVariancesType dv =>
    FullName ->
    CCRTypeParams dv gt decltype ->
    TypeConstruction
        dv
        gt
        [ ( ConstructorCodec decltype
          , extra
          )
        ]

data TypeInfo = MkTypeInfo
    { tiName :: FullName
    , tiStorable :: Bool
    , tiParams :: [SyntaxTypeParameter]
    , tiGDS :: Maybe FullName
    , tiDescription :: RawMarkdown
    }

tiDoc :: TypeInfo -> DefDoc
tiDoc MkTypeInfo{..} = let
    gdst = fmap (\gds -> exprShow gds <> concatmap (\p -> " " <> exprShow p) tiParams) tiGDS
    in MkDefDoc (typeDocItem tiName tiStorable tiParams gdst) tiDescription

data TypeData (dv :: CCRVariances) (gt :: CCRVariancesKind dv) = MkTypeData
    { tdInfo :: TypeInfo
    , tdID :: FamilialType gt
    , tdSupertype :: Maybe (TypeData dv gt)
    , tdSubtypes :: [FamilialType gt] -- includes self
    }

tdName :: TypeData dv t -> FullName
tdName = tiName . tdInfo

tdDoc :: TypeData dv t -> DefDoc
tdDoc = tiDoc . tdInfo

getConsSubtypeData ::
    forall (dv :: CCRVariances) (gt :: CCRVariancesKind dv) extra.
    FullName ->
    TypeData dv gt ->
    SyntaxWithDoc (SyntaxConstructorOrSubtype extra) ->
    QInterpreter [TypeData dv gt]
getConsSubtypeData mainName superTD (MkSyntaxWithDoc md (SubtypeSyntaxConstructorOrSubtype tname conss)) = do
    subTypeID <- newIdentifiedType @dv @gt
    rec let
            superInfo :: TypeInfo
            superInfo = tdInfo superTD
            subInfo :: TypeInfo
            subInfo =
                MkTypeInfo
                    { tiName = tname
                    , tiStorable = tiStorable superInfo
                    , tiParams = tiParams superInfo
                    , tiGDS = Just mainName
                    , tiDescription = md
                    }
            subtypes = tdID superTD : concatmap tdSubtypes subtdata
            subTD :: TypeData dv gt
            subTD = MkTypeData{tdInfo = subInfo, tdID = subTypeID, tdSupertype = Just superTD, tdSubtypes = subtypes}
        subtdata <- getConssSubtypeData subTD conss
    return $ subTD : subtdata
getConsSubtypeData _ _ _ = return mempty

getConssSubtypeData ::
    forall (dv :: CCRVariances) (gt :: CCRVariancesKind dv) extra.
    TypeData dv gt ->
    [SyntaxWithDoc (SyntaxConstructorOrSubtype extra)] ->
    QInterpreter [TypeData dv gt]
getConssSubtypeData superTD conss = do
    tdatas <- for conss $ getConsSubtypeData @dv (tiName $ tdInfo superTD) superTD
    return $ mconcat tdatas

data Constructor dv gt extra = MkConstructor
    { ctName :: FullName
    , ctDoc :: DefDoc
    , ctOuterType :: TypeData dv gt
    , ctContents :: Either [SyntaxType] [SyntaxSignature]
    , ctExtra :: extra
    }

typeDataLookup :: [TypeData dv gt] -> FullName -> QInterpreter (TypeData dv gt)
typeDataLookup [] _ = liftIO $ fail "type name not found"
typeDataLookup (t : _) name
    | tdName t == name = return t
typeDataLookup (_ : tt) name = typeDataLookup tt name

getConstructor :: TypeData dv gt -> FullName -> DefDoc -> SyntaxDataConstructor extra -> Constructor dv gt extra
getConstructor typeNM consName doc (PlainSyntaxConstructor stypes extra) =
    MkConstructor consName doc typeNM (Left stypes) extra
getConstructor typeNM consName doc (RecordSyntaxConstructor sigs) =
    MkConstructor consName doc typeNM (Right sigs) $ error "record extra data"

getConstructorOrSubtype ::
    [TypeData dv gt] ->
    TypeData dv gt ->
    SyntaxWithDoc (SyntaxConstructorOrSubtype extra) ->
    QInterpreter [Constructor dv gt extra]
getConstructorOrSubtype _ typeNM (MkSyntaxWithDoc md (ConstructorSyntaxConstructorOrSubtype consName dcons)) =
    return
        $ pure
        $ getConstructor typeNM consName (MkDefDoc (constructorDocItem (tdName typeNM) consName dcons) md) dcons
getConstructorOrSubtype tdata _ (MkSyntaxWithDoc _ (SubtypeSyntaxConstructorOrSubtype subtypeName stypes)) = do
    subtypeTD <- typeDataLookup tdata subtypeName
    getConstructors tdata subtypeTD stypes

getConstructors ::
    [TypeData dv gt] ->
    TypeData dv gt ->
    [SyntaxWithDoc (SyntaxConstructorOrSubtype extra)] ->
    QInterpreter [Constructor dv gt extra]
getConstructors tdata typeNM syntaxConstructorList =
    fmap mconcat $ for syntaxConstructorList $ getConstructorOrSubtype tdata typeNM

data RecordConstructorData = MkRecordConstructorData
    { rcdName :: FullNameRef
    , rcdType :: Some (QGroundType '[])
    , rcdRecordConstructor :: QRecordConstructor
    }

instance ExprShow RecordConstructorData where
    exprShowPrec rcd = exprShowPrec $ rcdName rcd

type SomeSignature = Some (QSignature 'Positive)

rcdSignatures :: RecordConstructorData -> [SomeSignature]
rcdSignatures MkRecordConstructorData{..} =
    case rcdRecordConstructor of
        (MkQRecordConstructor sigs _ _ _) -> listVTypeToList MkSome sigs

lookupRecordConstructorData :: [Some (QGroundType '[])] -> FullNameRef -> QInterpreter RecordConstructorData
lookupRecordConstructorData supertypes rcdName = do
    rcdRecordConstructor@(MkQRecordConstructor _ stpw _ _) <- lookupRecordConstructor rcdName
    rcdType <-
        case stpw of
            MkShimWit (MkDolanGroundedType gt NilCCRArguments) _
                | elem (MkSome gt) supertypes -> return $ MkSome gt
            _ -> throw $ DeclareDatatypeConstructorNotSupertypeError rcdName (exprShow stpw) $ fmap exprShow supertypes
    return MkRecordConstructorData{..}

interpretSignature' ::
    (?interpretExpression :: SyntaxExpression -> QInterpreter QExpression) =>
    SomeFamilialType ->
    [Some (QGroundType '[])] ->
    SyntaxSignature' ->
    QInterpreter (Maybe RecordConstructorData, [SomeSignature])
interpretSignature' tid _ (ValueSyntaxSignature name stype msdefv) = do
    aqtype <- interpretType stype
    case aqtype of
        MkSome qtype -> do
            modefv <-
                for msdefv $ \sdefv -> do
                    defv <- ?interpretExpression sdefv
                    qSubsumeExpressionToOpen qtype defv
            return $ (Nothing, pure $ MkSome $ ValueSignature (Just tid) name qtype modefv)
interpretSignature' _ supertypes (SupertypeConstructorSyntaxSignature name) = do
    rcd <- lookupRecordConstructorData supertypes name
    return $ (Just rcd, rcdSignatures rcd)

interpretSignature ::
    (?interpretExpression :: SyntaxExpression -> QInterpreter QExpression) =>
    SomeFamilialType ->
    [Some (QGroundType '[])] ->
    SyntaxSignature ->
    QInterpreter (Maybe RecordConstructorData, [SomeSignature])
interpretSignature tid supertypes (MkSyntaxWithDoc _ (MkWithSourcePos _ ssig)) = interpretSignature' tid supertypes ssig

matchSigName :: SomeSignature -> SomeSignature -> Bool
matchSigName (MkSome (ValueSignature _ na _ _)) (MkSome (ValueSignature _ nb _ _)) = na == nb

checkDuplicates :: (Name -> QError) -> [Name] -> QInterpreter ()
checkDuplicates _ [] = return ()
checkDuplicates f (a : aa) =
    if elem a aa
        then throw $ f a
        else checkDuplicates f aa

signatureName :: SyntaxSignature -> Maybe Name
signatureName (MkSyntaxWithDoc _ (MkWithSourcePos _ (ValueSyntaxSignature n _ _))) = Just n
signatureName _ = Nothing

someSignatureName :: SomeSignature -> Maybe Name
someSignatureName (MkSome (ValueSignature _ n _ _)) = Just n

interpretConstructorTypes ::
    (?interpretExpression :: SyntaxExpression -> QInterpreter QExpression) =>
    SomeFamilialType ->
    [Some (QGroundType '[])] ->
    Constructor dv t extra ->
    QInterpreter (Some ConstructorType)
interpretConstructorTypes tid supertypes c = let
    consname = fnName $ ctName c
    in paramWith currentNamespaceParam (fnCompanion $ tdName $ ctOuterType c)
        $ case ctContents c of
            Left innerTypes -> do
                etypes <- for innerTypes interpretNonpolarType
                case assembleListVType $ fromList etypes of
                    MkSome npts -> return $ MkSome $ MkConstructorType consname PositionalCF npts
            Right sigs -> do
                checkDuplicates DeclareDatatypeDuplicateMembers $ mapMaybe signatureName sigs
                rcdsigss <- for sigs $ interpretSignature tid supertypes
                let
                    allSigs :: [SomeSignature]
                    allSigs = concatmap snd rcdsigss
                    splitSigs :: SomeSignature -> ([SomeSignature], [SomeSignature])
                    splitSigs sig@(MkSome (ValueSignature sigtid _ _ _)) =
                        if sigtid == Just tid
                            then ([sig], [])
                            else ([], [sig])
                    (thisQSigs, supertypeQSigs) = concatmap splitSigs allSigs
                    matchSigs :: SomeSignature -> SomeSignature -> Bool
                    matchSigs (MkSome (ValueSignature ta na _ _)) (MkSome (ValueSignature tb nb _ _)) =
                        (ta, na) == (tb, nb)
                    inheritedQSigs :: [SomeSignature] -- sigs from supertypes that are not overridden
                    inheritedQSigs =
                        nubBy matchSigs $ filter (\isig -> not $ any (matchSigName isig) thisQSigs) supertypeQSigs
                    rawrcds :: [RecordConstructorData]
                    rawrcds = catMaybes $ fmap fst rcdsigss
                checkDuplicates DeclareDatatypeDuplicateInheritedMembers $ mapMaybe someSignatureName inheritedQSigs
                rcds <-
                    for supertypes $ \supertype ->
                        case filter (\rcd -> rcdType rcd == supertype) rawrcds of
                            [] -> throw $ DeclareDatatypeNoSupertypeConstructorError (exprShow supertype)
                            [rcd] -> return rcd
                            rcds ->
                                throw
                                    $ DeclareDatatypeMultipleSupertypeConstructorsError (exprShow supertype)
                                    $ fmap exprShow rcds
                case assembleListVType $ fromList $ inheritedQSigs <> thisQSigs of
                    MkSome qsiglist -> return $ MkSome $ MkConstructorType consname (RecordCF rcds) qsiglist

pickMember ::
    QSignature 'Positive b ->
    ListType (QSignature 'Positive) aa ->
    QInterpreter (QOpenExpression (ListVProduct aa -> b))
pickMember (ValueSignature _ nb tb _) tt =
    case listTypeFind
        ( \(MkPairType (ValueSignature _ na ta _) f) ->
            if na == nb
                then Just $ MkSomeFor ta f
                else Nothing
        )
        $ pairListType tt
        $ listVProductGetters tt of
        Just (MkSomeFor ta f) -> do
            convexpr <- qSubsume (mkShimWit ta) tb
            return $ fmap (\conv -> shimToFunction conv . f) convexpr
        Nothing -> throw $ DeclareDatatypeMissingSupertypeMember nb

getMatchMemberConvert ::
    ListVType (QSignature 'Positive) aa ->
    ListVType (QSignature 'Positive) bb ->
    QInterpreter (QOpenExpression (ListVProduct aa -> ListVProduct bb))
getMatchMemberConvert ta tb =
    getCompose
        $ getCompose
        $ listVProductSequence
        $ mapListVType (\sig -> Compose $ Compose $ pickMember sig $ listVTypeToType ta) tb

makeBox ::
    forall extra (dv :: CCRVariances).
    (?interpretExpression :: SyntaxExpression -> QInterpreter QExpression) =>
    GroundTypeMaker extra ->
    [Some (QGroundType '[])] ->
    TypeInfo ->
    [SyntaxWithDoc (SyntaxConstructorOrSubtype extra)] ->
    [Name] ->
    GenCCRTypeParams dv ->
    QInterpreter (QFixBox () ())
makeBox gmaker supertypes tinfo syntaxConstructorList doubleParams gtparams =
    withSemiIdentifiedType @dv $ \(mainFamType :: _ maintype) -> do
        rec let
                mainTypeName = tiName tinfo
                mainTypeData :: TypeData dv maintype
                mainTypeData =
                    MkTypeData
                        { tdInfo = tinfo
                        , tdID = mainFamType
                        , tdSupertype = Nothing
                        , tdSubtypes = mainFamType : concatmap tdSubtypes subtypeDatas
                        }
            subtypeDatas <- getConssSubtypeData mainTypeData syntaxConstructorList
        constructorList <- getConstructors subtypeDatas mainTypeData syntaxConstructorList
        fixedFromList constructorList $ \(constructorCount :: _ conscount) (constructorFixedList :: FixedList conscount (Constructor dv maintype extra)) ->
            withRepresentative constructorCount
                $ case gtparams @maintype of
                    MkSome (tparams :: CCRTypeParams dv maintype decltype) ->
                        withRepresentative (ccrArgumentsType tparams)
                            $ case gmaker mainTypeName tparams of
                                MkTypeConstruction mkx (mkgt :: x -> _ (_ y)) postregister -> let
                                    mainRegister :: x -> QScopeBuilder ()
                                    mainRegister x = do
                                        MkGroundTypeFromTypeID gttid <- builderLift $ mkgt x
                                        let (gt, y) = gttid mainTypeName mainFamType
                                        registerGroundType mainTypeName (tdDoc mainTypeData) gt
                                        postregister gt y
                                    mainConstruct ::
                                        () ->
                                        QScopeBuilder
                                            ( x
                                            , ( x
                                              , CCRVariancesMap dv maintype
                                              , FixedList conscount (ConstructorCodec decltype)
                                              , decltype -> TypeData dv maintype
                                              )
                                            )
                                    mainConstruct () = builderLift $ do
                                        constructorInnerTypes <-
                                            for constructorFixedList
                                                $ interpretConstructorTypes (MkSomeFamilialType mainFamType) supertypes
                                        assembleDataType (doubleTypeParameters doubleParams constructorInnerTypes) $ \structCodecs (vmap :: VarMapping structtype) pickn -> do
                                            let
                                                freevars :: ListSet SomeTypeVarT
                                                freevars = concatmap freeTypeVariables $ toList structCodecs
                                                declaredvars :: [SomeTypeVarT]
                                                declaredvars = tParamsVars tparams
                                                unboundvars :: [SomeTypeVarT]
                                                unboundvars = toList freevars \\ declaredvars
                                            case nonEmpty $ duplicates declaredvars of
                                                Nothing -> return ()
                                                Just vv ->
                                                    throw
                                                        $ InterpretTypeDeclDuplicateTypeVariablesError mainTypeName
                                                        $ fmap someTypeVarToName vv
                                            case nonEmpty unboundvars of
                                                Nothing -> return ()
                                                Just vv ->
                                                    throw
                                                        $ InterpretTypeDeclUnboundTypeVariablesError mainTypeName
                                                        $ fmap someTypeVarToName vv
                                            MkCoercion <- pure $ unsafeCoercion @Type @structtype @decltype
                                            let
                                                declCodecs :: FixedList _ (ConstructorCodec decltype)
                                                declCodecs = coerce structCodecs
                                            dvm :: CCRVariancesMap dv maintype <-
                                                getCCRVariancesMap @dv mainTypeName tparams $ coerce vmap
                                            x <-
                                                mkx dvm
                                                    $ toList
                                                    $ liftA2 (,) declCodecs
                                                    $ fmap ctExtra constructorFixedList
                                            return $ (x, (x, dvm, declCodecs, \t -> ctOuterType $ pickn (coerce t) constructorFixedList))
                                    mainBox ::
                                        QFixBox
                                            ()
                                            ( x
                                            , CCRVariancesMap dv maintype
                                            , FixedList conscount (ConstructorCodec decltype)
                                            , decltype -> TypeData dv maintype
                                            )
                                    mainBox = mkFixBox mainRegister mainConstruct
                                    mainTypeBox :: QFixBox x (QGroundType dv maintype)
                                    mainTypeBox =
                                        mkConstructFixBox $ \x -> builderLift $ do
                                            gtft <- mkgt x
                                            return $ fst $ unGroundTypeFromTypeID gtft mainTypeName mainFamType
                                    getGroundType ::
                                        QGroundType dv maintype ->
                                        (decltype -> TypeData dv maintype) ->
                                        GroundTypeFromTypeID dv maintype y ->
                                        TypeData dv maintype ->
                                        QGroundType dv maintype
                                    getGroundType mainGroundType picktype (MkGroundTypeFromTypeID gttid) (tdata :: _ subtid) = let
                                        typeID = tdID tdata
                                        baseGroundType :: QGroundType dv maintype
                                        (baseGroundType, _) = gttid (tdName tdata) typeID
                                        gds :: QPolyGreatestDynamicSupertype dv maintype
                                        gds =
                                            MkPolyGreatestDynamicSupertype $ \(args :: _ argstype) ->
                                                case unsafeCoercion @Type @decltype @argstype of
                                                    MkCoercion ->
                                                        MkShimWit (MkDolanGroundedType mainGroundType args)
                                                            $ MkPolarShim
                                                            $ pureComposeShim
                                                            $ functionToShim "supertype"
                                                            $ \t ->
                                                                if elem (tdID $ picktype $ coerce t) $ tdSubtypes tdata
                                                                    then Just t
                                                                    else Nothing
                                        in baseGroundType
                                            { qgtGreatestDynamicSupertype =
                                                if tdID tdata == mainFamType
                                                    then nullPolyGreatestDynamicSupertype
                                                    else gds
                                            }
                                    declTypes ::
                                        QGroundType dv maintype ->
                                        CCRVariancesMap dv maintype ->
                                        (QGroundedShimWit 'Positive decltype, QGroundedShimWit 'Negative decltype)
                                    declTypes groundType dvm = let
                                        groundedDeclType ::
                                            forall polarity.
                                            Is PolarityType polarity =>
                                            QGroundedShimWit polarity decltype
                                        groundedDeclType =
                                            mkDolanGroundedShimWit groundType
                                                $ mapPolarCCRArguments
                                                    @QPolyShim
                                                    @CCRTypeParam
                                                    @(CCRPolarArgument QType polarity)
                                                    @dv
                                                    @polarity
                                                    @maintype
                                                    tParamToPolarArgument
                                                    dvm
                                                    tparams
                                        in (groundedDeclType @'Positive, groundedDeclType @'Negative)
                                    registerConstructor ::
                                        Constructor dv maintype extra ->
                                        ( QGroundType dv maintype
                                        , decltype -> TypeData dv maintype
                                        , x
                                        , CCRVariancesMap dv maintype
                                        , ConstructorCodec decltype
                                        ) ->
                                        QScopeBuilder ()
                                    registerConstructor constructor (mainGroundType, picktype, x, dvm, MkSomeFor ctype codec) = do
                                        gttid <- builderLift $ mkgt x
                                        let
                                            groundType :: QGroundType dv maintype
                                            groundType = getGroundType mainGroundType picktype gttid $ ctOuterType constructor
                                            (declpos, declneg) = declTypes groundType dvm
                                            ctfullname = ctName constructor
                                            ctfpos :: QShimWit 'Positive decltype
                                            ctfpos = shimWitToDolan declpos
                                            ctfneg :: QShimWit 'Negative decltype
                                            ctfneg = shimWitToDolan declneg
                                            in case ctype of
                                                MkConstructorType _ PositionalCF lt -> let
                                                    ltp = listVTypeToType $ mapListVType (nonpolarToPositive @QTypeSystem) lt
                                                    ltn = listVTypeToType $ mapListVType (nonpolarToNegative @QTypeSystem) lt
                                                    expr =
                                                        qConstValue
                                                            $ MkSomeOf (qFunctionPosWitnesses ltn ctfpos)
                                                            $ encode codec
                                                            . listProductToVProduct lt
                                                    pc =
                                                        toPatternConstructor ctfneg ltp
                                                            $ ImpureFunction
                                                            $ pure
                                                            $ fmap listVProductToProduct
                                                            . decode codec
                                                    in registerPatternConstructor ctfullname (ctDoc constructor) expr pc
                                                MkConstructorType _ (RecordCF _) lt -> let
                                                    recordcons = MkQRecordConstructor lt declpos declneg codec
                                                    in registerRecordConstructor ctfullname (ctDoc constructor) recordcons
                                    constructorBox ::
                                        Constructor dv maintype extra ->
                                        QFixBox
                                            ( QGroundType dv maintype
                                            , decltype -> TypeData dv maintype
                                            , x
                                            , CCRVariancesMap dv maintype
                                            , ConstructorCodec decltype
                                            )
                                            ()
                                    constructorBox constructor = mkConstructFixBox $ registerConstructor constructor
                                    subtypeRegister ::
                                        TypeData dv maintype ->
                                        (QGroundType dv maintype, decltype -> TypeData dv maintype, x) ->
                                        QScopeBuilder ()
                                    subtypeRegister tdata (~(mainGroundType, picktype, x)) = do
                                        gttid <- builderLift $ mkgt x
                                        let
                                            subGroundType :: QGroundType dv maintype
                                            subGroundType = getGroundType mainGroundType picktype gttid tdata
                                        registerGroundType (tdName tdata) (tdDoc tdata) subGroundType
                                        for_ (tdSupertype tdata) $ \supertdata -> let
                                            superGroundType :: QGroundType dv maintype
                                            superGroundType = getGroundType mainGroundType picktype gttid supertdata
                                            in registerSubtypeConversion
                                                $ MkSubtypeConversionEntry
                                                    Verify
                                                    subGroundType
                                                    superGroundType
                                                    identitySubtypeConversion
                                    subtypeBox ::
                                        TypeData dv maintype ->
                                        QFixBox (QGroundType dv maintype, decltype -> TypeData dv maintype, x) ()
                                    subtypeBox tdata = mkRegisterFixBox $ subtypeRegister tdata
                                    supertypeBox ::
                                        forall.
                                        (Int, Some (QGroundType '[])) ->
                                        QFixBox
                                            ( QGroundType dv maintype
                                            , CCRVariancesMap dv maintype
                                            , FixedList conscount (ConstructorCodec decltype)
                                            )
                                            ()
                                    supertypeBox (i, MkSome (supergroundtype :: QGroundType '[] supertype)) =
                                        mkRegisterFixBox $ \(~(mainGroundType, dvm, codecs)) -> do
                                            let
                                                getConstypeConvert ::
                                                    forall t.
                                                    ConstructorType t ->
                                                    QInterpreter ((Name, Name), QOpenExpression (t -> supertype))
                                                getConstypeConvert (MkConstructorType _ PositionalCF _) =
                                                    throw DeclareDatatypePositionalConstructorWithSupertypeError
                                                getConstypeConvert (MkConstructorType consname (RecordCF rcds) sigs) = let
                                                    rcd = unsafeIndex rcds i
                                                    in case rcdRecordConstructor rcd of
                                                        MkQRecordConstructor stmembers (MkShimWit (MkDolanGroundedType sgt NilCCRArguments) (MkPolarShim sgtconv)) _ stcodec
                                                            | Just Refl <- testEquality supergroundtype sgt -> do
                                                                memberconvexpr <- getMatchMemberConvert sigs stmembers
                                                                let
                                                                    convexpr =
                                                                        fmap
                                                                            ( \memberconv ->
                                                                                shimToFunction sgtconv
                                                                                    . encode stcodec
                                                                                    . memberconv
                                                                            )
                                                                            memberconvexpr
                                                                return ((consname, fnrName $ rcdName rcd), convexpr)
                                                        _ -> error $ "broken supertype in datatype: " <> show mainTypeName
                                                getCodecConvert ::
                                                    ConstructorCodec decltype ->
                                                    QInterpreter
                                                        ( (Name, Name)
                                                        , QOpenExpression (decltype -> Maybe supertype)
                                                        )
                                                getCodecConvert (MkSomeFor constype codec) = do
                                                    (chint, convexpr) <- getConstypeConvert constype
                                                    return $ (chint, fmap (\conv -> fmap conv . decode codec) convexpr)
                                            hintcconvexprs <- builderLift $ for (toList codecs) getCodecConvert
                                            let
                                                hint :: QSubtypeHint
                                                hint = mkQSubtypeHint $ fmap fst hintcconvexprs
                                                cconvexprs = fmap snd hintcconvexprs
                                                (_, mainGroundedType) = declTypes mainGroundType dvm
                                                superGroundedType =
                                                    mkShimWit $ MkDolanGroundedType supergroundtype NilCCRArguments
                                                joinConvs :: forall a b. [a -> Maybe b] -> a -> b
                                                joinConvs convs a =
                                                    fromMaybe (error $ "broken datatype: " <> show mainTypeName)
                                                        $ choice
                                                        $ fmap (\amb -> amb a) convs
                                                conversion :: QOpenExpression (decltype -> supertype)
                                                conversion = fmap joinConvs $ sequenceA cconvexprs
                                            registerSubtypeConversion
                                                $ subtypeConversionEntry Verify (Just hint) mainGroundedType superGroundedType
                                                $ fmap (functionToShim "supertype") conversion
                                    in return
                                        $ proc () -> do
                                            (x, dvm, codecs, picktype) <- mainBox -< ()
                                            mainGroundType <- mainTypeBox -< x
                                            concatmap subtypeBox subtypeDatas -< (mainGroundType, picktype, x)
                                            for_ (zip [0 ..] supertypes) supertypeBox -< (mainGroundType, dvm, codecs)
                                            fixedListArrowSequence_ (fmap constructorBox constructorFixedList)
                                                -<
                                                    fmap (\codec -> (mainGroundType, picktype, x, dvm, codec)) codecs

makeDataTypeBox ::
    forall extra.
    (?interpretExpression :: SyntaxExpression -> QInterpreter QExpression) =>
    GroundTypeMaker extra ->
    Bool ->
    [Some (QGroundType '[])] ->
    FullName ->
    RawMarkdown ->
    [SyntaxTypeParameter] ->
    [SyntaxWithDoc (SyntaxConstructorOrSubtype extra)] ->
    QInterpreter (QFixBox () ())
makeDataTypeBox gmaker storable supertypes name md params syntaxConstructorList =
    case getAnyCCRTypeParams params of
        (doubleParams, MkAnyCCRTypeParams gtparams) ->
            makeBox
                gmaker
                supertypes
                (MkTypeInfo name storable params Nothing md)
                syntaxConstructorList
                doubleParams
                gtparams

makePlainGroundType ::
    forall (dv :: CCRVariances) (gt :: CCRVariancesKind dv) (decltype :: Type).
    Is CCRVariancesType dv =>
    FullName ->
    CCRTypeParams dv gt decltype ->
    TypeConstruction dv gt [(ConstructorCodec decltype, ())]
makePlainGroundType _ tparams = let
    dvt :: CCRVariancesType dv
    dvt = ccrArgumentsType tparams
    mkx :: CCRVariancesMap dv gt -> [(ConstructorCodec decltype, ())] -> QInterpreter (CCRVariancesMap dv gt)
    mkx dvm _ = return dvm
    mkgt :: CCRVariancesMap dv gt -> QInterpreter (GroundTypeFromTypeID dv gt ())
    mkgt dvm =
        return
            $ MkGroundTypeFromTypeID
            $ \name mainFamType -> let
                gt =
                    MkQGroundType
                        { qgtVarianceType = dvt
                        , qgtVarianceMap = lazyCCRVariancesMap dvt dvm
                        , qgtShowType = standardListTypeExprShow @dv $ showNamedText name
                        , qgtFamilyType = mainFamType
                        , qgtSubtypeGroup = Nothing
                        , qgtProperties = mempty
                        , qgtGreatestDynamicSupertype = nullPolyGreatestDynamicSupertype
                        }
                in (gt, ())
    in MkTypeConstruction mkx mkgt $ \_ _ -> return ()

makePlainDataTypeBox ::
    (?interpretExpression :: SyntaxExpression -> QInterpreter QExpression) =>
    [Some (QGroundType '[])] ->
    FullName ->
    RawMarkdown ->
    [SyntaxTypeParameter] ->
    [SyntaxWithDoc SyntaxPlainDatatypeConstructorOrSubtype] ->
    QInterpreter (QFixBox () ())
makePlainDataTypeBox = makeDataTypeBox makePlainGroundType False
