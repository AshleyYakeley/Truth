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

import Pinafore.Language.DefDoc
import Pinafore.Language.Error
import Pinafore.Language.Expression
import Pinafore.Language.Grammar.Interpret.Type
import Pinafore.Language.Grammar.Interpret.TypeDecl.Parameter
import Pinafore.Language.Grammar.Interpret.TypeDecl.Representation
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.Grammar.SyntaxDoc
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Shim
import Pinafore.Language.Type
import Pinafore.Text
import Shapes
import Shapes.Unsafe (unsafeGetRefl, unsafeRefl)

data ConstructorFlavour (w :: Type -> Type) where
    PositionalCF :: ConstructorFlavour QNonpolarType
    RecordCF :: [RecordConstructorData] -> ConstructorFlavour (QSignature 'Positive)

data ConstructorType (t :: Type) where
    MkConstructorType :: Name -> ConstructorFlavour w -> ListVType w tt -> ConstructorType (ListVProduct tt)

instance MaybeUnitWitness ConstructorType where
    maybeUnitWitness (MkConstructorType _ _ (MkListVType t :: ListVType w tt)) = do
        Refl <- listVectorIsEmpty t
        case emptyMapTypeRefl @Type @Type @w @tt of
            Refl -> return Dict

instance HasVarMapping ConstructorType where
    getVarMapping (MkConstructorType _ PositionalCF tt) = getVarMapping $ MkListVProductType tt
    getVarMapping (MkConstructorType _ (RecordCF _) tt) = getVarMapping $ MkListVProductType tt

type ConstructorCodec t = SomeFor (Codec t) ConstructorType

constructorTypeFreeVariables :: ConstructorFlavour w -> w t -> FiniteSet SomeTypeVarT
constructorTypeFreeVariables PositionalCF wt = freeTypeVariables wt
constructorTypeFreeVariables (RecordCF _) _ = mempty

instance FreeTypeVariables (ConstructorCodec t) where
    freeTypeVariables (MkSomeFor (MkConstructorType _ cf w) _) =
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
withCCRTypeParam (PositiveSyntaxTypeParameter n) cont = nameToTypeVarT n $ \v -> cont $ CoCCRTypeParam v
withCCRTypeParam (NegativeSyntaxTypeParameter n) cont = nameToTypeVarT n $ \v -> cont $ ContraCCRTypeParam v
withCCRTypeParam (RangeSyntaxTypeParameter np nq) cont =
    nameToTypeVarT np $ \vp -> nameToTypeVarT nq $ \vq -> cont $ RangeCCRTypeParam vp vq

tParamToPolarArgument ::
       forall sv (t :: CCRVarianceKind sv) polarity. Is PolarityType polarity
    => CCRTypeParam sv t
    -> CCRPolarArgumentShimWit (QPolyShim Type) QType polarity sv t
tParamToPolarArgument (CoCCRTypeParam var) =
    case shimWitToDolan $ mkShimWit $ VarDolanSingularType var of
        MkShimWit arg conv -> MkShimWit (CoCCRPolarArgument arg) conv
tParamToPolarArgument (ContraCCRTypeParam var) =
    withInvertPolarity @polarity $
    case shimWitToDolan $ mkShimWit $ VarDolanSingularType var of
        MkShimWit arg conv -> MkShimWit (ContraCCRPolarArgument arg) $ MkCatDual $ uninvertPolarMap conv
tParamToPolarArgument (RangeCCRTypeParam varp varq) =
    withInvertPolarity @polarity $
    case ( shimWitToDolan $ mkShimWit $ VarDolanSingularType varp
         , shimWitToDolan $ mkShimWit $ VarDolanSingularType varq) of
        (MkShimWit argp convp, MkShimWit argq convq) ->
            MkShimWit (RangeCCRPolarArgument argp argq) $ MkCatRange (uninvertPolarMap convp) convq

type GenCCRTypeParams dv = forall (gt :: DolanVarianceKind dv). Some (CCRTypeParams dv gt)

data AnyCCRTypeParams where
    MkAnyCCRTypeParams :: forall (dv :: DolanVariance). GenCCRTypeParams dv -> AnyCCRTypeParams

tParamsVars :: CCRTypeParams dv gt t -> [SomeTypeVarT]
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
       forall v tv t. FullName -> VarianceType v -> TypeVarT tv -> VarMapping t -> QInterpreter (Mapping tv t)
getDataTypeMappingOrError tname vt var vm =
    case runVarMapping vm vt var of
        Nothing -> throw $ InterpretTypeDeclTypeVariableWrongPolarityError tname $ typeVarToName var
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
                                   (QGroundType dv gt -> y -> QScopeBuilder ())

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

data TypeInfo = MkTypeInfo
    { tiName :: FullName
    , tiStorable :: Bool
    , tiParams :: [SyntaxTypeParameter]
    , tiDescription :: RawMarkdown
    }

tiDoc :: TypeInfo -> DefDoc
tiDoc MkTypeInfo {..} = MkDefDoc (typeDocItem tiName tiStorable tiParams) tiDescription

data TypeData dv t = MkTypeData
    { tdInfo :: TypeInfo
    , tdID :: MatchingTypeID dv t
    , tdSupertype :: Maybe (TypeData dv t)
    , tdSubtypes :: [MatchingTypeID dv t] -- includes self
    }

tdName :: TypeData dv t -> FullName
tdName = tiName . tdInfo

tdDoc :: TypeData dv t -> DefDoc
tdDoc = tiDoc . tdInfo

getConsSubtypeData ::
       forall (dv :: DolanVariance) (t :: DolanVarianceKind dv) extra.
       TypeData dv t
    -> SyntaxWithDoc (SyntaxConstructorOrSubtype extra)
    -> QInterpreter [TypeData dv t]
getConsSubtypeData superTD (MkSyntaxWithDoc md (SubtypeSyntaxConstructorOrSubtype tname conss)) = do
    ssubtid <- newMatchingTypeID @dv
    case ssubtid of
        MkSome subMTID@(MkMatchingTypeID subTypeID) -> do
            Refl <- unsafeIdentify @_ @t subTypeID
            rec
                let
                    superInfo :: TypeInfo
                    superInfo = tdInfo superTD
                    subInfo :: TypeInfo
                    subInfo =
                        MkTypeInfo
                            { tiName = tname
                            , tiStorable = tiStorable superInfo
                            , tiParams = tiParams superInfo
                            , tiDescription = md
                            }
                    subtypes = tdID superTD : mconcat (fmap tdSubtypes subtdata)
                    subTD :: TypeData dv t
                    subTD =
                        MkTypeData {tdInfo = subInfo, tdID = subMTID, tdSupertype = Just superTD, tdSubtypes = subtypes}
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
    , ctDoc :: DefDoc
    , ctOuterType :: TypeData dv t
    , ctContents :: Either [SyntaxType] [SyntaxSignature]
    , ctExtra :: extra
    }

typeDataLookup :: [TypeData dv t] -> FullName -> QInterpreter (TypeData dv t)
typeDataLookup [] _ = liftIO $ fail "type name not found"
typeDataLookup (t:_) name
    | tdName t == name = return t
typeDataLookup (_:tt) name = typeDataLookup tt name

getConstructor :: TypeData dv t -> FullName -> DefDoc -> SyntaxDataConstructor extra -> Constructor dv t extra
getConstructor typeNM consName doc (PlainSyntaxConstructor stypes extra) =
    MkConstructor consName doc typeNM (Left stypes) extra
getConstructor typeNM consName doc (RecordSyntaxConstructor sigs) =
    MkConstructor consName doc typeNM (Right sigs) $ error "record extra data"

getConstructorOrSubtype ::
       [TypeData dv t]
    -> TypeData dv t
    -> SyntaxWithDoc (SyntaxConstructorOrSubtype extra)
    -> QInterpreter [Constructor dv t extra]
getConstructorOrSubtype _ typeNM (MkSyntaxWithDoc md (ConstructorSyntaxConstructorOrSubtype consName dcons)) =
    return $
    pure $ getConstructor typeNM consName (MkDefDoc (constructorDocItem (tdName typeNM) consName dcons) md) dcons
getConstructorOrSubtype tdata _ (MkSyntaxWithDoc _ (SubtypeSyntaxConstructorOrSubtype subtypeName stypes)) = do
    subtypeTD <- typeDataLookup tdata subtypeName
    getConstructors tdata subtypeTD stypes

getConstructors ::
       [TypeData dv t]
    -> TypeData dv t
    -> [SyntaxWithDoc (SyntaxConstructorOrSubtype extra)]
    -> QInterpreter [Constructor dv t extra]
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
rcdSignatures MkRecordConstructorData {..} =
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
    return MkRecordConstructorData {..}

interpretSignature' ::
       (?interpretExpression :: SyntaxExpression -> QInterpreter QExpression)
    => TypeID
    -> [Some (QGroundType '[])]
    -> SyntaxSignature'
    -> QInterpreter (Maybe RecordConstructorData, [SomeSignature])
interpretSignature' tid _ (ValueSyntaxSignature name stype msdefv) = do
    aqtype <- interpretType stype
    case aqtype of
        MkSome qtype -> do
            modefv <-
                for msdefv $ \sdefv -> do
                    defv <- ?interpretExpression sdefv
                    typedSubsumeExpressionToOpen mempty qtype defv
            return $ (Nothing, pure $ MkSome $ ValueSignature tid name qtype modefv)
interpretSignature' _ supertypes (SupertypeConstructorSyntaxSignature name) = do
    rcd <- lookupRecordConstructorData supertypes name
    return $ (Just rcd, rcdSignatures rcd)

interpretSignature ::
       (?interpretExpression :: SyntaxExpression -> QInterpreter QExpression)
    => TypeID
    -> [Some (QGroundType '[])]
    -> SyntaxSignature
    -> QInterpreter (Maybe RecordConstructorData, [SomeSignature])
interpretSignature tid supertypes (MkSyntaxWithDoc _ (MkWithSourcePos _ ssig)) = interpretSignature' tid supertypes ssig

matchSigName :: SomeSignature -> SomeSignature -> Bool
matchSigName (MkSome (ValueSignature _ na _ _)) (MkSome (ValueSignature _ nb _ _)) = na == nb

checkDuplicates :: (Name -> ErrorType) -> [Name] -> QInterpreter ()
checkDuplicates _ [] = return ()
checkDuplicates f (a:aa) =
    if elem a aa
        then throw $ f a
        else checkDuplicates f aa

signatureName :: SyntaxSignature -> Maybe Name
signatureName (MkSyntaxWithDoc _ (MkWithSourcePos _ (ValueSyntaxSignature n _ _))) = Just n
signatureName _ = Nothing

someSignatureName :: SomeSignature -> Maybe Name
someSignatureName (MkSome (ValueSignature _ n _ _)) = Just n

interpretConstructorTypes ::
       (?interpretExpression :: SyntaxExpression -> QInterpreter QExpression)
    => TypeID
    -> [Some (QGroundType '[])]
    -> Constructor dv t extra
    -> QInterpreter (Some ConstructorType)
interpretConstructorTypes tid supertypes c = let
    consname = fnName $ ctName c
    in paramWith currentNamespaceParam (fnCompanion $ tdName $ ctOuterType c) $
       case ctContents c of
           Left innerTypes -> do
               etypes <- for innerTypes interpretNonpolarType
               case assembleListVType $ fromList etypes of
                   MkSome npts -> return $ MkSome $ MkConstructorType consname PositionalCF npts
           Right sigs -> do
               checkDuplicates DeclareDatatypeDuplicateMembers $ mapMaybe signatureName sigs
               rcdsigss <- for sigs $ interpretSignature tid supertypes
               let
                   allSigs :: [SomeSignature]
                   allSigs = mconcat $ fmap snd rcdsigss
                   splitSigs :: SomeSignature -> ([SomeSignature], [SomeSignature])
                   splitSigs sig@(MkSome (ValueSignature sigtid _ _ _)) =
                       if sigtid == tid
                           then ([sig], [])
                           else ([], [sig])
                   (thisQSigs, supertypeQSigs) = mconcat $ fmap splitSigs allSigs
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
                               throw $
                               DeclareDatatypeMultipleSupertypeConstructorsError (exprShow supertype) $
                               fmap exprShow rcds
               case assembleListVType $ fromList $ inheritedQSigs <> thisQSigs of
                   MkSome qsiglist -> return $ MkSome $ MkConstructorType consname (RecordCF rcds) qsiglist

pickMember ::
       QSignature 'Positive b
    -> ListType (QSignature 'Positive) aa
    -> QInterpreter (QOpenExpression (ListVProduct aa -> b))
pickMember (ValueSignature _ nb tb _) tt =
    case listTypeFind
             (\(MkPairType (ValueSignature _ na ta _) f) ->
                  if na == nb
                      then Just $ MkSomeFor ta f
                      else Nothing) $
         pairListType tt $ listVProductGetters tt of
        Just (MkSomeFor ta f) -> do
            convexpr <- tsSubsume @QTypeSystem (mkShimWit ta) tb
            return $ fmap (\conv -> shimToFunction conv . f) convexpr
        Nothing -> throw $ DeclareDatatypeMissingSupertypeMember nb

getMatchMemberConvert ::
       ListVType (QSignature 'Positive) aa
    -> ListVType (QSignature 'Positive) bb
    -> QInterpreter (QOpenExpression (ListVProduct aa -> ListVProduct bb))
getMatchMemberConvert ta tb =
    getCompose $
    getCompose $
    listVProductSequence $ mapListVType (\sig -> Compose $ Compose $ pickMember sig $ listVTypeToType ta) tb

makeBox ::
       forall extra (dv :: DolanVariance). (?interpretExpression :: SyntaxExpression -> QInterpreter QExpression)
    => GroundTypeMaker extra
    -> [Some (QGroundType '[])]
    -> TypeInfo
    -> [SyntaxWithDoc (SyntaxConstructorOrSubtype extra)]
    -> GenCCRTypeParams dv
    -> QInterpreter (QFixBox () ())
makeBox gmaker supertypes tinfo syntaxConstructorList gtparams = do
    stid <- newMatchingTypeID @dv
    case stid of
        MkSome (mainMTID@(MkMatchingTypeID mainTypeID) :: _ maintype) -> do
            rec
                let
                    mainTypeName = tiName tinfo
                    mainTypeData :: TypeData dv maintype
                    mainTypeData =
                        MkTypeData
                            { tdInfo = tinfo
                            , tdID = mainMTID
                            , tdSupertype = Nothing
                            , tdSubtypes = mainMTID : mconcat (fmap tdSubtypes subtypeDatas)
                            }
                subtypeDatas <- getConssSubtypeData mainTypeData syntaxConstructorList
            constructorList <- getConstructors subtypeDatas mainTypeData syntaxConstructorList
            fixedFromList constructorList $ \(constructorCount :: _ conscount) (constructorFixedList :: FixedList conscount (Constructor dv maintype extra)) ->
                withRepresentative constructorCount $
                case gtparams @maintype of
                    MkSome (tparams :: CCRTypeParams dv maintype decltype) ->
                        withRepresentative (ccrArgumentsType tparams) $
                        case gmaker mainTypeName tparams of
                            MkTypeConstruction mkx (mkgt :: x -> _ (_ y)) postregister -> let
                                mainRegister :: x -> QScopeBuilder ()
                                mainRegister x = do
                                    MkGroundTypeFromTypeID gttid <- builderLift $ mkgt x
                                    let (gt, y) = gttid mainTypeName mainTypeID
                                    registerType mainTypeName (tdDoc mainTypeData) gt
                                    postregister gt y
                                mainConstruct ::
                                       ()
                                    -> QScopeBuilder ( x
                                                     , ( x
                                                       , DolanVarianceMap dv maintype
                                                       , FixedList conscount (ConstructorCodec decltype)
                                                       , decltype -> TypeData dv maintype))
                                mainConstruct () = do
                                    constructorInnerTypes <-
                                        builderLift $
                                        for constructorFixedList $
                                        interpretConstructorTypes (witnessToValue mainTypeID) supertypes
                                    assembleDataType constructorInnerTypes $ \codecs (vmap :: VarMapping structtype) pickn -> do
                                        let
                                            freevars :: FiniteSet SomeTypeVarT
                                            freevars = mconcat $ fmap freeTypeVariables $ toList codecs
                                            declaredvars :: [SomeTypeVarT]
                                            declaredvars = tParamsVars tparams
                                            unboundvars :: [SomeTypeVarT]
                                            unboundvars = toList freevars \\ declaredvars
                                        case nonEmpty $ duplicates declaredvars of
                                            Nothing -> return ()
                                            Just vv ->
                                                builderLift $
                                                throw $
                                                InterpretTypeDeclDuplicateTypeVariablesError mainTypeName $
                                                fmap someTypeVarToName vv
                                        case nonEmpty unboundvars of
                                            Nothing -> return ()
                                            Just vv ->
                                                builderLift $
                                                throw $
                                                InterpretTypeDeclUnboundTypeVariablesError mainTypeName $
                                                fmap someTypeVarToName vv
                                        Refl <- unsafeGetRefl @Type @structtype @decltype
                                        dvm :: DolanVarianceMap dv maintype <-
                                            builderLift $ getDolanVarianceMap @dv mainTypeName tparams vmap
                                        x <-
                                            builderLift $
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
                                        gtft <- builderLift $ mkgt x
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
                                declTypes ::
                                       QGroundType dv maintype
                                    -> DolanVarianceMap dv maintype
                                    -> (QGroundedShimWit 'Positive decltype, QGroundedShimWit 'Negative decltype)
                                declTypes groundType dvm = let
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
                                    in case (getargs @'Positive, getargs @'Negative) of
                                           (MkShimWit posargs posconv, MkShimWit negargs negconv) -> let
                                               declpos :: QGroundedShimWit 'Positive decltype
                                               declpos = MkShimWit (MkDolanGroundedType groundType posargs) posconv
                                               declneg :: QGroundedShimWit 'Negative decltype
                                               declneg = MkShimWit (MkDolanGroundedType groundType negargs) negconv
                                               in (declpos, declneg)
                                registerConstructor ::
                                       Constructor dv maintype extra
                                    -> ( QGroundType dv maintype
                                       , decltype -> TypeData dv maintype
                                       , x
                                       , DolanVarianceMap dv maintype
                                       , ConstructorCodec decltype)
                                    -> QScopeBuilder ()
                                registerConstructor constructor (mainGroundType, picktype, x, dvm, MkSomeFor ctype codec) = do
                                    gttid <- builderLift $ mkgt x
                                    let
                                        groundType :: QGroundType dv maintype
                                        groundType =
                                            getGroundType mainGroundType picktype gttid $ ctOuterType constructor
                                        (declpos, declneg) = declTypes groundType dvm
                                        ctfullname = ctName constructor
                                        ctfpos :: QShimWit 'Positive decltype
                                        ctfpos = shimWitToDolan declpos
                                        ctfneg :: QShimWit 'Negative decltype
                                        ctfneg = shimWitToDolan declneg
                                        in case ctype of
                                               MkConstructorType _ PositionalCF lt -> let
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
                                               MkConstructorType _ (RecordCF _) lt -> let
                                                   recordcons = MkQRecordConstructor lt declpos declneg codec
                                                   in registerRecord ctfullname (ctDoc constructor) recordcons
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
                                    -> QScopeBuilder ()
                                subtypeRegister tdata (~(mainGroundType, picktype, x)) = do
                                    gttid <- builderLift $ mkgt x
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
                                supertypeBox ::
                                       forall .
                                       (Int, Some (QGroundType '[]))
                                    -> QFixBox ( QGroundType dv maintype
                                               , DolanVarianceMap dv maintype
                                               , FixedList conscount (ConstructorCodec decltype)) ()
                                supertypeBox (i, MkSome (supergroundtype :: QGroundType '[] supertype)) =
                                    mkRegisterFixBox $ \(~(mainGroundType, dvm, codecs)) -> do
                                        let
                                            getConstypeConvert ::
                                                   forall t.
                                                   ConstructorType t
                                                -> QScopeBuilder ((Name, Name), QOpenExpression (t -> supertype))
                                            getConstypeConvert (MkConstructorType _ PositionalCF _) =
                                                builderLift $
                                                throw DeclareDatatypePositionalConstructorWithSupertypeError
                                            getConstypeConvert (MkConstructorType consname (RecordCF rcds) sigs) = let
                                                rcd = unsafeIndex rcds i
                                                in case rcdRecordConstructor rcd of
                                                       MkQRecordConstructor stmembers (MkShimWit (MkDolanGroundedType sgt NilCCRArguments) (MkPolarMap sgtconv)) _ stcodec
                                                           | Just Refl <- testEquality supergroundtype sgt -> do
                                                               memberconvexpr <-
                                                                   builderLift $ getMatchMemberConvert sigs stmembers
                                                               let
                                                                   convexpr =
                                                                       fmap
                                                                           (\memberconv ->
                                                                                shimToFunction sgtconv .
                                                                                encode stcodec . memberconv)
                                                                           memberconvexpr
                                                               return ((consname, fnrName $ rcdName rcd), convexpr)
                                                       _ ->
                                                           error $ "broken supertype in datatype: " <> show mainTypeName
                                            getCodecConvert ::
                                                   ConstructorCodec decltype
                                                -> QScopeBuilder ( (Name, Name)
                                                                 , QOpenExpression (decltype -> Maybe supertype))
                                            getCodecConvert (MkSomeFor constype codec) = do
                                                (chint, convexpr) <- getConstypeConvert constype
                                                return $ (chint, fmap (\conv -> fmap conv . decode codec) convexpr)
                                        hintcconvexprs <- for (toList codecs) getCodecConvert
                                        let
                                            hint :: QSubtypeHint
                                            hint = mkQSubtypeHint $ fmap fst hintcconvexprs
                                            cconvexprs = fmap snd hintcconvexprs
                                            (_, mainGroundedType) = declTypes mainGroundType dvm
                                            superGroundedType =
                                                mkShimWit $ MkDolanGroundedType supergroundtype NilCCRArguments
                                            joinConvs :: forall a b. [a -> Maybe b] -> a -> b
                                            joinConvs convs a =
                                                fromMaybe (error $ "broken datatype: " <> show mainTypeName) $
                                                choice $ fmap (\amb -> amb a) convs
                                            conversion :: QOpenExpression (decltype -> supertype)
                                            conversion = fmap joinConvs $ sequenceA cconvexprs
                                        registerSubtypeConversion $
                                            subtypeConversionEntry Verify (Just hint) mainGroundedType superGroundedType $
                                            fmap (functionToShim "supertype") conversion
                                in return $
                                   proc () -> do
                                       (x, dvm, codecs, picktype) <- mainBox -< ()
                                       mainGroundType <- mainTypeBox -< x
                                       mconcat (fmap subtypeBox subtypeDatas) -< (mainGroundType, picktype, x)
                                       for_ (zip [0 ..] supertypes) supertypeBox -< (mainGroundType, dvm, codecs)
                                       fixedListArrowSequence_ (fmap constructorBox constructorFixedList) -<
                                           fmap (\codec -> (mainGroundType, picktype, x, dvm, codec)) codecs

makeDataTypeBox ::
       forall extra. (?interpretExpression :: SyntaxExpression -> QInterpreter QExpression)
    => GroundTypeMaker extra
    -> Bool
    -> [Some (QGroundType '[])]
    -> FullName
    -> RawMarkdown
    -> [SyntaxTypeParameter]
    -> [SyntaxWithDoc (SyntaxConstructorOrSubtype extra)]
    -> QInterpreter (QFixBox () ())
makeDataTypeBox gmaker storable supertypes name md params syntaxConstructorList =
    case getAnyCCRTypeParams params of
        MkAnyCCRTypeParams gtparams ->
            makeBox gmaker supertypes (MkTypeInfo name storable params md) syntaxConstructorList gtparams

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
                    , qgtShowType = standardListTypeExprShow @dv $ showNamedText name
                    , qgtFamilyType = MkFamilialType identifiedFamilyWitness $ MkIdentifiedTypeFamily mainTypeID
                    , qgtSubtypeGroup = Nothing
                    , qgtProperties = mempty
                    , qgtGreatestDynamicSupertype = nullPolyGreatestDynamicSupertype
                    }
            in (gt, ())
    in MkTypeConstruction mkx mkgt $ \_ _ -> return ()

makePlainDataTypeBox ::
       (?interpretExpression :: SyntaxExpression -> QInterpreter QExpression)
    => [Some (QGroundType '[])]
    -> FullName
    -> RawMarkdown
    -> [SyntaxTypeParameter]
    -> [SyntaxWithDoc SyntaxPlainDatatypeConstructorOrSubtype]
    -> QInterpreter (QFixBox () ())
makePlainDataTypeBox = makeDataTypeBox makePlainGroundType False
