module Pinafore.Language.Grammar.Interpret.TypeDecl.Data
    ( ConstructorCodec
    , Stages(..)
    , GroundTypeFromTypeID(..)
    , ConstructorData(..)
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

data ConstructorData extra = MkConstructorData
    { cdName :: Name
    , cdDoc :: Markdown
    , cdType :: Maybe Name
    , cdExtra :: extra
    }

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
                                                                  , [(ConstructorCodec decltype, extra)]) (GroundTypeFromTypeID dv gt)

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
    , tdSubtypes :: [MatchingTypeID dv t] -- includes self
    , tdName :: Name
    , tdDoc :: Markdown
    }

getConsSubtypeData ::
       forall (dv :: DolanVariance) (t :: DolanVarianceKind dv) extra.
       MatchingTypeID dv t
    -> SyntaxWithDoc (SyntaxConstructorOrSubtype extra)
    -> PinaforeInterpreter [TypeData dv t]
getConsSubtypeData _ (MkSyntaxWithDoc _ (ConstructorSyntaxConstructorOrSubtype _ _ _)) = return mempty
getConsSubtypeData (superMTID :: _ supertype) (MkSyntaxWithDoc doc (SubtypeSyntaxConstructorOrSubtype tname conss)) = do
    ssubtid <- newMatchingTypeID @dv
    case ssubtid of
        MkSome subMTID@(MkMatchingTypeID subTypeID) -> do
            Refl <- unsafeIdentify @_ @supertype subTypeID
            subtdata <- getConssSubtypeData subMTID conss
            let subtypes = superMTID : mconcat (fmap tdSubtypes subtdata)
            return $ (MkTypeData subMTID subtypes tname doc) : subtdata

getConssSubtypeData ::
       MatchingTypeID dv t -> [SyntaxWithDoc (SyntaxConstructorOrSubtype extra)] -> PinaforeInterpreter [TypeData dv t]
getConssSubtypeData stid conss = do
    tdatas <- for conss $ getConsSubtypeData stid
    return $ mconcat tdatas

data Constructor dv t extra = MkConstructor
    { ctName :: Name
    , ctDoc :: Markdown
    , ctOuterType :: MatchingTypeID dv t
    , ctInnerTypes :: [SyntaxType]
    , ctExtra :: extra
    }

typeDataLookup :: [TypeData dv t] -> Name -> PinaforeInterpreter (MatchingTypeID dv t)
typeDataLookup [] _ = liftIO $ fail "type name not found"
typeDataLookup (t:_) name
    | tdName t == name = return $ tdID t
typeDataLookup (_:tt) name = typeDataLookup tt name

getConstructor ::
       [TypeData dv t]
    -> MatchingTypeID dv t
    -> SyntaxWithDoc (SyntaxConstructorOrSubtype extra)
    -> PinaforeInterpreter [Constructor dv t extra]
getConstructor _ typeMTID (MkSyntaxWithDoc doc (ConstructorSyntaxConstructorOrSubtype consName stypes extra)) =
    return $ pure $ MkConstructor consName doc typeMTID stypes extra
getConstructor tdata _ (MkSyntaxWithDoc _ (SubtypeSyntaxConstructorOrSubtype subtypeName stypes)) = do
    subtypeMTID <- typeDataLookup tdata subtypeName
    getConstructors tdata subtypeMTID stypes

getConstructors ::
       [TypeData dv t]
    -> MatchingTypeID dv t
    -> [SyntaxWithDoc (SyntaxConstructorOrSubtype extra)]
    -> PinaforeInterpreter [Constructor dv t extra]
getConstructors tdata typeMTID syntaxConstructorList =
    fmap mconcat $ for syntaxConstructorList $ getConstructor tdata typeMTID

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
            subtypeDatas <- getConssSubtypeData mainMTID syntaxConstructorList
            constructorList <- getConstructors subtypeDatas mainMTID syntaxConstructorList
            fixedFromList constructorList $ \(constructorCount :: _ conscount) constructorFixedList ->
                withRepresentative constructorCount $
                case gtparams @maintype of
                    MkSome (tparams :: CCRTypeParams dv maintype decltype) ->
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
                                                                  , DolanVarianceMap dv maintype
                                                                  , FixedList conscount (ConstructorCodec decltype)
                                                                  , decltype -> MatchingTypeID dv maintype))
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
                                            mkx (dvm, toList $ liftA2 (,) codecs $ fmap ctExtra constructorFixedList)
                                        return $ (x, (x, dvm, codecs, \t -> ctOuterType $ pickn t constructorFixedList))
                                mainBox ::
                                       PinaforeFixBox () ( x
                                                         , DolanVarianceMap dv maintype
                                                         , FixedList conscount (ConstructorCodec decltype)
                                                         , decltype -> MatchingTypeID dv maintype)
                                mainBox = mkFixBox mainRegister mainConstruct
                                mainTypeBox :: PinaforeFixBox x (PinaforeGroundType dv maintype)
                                mainTypeBox =
                                    mkConstructFixBox $ \x -> do
                                        gtft <- lift $ mkgt x
                                        return $ unGroundTypeFromTypeID gtft mainTypeName mainTypeID
                                registerConstructor ::
                                       PinaforeGroundType dv maintype
                                    -> DolanVarianceMap dv maintype
                                    -> Constructor dv t extra
                                    -> ConstructorCodec decltype
                                    -> PinaforeScopeInterpreter ()
                                registerConstructor groundType dvm constructor (MkSomeFor (MkListProductType lt) codec) = let
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
                                    in case (getargs @'Positive, getargs @'Negative) of
                                           (MkShimWit posargs posconv, MkShimWit negargs negconv) -> do
                                               let
                                                   ctfpos :: PinaforeShimWit 'Positive decltype
                                                   ctfpos =
                                                       mapShimWit posconv $
                                                       singleDolanShimWit $
                                                       mkPolarShimWit $ GroundedDolanSingularType groundType posargs
                                                   ctfneg :: PinaforeShimWit 'Negative decltype
                                                   ctfneg =
                                                       mapShimWit negconv $
                                                       singleDolanShimWit $
                                                       mkPolarShimWit $ GroundedDolanSingularType groundType negargs
                                               ltp <- return $ mapListType nonpolarToDolanType lt
                                               ltn <- return $ mapListType nonpolarToDolanType lt
                                               let
                                                   expr =
                                                       qConstExprAny $
                                                       MkSomeOf (qFunctionPosWitnesses ltn ctfpos) $ encode codec
                                                   pc = toPatternConstructor ctfneg ltp $ decode codec
                                               registerPatternConstructor
                                                   (ctName constructor)
                                                   (ctDoc constructor)
                                                   expr
                                                   pc
                                constructorBox ::
                                       Constructor dv t extra
                                    -> PinaforeFixBox ( PinaforeGroundType dv maintype
                                                      , DolanVarianceMap dv maintype
                                                      , ConstructorCodec decltype) ()
                                constructorBox constructor =
                                    mkConstructFixBox $ \(groundType, dvm, codec) ->
                                        registerConstructor groundType dvm constructor codec
                                subRegister ::
                                       TypeData dv maintype
                                    -> (PinaforeGroundType dv maintype, decltype -> Bool, x)
                                    -> PinaforeScopeInterpreter ()
                                subRegister tdata (~(mainGroundType, match, x)) = do
                                    MkGroundTypeFromTypeID gttid <- lift $ mkgt x
                                    case tdID tdata of
                                        MkMatchingTypeID (subTypeID :: _ subtid) -> do
                                            let
                                                subGroundType :: PinaforeGroundType dv (Identified subtid)
                                                subGroundType =
                                                    (gttid (tdName tdata) subTypeID)
                                                        { pgtGreatestDynamicSupertype =
                                                              GeneralPolyGreatestDynamicSupertype $ \(args :: _ argstype) ->
                                                                  case unsafeRefl @Type @decltype @argstype of
                                                                      Refl ->
                                                                          Just $
                                                                          MkShimWit
                                                                              (GroundedDolanSingularType
                                                                                   mainGroundType
                                                                                   args) $
                                                                          MkPolarMap $
                                                                          functionToShim "supertype" $ \t ->
                                                                              if match t
                                                                                  then Just t
                                                                                  else Nothing
                                                        }
                                            registerType (tdName tdata) (tdDoc tdata) subGroundType
                                            registerSubtypeConversion $
                                                simpleSubtypeConversionEntry
                                                    subGroundType
                                                    mainGroundType
                                                    idSubtypeConversion
                                subConstruct ::
                                       TypeData dv maintype
                                    -> (PinaforeGroundType dv maintype, decltype -> MatchingTypeID dv maintype, x)
                                    -> PinaforeScopeInterpreter ( (PinaforeGroundType dv maintype, decltype -> Bool, x)
                                                                , ())
                                subConstruct tdata (mainGroundType, picktype, x) = do
                                    let
                                        match :: decltype -> Bool
                                        match t = elem (picktype t) (tdSubtypes tdata)
                                    return ((mainGroundType, match, x), ())
                                subBox ::
                                       TypeData dv maintype
                                    -> PinaforeFixBox ( PinaforeGroundType dv maintype
                                                      , decltype -> MatchingTypeID dv maintype
                                                      , x) ()
                                subBox tdata = mkFixBox (subRegister tdata) (subConstruct tdata)
                                in return $
                                   proc () -> do
                                       (x, dvm, codecs, picktype) <- mainBox -< ()
                                       mainGroundType <- mainTypeBox -< x
                                       mconcat (fmap subBox subtypeDatas) -< (mainGroundType, picktype, x)
                                       fixedListArrowSequence_ (fmap constructorBox constructorFixedList) -<
                                           fmap (\codec -> (mainGroundType, dvm, codec)) codecs

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
    -> Stages (DolanVarianceMap dv gt, [(ConstructorCodec decltype, ())]) (GroundTypeFromTypeID dv gt)
makeDataGroundType _ tparams = let
    dvt :: DolanVarianceType dv
    dvt = ccrArgumentsType tparams
    mkx :: (DolanVarianceMap dv gt, [(ConstructorCodec decltype, ())]) -> PinaforeInterpreter (DolanVarianceMap dv gt)
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
    -> [SyntaxWithDoc SyntaxDatatypeConstructorOrSubtype]
    -> PinaforeInterpreter (PinaforeFixBox () ())
makeDataTypeBox = makeDeclTypeBox makeDataGroundType
