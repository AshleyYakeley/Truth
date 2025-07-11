module Pinafore.Language.Interpret.TypeDecl.StorableData
    ( makeStorableDataTypeBox
    )
where

import Import
import Pinafore.Language.Error
import Pinafore.Language.Interpret.TypeDecl.Data
import Pinafore.Language.Interpret.TypeDecl.Parameter
import Pinafore.Language.Interpret.TypeDecl.Storage
import Pinafore.Language.Interpreter
import Pinafore.Language.Type

data Thing (x :: Type) (decltype :: Type) (ta :: Type)
    = MkThing
        x
        (decltype :~: ta)

makeConstructorAdapter ::
    CovParams dv gt decltype ->
    ListType QNonpolarType lt ->
    QInterpreter (QExprRec (WithStoreAdapterArgs gt (Thing (ListType StoreAdapter lt) decltype)))
makeConstructorAdapter params pts = do
    etsexpr <- getCompose $ mapMListType (Compose . nonpolarToStoreAdapter params) pts
    return
        $ fmap
            (\ets -> assignWithStoreAdapterArgs params $ \args -> MkThing (mapListType (\(Compose f) -> f args) ets) Refl)
            etsexpr

makeTypeAdapter ::
    CovParams dv gt decltype ->
    [(ConstructorCodec decltype, Anchor)] ->
    QInterpreter (QExprRec (WithStoreAdapterArgs gt StoreAdapter))
makeTypeAdapter params conss = do
    ffexpr :: [QExprRec (WithStoreAdapterArgs gt (Compose Endo StoreAdapter))] <-
        for conss $ \case
            (MkSomeFor (MkConstructorType _ PositionalCF cc) codec, anchor) -> do
                caexpr <- makeConstructorAdapter params $ listVTypeToType cc
                return
                    $ fmap
                        ( mapAllFor $ \(MkThing tt Refl) -> let
                            vcodec = invmap listVProductToProduct (listProductToVProduct $ listTypeToVType tt) codec
                            in Compose $ Endo $ codecSum vcodec $ constructorStoreAdapter anchor tt
                        )
                        caexpr
            (MkSomeFor (MkConstructorType _ (RecordCF _) _) _, _) -> throw InterpretTypeDeclTypeStorableRecord
    return
        $ fmap (\ff -> MkAllFor $ \args -> appEndo (concatmap (\(MkAllFor f) -> getCompose $ f args) ff) nullStoreAdapter)
        $ sequenceA ffexpr

makeStorableGroundType ::
    forall (dv :: CCRVariances) (gt :: CCRVariancesKind dv) (decltype :: Type).
    Is CCRVariancesType dv =>
    FullName ->
    CCRTypeParams dv gt decltype ->
    TypeConstruction dv gt [(ConstructorCodec decltype, Anchor)]
makeStorableGroundType mainTypeName tparams = let
    dvt = ccrArgumentsType tparams
    mkx ::
        CCRVariancesMap dv gt ->
        [(ConstructorCodec decltype, Anchor)] ->
        QInterpreter (CCRVariancesMap dv gt, QExprRec (WithStoreAdapterArgs gt StoreAdapter))
    mkx dvm conss = do
        cvt <-
            case ccrVariancesToCovaryType dvt of
                Just cvt -> return cvt
                Nothing -> throw $ InterpretTypeDeclTypeVariableNotCovariantError mainTypeName
        let cparams = paramsToCovParams cvt tparams
        adapterexpr <- makeTypeAdapter cparams conss
        return (dvm, adapterexpr)
    mkgt ::
        (CCRVariancesMap dv gt, QExprRec (WithStoreAdapterArgs gt StoreAdapter)) ->
        QInterpreter (GroundTypeFromTypeID dv gt (Storability dv gt))
    mkgt ~(dvm, adapterexpr) = do
        wit <- liftIO newIOWitness
        cvt <-
            case ccrVariancesToCovaryType dvt of
                Just cvt -> return cvt
                Nothing -> throw $ InterpretTypeDeclTypeVariableNotCovariantError mainTypeName
        return
            $ MkGroundTypeFromTypeID
            $ \subTypeName famType -> let
                stbKind :: CovaryType dv
                stbKind = cvt
                stbCovaryMap = ccrVariancesMapToCovary cvt $ lazyCCRVariancesMap dvt dvm
                showType = standardListTypeExprShow @dv $ exprShow subTypeName
                stbAdapterExprKnot = knotAppRec wit adapterexpr
                storability :: Storability dv gt
                storability = MkStorability{..}
                gt :: QGroundType dv gt
                gt =
                    MkQGroundType
                        { qgtVarianceType = covaryToCCRVariancesType stbKind
                        , qgtVarianceMap = covaryToCCRVariancesMap stbKind stbCovaryMap
                        , qgtShowType = showType
                        , qgtFamilyType = famType
                        , qgtSubtypeGroup = Nothing
                        , qgtProperties = singleGroundProperty storabilityProperty storability
                        , qgtGreatestDynamicSupertype = nullPolyGreatestDynamicSupertype
                        }
                in (gt, storability)
    postregister :: QGroundType dv gt -> Storability dv gt -> QScopeBuilder ()
    postregister gt storability = do
        sce <-
            builderLift
                $ storabilitySaturatedAdapter
                    (typeToDolan $ MkDolanGroundedType entityGroundType NilCCRArguments)
                    (pure plainStoreAdapter)
                    storability
                $ \(MkShimWit args conv) eatexpr ->
                    return
                        $ subtypeConversionEntry
                            TrustMe
                            Nothing
                            (MkShimWit (MkDolanGroundedType gt args) conv)
                            (mkShimWit $ MkDolanGroundedType entityGroundType NilCCRArguments)
                        $ fmap (functionToShim "datatype-storable" . storeAdapterConvert) eatexpr
        registerSubtypeConversion sce
    in MkTypeConstruction mkx mkgt postregister

makeStorableDataTypeBox ::
    (?interpretExpression :: SyntaxExpression -> QInterpreter QExpression) =>
    FullName ->
    RawMarkdown ->
    [SyntaxTypeParameter] ->
    [SyntaxWithDoc SyntaxStorableDatatypeConstructorOrSubtype] ->
    QInterpreter (QFixBox () ())
makeStorableDataTypeBox = makeDataTypeBox makeStorableGroundType True []
