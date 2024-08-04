module Pinafore.Language.Interpret.TypeDecl.StorableData
    ( makeStorableDataTypeBox
    ) where

import Import
import Pinafore.Language.Error
import Pinafore.Language.Interpret.TypeDecl.Data
import Pinafore.Language.Interpret.TypeDecl.Parameter
import Pinafore.Language.Interpret.TypeDecl.Storage
import Pinafore.Language.Interpreter
import Pinafore.Language.Type

type WithArgs :: forall k. (Type -> Type) -> k -> Type
newtype WithArgs f gt =
    MkWithArgs (forall (ta :: Type). Arguments StoreAdapter gt ta -> f ta)

assignArgumentParams ::
       forall (f :: Type -> Type) dv (gt :: CCRVariancesKind dv) (decltype :: Type) (ta :: Type).
       CovParams dv gt decltype
    -> Arguments f gt ta
    -> decltype :~: ta
assignArgumentParams NilCCRArguments NilArguments = Refl
assignArgumentParams (ConsCCRArguments (MkCovParam var) args) (ConsArguments arg args') =
    assignTypeVarWit var arg $
    case assignArgumentParams args args' of
        Refl -> Refl

data Thing x decltype ta =
    MkThing x
            (decltype :~: ta)

makeConstructorAdapter ::
       CovParams dv gt decltype
    -> ListType QNonpolarType lt
    -> QInterpreter (WithArgs (Thing (ListType StoreAdapter lt) decltype) gt)
makeConstructorAdapter params pts = do
    ets <- mapMListType (nonpolarToStoreAdapter params) pts
    return $
        MkWithArgs $ \args ->
            case assignArgumentParams params args of
                Refl -> MkThing (mapListType (\(Compose f) -> f args) ets) Refl

makeTypeAdapter ::
       CovParams dv gt decltype -> [(ConstructorCodec decltype, Anchor)] -> QInterpreter (WithArgs StoreAdapter gt)
makeTypeAdapter params conss = do
    ff <-
        for conss $ \case
            (MkSomeFor (MkConstructorType _ PositionalCF cc) codec, anchor) -> do
                MkWithArgs wa <- makeConstructorAdapter params $ listVTypeToType cc
                return $
                    MkWithArgs $ \args ->
                        case wa args of
                            MkThing tt Refl -> let
                                vcodec = invmap listVProductToProduct (listProductToVProduct $ listTypeToVType tt) codec
                                in Compose $ Endo $ codecSum vcodec $ constructorStoreAdapter anchor tt
            (MkSomeFor (MkConstructorType _ (RecordCF _) _) _, _) -> throw InterpretTypeDeclTypeStorableRecord
    return $ MkWithArgs $ \args -> appEndo (concatmap (\(MkWithArgs f) -> getCompose $ f args) ff) nullStoreAdapter

makeStorableGroundType ::
       forall (dv :: CCRVariances) (gt :: CCRVariancesKind dv) (decltype :: Type). Is CCRVariancesType dv
    => FullName
    -> CCRTypeParams dv gt decltype
    -> TypeConstruction dv gt [(ConstructorCodec decltype, Anchor)]
makeStorableGroundType mainTypeName tparams = let
    dvt = ccrArgumentsType tparams
    mkx :: CCRVariancesMap dv gt
        -> [(ConstructorCodec decltype, Anchor)]
        -> QInterpreter (CCRVariancesMap dv gt, WithArgs StoreAdapter gt)
    mkx dvm conss = do
        cvt <-
            case ccrVariancesToCovaryType dvt of
                Just cvt -> return cvt
                Nothing -> throw $ InterpretTypeDeclTypeVariableNotCovariantError mainTypeName
        let cparams = paramsToCovParams cvt tparams
        adapter <- makeTypeAdapter cparams conss
        return (dvm, adapter)
    mkgt ::
           (CCRVariancesMap dv gt, WithArgs StoreAdapter gt)
        -> QInterpreter (GroundTypeFromTypeID dv gt (Storability dv gt))
    mkgt ~(dvm, ~(MkWithArgs stba)) = do
        cvt <-
            case ccrVariancesToCovaryType dvt of
                Just cvt -> return cvt
                Nothing -> throw $ InterpretTypeDeclTypeVariableNotCovariantError mainTypeName
        return $
            MkGroundTypeFromTypeID $ \subTypeName famType -> let
                stbKind :: CovaryType dv
                stbKind = cvt
                stbCovaryMap = ccrVariancesMapToCovary cvt $ lazyCCRVariancesMap dvt dvm
                showType = standardListTypeExprShow @dv $ exprShow subTypeName
                stbAdapter = return $ MkAllFor stba
                storability :: Storability dv gt
                storability = MkStorability {..}
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
            builderLift $
            storabilitySaturatedAdapter
                (typeToDolan $ MkDolanGroundedType entityGroundType NilCCRArguments)
                plainStoreAdapter
                storability $ \(MkShimWit args conv) eat ->
                return $
                subtypeConversionEntry
                    TrustMe
                    Nothing
                    (MkShimWit (MkDolanGroundedType gt args) conv)
                    (mkShimWit $ MkDolanGroundedType entityGroundType NilCCRArguments) $
                pure $ functionToShim "datatype-storable" $ storeAdapterConvert eat
        registerSubtypeConversion sce
    in MkTypeConstruction mkx mkgt postregister

makeStorableDataTypeBox ::
       (?interpretExpression :: SyntaxExpression -> QInterpreter QExpression)
    => FullName
    -> RawMarkdown
    -> [SyntaxTypeParameter]
    -> [SyntaxWithDoc SyntaxStorableDatatypeConstructorOrSubtype]
    -> QInterpreter (QFixBox () ())
makeStorableDataTypeBox = makeDataTypeBox makeStorableGroundType True []
