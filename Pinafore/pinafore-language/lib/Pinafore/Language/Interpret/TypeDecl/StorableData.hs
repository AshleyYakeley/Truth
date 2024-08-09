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

{-
data StorabilityWitness (t :: Type) where
    MkStorabilityWitness
        :: forall dv (gt :: CCRVariancesKind dv).
           FullName
        -> FamilialType gt
        -> StorabilityWitness (AllFor StoreAdapter (Arguments StoreAdapter gt))

type StorabilityBox = ExpressionBox QOpenExpression StorabilityWitness (QScopeBuilder ())
-}
type WithArgs :: forall k. (Type -> Type) -> k -> Type
type WithArgs f gt = AllFor f (Arguments StoreAdapter gt)

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
    -> QInterpreter (QOpenExpression (WithArgs (Thing (ListType StoreAdapter lt) decltype) gt))
makeConstructorAdapter params pts = do
    etsexpr <- getCompose $ mapMListType (Compose . nonpolarToStoreAdapter params) pts
    return $
        fmap
            (\ets ->
                 MkAllFor $ \args ->
                     case assignArgumentParams params args of
                         Refl -> MkThing (mapListType (\(Compose f) -> f args) ets) Refl)
            etsexpr

makeTypeAdapter ::
       CovParams dv gt decltype
    -> [(ConstructorCodec decltype, Anchor)]
    -> QInterpreter (QOpenExpression (WithArgs StoreAdapter gt))
makeTypeAdapter params conss = do
    ffexpr :: [QOpenExpression (WithArgs (Compose Endo StoreAdapter) gt)] <-
        for conss $ \case
            (MkSomeFor (MkConstructorType _ PositionalCF cc) codec, anchor) -> do
                caexpr <- makeConstructorAdapter params $ listVTypeToType cc
                return $
                    fmap
                        (\(MkAllFor wa) ->
                             MkAllFor $ \args ->
                                 case wa args of
                                     MkThing tt Refl -> let
                                         vcodec =
                                             invmap
                                                 listVProductToProduct
                                                 (listProductToVProduct $ listTypeToVType tt)
                                                 codec
                                         in Compose $ Endo $ codecSum vcodec $ constructorStoreAdapter anchor tt)
                        caexpr
            (MkSomeFor (MkConstructorType _ (RecordCF _) _) _, _) -> throw InterpretTypeDeclTypeStorableRecord
    return $
        fmap (\ff -> MkAllFor $ \args -> appEndo (concatmap (\(MkAllFor f) -> getCompose $ f args) ff) nullStoreAdapter) $
        sequenceA ffexpr

makeStorableGroundType ::
       forall (dv :: CCRVariances) (gt :: CCRVariancesKind dv) (decltype :: Type). Is CCRVariancesType dv
    => FullName
    -> CCRTypeParams dv gt decltype
    -> TypeConstruction dv gt [(ConstructorCodec decltype, Anchor)]
makeStorableGroundType mainTypeName tparams = let
    dvt = ccrArgumentsType tparams
    mkx :: CCRVariancesMap dv gt
        -> [(ConstructorCodec decltype, Anchor)]
        -> QInterpreter (CCRVariancesMap dv gt, QOpenExpression (WithArgs StoreAdapter gt))
    mkx dvm conss = do
        cvt <-
            case ccrVariancesToCovaryType dvt of
                Just cvt -> return cvt
                Nothing -> throw $ InterpretTypeDeclTypeVariableNotCovariantError mainTypeName
        let cparams = paramsToCovParams cvt tparams
        adapterexpr <- makeTypeAdapter cparams conss
        return (dvm, adapterexpr)
    mkgt ::
           (CCRVariancesMap dv gt, QOpenExpression (WithArgs StoreAdapter gt))
        -> QInterpreter (GroundTypeFromTypeID dv gt (Storability dv gt))
    mkgt ~(dvm, adapterexpr) = do
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
                stbAdapterExpr = adapterexpr
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
                (pure plainStoreAdapter)
                storability $ \(MkShimWit args conv) eatexpr ->
                return $
                subtypeConversionEntry
                    TrustMe
                    Nothing
                    (MkShimWit (MkDolanGroundedType gt args) conv)
                    (mkShimWit $ MkDolanGroundedType entityGroundType NilCCRArguments) $
                fmap (functionToShim "datatype-storable" . storeAdapterConvert) eatexpr
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
