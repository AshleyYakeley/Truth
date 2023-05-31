module Pinafore.Language.Grammar.Interpret.TypeDecl.StorableData
    ( makeStorableDataTypeBox
    ) where

import Pinafore.Base
import Pinafore.Language.Error
import Pinafore.Language.Grammar.Interpret.TypeDecl.Data
import Pinafore.Language.Grammar.Interpret.TypeDecl.Parameter
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type
import Pinafore.Text
import Shapes

type CovParam :: CCRArgumentKind
data CovParam (sv :: CCRVariance) (t :: CCRVarianceKind sv) where
    MkCovParam :: TypeVarT tv -> CovParam CoCCRVariance tv

type CovParams :: forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type -> Type
type CovParams = CCRArguments CovParam

paramsToCovParams :: CovaryType dv -> CCRTypeParams dv gt decltype -> CovParams dv gt decltype
paramsToCovParams NilListType NilCCRArguments = NilCCRArguments
paramsToCovParams (ConsListType Refl ct) (ConsCCRArguments (CoCCRTypeParam n) args) =
    ConsCCRArguments (MkCovParam n) $ paramsToCovParams ct args

type WithArgs :: forall k. (Type -> Type) -> k -> Type
newtype WithArgs f gt =
    MkWithArgs (forall (ta :: Type). Arguments StoreAdapter gt ta -> f ta)

assignArgumentParams ::
       forall (f :: Type -> Type) dv (gt :: DolanVarianceKind dv) (decltype :: Type) (ta :: Type).
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

matchParamArgs ::
       forall f dv (gt1 :: DolanVarianceKind dv) (gt2 :: DolanVarianceKind dv) ta.
       CovParams dv gt1 ta
    -> Arguments f gt2 ta
    -> gt1 :~: gt2
matchParamArgs NilCCRArguments NilArguments = Refl
matchParamArgs (ConsCCRArguments _ args) (ConsArguments _ args') =
    case matchParamArgs args args' of
        Refl -> Refl

lookupVar ::
       forall f dv (gt :: DolanVarianceKind dv) tv ta.
       CovParams dv gt ta
    -> TypeVarT tv
    -> QInterpreter (Arguments f gt ta -> f tv)
lookupVar NilCCRArguments var = throwWithName $ \ntt -> InterpretTypeNotEntityError $ ntt $ exprShow var
lookupVar (ConsCCRArguments (MkCovParam var') params) var
    | Just Refl <- testEquality var var' =
        return $ \(ConsArguments ft args) ->
            case matchParamArgs params args of
                Refl -> ft
lookupVar (ConsCCRArguments _ params) var = do
    f <- lookupVar @f params var
    return $ \(ConsArguments _ args) ->
        case matchParamArgs params args of
            Refl -> f args

nonpolarToStoreAdapter ::
       CovParams dv gt ta
    -> QNonpolarType t
    -> QInterpreter (Compose ((->) (Arguments StoreAdapter gt ta)) StoreAdapter t)
nonpolarToStoreAdapter params (VarNonpolarType var) = fmap Compose $ lookupVar params var
nonpolarToStoreAdapter params (GroundedNonpolarType ground args) = do
    (cvt, MkStorableGroundType _ (MkSealedStorability _ storability)) <-
        case dolanToMonoGroundType ground of
            Nothing -> throwWithName $ \ntt -> InterpretTypeNotEntityError $ ntt $ showGroundType ground
            Just x -> return x
    aargs <- ccrArgumentsToArgumentsM (\(CoNonpolarArgument arg) -> nonpolarToStoreAdapter params arg) cvt args
    return $ Compose $ \eargs -> stbAdapter storability $ mapArguments (\(Compose eaf) -> eaf eargs) aargs
nonpolarToStoreAdapter _ t@(RecursiveNonpolarType {}) =
    throwWithName $ \ntt -> InterpretTypeNotEntityError $ ntt $ exprShow t

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
    return $ MkWithArgs $ \args -> appEndo (mconcat $ fmap (\(MkWithArgs f) -> getCompose $ f args) ff) nullStoreAdapter

makeStorableGroundType ::
       forall (dv :: DolanVariance) (gt :: DolanVarianceKind dv) (decltype :: Type). Is DolanVarianceType dv
    => FullName
    -> CCRTypeParams dv gt decltype
    -> TypeConstruction dv gt [(ConstructorCodec decltype, Anchor)]
makeStorableGroundType mainTypeName tparams = let
    dvt = ccrArgumentsType tparams
    mkx :: DolanVarianceMap dv gt
        -> [(ConstructorCodec decltype, Anchor)]
        -> QInterpreter (DolanVarianceMap dv gt, WithArgs StoreAdapter gt)
    mkx dvm conss = do
        cvt <-
            case dolanVarianceToCovaryType dvt of
                Just cvt -> return cvt
                Nothing -> throw $ InterpretTypeDeclTypeVariableNotCovariantError mainTypeName
        let cparams = paramsToCovParams cvt tparams
        adapter <- makeTypeAdapter cparams conss
        return (dvm, adapter)
    mkgt ::
           (DolanVarianceMap dv gt, WithArgs StoreAdapter gt)
        -> QInterpreter (GroundTypeFromTypeID dv gt (Storability dv gt))
    mkgt ~(dvm, ~(MkWithArgs stbAdapter)) = do
        cvt <-
            case dolanVarianceToCovaryType dvt of
                Just cvt -> return cvt
                Nothing -> throw $ InterpretTypeDeclTypeVariableNotCovariantError mainTypeName
        return $
            MkGroundTypeFromTypeID $ \subTypeName tidsym -> let
                stbKind :: CovaryType dv
                stbKind = cvt
                stbCovaryMap = dolanVarianceMapToCovary cvt $ lazyDolanVarianceMap dvt dvm
                showType = standardListTypeExprShow @dv $ exprShow subTypeName
                storability :: Storability dv gt
                storability = MkStorability {..}
                gt :: QGroundType dv gt
                gt =
                    MkQGroundType
                        { qgtVarianceType = covaryToDolanVarianceType stbKind
                        , qgtVarianceMap = covaryToDolanVarianceMap stbKind stbCovaryMap
                        , qgtShowType = showType
                        , qgtFamilyType = MkFamilialType identifiedFamilyWitness $ MkIdentifiedTypeFamily tidsym
                        , qgtSubtypeGroup = Nothing
                        , qgtProperties = singleGroundProperty storabilityProperty storability
                        , qgtGreatestDynamicSupertype = nullPolyGreatestDynamicSupertype
                        }
                in (gt, storability)
    postregister :: QGroundType dv gt -> Storability dv gt -> QScopeInterpreter ()
    postregister gt storability =
        registerSubtypeConversion $
        MkSubtypeConversionEntry TrustMe gt entityGroundType $
        storabilitySaturatedAdapter
            (typeToDolan $ MkDolanGroundedType entityGroundType NilCCRArguments)
            plainStoreAdapter
            storability $ \args eat ->
            subtypeConversion Nothing gt args entityGroundType nilDolanArgumentsShimWit $
            pure $ functionToShim "datatype-storable" $ storeAdapterConvert eat
    in MkTypeConstruction mkx mkgt postregister

makeStorableDataTypeBox ::
       FullName
    -> RawMarkdown
    -> [SyntaxTypeParameter]
    -> [SyntaxWithDoc SyntaxStorableDatatypeConstructorOrSubtype]
    -> QInterpreter (QFixBox () ())
makeStorableDataTypeBox = makeDataTypeBox makeStorableGroundType []
