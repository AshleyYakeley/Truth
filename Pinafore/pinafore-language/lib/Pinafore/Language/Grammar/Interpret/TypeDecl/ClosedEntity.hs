module Pinafore.Language.Grammar.Interpret.TypeDecl.ClosedEntity
    ( makeClosedEntityTypeBox
    ) where

import Pinafore.Base
import Pinafore.Language.Error
import Pinafore.Language.ExprShow
import Pinafore.Language.Grammar.Interpret.TypeDecl.Data
import Pinafore.Language.Grammar.Interpret.TypeDecl.Parameter
import Pinafore.Language.Grammar.Syntax
import Pinafore.Language.Interpreter
import Pinafore.Language.Name
import Pinafore.Language.Type
import Pinafore.Markdown
import Shapes

type CovParam :: CCRArgumentKind
data CovParam (sv :: CCRVariance) (t :: CCRVarianceKind sv) where
    MkCovParam :: SymbolType n -> CovParam CoCCRVariance (UVarT n)

type CovParams :: forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type -> Type
type CovParams = CCRArguments CovParam

paramsToCovParams :: CovaryType dv -> CCRTypeParams dv gt decltype -> CovParams dv gt decltype
paramsToCovParams NilListType NilCCRArguments = NilCCRArguments
paramsToCovParams (ConsListType Refl ct) (ConsCCRArguments (CoCCRTypeParam n) args) =
    ConsCCRArguments (MkCovParam n) $ paramsToCovParams ct args

type WithArgs :: forall k. (Type -> Type) -> k -> Type
newtype WithArgs f gt =
    MkWithArgs (forall (ta :: Type). Arguments EntityAdapter gt ta -> f ta)

assignArgumentParams ::
       forall (f :: Type -> Type) dv (gt :: DolanVarianceKind dv) (decltype :: Type) (ta :: Type).
       CovParams dv gt decltype
    -> Arguments f gt ta
    -> decltype :~: ta
assignArgumentParams NilCCRArguments NilArguments = Refl
assignArgumentParams (ConsCCRArguments (MkCovParam var) args) (ConsArguments arg args') =
    assignUVarWit var arg $
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
       forall f dv (gt :: DolanVarianceKind dv) name ta.
       CovParams dv gt ta
    -> SymbolType name
    -> QInterpreter (Arguments f gt ta -> f (UVarT name))
lookupVar NilCCRArguments var = throw $ InterpretTypeNotEntityError $ exprShow var
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

nonpolarToEntityAdapter ::
       CovParams dv gt ta
    -> PinaforeNonpolarType t
    -> QInterpreter (Compose ((->) (Arguments EntityAdapter gt ta)) EntityAdapter t)
nonpolarToEntityAdapter params (VarNonpolarType var) = fmap Compose $ lookupVar params var
nonpolarToEntityAdapter params (GroundedNonpolarType ground args) = do
    (cvt, MkEntityGroundType _ (MkSealedEntityProperties eprops)) <-
        case dolanToMonoGroundType ground of
            Nothing -> throw $ InterpretTypeNotEntityError $ showGroundType ground
            Just x -> return x
    aargs <- ccrArgumentsToArgumentsM (\(CoNonpolarArgument arg) -> nonpolarToEntityAdapter params arg) cvt args
    return $ Compose $ \eargs -> epAdapter eprops $ mapArguments (\(Compose eaf) -> eaf eargs) aargs

closedEntityConstructorAdapter ::
       CovParams dv gt decltype
    -> ListType PinaforeNonpolarType lt
    -> QInterpreter (WithArgs (Thing (ListType EntityAdapter lt) decltype) gt)
closedEntityConstructorAdapter params pts = do
    ets <- mapMListType (nonpolarToEntityAdapter params) pts
    return $
        MkWithArgs $ \args ->
            case assignArgumentParams params args of
                Refl -> MkThing (mapListType (\(Compose f) -> f args) ets) Refl

closedEntityTypeAdapter ::
       CovParams dv gt decltype -> [(ConstructorCodec decltype, Anchor)] -> QInterpreter (WithArgs EntityAdapter gt)
closedEntityTypeAdapter params conss = do
    ff <-
        for conss $ \(MkSomeFor (MkListVProductType cc) codec, anchor) -> do
            MkWithArgs wa <- closedEntityConstructorAdapter params $ listVTypeToType cc
            return $
                MkWithArgs $ \args ->
                    case wa args of
                        MkThing tt Refl -> let
                            vcodec = invmap listVProductToProduct (listProductToVProduct $ listTypeToVType tt) codec
                            in Compose $ Endo $ codecSum vcodec $ constructorEntityAdapter anchor tt
    return $
        MkWithArgs $ \args -> appEndo (mconcat $ fmap (\(MkWithArgs f) -> getCompose $ f args) ff) nullEntityAdapter

makeClosedEntityGroundType ::
       forall (dv :: DolanVariance) (gt :: DolanVarianceKind dv) (decltype :: Type). Is DolanVarianceType dv
    => Name
    -> CCRTypeParams dv gt decltype
    -> TypeConstruction dv gt [(ConstructorCodec decltype, Anchor)]
makeClosedEntityGroundType mainTypeName tparams = let
    dvt = ccrArgumentsType tparams
    mkx :: DolanVarianceMap dv gt
        -> [(ConstructorCodec decltype, Anchor)]
        -> QInterpreter (DolanVarianceMap dv gt, WithArgs EntityAdapter gt)
    mkx dvm conss = do
        cvt <-
            case dolanVarianceToCovaryType dvt of
                Just cvt -> return cvt
                Nothing -> throw $ InterpretTypeDeclTypeVariableNotCovariantError mainTypeName
        let cparams = paramsToCovParams cvt tparams
        adapter <- closedEntityTypeAdapter cparams conss
        return (dvm, adapter)
    mkgt ::
           (DolanVarianceMap dv gt, WithArgs EntityAdapter gt)
        -> QInterpreter (GroundTypeFromTypeID dv gt (EntityProperties dv gt))
    mkgt ~(dvm, ~(MkWithArgs epAdapter)) = do
        cvt <-
            case dolanVarianceToCovaryType dvt of
                Just cvt -> return cvt
                Nothing -> throw $ InterpretTypeDeclTypeVariableNotCovariantError mainTypeName
        return $
            MkGroundTypeFromTypeID $ \subTypeName tidsym -> let
                epKind :: CovaryType dv
                epKind = cvt
                epCovaryMap = dolanVarianceMapToCovary cvt $ lazyDolanVarianceMap dvt dvm
                epShowType = standardListTypeExprShow @dv $ exprShow subTypeName
                eprops :: EntityProperties dv gt
                eprops = MkEntityProperties {..}
                in (closedEntityGroundType tidsym eprops, eprops)
    postregister :: QGroundType dv gt -> EntityProperties dv gt -> QScopeInterpreter ()
    postregister gt eprops =
        registerSubtypeConversion $
        MkSubtypeConversionEntry TrustMe gt entityGroundType $
        entityPropertiesSaturatedAdapter
            (typeToDolan $ MkDolanGroundedType entityGroundType NilCCRArguments)
            plainEntityAdapter
            eprops $ \args eat ->
            subtypeConversion gt args entityGroundType nilDolanArgumentsShimWit $
            pure $ functionToShim "ClosedEntity" $ entityAdapterConvert eat
    in MkTypeConstruction mkx mkgt postregister

makeClosedEntityTypeBox ::
       Name
    -> Markdown
    -> [SyntaxTypeParameter]
    -> [SyntaxWithDoc SyntaxClosedEntityConstructorOrSubtype]
    -> QInterpreter (QFixBox () ())
makeClosedEntityTypeBox = makeDeclTypeBox makeClosedEntityGroundType
