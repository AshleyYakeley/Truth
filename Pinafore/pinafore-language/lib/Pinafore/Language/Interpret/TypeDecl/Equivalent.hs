module Pinafore.Language.Interpret.TypeDecl.Equivalent
    ( makeEquivalentType
    )
where

import Shapes.Unsafe (unsafeCoercion)

import Import
import Pinafore.Language.Error
import Pinafore.Language.Interpret.Type
import Pinafore.Language.Interpret.TypeDecl.DoubleParams
import Pinafore.Language.Interpret.TypeDecl.Parameter
import Pinafore.Language.Interpret.TypeDecl.Storage
import Pinafore.Language.Interpreter
import Pinafore.Language.Type

withSemiIdentifiedType' ::
    forall (dv :: CCRVariances) r.
    (forall (gt :: CCRVariancesKind dv). FamilialType gt -> QScopeBuilder r) ->
    QScopeBuilder r
withSemiIdentifiedType' call = do
    mr <- builderLift $ withSemiIdentifiedType @dv $ \ft -> return $ call ft
    mr

assignNonpolarGroundedToStoreAdapter ::
    forall dv (gt :: CCRVariancesKind dv) (decltype :: Type) (structtype :: Type).
    Coercible decltype structtype =>
    CovParams dv gt decltype ->
    QNonpolarGroundedType structtype ->
    QInterpreter (QExprRec (WithStoreAdapterArgs gt StoreAdapter))
assignNonpolarGroundedToStoreAdapter params t = do
    expr <- nonpolarGroundedToStoreAdapter params t
    let
        toWSAA :: Compose ((->) (Arguments StoreAdapter gt decltype)) StoreAdapter structtype -> WithStoreAdapterArgs gt StoreAdapter
        toWSAA (Compose f) = assignWithStoreAdapterArgs params $ \args -> invmap coerce coerce $ f args
    return $ fmap toWSAA expr

makeEquivalentType ::
    FullName ->
    RawMarkdown ->
    Bool ->
    [SyntaxTypeParameter] ->
    SyntaxType ->
    QScopeBuilder ()
makeEquivalentType name md storable sparams sparent =
    case getAnyCCRTypeParams sparams of
        (doubleParams, MkAnyCCRTypeParams (gtparams :: GenCCRTypeParams dv)) ->
            withSemiIdentifiedType' @dv $ \(mainFamType :: _ maintype) ->
                case gtparams @maintype of
                    MkSome (tparams :: CCRTypeParams dv maintype decltype) -> let
                        dvt = ccrArgumentsType tparams
                        in withRepresentative dvt $ do
                            smparent <- builderLift $ interpretNonpolarGroundedType sparent
                            case doubleTypeParameters doubleParams smparent of
                                MkSome (parent :: _ structtype) -> do
                                    MkCoercion <- pure $ unsafeCoercion @Type @structtype @decltype
                                    varianceMap <- builderLift $ getCCRVariancesMap name tparams $ coerce $ getVarMapping parent
                                    props :: GroundProperties dv maintype <-
                                        if storable
                                            then do
                                                cvt <-
                                                    case ccrVariancesToCovaryType dvt of
                                                        Just cvt -> return cvt
                                                        Nothing -> throw $ InterpretTypeDeclTypeVariableNotCovariantError name
                                                let
                                                    cparams :: CovParams dv maintype decltype
                                                    cparams = paramsToCovParams cvt tparams
                                                storeadapterexpr :: QExprRec (WithStoreAdapterArgs maintype StoreAdapter) <-
                                                    builderLift $ assignNonpolarGroundedToStoreAdapter cparams parent
                                                wit <- liftIO newIOWitness
                                                let
                                                    storability :: Storability dv maintype
                                                    storability = let
                                                        stbKind = cvt
                                                        stbCovaryMap = ccrVariancesMapToCovary cvt varianceMap
                                                        stbAdapterExprKnot = knotAppRec wit storeadapterexpr
                                                        in MkStorability{..}
                                                return $ singleGroundProperty storabilityProperty storability
                                            else return mempty
                                    let
                                        gt :: QGroundType dv maintype
                                        gt =
                                            MkQGroundType
                                                { qgtVarianceType = representative
                                                , qgtVarianceMap = varianceMap
                                                , qgtShowType = standardListTypeExprShow @dv $ showNamedText name
                                                , qgtFamilyType = mainFamType
                                                , qgtSubtypeGroup = Nothing
                                                , qgtProperties = props
                                                , qgtGreatestDynamicSupertype = nullPolyGreatestDynamicSupertype
                                                }
                                        parentText = exprShow parent
                                        doc = MkDefDoc (typeEquivalentDocItem name storable sparams parentText) md
                                        declGroundedType :: QNonpolarGroundedType decltype
                                        declGroundedType = MkNonpolarGroundedType gt $ mapSameCCRArguments tParamToNonpolarArgument tparams
                                    registerGroundType name doc gt
                                    registerSubtypeConversion $ neutralSubtypeConversionEntry declGroundedType parent
                                    registerSubtypeConversion $ neutralSubtypeConversionEntry parent declGroundedType
                                    registerDocs $ pure doc
