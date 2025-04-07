module Pinafore.Language.Interpret.TypeDecl.Synonym
    ( makeSynonymTypeBox
    )
where

import Shapes.Unsafe (unsafeGetRefl)

import Import
import Pinafore.Language.Interpret.Type
import Pinafore.Language.Interpret.TypeDecl.Parameter
import Pinafore.Language.Interpreter
import Pinafore.Language.Type

withSemiIdentifiedType' ::
    forall (dv :: CCRVariances) r.
    (forall (gt :: CCRVariancesKind dv). FamilialType gt -> QScopeBuilder r) ->
    QScopeBuilder r
withSemiIdentifiedType' call = do
    mr <- builderLift $ withSemiIdentifiedType @dv $ \ft -> return $ call ft
    mr

makeSynonymTypeBox ::
    FullName ->
    RawMarkdown ->
    Bool ->
    [SyntaxTypeParameter] ->
    SyntaxType ->
    QScopeBuilder ()
makeSynonymTypeBox name md storable sparams sparent =
    case getAnyCCRTypeParams sparams of
        (_, MkAnyCCRTypeParams (gtparams :: GenCCRTypeParams dv)) ->
            withSemiIdentifiedType' @dv $ \(mainFamType :: _ maintype) ->
                case gtparams @maintype of
                    MkSome (tparams :: CCRTypeParams dv maintype decltype) ->
                        withRepresentative (ccrArgumentsType tparams) $ do
                            smparent <- builderLift $ interpretNonpolarGroundedType sparent
                            case smparent of
                                MkSome (parent :: _ structtype) -> do
                                    props :: GroundProperties dv maintype <-
                                        if storable
                                            then return mempty
                                            else return mempty
                                    Refl <- unsafeGetRefl @Type @structtype @decltype
                                    varianceMap <- builderLift $ getCCRVariancesMap name tparams $ getVarMapping parent
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
                                        gdsname = exprShow name
                                        doc = MkDefDoc (typeDocItem name storable [] (Just gdsname)) md
                                    registerGroundType name doc gt

-- registerSubtypeConversion $ neutralSubtypeConversionEntry (MkNonpolarGroundedType gt NilCCRArguments) parent
