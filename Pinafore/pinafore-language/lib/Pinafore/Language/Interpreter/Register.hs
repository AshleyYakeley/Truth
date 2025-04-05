module Pinafore.Language.Interpreter.Register
    ( registerScope
    , registerDocs
    , registerLetBindings
    , registerLetBindingsDocs
    , registerLetBinding
    , registerMatchBindings
    , registerGroundType
    , registerPatternConstructor
    , registerRecordConstructor
    , registerRecordValue
    , registerSubtypeConversion
    , updateGroundType
    )
where

import Import
import Pinafore.Language.Error
import Pinafore.Language.Interpreter.Binding
import Pinafore.Language.Interpreter.Declarations
import Pinafore.Language.Interpreter.Interpreter
import Pinafore.Language.Interpreter.Lookup
import Pinafore.Language.Interpreter.Scope
import Pinafore.Language.Interpreter.ScopeBuilder
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Subtype ()
import Pinafore.Language.VarID

registerScope :: QScope -> QScopeBuilder ()
registerScope scope = registerDeclarations $ declarations scope

registerDocs :: Docs -> QScopeBuilder ()
registerDocs docs = registerDeclarations $ mempty{declsDocs = docs}

registerBindings :: [(FullName, QScopeItem)] -> QScopeBuilder ()
registerBindings bb = registerScope $ bindingInfosToScope bb

registerBinding :: FullName -> QScopeItem -> QScopeBuilder ()
registerBinding name db = registerBindings $ singletonMap name db

updateBindingInfo :: FullNameRef -> (QScopeItem -> QScopeBuilder QScopeItem) -> QScopeBuilder ()
updateBindingInfo fnref f = do
    (fname, oldbi) <- builderLift $ lookupBindingInfo fnref
    newbi <- f oldbi
    registerBinding fname newbi

registerLetBindings :: [(FullName, DefDoc, QExpression)] -> QScopeBuilder ()
registerLetBindings bb =
    registerBindings $ fmap (\(fname, doc, expr) -> (fname, MkQScopeItem fname doc $ ValueItem expr)) bb

registerLetBindingsDocs :: [(FullName, DefDoc, QExpression)] -> QScopeBuilder ()
registerLetBindingsDocs bb = do
    registerLetBindings bb
    registerDocs $ concatmap (\(_, doc, _) -> pure doc) bb

registerLetBinding :: FullName -> DefDoc -> QExpression -> QScopeBuilder ()
registerLetBinding name doc expr = registerLetBindings $ pure (name, doc, expr)

registerMatchBindings :: QMatch -> QScopeBuilder ()
registerMatchBindings match = do
    err <- builderLift getMissingCaseError
    bb <-
        for (tsMatchBindings @QTypeSystem err match) $ \case
            (LambdaVarID _ vn, expr) -> return (vn, MkDefDoc (ValueDocItem (pure $ fullNameRef vn) "") "lambda", expr)
            (v, _) -> builderLift $ throw $ InternalError Nothing $ "bad match var: " <> showNamedText v
    registerLetBindings bb

registerSelector :: ItemSelector t -> FullName -> DefDoc -> t -> QScopeBuilder ()
registerSelector bst name doc t = do
    builderLift $ checkNameForRegister name
    registerBinding name $ MkQScopeItem name doc $ isEncode bst t

updateSelector :: ItemSelector t -> FullNameRef -> (t -> QScopeBuilder t) -> QScopeBuilder ()
updateSelector bst fnref f =
    updateBindingInfo fnref $ \oldbi -> do
        let oldbind = siItem oldbi
        oldt <-
            case isDecode bst oldbind of
                Just t -> return t
                Nothing -> throw $ isError bst fnref
        newt <- f oldt
        let
            newbind = isEncode bst newt
            newbi = oldbi{siItem = newbind}
        return newbi

updateGroundType :: FullNameRef -> (forall dv t. QGroundType dv t -> QGroundType dv t) -> QScopeBuilder ()
updateGroundType fnref f =
    updateSelector typeItemSelector fnref $ \(MkSomeGroundType t) -> return $ MkSomeGroundType $ f t

registerType :: FullName -> DefDoc -> QSomeGroundType -> QScopeBuilder ()
registerType = registerSelector typeItemSelector

registerGroundType :: forall dv t. FullName -> DefDoc -> QGroundType dv t -> QScopeBuilder ()
registerGroundType name doc t = registerType name doc $ MkSomeGroundType t

registerPatternConstructor :: FullName -> DefDoc -> QExpression -> QPatternConstructor -> QScopeBuilder ()
registerPatternConstructor name doc expr pc = do
    builderLift $ checkNameForRegister name
    registerBinding name $ MkQScopeItem name doc $ PatternConstructorItem expr pc

registerRecordConstructor :: FullName -> DefDoc -> QRecordConstructor -> QScopeBuilder ()
registerRecordConstructor = registerSelector recordConstructorItemSelector

registerRecordValue :: FullName -> DefDoc -> QRecordValue -> QScopeBuilder ()
registerRecordValue = registerSelector recordValueItemSelector

registerSubtypeConversion :: QSubtypeConversionEntry -> QScopeBuilder ()
registerSubtypeConversion sce = do
    newscope <- builderLift $ getSubtypeScope sce
    registerScope newscope
