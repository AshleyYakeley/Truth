module Pinafore.Language.Interpreter.Register
    ( registerScope
    , registerDocs
    , registerLetBindings
    , registerLetBindingsDocs
    , registerLetBinding
    , registerMatchBindings
    , registerGroundType
    , registerPatternConstructor
    , registerRecord
    , registerSubtypeConversion
    ) where

import Import
import Pinafore.Language.Error
import Pinafore.Language.Interpreter.Binding
import Pinafore.Language.Interpreter.Interpreter
import Pinafore.Language.Interpreter.Lookup
import Pinafore.Language.Interpreter.Scope
import Pinafore.Language.Interpreter.ScopeBuilder
import Pinafore.Language.Interpreter.ScopeDocs
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Subtype ()
import Pinafore.Language.VarID

registerScope :: QScope -> QScopeBuilder ()
registerScope scope = registerScopeDocs $ mempty {sdScopes = [scope]}

registerDocs :: Docs -> QScopeBuilder ()
registerDocs docs = registerScopeDocs $ mempty {sdDocs = docs}

registerBindings :: [(FullName, QBindingInfo)] -> QScopeBuilder ()
registerBindings bb = registerScope $ bindingInfosToScope bb

registerBinding :: FullName -> QBindingInfo -> QScopeBuilder ()
registerBinding name db = registerBindings $ singletonMap name db

registerLetBindings :: [(FullName, DefDoc, QExpression)] -> QScopeBuilder ()
registerLetBindings bb =
    registerBindings $ fmap (\(fname, doc, exp) -> (fname, MkQBindingInfo fname doc $ ValueBinding exp)) bb

registerLetBindingsDocs :: [(FullName, DefDoc, QExpression)] -> QScopeBuilder ()
registerLetBindingsDocs bb = do
    registerLetBindings bb
    registerDocs $ mconcat $ fmap (\(_, doc, _) -> pure doc) bb

registerLetBinding :: FullName -> DefDoc -> QExpression -> QScopeBuilder ()
registerLetBinding name doc expr = registerLetBindings $ pure (name, doc, expr)

registerMatchBindings :: QMatch -> QScopeBuilder ()
registerMatchBindings match = do
    bb <-
        for (tsMatchBindings @QTypeSystem match) $ \case
            (GoodVarID _ vn, expr) -> return (vn, MkDefDoc (ValueDocItem (pure $ fullNameRef vn) "") "lambda", expr)
            (v, _) -> builderLift $ throw $ InternalError Nothing $ "bad match var: " <> showNamedText v
    registerLetBindings bb

registerSelector :: BindingSelector t -> FullName -> DefDoc -> t -> QScopeBuilder ()
registerSelector bst name doc t = do
    builderLift $ checkNameForRegister name
    registerBinding name $ MkQBindingInfo name doc $ bsEncode bst t

registerType :: FullName -> DefDoc -> QSomeGroundType -> QScopeBuilder ()
registerType = registerSelector typeBindingSelector

registerGroundType :: forall dv t. FullName -> DefDoc -> QGroundType dv t -> QScopeBuilder ()
registerGroundType name doc t = do registerType name doc $ MkSomeGroundType t

registerPatternConstructor :: FullName -> DefDoc -> QExpression -> QPatternConstructor -> QScopeBuilder ()
registerPatternConstructor name doc exp pc = do
    builderLift $ checkNameForRegister name
    registerBinding name $ MkQBindingInfo name doc $ PatternConstructorBinding exp pc

registerRecord :: FullName -> DefDoc -> QRecordConstructor -> QScopeBuilder ()
registerRecord = registerSelector recordConstructorBindingSelector

registerSubtypeConversion :: QSubtypeConversionEntry -> QScopeBuilder ()
registerSubtypeConversion sce = do
    newscope <- builderLift $ getSubtypeScope sce
    registerScope newscope
