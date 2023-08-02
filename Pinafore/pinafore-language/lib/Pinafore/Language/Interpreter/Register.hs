module Pinafore.Language.Interpreter.Register
    ( registerScope
    , registerDocs
    , registerLetBindings
    , registerLetBinding
    , registerMatchBindings
    , registerType
    , registerPatternConstructor
    , registerRecord
    , registerSubtypeConversion
    ) where

import Language.Expression.Common
import Language.Expression.Dolan
import Pinafore.Language.Error
import Pinafore.Language.Grammar.Docs
import Pinafore.Language.Interpreter.Binding
import Pinafore.Language.Interpreter.Interpreter
import Pinafore.Language.Interpreter.Lookup
import Pinafore.Language.Interpreter.Scope
import Pinafore.Language.Interpreter.ScopeBuilder
import Pinafore.Language.Name
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Subtype ()
import Pinafore.Language.VarID
import Pinafore.Text
import Shapes

registerScope :: QScope -> QScopeBuilder ()
registerScope scope = registerScopeDocs $ MkQScopeDocs [scope] mempty

registerDocs :: Docs -> QScopeBuilder ()
registerDocs docs = registerScopeDocs $ MkQScopeDocs mempty docs

registerBindings :: [(FullName, QBindingInfo)] -> QScopeBuilder ()
registerBindings bb = registerScope $ bindingInfosToScope bb

registerBinding :: FullName -> QBindingInfo -> QScopeBuilder ()
registerBinding name db = registerBindings $ singletonMap name db

registerLetBindings :: [(FullName, RawMarkdown, QExpression)] -> QScopeBuilder ()
registerLetBindings bb =
    registerBindings $ fmap (\(fname, doc, exp) -> (fname, MkQBindingInfo fname doc $ ValueBinding exp Nothing)) bb

registerLetBinding :: FullName -> RawMarkdown -> QExpression -> QScopeBuilder ()
registerLetBinding name doc expr = registerLetBindings $ pure (name, doc, expr)

registerMatchBindings :: QMatch -> QScopeBuilder ()
registerMatchBindings match = do
    let
        rbb =
            for (tsMatchBindings @QTypeSystem match) $ \(wvar, expr) -> do
                vn <- varIdNameRef wvar
                return (vn, "lambda", expr)
    case rbb of
        SuccessResult bb -> registerLetBindings bb
        FailureResult fn -> builderLift $ throw $ KnownIssueError 0 $ "bad match var: " <> showNamedText fn

registerBoundType :: FullName -> RawMarkdown -> QSomeGroundType -> QScopeBuilder ()
registerBoundType name doc t = do
    builderLift $ checkNameForRegister name
    registerBinding name $ MkQBindingInfo name doc $ TypeBinding t

registerType :: forall dv t. FullName -> RawMarkdown -> QGroundType dv t -> QScopeBuilder ()
registerType name doc t = do
    builderLift $ checkNameForRegister name
    registerBoundType name doc $ MkSomeGroundType t

registerPatternConstructor :: FullName -> RawMarkdown -> QExpression -> QPatternConstructor -> QScopeBuilder ()
registerPatternConstructor name doc exp pc = do
    builderLift $ checkNameForRegister name
    registerBinding name $ MkQBindingInfo name doc $ ValueBinding exp $ Just pc

registerRecord :: FullName -> RawMarkdown -> QRecordConstructor -> QScopeBuilder ()
registerRecord name doc rc = do
    builderLift $ checkNameForRegister name
    registerBinding name $ MkQBindingInfo name doc $ RecordConstructorBinding rc

registerSubtypeConversion :: QSubtypeConversionEntry -> QScopeBuilder ()
registerSubtypeConversion sce = do
    newscope <- builderLift $ getSubtypeScope sce
    registerScope newscope
