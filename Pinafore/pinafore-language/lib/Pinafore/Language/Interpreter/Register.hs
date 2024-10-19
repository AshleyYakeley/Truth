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
registerScope scope = registerScopeDocs $ scopeDocs scope

registerDocs :: Docs -> QScopeBuilder ()
registerDocs docs = registerScopeDocs $ mempty {sdDocs = docs}

registerBindings :: [(FullName, QBindingInfo)] -> QScopeBuilder ()
registerBindings bb = registerScope $ bindingInfosToScope bb

registerBinding :: FullName -> QBindingInfo -> QScopeBuilder ()
registerBinding name db = registerBindings $ singletonMap name db

updateBindingInfo :: FullNameRef -> (QBindingInfo -> QScopeBuilder QBindingInfo) -> QScopeBuilder ()
updateBindingInfo fnref f = do
    (fname, oldbi) <- builderLift $ lookupBindingInfo fnref
    newbi <- f oldbi
    registerBinding fname newbi

registerLetBindings :: [(FullName, DefDoc, QExpression)] -> QScopeBuilder ()
registerLetBindings bb =
    registerBindings $ fmap (\(fname, doc, exp) -> (fname, MkQBindingInfo fname doc $ ValueBinding exp)) bb

registerLetBindingsDocs :: [(FullName, DefDoc, QExpression)] -> QScopeBuilder ()
registerLetBindingsDocs bb = do
    registerLetBindings bb
    registerDocs $ concatmap (\(_, doc, _) -> pure doc) bb

registerLetBinding :: FullName -> DefDoc -> QExpression -> QScopeBuilder ()
registerLetBinding name doc expr = registerLetBindings $ pure (name, doc, expr)

registerMatchBindings :: QMatch -> QScopeBuilder ()
registerMatchBindings match = do
    bb <-
        for (tsMatchBindings @QTypeSystem match) $ \case
            (LambdaVarID _ vn, expr) -> return (vn, MkDefDoc (ValueDocItem (pure $ fullNameRef vn) "") "lambda", expr)
            (v, _) -> builderLift $ throw $ InternalError Nothing $ "bad match var: " <> showNamedText v
    registerLetBindings bb

registerSelector :: BindingSelector t -> FullName -> DefDoc -> t -> QScopeBuilder ()
registerSelector bst name doc t = do
    builderLift $ checkNameForRegister name
    registerBinding name $ MkQBindingInfo name doc $ bsEncode bst t

updateSelector :: BindingSelector t -> FullNameRef -> (t -> QScopeBuilder t) -> QScopeBuilder ()
updateSelector bst fnref f =
    updateBindingInfo fnref $ \oldbi -> do
        let oldbind = biValue oldbi
        oldt <-
            case bsDecode bst oldbind of
                Just t -> return t
                Nothing -> throw $ bsError bst fnref
        newt <- f oldt
        let
            newbind = bsEncode bst newt
            newbi = oldbi {biValue = newbind}
        return newbi

updateGroundType :: FullNameRef -> (forall dv t. QGroundType dv t -> QGroundType dv t) -> QScopeBuilder ()
updateGroundType fnref f =
    updateSelector typeBindingSelector fnref $ \(MkSomeGroundType t) -> return $ MkSomeGroundType $ f t

registerType :: FullName -> DefDoc -> QSomeGroundType -> QScopeBuilder ()
registerType = registerSelector typeBindingSelector

registerGroundType :: forall dv t. FullName -> DefDoc -> QGroundType dv t -> QScopeBuilder ()
registerGroundType name doc t = registerType name doc $ MkSomeGroundType t

registerPatternConstructor :: FullName -> DefDoc -> QExpression -> QPatternConstructor -> QScopeBuilder ()
registerPatternConstructor name doc exp pc = do
    builderLift $ checkNameForRegister name
    registerBinding name $ MkQBindingInfo name doc $ PatternConstructorBinding exp pc

registerRecordConstructor :: FullName -> DefDoc -> QRecordConstructor -> QScopeBuilder ()
registerRecordConstructor = registerSelector recordConstructorBindingSelector

registerRecordValue :: FullName -> DefDoc -> QRecordValue -> QScopeBuilder ()
registerRecordValue = registerSelector recordValueBindingSelector

registerSubtypeConversion :: QSubtypeConversionEntry -> QScopeBuilder ()
registerSubtypeConversion sce = do
    newscope <- builderLift $ getSubtypeScope sce
    registerScope newscope
