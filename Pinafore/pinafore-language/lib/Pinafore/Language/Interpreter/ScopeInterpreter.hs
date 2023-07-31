module Pinafore.Language.Interpreter.ScopeInterpreter
    ( QScopeInterpreter
    , runScopeInterpreter
    , execScopeInterpreter
    , QFixBox
    , scopeSetSourcePos
    , allocateVar
    , withCurrentNamespaceScope
    , usingNamespace
    , registerScope
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
import Pinafore.Language.Interpreter.Binding
import Pinafore.Language.Interpreter.Interpreter
import Pinafore.Language.Interpreter.Scope
import Pinafore.Language.Name
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Subtype ()
import Pinafore.Language.VarID
import Pinafore.Text
import Shapes
import Text.Parsec.Pos (SourcePos)

type QScopeInterpreter = TransformT QInterpreter

runScopeInterpreter :: QScopeInterpreter a -> (a -> QInterpreter b) -> QInterpreter b
runScopeInterpreter sb = unTransformT sb

execScopeInterpreter :: QInterpreter (QScopeInterpreter a) -> QScopeInterpreter a
execScopeInterpreter = execMapTransformT

type QFixBox = FixBox QScopeInterpreter

scopeRef :: Ref QScopeInterpreter QScope
scopeRef = transformParamRef scopeParam

bindingsRef :: Ref QScopeInterpreter QBindingMap
bindingsRef = transformParamRef bindingsParam

currentNamespaceRef :: Ref QScopeInterpreter Namespace
currentNamespaceRef = transformParamRef currentNamespaceParam

varIDStateRef :: Ref QScopeInterpreter VarIDState
varIDStateRef = transformParamRef varIDStateParam

scopeSetSourcePos :: SourcePos -> QScopeInterpreter ()
scopeSetSourcePos = refPut (transformParamRef sourcePosParam)

allocateVar :: Maybe FullName -> QScopeInterpreter (FullName, VarID)
allocateVar mname = do
    vs <- refGet varIDStateRef
    let
        (vid, name) =
            case mname of
                Just name' -> (mkVarID vs name', name')
                Nothing -> mkUniqueVarID vs
        biOriginalName = name
        biDocumentation = fromString "variable"
        biValue = ValueBinding (tsVar @QTypeSystem vid) Nothing
        insertScope = MkQScope (bindingInfoToMap (name, MkQBindingInfo {..})) mempty
    refPut varIDStateRef $ nextVarIDState vs
    refModifyM scopeRef $ \oldScope -> lift $ joinScopes insertScope oldScope
    return (name, vid)

getRestore :: Monad m => Ref m a -> m (m ())
getRestore r = do
    old <- refGet r
    return $ refPut r old

withCurrentNamespaceScope :: Namespace -> QScopeInterpreter (QScopeInterpreter ())
withCurrentNamespaceScope ns = do
    nrestore <- getRestore currentNamespaceRef
    refPut currentNamespaceRef ns
    return nrestore

usingNamespace :: Namespace -> Namespace -> (FullNameRef -> Bool) -> QScopeInterpreter ()
usingNamespace sourcens destns ff = refModify bindingsRef $ bindingMapLookupNamespace sourcens destns ff

registerScope :: QScope -> QScopeInterpreter ()
registerScope insertScope = refModifyM scopeRef $ \oldScope -> lift $ joinScopes insertScope oldScope

registerBinding :: FullName -> QBindingInfo -> QScopeInterpreter ()
registerBinding name db = registerBindings $ singletonMap name db

registerBindings :: [(FullName, QBindingInfo)] -> QScopeInterpreter ()
registerBindings bb = refModify bindingsRef $ \oldBindings -> oldBindings <> bindingInfosToMap bb

registerLetBindings :: [(FullName, RawMarkdown, QExpression)] -> QScopeInterpreter ()
registerLetBindings bb =
    registerBindings $ fmap (\(fname, doc, exp) -> (fname, MkQBindingInfo fname doc $ ValueBinding exp Nothing)) bb

registerLetBinding :: FullName -> RawMarkdown -> QExpression -> QScopeInterpreter ()
registerLetBinding name doc expr = registerLetBindings $ pure (name, doc, expr)

registerMatchBindings :: QMatch -> QScopeInterpreter ()
registerMatchBindings match = do
    let
        rbb =
            for (tsMatchBindings @QTypeSystem match) $ \(wvar, expr) -> do
                vn <- varIdNameRef wvar
                return (vn, "lambda", expr)
    case rbb of
        SuccessResult bb -> registerLetBindings bb
        FailureResult fn -> lift $ throw $ KnownIssueError 0 $ "bad match var: " <> showNamedText fn

checkName :: FullName -> QScopeInterpreter ()
checkName name = do
    mnt <- lift getBindingLookup
    case mnt $ fullNameRef name of
        Just _ -> lift $ throw $ DeclareTypeDuplicateError name
        Nothing -> return ()

registerBoundType :: FullName -> RawMarkdown -> QSomeGroundType -> QScopeInterpreter ()
registerBoundType name doc t = do
    checkName name
    registerBinding name $ MkQBindingInfo name doc $ TypeBinding t

registerType :: forall dv t. FullName -> RawMarkdown -> QGroundType dv t -> QScopeInterpreter ()
registerType name doc t = do
    checkName name
    registerBoundType name doc $ MkSomeGroundType t

registerPatternConstructor :: FullName -> RawMarkdown -> QExpression -> QPatternConstructor -> QScopeInterpreter ()
registerPatternConstructor name doc exp pc = do
    checkName name
    registerBinding name $ MkQBindingInfo name doc $ ValueBinding exp $ Just pc

registerRecord :: FullName -> RawMarkdown -> QRecordConstructor -> QScopeInterpreter ()
registerRecord name doc rc = do
    checkName name
    registerBinding name $ MkQBindingInfo name doc $ RecordConstructorBinding rc

registerSubtypeConversion :: QSubtypeConversionEntry -> QScopeInterpreter ()
registerSubtypeConversion sce = do
    newscope <- lift $ getSubtypeScope sce
    registerScope newscope
