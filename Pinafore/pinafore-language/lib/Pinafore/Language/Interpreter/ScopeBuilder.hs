module Pinafore.Language.Interpreter.ScopeBuilder
    ( QScopeBuilder
    , builderLift
    , withScopeBuilder
    , runScopeBuilder
    , scopeRef
    , builderScopeDocsProd
    , builderDocsProd
    , registerScopeDocs
    , outputScopeDocs
    , QFixBox
    , scopeSetSourcePos
    , allocateVar
    , withCurrentNamespaceScope
    ) where

import Import
import Pinafore.Language.Error
import Pinafore.Language.Interpreter.Binding
import Pinafore.Language.Interpreter.Interpreter
import Pinafore.Language.Interpreter.Scope
import Pinafore.Language.Interpreter.ScopeDocs
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Subtype ()
import Pinafore.Language.VarID

newtype QScopeBuilder a =
    MkQScopeBuilder (WriterT QScopeDocs (WithT QInterpreter) a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadException
             , MonadThrow QError
             , MonadCatch QError
             , MonadThrow PatternError
             , MonadThrow QErrorType
             , MonadHoistIO
             )

instance Semigroup a => Semigroup (QScopeBuilder a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (QScopeBuilder a) where
    mappend = (<>)
    mempty = pure mempty

withScopeBuilder :: QScopeBuilder a -> (a -> QInterpreter b) -> QInterpreter b
withScopeBuilder (MkQScopeBuilder wsb) aib = unWithT (runWriterT wsb) $ \(a, _) -> aib a

runScopeBuilder :: QScopeBuilder () -> QInterpreter QScopeDocs
runScopeBuilder (MkQScopeBuilder wsb) = unWithT (runWriterT wsb) $ \((), sd) -> return sd

builderScopeDocsProd :: Prod QScopeBuilder QScopeDocs
builderScopeDocsProd =
    MkProd
        (\a -> MkQScopeBuilder $ prodTell writerProd a)
        (\(MkQScopeBuilder mr) -> MkQScopeBuilder $ prodCollect writerProd mr)

builderDocsProd :: Prod QScopeBuilder Docs
builderDocsProd = lensMapProd (\bfb a -> fmap (\b -> a {sdDocs = b}) $ bfb $ sdDocs a) builderScopeDocsProd

type QFixBox = FixBox QScopeBuilder

builderLift :: QInterpreter --> QScopeBuilder
builderLift ma = MkQScopeBuilder $ lift $ lift ma

builderLiftRef :: Param QInterpreter a -> Ref QScopeBuilder a
builderLiftRef param = let
    ref = liftRef $ withParamRef param
    in MkRef (MkQScopeBuilder $ refGet ref) (\a -> MkQScopeBuilder $ refPut ref a)

scopeRef :: Ref QScopeBuilder QScope
scopeRef = builderLiftRef scopeParam

currentNamespaceRef :: Ref QScopeBuilder Namespace
currentNamespaceRef = builderLiftRef currentNamespaceParam

varIDStateRef :: Ref QScopeBuilder VarIDState
varIDStateRef = builderLiftRef varIDStateParam

scopeSetSourcePos :: SourcePos -> QScopeBuilder ()
scopeSetSourcePos = refPut (builderLiftRef sourcePosParam)

allocateVar :: Maybe FullName -> QScopeBuilder (FullName, VarID)
allocateVar mname = do
    vs <- refSucc varIDStateRef
    let
        (vid, name) =
            case mname of
                Just name' -> (mkVarID vs name', name')
                Nothing -> mkUniqueVarID vs
        biOriginalName = name
        biDocumentation = MkDefDoc (ValueDocItem (pure $ fullNameRef name) "") "variable"
        biValue = ValueBinding (tsVar @QTypeSystem vid) Nothing
        insertScope = MkQScope (bindingInfoToMap (name, MkQBindingInfo {..})) mempty
    refModifyM scopeRef $ \oldScope -> builderLift $ joinScopes oldScope insertScope
    return (name, vid)

withCurrentNamespaceScope :: Namespace -> QScopeBuilder a -> QScopeBuilder a
withCurrentNamespaceScope ns ma = paramWith (refParam currentNamespaceRef) ns ma

outputScopeDocs :: QScopeDocs -> QScopeBuilder ()
outputScopeDocs sd = prodTell builderScopeDocsProd sd

registerScopeDocs :: QScopeDocs -> QScopeBuilder ()
registerScopeDocs sd = do
    refModifyM scopeRef $ \oldScope -> builderLift $ joinAllScopes $ oldScope : sdScopes sd
    outputScopeDocs sd
