module Pinafore.Language.Interpreter.ScopeBuilder
    ( QScopeBuilder
    , builderLift
    , withScopeBuilder
    , runScopeBuilder
    , scopeRef
    , builderDeclarationsProd
    , builderDocsProd
    , registerDeclarations
    , outputDeclarations
    , QFixBox
    , scopeSetSourcePos
    , allocateLambdaVar
    , allocatePolymorphicVar
    , withCurrentNamespaceScope
    )
where

import Import
import Pinafore.Language.Error
import Pinafore.Language.Interpreter.Binding
import Pinafore.Language.Interpreter.Declarations
import Pinafore.Language.Interpreter.Interpreter
import Pinafore.Language.Interpreter.Scope
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Subtype ()
import Pinafore.Language.VarID

newtype QScopeBuilder a
    = MkQScopeBuilder (WriterT QDeclarations (WithT QInterpreter) a)
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadException
        , MonadThrow QLocatedError
        , MonadCatch QLocatedError
        , MonadThrow PatternError
        , MonadThrow QError
        , MonadHoistIO
        )

instance Semigroup a => Semigroup (QScopeBuilder a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (QScopeBuilder a) where
    mappend = (<>)
    mempty = pure mempty

withScopeBuilder :: QScopeBuilder a -> (a -> QInterpreter b) -> QInterpreter b
withScopeBuilder (MkQScopeBuilder wsb) aib = unWithT (runWriterT wsb) $ \(a, _) -> aib a

runScopeBuilder :: QScopeBuilder () -> QInterpreter QDeclarations
runScopeBuilder (MkQScopeBuilder wsb) = unWithT (runWriterT wsb) $ \((), sd) -> return sd

builderDeclarationsProd :: Prod QScopeBuilder QDeclarations
builderDeclarationsProd =
    MkProd
        (\a -> MkQScopeBuilder $ prodTell writerProd a)
        (\(MkQScopeBuilder mr) -> MkQScopeBuilder $ prodCollect writerProd mr)

builderDocsProd :: Prod QScopeBuilder Docs
builderDocsProd = lensMapProd (\bfb a -> fmap (\b -> a{declsDocs = b}) $ bfb $ declsDocs a) builderDeclarationsProd

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

allocateVar :: (VarIDState -> (VarID, FullName)) -> QScopeBuilder (FullName, VarID)
allocateVar mkvar = do
    vs <- refSucc varIDStateRef
    let
        (vid, name) = mkvar vs
        siOriginalName = name
        siDocumentation = MkDefDoc (ValueDocItem (pure $ fullNameRef name) "") "variable"
        siItem = ValueItem $ tsVar @QTypeSystem vid
        insertScope = MkQScope (bindingInfoToMap (name, MkQScopeItem{..})) mempty
    refModifyM scopeRef $ \oldScope -> builderLift $ joinScopes oldScope insertScope
    return (name, vid)

allocateLambdaVar :: Maybe FullName -> QScopeBuilder (FullName, VarID)
allocateLambdaVar mname = allocateVar $ \vs -> mkLambdaVarID vs mname

allocatePolymorphicVar :: FullName -> QScopeBuilder (FullName, VarID)
allocatePolymorphicVar name = allocateVar $ \vs -> (mkPolymorphicVarID vs name, name)

withCurrentNamespaceScope :: Namespace -> QScopeBuilder a -> QScopeBuilder a
withCurrentNamespaceScope ns ma = paramWith (refParam currentNamespaceRef) ns ma

outputDeclarations :: QDeclarations -> QScopeBuilder ()
outputDeclarations sd = prodTell builderDeclarationsProd sd

registerDeclarations :: QDeclarations -> QScopeBuilder ()
registerDeclarations sd = do
    refModifyM scopeRef $ \oldScope -> builderLift $ joinAllScopes $ oldScope : declsScopes sd
    outputDeclarations sd
