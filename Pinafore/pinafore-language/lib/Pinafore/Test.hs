module Pinafore.Test
    ( parseType
    , runInterpreter
    , QTypeSystem
    , Name
    , VarID
    , mkVarID
    , firstVarIDState
    , UVar
    , Var(..)
    , QGroundType(..)
    , EntityGroundType(..)
    , QValue
    , QType
    , QPolyShim
    , QOpenExpression
    , QExpression
    , QShimWit
    , QSingularType
    , QSingularShimWit
    , QInterpreter
    , toJMShimWit
    , allocateVar
    , QScopeInterpreter
    , registerType
    , registerLetBindings
    , registerLetBinding
    , registerPatternConstructor
    , registerSubtypeConversion
    , module Pinafore.Language.Expression
    , QTableSubject(..)
    , module Pinafore.Test
    ) where

import Changes.Core
import Pinafore
import Pinafore.Language.Expression
import Pinafore.Language.Grammar
import Pinafore.Language.Interpreter
import Pinafore.Language.Shim
import Pinafore.Language.Type
import Pinafore.Language.Var
import Pinafore.Language.VarID
import Shapes

makeTestStorageModel :: Lifecycle (Model QStorageUpdate, IO QTableSubject)
makeTestStorageModel = do
    tableStateReference :: Reference (WholeEdit QTableSubject) <-
        liftIO $ makeMemoryReference (MkQTableSubject [] [] [] []) $ \_ -> True
    let
        tableReference :: Reference QTableEdit
        tableReference = convertReference tableStateReference
        getTableState :: IO QTableSubject
        getTableState = getReferenceSubject emptyResourceContext tableStateReference
    (model, ()) <- makeSharedModel $ reflectingPremodel $ qTableEntityReference tableReference
    return (model, getTableState)

makeTestQContext :: Handle -> Lifecycle (QContext, IO QTableSubject)
makeTestQContext hout = do
    (model, getTableState) <- makeTestStorageModel
    pc <- makeQContext nullInvocationInfo {iiStdOut = handleSinkText hout} model
    return (pc, getTableState)

withTestQContext ::
       FetchModule
    -> Handle
    -> ((?qcontext :: QContext, ?library :: LibraryContext) => IO QTableSubject -> Lifecycle r)
    -> IO r
withTestQContext fetchModule hout call =
    runLifecycle $ do
        (pc, getTableState) <- makeTestQContext hout
        runWithContext pc fetchModule $ call getTableState

withNullQContext :: MonadIO m => ((?qcontext :: QContext, ?library :: LibraryContext) => m r) -> m r
withNullQContext = runWithContext nullQContext mempty

runTestPinaforeSourceScoped :: QInterpreter a -> InterpretResult a
runTestPinaforeSourceScoped sa = withNullQContext $ runPinaforeScoped "<input>" sa

checkUpdateEditor ::
       forall a. Eq a
    => a
    -> View ()
    -> Editor (WholeUpdate a) ()
checkUpdateEditor val push =
    MkEditor $ \_ -> do
        var <- liftIO newEmptyMVar
        let
            editingUpdate :: NonEmpty (WholeUpdate a) -> EditContext -> View ()
            editingUpdate updates _ = liftIO $ putMVar var updates
            editingDo :: Task IO () -> View ()
            editingDo _ = do
                push
                updates <- liftIO $ takeMVar var
                case updates of
                    MkWholeReaderUpdate v :| []
                        | v == val -> return ()
                    _ -> fail "unexpected push"
            editingTask = mempty
        return MkEditing {..}
