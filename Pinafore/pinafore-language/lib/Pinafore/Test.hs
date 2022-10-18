module Pinafore.Test
    ( parseType
    , runInterpreter
    , PinaforeTypeSystem
    , Name
    , VarID
    , mkVarID
    , firstVarIDState
    , UVar
    , Var(..)
    , PinaforeGroundType(..)
    , EntityGroundType(..)
    , PinaforeValue
    , PinaforeType
    , PinaforePolyShim
    , PinaforeOpenExpression
    , PinaforeExpression
    , PinaforeShimWit
    , PinaforeSingularType
    , PinaforeSingularShimWit
    , PinaforeInterpreter
    , toJMShimWit
    , allocateVar
    , PinaforeScopeInterpreter
    , registerType
    , registerLetBindings
    , registerLetBinding
    , registerPatternConstructor
    , registerSubtypeConversion
    , module Pinafore.Language.Expression
    , PinaforeTableSubject(..)
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

makeTestStorageModel :: Lifecycle (Model PinaforeStorageUpdate, IO PinaforeTableSubject)
makeTestStorageModel = do
    tableStateReference :: Reference (WholeEdit PinaforeTableSubject) <-
        liftIO $ makeMemoryReference (MkPinaforeTableSubject [] [] [] []) $ \_ -> True
    let
        tableReference :: Reference PinaforeTableEdit
        tableReference = convertReference tableStateReference
        getTableState :: IO PinaforeTableSubject
        getTableState = getReferenceSubject emptyResourceContext tableStateReference
    (model, ()) <- makeSharedModel $ reflectingPremodel $ pinaforeTableEntityReference tableReference
    return (model, getTableState)

makeTestPinaforeContext :: Handle -> Lifecycle (PinaforeContext, IO PinaforeTableSubject)
makeTestPinaforeContext hout = do
    (model, getTableState) <- makeTestStorageModel
    pc <- makePinaforeContext nullInvocationInfo hout model
    return (pc, getTableState)

withTestPinaforeContext ::
       FetchModule
    -> Handle
    -> ((?pinafore :: PinaforeContext, ?library :: LibraryContext) => IO PinaforeTableSubject -> Lifecycle r)
    -> IO r
withTestPinaforeContext fetchModule hout call =
    runLifecycle $ do
        (pc, getTableState) <- makeTestPinaforeContext hout
        runWithContext pc fetchModule $ call getTableState

withNullPinaforeContext :: MonadIO m => ((?pinafore :: PinaforeContext, ?library :: LibraryContext) => m r) -> m r
withNullPinaforeContext = runWithContext nullPinaforeContext mempty

runTestPinaforeSourceScoped :: PinaforeInterpreter a -> InterpretResult a
runTestPinaforeSourceScoped sa = withNullPinaforeContext $ runPinaforeScoped "<input>" sa

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
