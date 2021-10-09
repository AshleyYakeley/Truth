module Pinafore.Test
    ( parseType
    , runInterpreter
    , runSourcePos
    , PinaforeTypeSystem
    , Name
    , VarID
    , mkVarID
    , firstVarIDState
    , UVar
    , Var(..)
    , PinaforeGroundType(..)
    , EntityGroundType(..)
    , PinaforeType
    , PinaforePolyShim
    , PinaforeShimWit
    , PinaforeSingularType
    , PinaforeSingularShimWit
    , PinaforeInterpreter
    , PinaforeSourceInterpreter
    , toJMShimWit
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

makeTestStorageModel :: LifeCycle (Model PinaforeStorageUpdate, IO PinaforeTableSubject)
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

makeTestPinaforeContext :: ChangesContext -> Handle -> LifeCycle (PinaforeContext, IO PinaforeTableSubject)
makeTestPinaforeContext cc hout = do
    (model, getTableState) <- makeTestStorageModel
    pc <- makePinaforeContext nullInvocationInfo hout model cc
    return (pc, getTableState)

withTestPinaforeContext ::
       FetchModule
    -> Handle
    -> ((?pinafore :: PinaforeContext, ?library :: LibraryContext) => ChangesContext -> IO PinaforeTableSubject -> IO r)
    -> IO r
withTestPinaforeContext fetchModule hout call =
    runLifeCycle @LifeCycle $
    liftIOWithUnlift $ \unlift -> do
        let cc = nullChangesContext unlift
        (pc, getTableState) <- unlift $ makeTestPinaforeContext cc hout
        runWithContext pc fetchModule $ call cc getTableState

withNullPinaforeContext :: MonadIO m => ((?pinafore :: PinaforeContext, ?library :: LibraryContext) => m r) -> m r
withNullPinaforeContext = runWithContext nullPinaforeContext mempty

runTestPinaforeSourceScoped :: PinaforeSourceInterpreter a -> InterpretResult a
runTestPinaforeSourceScoped sa = withNullPinaforeContext $ runPinaforeSourceScoped "<input>" sa

checkUpdateEditor ::
       forall a. Eq a
    => a
    -> CreateView ()
    -> Editor (WholeUpdate a) ()
checkUpdateEditor val push =
    MkEditor $ \_ -> do
        var <- liftIO newEmptyMVar
        let
            editingUpdate :: NonEmpty (WholeUpdate a) -> EditContext -> View ()
            editingUpdate updates _ = liftIO $ putMVar var updates
            editingDo :: Task () -> CreateView ()
            editingDo _ = do
                push
                updates <- liftIO $ takeMVar var
                case updates of
                    MkWholeReaderUpdate v :| []
                        | v == val -> return ()
                    _ -> fail "unexpected push"
            editingTask = mempty
        return MkEditing {..}
