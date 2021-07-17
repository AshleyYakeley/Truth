module Pinafore.Test
    ( parseType
    , runInterpreter
    , runSourcePos
    , PinaforeTypeSystem
    , Name
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
import Shapes
import Changes.Debug.Subscriber

makeTestStorageModel :: LifeCycle (Model PinaforeStorageUpdate, IO PinaforeTableSubject)
makeTestStorageModel = do
    tableStateReference :: Reference (WholeEdit PinaforeTableSubject) <-
        fmap (traceThing "makeTestPinaforeContext.tableStateObject") $
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
    -> ((?pinafore :: PinaforeContext, ?library :: LibraryContext) =>
                ChangesContext -> MFunction LifeCycle IO -> IO PinaforeTableSubject -> IO r)
    -> IO r
withTestPinaforeContext fetchModule hout call =
    runLifeCycle @LifeCycle $
    liftIOWithUnlift $ \unlift -> do
        let cc = nullChangesContext unlift
        (pc, getTableState) <- unlift $ makeTestPinaforeContext cc hout
        runWithContext pc fetchModule $ call cc unlift getTableState

withNullPinaforeContext :: MonadIO m => ((?pinafore :: PinaforeContext, ?library :: LibraryContext) => m r) -> m r
withNullPinaforeContext = runWithContext nullPinaforeContext mempty

runTestPinaforeSourceScoped :: PinaforeSourceInterpreter a -> InterpretResult a
runTestPinaforeSourceScoped sa = withNullPinaforeContext $ runPinaforeSourceScoped "<input>" sa

checkUpdateEditor ::
       forall a. Eq a
    => a
    -> IO ()
    -> Editor (WholeUpdate a) ()
checkUpdateEditor val push = let
    editorInit :: Reference (WholeEdit a) -> LifeCycle (MVar (NonEmpty (WholeUpdate a)))
    editorInit _ = liftIO newEmptyMVar
    editorUpdate ::
           MVar (NonEmpty (WholeUpdate a))
        -> Reference (WholeEdit a)
        -> ResourceContext
        -> NonEmpty (WholeUpdate a)
        -> EditContext
        -> IO ()
    editorUpdate var _ _ edits _ = putMVar var edits
    editorDo :: MVar (NonEmpty (WholeUpdate a)) -> Reference (WholeEdit a) -> Task () -> LifeCycle ()
    editorDo var _ _ =
        traceBracket "checkUpdateEditor.do" $ liftIO $ do
            traceBracket "checkUpdateEditor.push" $ push
            edits <- traceBracket "checkUpdateEditor.takeMVar" $ takeMVar var
            case edits of
                MkWholeReaderUpdate v :| []
                    | v == val -> return ()
                _ -> fail "unexpected push"
    editorTask = mempty
    in MkEditor {..}
