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
    , PinaforeTableSubject(..)
    , module Pinafore.Test
    ) where

import Changes.Core
import Pinafore.Base
import Pinafore.Context
import Pinafore.Language
import Pinafore.Language.Interpret
import Pinafore.Language.Read
import Pinafore.Language.Shim
import Pinafore.Language.Type
import Pinafore.Language.Var
import Pinafore.Storage
import Shapes

textFetchModule :: (ModuleName -> IO (Maybe Text)) -> FetchModule
textFetchModule getText =
    MkFetchModule $ \mname -> do
        mtext <- getText mname
        return $ do
            text <- mtext
            return $ (show mname, SuccessResult text)

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

makeTestPinaforeContext ::
       FetchModule -> ChangesContext -> Handle -> LifeCycle (PinaforeContext, IO PinaforeTableSubject)
makeTestPinaforeContext fetchModule cc hout = do
    (model, getTableState) <- makeTestStorageModel
    pc <- makePinaforeContext fetchModule nullInvocationInfo hout model cc
    return (pc, getTableState)

withTestPinaforeContext ::
       FetchModule
    -> Handle
    -> ((?pinafore :: PinaforeContext) => ChangesContext -> MFunction LifeCycle IO -> IO PinaforeTableSubject -> IO r)
    -> IO r
withTestPinaforeContext fetchModule hout call =
    runLifeCycle @LifeCycle $
    liftIOWithUnlift $ \unlift -> do
        let cc = nullChangesContext unlift
        (pc, getTableState) <- unlift $ makeTestPinaforeContext fetchModule cc hout
        let
            ?pinafore = pc
            in call cc unlift getTableState

withNullPinaforeContext :: ((?pinafore :: PinaforeContext) => r) -> r
withNullPinaforeContext f = let
    ?pinafore = nullPinaforeContext
    in f

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
        liftIO $ do
            push
            edits <- takeMVar var
            case edits of
                MkWholeReaderUpdate v :| []
                    | v == val -> return ()
                _ -> fail "unexpected push"
    editorTask = mempty
    in MkEditor {..}
