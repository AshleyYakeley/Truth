module Pinafore.Test
    ( parseType
    , runScoped
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
    , PinaforeScoped
    , PinaforeSourceScoped
    , toJMShimWit
    , PinaforeTableSubject(..)
    , module Pinafore.Test
    ) where

import Changes.Core
import Pinafore.Context
import Pinafore.Language
import Pinafore.Language.Read
import Pinafore.Language.Scope
import Pinafore.Language.Shim
import Pinafore.Language.Type
import Pinafore.Language.Var
import Pinafore.Storage
import Shapes

nullFetchModuleText :: ModuleName -> IO (Maybe Text)
nullFetchModuleText _ = return Nothing

makeTestPinaforeContext ::
       (ModuleName -> IO (Maybe Text))
    -> ChangesContext
    -> Handle
    -> LifeCycle (PinaforeContext, IO PinaforeTableSubject)
makeTestPinaforeContext fetchModuleText tc hout = do
    let rc = emptyResourceContext
    tableStateReference :: Reference (WholeEdit PinaforeTableSubject) <-
        liftIO $ makeMemoryReference (MkPinaforeTableSubject [] [] [] []) $ \_ -> True
    let
        tableReference :: Reference PinaforeTableEdit
        tableReference = convertReference tableStateReference
        getTableState :: IO PinaforeTableSubject
        getTableState = getReferenceSubject rc tableStateReference
        fetchModule :: ModuleName -> IO (Maybe (FilePath, Result UnicodeException Text))
        fetchModule mname = do
            mtext <- fetchModuleText mname
            return $ do
                text <- mtext
                return $ (show mname, SuccessResult text)
    (model, ()) <- makeSharedModel $ reflectingPremodel $ pinaforeTableEntityReference tableReference
    pc <- makePinaforeContext fetchModule nullInvocationInfo hout model tc
    return (pc, getTableState)

withTestPinaforeContext ::
       (ModuleName -> IO (Maybe Text))
    -> Handle
    -> ((?pinafore :: PinaforeContext) => ChangesContext -> MFunction LifeCycle IO -> IO PinaforeTableSubject -> IO r)
    -> IO r
withTestPinaforeContext fetchModuleText hout call =
    runLifeCycle @LifeCycle $
    liftIOWithUnlift $ \unlift -> do
        let tc = nullChangesContext unlift
        (pc, getTableState) <- unlift $ makeTestPinaforeContext fetchModuleText tc hout
        let
            ?pinafore = pc
            in call tc unlift getTableState

withNullPinaforeContext :: ((?pinafore :: PinaforeContext) => r) -> r
withNullPinaforeContext f = let
    ?pinafore = nullPinaforeContext
    in f

runTestPinaforeSourceScoped :: PinaforeSourceScoped a -> InterpretResult a
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
