module Pinafore.Test
    ( parseType
    , runScoped
    , runSourceScoped
    , runSourcePos
    , PinaforeTypeSystem
    , Name
    , UVar
    , PinaforeType
    , PinaforeShimWit
    , PinaforeScoped
    , PinaforeSourceScoped
    , pinaforeSimplifyTypes
    , toJMShimWit
    , module Pinafore.Test
    ) where

import Data.Shim
import Pinafore.Base
import Pinafore.Language
import Pinafore.Language.Name
import Pinafore.Language.Read
import Pinafore.Language.TypeSystem
import Pinafore.Language.TypeSystem.Simplify
import Pinafore.Storage
import Shapes
import Truth.Core

makeTestPinaforeContext :: UIToolkit -> LifeCycleIO (PinaforeContext, IO (EditSubject PinaforeTableEdit))
makeTestPinaforeContext uitoolkit = do
    let rc = emptyResourceContext
    tableStateReference :: Reference (WholeEdit (EditSubject PinaforeTableEdit)) <-
        liftIO $ makeMemoryReference ([], []) $ \_ -> True
    let
        tableReference :: Reference PinaforeTableEdit
        tableReference = convertReference tableStateReference
        getTableState :: IO (EditSubject PinaforeTableEdit)
        getTableState = getReferenceSubject rc tableStateReference
    (model, ()) <- makeSharedModel $ reflectingPremodel $ pinaforeTableEntityReference tableReference
    pc <- makePinaforeContext model uitoolkit
    return (pc, getTableState)

withTestPinaforeContext ::
       ((?pinafore :: PinaforeContext) =>
                UIToolkit -> MFunction LifeCycleIO IO -> IO (EditSubject PinaforeTableEdit) -> IO r)
    -> IO r
withTestPinaforeContext call =
    runLifeCycle $
    liftWithUnlift $ \unlift -> do
        let uitoolkit = nullUIToolkit unlift
        (pc, getTableState) <- unlift $ makeTestPinaforeContext uitoolkit
        let
            ?pinafore = pc
            in call uitoolkit unlift getTableState

withNullPinaforeContext :: ((?pinafore :: PinaforeContext) => r) -> r
withNullPinaforeContext f = let
    ?pinafore = nullPinaforeContext
    in f

runTestPinaforeSourceScoped :: PinaforeSourceScoped baseupdate a -> InterpretResult a
runTestPinaforeSourceScoped sa = withNullPinaforeContext $ runPinaforeSourceScoped "<input>" sa

checkUpdateEditor ::
       forall a. Eq a
    => a
    -> IO ()
    -> Editor (WholeUpdate a) ()
checkUpdateEditor val push = let
    editorInit :: Reference (WholeEdit a) -> LifeCycleIO (MVar (NonEmpty (WholeUpdate a)))
    editorInit _ = liftIO newEmptyMVar
    editorUpdate ::
           MVar (NonEmpty (WholeUpdate a))
        -> Reference (WholeEdit a)
        -> ResourceContext
        -> NonEmpty (WholeUpdate a)
        -> EditContext
        -> IO ()
    editorUpdate var _ _ edits _ = do
        hPutStrLn stderr "X"
        putMVar var edits
        hPutStrLn stderr "Y"
    editorDo :: MVar (NonEmpty (WholeUpdate a)) -> Reference (WholeEdit a) -> Task () -> LifeCycleIO ()
    editorDo var _ _ =
        liftIO $ do
            hPutStrLn stderr "A"
            push
            hPutStrLn stderr "B"
            edits <- takeMVar var
            hPutStrLn stderr "C"
            case edits of
                MkWholeReaderUpdate v :| []
                    | v == val -> return ()
                _ -> fail "unexpected push"
    editorTask = mempty
    in MkEditor {..}
