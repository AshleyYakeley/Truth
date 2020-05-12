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
import Pinafore.Pinafore
import Pinafore.Storage
import Shapes
import Truth.Core
import Truth.World.ReferenceStore
import Truth.Debug.Subscriber

makeTestPinaforeContext :: UIToolkit -> LifeCycleIO (PinaforeContext PinaforeUpdate, IO (EditSubject PinaforeTableEdit))
makeTestPinaforeContext uitoolkit = do
    let rc = emptyResourceContext
    tableStateReference :: Reference (WholeEdit (EditSubject PinaforeTableEdit)) <-
        fmap (traceThing "makeTestPinaforeContext.tableStateObject") $
        liftIO $ makeMemoryReference ([], []) $ \_ -> True
    let
        tableReference :: Reference PinaforeTableEdit
        tableReference = convertReference tableStateReference
        getTableState :: IO (EditSubject PinaforeTableEdit)
        getTableState = getReferenceSubject rc tableStateReference
    memoryReference <- liftIO makeMemoryCellReference
    let
        picker :: forall update. PinaforeSelector update -> Premodel update ()
        picker PinaforeSelectPoint = traceThing "testObject.PinaforeSelectPoint" $ reflectingPremodel $ pinaforeTableEntityReference tableReference
        picker PinaforeSelectFile = traceThing "testObject.PinaforeSelectFile" $
            reflectingPremodel $
            mapReference (fromReadOnlyRejectingChangeLens @PinaforeFileUpdate) $
            readConstantReference $ constFunctionReadFunction nullSingleReferenceReadable
        picker PinaforeSelectMemory = traceThing "testObject.PinaforeSelectMemory" $ reflectingPremodel memoryReference
    (sub, ()) <- makeSharedModel $ tuplePremodel picker
    pc <- makePinaforeContext sub uitoolkit
    return (pc, getTableState)

withTestPinaforeContext ::
       UIToolkit
    -> ((?pinafore :: PinaforeContext PinaforeUpdate) => IO (EditSubject PinaforeTableEdit) -> IO r)
    -> IO r
withTestPinaforeContext uitoolkit f =
    withLifeCycle (makeTestPinaforeContext uitoolkit) $ \(pc, getTableState) -> let
        ?pinafore = pc
        in f getTableState

withNullPinaforeContext :: ((?pinafore :: PinaforeContext baseupdate) => r) -> r
withNullPinaforeContext f = let
    ?pinafore = nullPinaforeContext
    in f

runTestPinaforeSourceScoped ::
       PinaforePredefinitions baseupdate => PinaforeSourceScoped baseupdate a -> InterpretResult a
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
    editorUpdate var _ _ edits _ = do putMVar var edits
    editorDo :: MVar (NonEmpty (WholeUpdate a)) -> Reference (WholeEdit a) -> Task () -> LifeCycleIO ()
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
