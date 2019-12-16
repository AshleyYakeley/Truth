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
import Data.Time
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
import Truth.World.Clock
import Truth.World.ObjectStore

makeTestPinaforeContext :: UIToolkit -> LifeCycleIO (PinaforeContext PinaforeUpdate, IO (EditSubject PinaforeTableEdit))
makeTestPinaforeContext uitoolkit = do
    tableStateObject :: Object (WholeEdit (EditSubject PinaforeTableEdit)) <-
        liftIO $ freeIOObject ([], []) $ \_ -> True
    let
        tableObject :: Object PinaforeTableEdit
        tableObject = convertObject tableStateObject
        getTableState :: IO (EditSubject PinaforeTableEdit)
        getTableState = getObjectSubject tableStateObject
    memoryObject <- liftIO makeMemoryCellObject
    clockOM <- shareObjectMaker $ clockObjectMaker (UTCTime (fromGregorian 2000 1 1) 0) (secondsToNominalDiffTime 1)
    clockTimeEF <- makeClockTimeZoneEF
    let
        picker :: forall update. PinaforeSelector update -> ObjectMaker update ()
        picker PinaforeSelectPoint = reflectingObjectMaker $ pinaforeTableEntityObject tableObject
        picker PinaforeSelectFile =
            reflectingObjectMaker $
            mapObject (fromReadOnlyRejectingEditLens @PinaforeFileUpdate) $
            readConstantObject $ constFunctionReadFunction nullSingleObjectMutableRead
        picker PinaforeSelectMemory = reflectingObjectMaker memoryObject
        picker PinaforeSelectClock = clockOM
        picker PinaforeSelectTimeZone =
            mapObjectMaker (updateFunctionToEditLens $ clockTimeEF . fromReadOnlyUpdateFunction) clockOM
    (sub, ()) <- makeSharedSubscriber $ tupleObjectMaker picker
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
    editorInit :: Object (WholeEdit a) -> LifeCycleIO (MVar (NonEmpty (WholeUpdate a)))
    editorInit _ = liftIO newEmptyMVar
    editorUpdate ::
           MVar (NonEmpty (WholeUpdate a)) -> Object (WholeEdit a) -> NonEmpty (WholeUpdate a) -> EditContext -> IO ()
    editorUpdate var _ edits _ = do putMVar var edits
    editorDo :: MVar (NonEmpty (WholeUpdate a)) -> Object (WholeEdit a) -> LifeCycleIO ()
    editorDo var _ =
        liftIO $ do
            push
            edits <- takeMVar var
            case edits of
                MkWholeReaderUpdate v :| []
                    | v == val -> return ()
                _ -> fail "unexpected push"
    in MkEditor {..}
