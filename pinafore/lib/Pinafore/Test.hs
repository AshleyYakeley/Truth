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
import Pinafore.Language.Type
import Pinafore.Language.Type.Simplify
import Pinafore.Pinafore
import Pinafore.Storage
import Shapes
import Truth.Core
import Truth.World.Clock
import Truth.World.ObjectStore

makeTestPinaforeContext ::
       UpdateTiming -> UIToolkit -> LifeCycleIO (PinaforeContext PinaforeEdit, IO (EditSubject PinaforeTableEdit))
makeTestPinaforeContext ut uitoolkit = do
    tableStateObject :: Object (WholeEdit (EditSubject PinaforeTableEdit)) <-
        liftIO $ freeIOObject ([], []) $ \_ -> True
    let
        tableObject :: Object PinaforeTableEdit
        tableObject = convertObject tableStateObject
        getTableState :: IO (EditSubject PinaforeTableEdit)
        getTableState = getObjectSubject tableStateObject
    memoryObject <- liftIO makeMemoryCellObject
    clockUO <-
        shareUpdatingObject False $
        clockUpdatingObject (UTCTime (fromGregorian 2000 1 1) 0) (secondsToNominalDiffTime 1)
    clockTimeEF <- liftIO makeClockTimeZoneEF
    let
        picker :: forall edit. PinaforeSelector edit -> UpdatingObject edit ()
        picker PinaforeSelectPoint = updatingObject $ pinaforeTableEntityObject tableObject
        picker PinaforeSelectFile =
            updatingObject $ readConstantObject $ constFunctionReadFunction nullSingleObjectMutableRead
        picker PinaforeSelectMemory = updatingObject memoryObject
        picker PinaforeSelectClock = clockUO
        picker PinaforeSelectTimeZone = lensUpdatingObject (readOnlyEditLens clockTimeEF) clockUO
    (sub, ()) <- makeSharedSubscriber ut $ tupleUpdatingObject picker
    pc <- makePinaforeContext sub uitoolkit
    return (pc, getTableState)

withTestPinaforeContext ::
       UpdateTiming
    -> UIToolkit
    -> ((?pinafore :: PinaforeContext PinaforeEdit) => IO (EditSubject PinaforeTableEdit) -> IO r)
    -> IO r
withTestPinaforeContext ut uitoolkit f =
    withLifeCycle (makeTestPinaforeContext ut uitoolkit) $ \(pc, getTableState) -> let
        ?pinafore = pc
        in f getTableState

withNullPinaforeContext :: ((?pinafore :: PinaforeContext baseedit) => r) -> r
withNullPinaforeContext f = let
    ?pinafore = nullPinaforeContext
    in f

runTestPinaforeSourceScoped :: PinaforePredefinitions baseedit => PinaforeSourceScoped baseedit a -> InterpretResult a
runTestPinaforeSourceScoped sa = withNullPinaforeContext $ runPinaforeSourceScoped "<input>" sa

checkUpdateEditor ::
       forall a. Eq a
    => a
    -> IO ()
    -> Editor (WholeEdit a) ()
checkUpdateEditor val push = let
    editorInit :: Object (WholeEdit a) -> LifeCycleIO (MVar [WholeEdit a])
    editorInit _ = liftIO newEmptyMVar
    editorUpdate :: MVar [WholeEdit a] -> Object (WholeEdit a) -> [WholeEdit a] -> EditContext -> IO ()
    editorUpdate var _ edits _ = do putMVar var edits
    editorDo :: MVar [WholeEdit a] -> Object (WholeEdit a) -> LifeCycleIO ()
    editorDo var _ =
        liftIO $ do
            push
            edits <- takeMVar var
            case edits of
                [MkWholeEdit v]
                    | v == val -> return ()
                _ -> fail "unexpected push"
    in MkEditor {..}
