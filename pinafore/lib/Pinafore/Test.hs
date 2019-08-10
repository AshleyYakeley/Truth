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
       Bool -> UIToolkit -> LifeCycleIO (PinaforeContext PinaforeEdit, IO (EditSubject PinaforeTableEdit))
makeTestPinaforeContext async uitoolkit = do
    tableStateObject :: Object (WholeEdit (EditSubject PinaforeTableEdit)) <-
        liftIO $ freeIOObject ([], []) $ \_ -> True
    let
        tableObject :: Object PinaforeTableEdit
        tableObject = convertObject tableStateObject
        getTableState :: IO (EditSubject PinaforeTableEdit)
        getTableState = getObjectSubject tableStateObject
    memoryObject <- liftIO makeMemoryCellObject
    clockUO <-
        shareUpdatingObject async $
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
    (sub, ()) <- makeSharedSubscriber async $ tupleUpdatingObject picker
    pc <- makePinaforeContext sub uitoolkit
    return (pc, getTableState)

withTestPinaforeContext ::
       Bool
    -> UIToolkit
    -> ((?pinafore :: PinaforeContext PinaforeEdit) => IO (EditSubject PinaforeTableEdit) -> IO r)
    -> IO r
withTestPinaforeContext async uitoolkit f =
    withLifeCycle (makeTestPinaforeContext async uitoolkit) $ \(pc, getTableState) -> let
        ?pinafore = pc
        in f getTableState

withNullPinaforeContext :: ((?pinafore :: PinaforeContext baseedit) => r) -> r
withNullPinaforeContext f = let
    ?pinafore = nullPinaforeContext
    in f

runTestPinaforeSourceScoped :: PinaforePredefinitions baseedit => PinaforeSourceScoped baseedit a -> InterpretResult a
runTestPinaforeSourceScoped sa = withNullPinaforeContext $ runPinaforeSourceScoped "<input>" sa
