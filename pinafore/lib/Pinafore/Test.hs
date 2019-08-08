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
import Pinafore.Storage.File
import Shapes
import Truth.Core
import Truth.World.Clock
import Truth.World.ObjectStore
import Truth.Debug.Subscriber

makeTestPinaforeContext ::
       Bool -> UIToolkit -> LifeCycleIO (PinaforeContext PinaforeEdit, IO (EditSubject PinaforeTableEdit))
makeTestPinaforeContext async uitoolkit = do
    tableStateObject :: Object (WholeEdit (EditSubject PinaforeTableEdit)) <-
        liftIO $ freeIOObject ([], []) $ \_ -> True
    memoryObject <- liftIO makeMemoryCellObject
    subTable <- makeObjectSubscriber async $ pinaforeTableEntityObject $ convertObject tableStateObject
    subFile :: Subscriber PinaforeFileEdit <-
        makeObjectSubscriber False $ readConstantObject $ constFunctionReadFunction nullSingleObjectMutableRead
    subMemory <- makeObjectSubscriber False memoryObject
    (subClock, ()) <-
        makeSharedSubscriber False $
        clockUpdatingObject (UTCTime (fromGregorian 2000 1 1) 0) (secondsToNominalDiffTime 1)
    let
        picker :: forall edit. PinaforeSelector edit -> Subscriber edit
        picker PinaforeSelectPoint = traceThing "testObject.PinaforeSelectPoint" $ subTable
        picker PinaforeSelectFile = traceThing "testObject.PinaforeSelectFile" $ subFile
        picker PinaforeSelectMemory = traceThing "testObject.PinaforeSelectMemory" $ subMemory
        picker PinaforeSelectClock = traceThing "testObject.PinaforeSelectClock" $ subClock
        getTableState :: IO (EditSubject PinaforeTableEdit)
        getTableState = getObjectSubject tableStateObject
    pc <- makePinaforeContext (tupleSubscribers picker) uitoolkit
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
