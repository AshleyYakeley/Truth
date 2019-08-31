module Truth.Core.Object.MemoryCell where

import Data.IORef
import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.EditContext
import Truth.Core.Object.Object
import Truth.Core.Object.UnliftIO
import Truth.Core.Read
import Truth.Core.Types

type MemoryCellUpdate = DependentUpdate WitnessedIORef

makeMemoryCellObject :: IO (Object (UpdateEdit MemoryCellUpdate))
makeMemoryCellObject = do
    var <- newMVar ()
    let
        objRun = mvarUnliftIO var
        objRead :: MutableRead (StateT () IO) (UpdateReader MemoryCellUpdate)
        objRead (MkTupleUpdateReader (MkDependentSelector ioref) ReadWhole) = liftIO $ readIORef $ unWitnessed ioref
        objEdit :: [UpdateEdit MemoryCellUpdate] -> StateT () IO (Maybe (EditSource -> StateT () IO ()))
        objEdit =
            singleAlwaysEdit $ \(MkTupleUpdateEdit (MkDependentSelector ioref) (MkWholeReaderEdit a)) _ ->
                liftIO $ writeIORef (unWitnessed ioref) a
    return $ MkCloseUnliftIO objRun $ MkAnObject {..}

makeMemoryCellEditLens :: a -> IO (EditLens MemoryCellUpdate (WholeUpdate a))
makeMemoryCellEditLens a = do
    tvar <- newWitnessedIORef a
    return $ dependentEditLens tvar
