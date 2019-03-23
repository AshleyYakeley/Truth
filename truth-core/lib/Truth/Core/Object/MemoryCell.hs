module Truth.Core.Object.MemoryCell where

import Data.IORef
import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.Object
import Truth.Core.Read
import Truth.Core.Types

type MemoryCellEdit = DependentEdit IORef

makeMemoryCellObject :: IO (Object MemoryCellEdit)
makeMemoryCellObject = do
    var <- newMVar ()
    let
        objRun = mvarUnliftIO var
        objRead :: MutableRead (StateT () IO) (EditReader MemoryCellEdit)
        objRead (MkTupleEditReader (MkDependentSelector ioref) ReadWhole) = liftIO $ readIORef ioref
        objEdit :: [MemoryCellEdit] -> StateT () IO (Maybe (StateT () IO ()))
        objEdit =
            singleAlwaysEdit $ \(MkTupleEdit (MkDependentSelector ioref) (MkWholeEdit a)) -> liftIO $ writeIORef ioref a
    return MkObject {..}

makeMemoryCellEditLens :: a -> IO (EditLens MemoryCellEdit (WholeEdit a))
makeMemoryCellEditLens a = do
    tvar <- newIORef a
    return $ dependentEditLens tvar