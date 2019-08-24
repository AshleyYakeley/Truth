module Truth.Core.Object.MemoryCell where

import Data.IORef
import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.EditContext
import Truth.Core.Object.Object
import Truth.Core.Object.UnliftIO
import Truth.Core.Read
import Truth.Core.Types

type MemoryCellEdit = DependentEdit WitnessedIORef

makeMemoryCellObject :: IO (Object MemoryCellEdit)
makeMemoryCellObject = do
    var <- newMVar ()
    let
        objRun = mvarUnliftIO var
        objRead :: MutableRead (StateT () IO) (EditReader MemoryCellEdit)
        objRead (MkTupleEditReader (MkDependentSelector ioref) ReadWhole) = liftIO $ readIORef $ unWitnessed ioref
        objEdit :: [MemoryCellEdit] -> StateT () IO (Maybe (EditSource -> StateT () IO ()))
        objEdit =
            singleAlwaysEdit $ \(MkTupleEdit (MkDependentSelector ioref) (MkWholeEdit a)) _ ->
                liftIO $ writeIORef (unWitnessed ioref) a
    return $ MkCloseUnliftIO objRun $ MkAnObject {..}

makeMemoryCellEditLens :: a -> IO (EditLens MemoryCellEdit (WholeEdit a))
makeMemoryCellEditLens a = do
    tvar <- newWitnessedIORef a
    return $ dependentEditLens tvar
