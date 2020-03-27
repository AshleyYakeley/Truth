module Truth.Core.Reference.MemoryCell where

import Data.IORef
import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Lens
import Truth.Core.Read
import Truth.Core.Reference.EditContext
import Truth.Core.Reference.Reference
import Truth.Core.Resource
import Truth.Core.Types

type MemoryCellUpdate = DependentUpdate WitnessedIORef

makeMemoryCellReference :: IO (Reference (UpdateEdit MemoryCellUpdate))
makeMemoryCellReference = do
    iow <- newIOWitness
    var <- newMVar ()
    let
        objRun :: ResourceRunner '[ StateT ()]
        objRun = mvarResourceRunner iow var
        refRead :: Readable (StateT () IO) (UpdateReader MemoryCellUpdate)
        refRead (MkTupleUpdateReader (MkDependentSelector ioref) ReadWhole) = liftIO $ readIORef $ unWitnessed ioref
        refEdit :: NonEmpty (UpdateEdit MemoryCellUpdate) -> StateT () IO (Maybe (EditSource -> StateT () IO ()))
        refEdit =
            singleAlwaysEdit $ \(MkTupleUpdateEdit (MkDependentSelector ioref) (MkWholeReaderEdit a)) _ ->
                liftIO $ writeIORef (unWitnessed ioref) a
        refCommitTask = mempty
    return $ MkResource objRun $ MkAnReference {..}

makeMemoryCellChangeLens :: a -> IO (ChangeLens MemoryCellUpdate (WholeUpdate a))
makeMemoryCellChangeLens a = do
    tvar <- newWitnessedIORef a
    return $ dependentChangeLens tvar
