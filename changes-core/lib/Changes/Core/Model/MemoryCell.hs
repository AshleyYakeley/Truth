module Changes.Core.Model.MemoryCell where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Model.EditContext
import Changes.Core.Model.Reference
import Changes.Core.Read
import Changes.Core.Resource
import Changes.Core.Types
import Data.IORef

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
    return $ MkResource objRun $ MkAReference {..}

makeMemoryCellChangeLens :: a -> IO (ChangeLens MemoryCellUpdate (WholeUpdate a))
makeMemoryCellChangeLens a = do
    tvar <- newWitnessedIORef a
    return $ dependentChangeLens tvar
