module Truth.Core.Object.Update where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.Object
import Truth.Core.Read
import Truth.Core.Resource

objectMapUpdates ::
       forall updateA updateB.
       UpdateFunction updateA updateB
    -> Object (UpdateEdit updateA)
    -> [updateA]
    -> IO [updateB]
objectMapUpdates ef (MkResource rr MkAnObject {..}) updateAs =
    runResourceRunnerWith rr $ \run -> run $ ufUpdates ef updateAs objRead

mapUpdates ::
       forall updateA updateB m a. MonadUnliftIO m
    => UpdateFunction updateA updateB
    -> MutableRead m (UpdateReader updateA)
    -> [updateA]
    -> (MutableRead m (UpdateReader updateB) -> [updateB] -> m a)
    -> m a
mapUpdates ef@MkUpdateFunction {..} (mrA :: MutableRead m (UpdateReader updateA)) editsA call = do
    editsB <- ufUpdates ef editsA mrA
    let
        mrB :: MutableRead m (UpdateReader updateB)
        mrB = ufGet mrA
    call mrB editsB

type ReceiveUpdates update = forall m. MonadUnliftIO m => MutableRead m (UpdateReader update) -> [update] -> m ()

mapReceiveUpdates ::
       forall updateA updateB. UpdateFunction updateA updateB -> ReceiveUpdates updateB -> ReceiveUpdates updateA
mapReceiveUpdates uf recvB mrA updatesA = mapUpdates uf mrA updatesA recvB
