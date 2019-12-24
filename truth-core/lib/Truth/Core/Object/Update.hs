module Truth.Core.Object.Update where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.Object
import Truth.Core.Read

objectMapUpdates ::
       forall updateA updateB.
       UpdateFunction updateA updateB
    -> Object (UpdateEdit updateA)
    -> NonEmpty updateA
    -> IO [updateB]
objectMapUpdates ef obj updateAs =
    runResource obj $ \run MkAnObject {..} -> run $ ufUpdates ef (toList updateAs) objRead

mapUpdates ::
       forall updateA updateB m a. MonadUnliftIO m
    => UpdateFunction updateA updateB
    -> MutableRead m (UpdateReader updateA)
    -> NonEmpty updateA
    -> (MutableRead m (UpdateReader updateB) -> NonEmpty updateB -> m a)
    -> m (Maybe a)
mapUpdates ef@MkUpdateFunction {..} (mrA :: MutableRead m (UpdateReader updateA)) editsA call = do
    editsB <- ufUpdates ef (toList editsA) mrA
    case nonEmpty editsB of
        Nothing -> return Nothing
        Just editsB' -> let
            mrB :: MutableRead m (UpdateReader updateB)
            mrB = ufGet mrA
            in fmap Just $ call mrB editsB'

type ReceiveUpdates update = forall m. MonadUnliftIO m => MutableRead m (UpdateReader update) -> NonEmpty update -> m ()

mapReceiveUpdates ::
       forall updateA updateB. UpdateFunction updateA updateB -> ReceiveUpdates updateB -> ReceiveUpdates updateA
mapReceiveUpdates uf recvB mrA updatesA = void $ mapUpdates uf mrA updatesA recvB
