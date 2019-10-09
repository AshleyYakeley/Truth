module Truth.Core.Object.Update where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.Object
import Truth.Core.Object.Run
import Truth.Core.Read

anObjectMapUpdates ::
       forall updateA updateB t m. (MonadTransUntrans t, MonadUnliftIO m)
    => AnUpdateFunction t updateA updateB
    -> AnObject m (UpdateEdit updateA)
    -> [updateA]
    -> t m [updateB]
anObjectMapUpdates ef MkAnObject {..} editAs = withTransConstraintTM @MonadUnliftIO $ ufUpdates ef editAs objRead

objectMapUpdates ::
       forall updateA updateB.
       UpdateFunction updateA updateB
    -> Object (UpdateEdit updateA)
    -> [updateA]
    -> IO [updateB]
objectMapUpdates (MkRunnableT2 unlift ef) (MkRunnableIO objRun obj) editAs =
    objRun $ unlift $ anObjectMapUpdates ef obj editAs

mapUpdates ::
       forall updateA updateB m a. MonadUnliftIO m
    => UpdateFunction updateA updateB
    -> MutableRead m (UpdateReader updateA)
    -> [updateA]
    -> (forall t. MonadTransUntrans t => MutableRead (t m) (UpdateReader updateB) -> [updateB] -> t m a)
    -> m a
mapUpdates (MkRunnableT2 (unlift :: Untrans t) ef@MkAnUpdateFunction {..}) (mrA :: MutableRead m (UpdateReader updateA)) editsA call =
    unlift $
    withTransConstraintTM @MonadUnliftIO $ do
        editsB <- ufUpdates ef editsA mrA
        let
            mrB :: MutableRead (t m) (UpdateReader updateB)
            mrB = ufGet mrA
        call mrB editsB

type ReceiveUpdatesM m update = MutableRead m (UpdateReader update) -> [update] -> m ()

type ReceiveUpdates update = forall m. MonadUnliftIO m => ReceiveUpdatesM m update

type ReceiveUpdatesT t update = forall m. MonadUnliftIO m => MutableRead m (UpdateReader update) -> [update] -> t m ()

mapReceiveUpdates ::
       forall updateA updateB. UpdateFunction updateA updateB -> ReceiveUpdates updateB -> ReceiveUpdates updateA
mapReceiveUpdates (MkRunnableT2 (unlift :: Untrans t) ef@MkAnUpdateFunction {..}) call (mrA :: MutableRead m (UpdateReader updateA)) editsA =
    unlift $
    withTransConstraintTM @MonadUnliftIO $ do
        editsB <- ufUpdates ef editsA mrA
        let
            mrB :: MutableRead (t m) (UpdateReader updateB)
            mrB = ufGet mrA
        call mrB editsB

mapReceiveUpdatesT ::
       forall t updateA updateB. MonadTransUntrans t
    => UpdateFunction updateA updateB
    -> ReceiveUpdatesT t updateB
    -> ReceiveUpdatesT t updateA
mapReceiveUpdatesT (MkRunnableT2 (unlift :: Untrans tlens) ef@MkAnUpdateFunction {..}) call (mrA :: MutableRead m (UpdateReader updateA)) editsA =
    withTransConstraintTM @MonadUnliftIO $
    unlift $
    withTransConstraintTM @MonadUnliftIO $
    case hasTransConstraint @MonadUnliftIO @tlens @m of
        Dict -> do
            editsB <- remonad lift $ ufUpdates ef editsA mrA
            let
                mrB :: MutableRead (tlens m) (UpdateReader updateB)
                mrB = ufGet mrA
            commuteT $ call mrB editsB
