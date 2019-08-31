module Truth.Core.Object.Update where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.Object
import Truth.Core.Object.UnliftIO
import Truth.Core.Read

anObjectMapUpdates ::
       forall updateA updateB t m. (MonadTransUnlift t, MonadUnliftIO m)
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
objectMapUpdates (MkCloseUnlift unlift ef) (MkCloseUnliftIO objRun obj) editAs =
    runTransform objRun $ runUnlift unlift $ anObjectMapUpdates ef obj editAs

mapUpdates ::
       forall updateA updateB m a. MonadUnliftIO m
    => UpdateFunction updateA updateB
    -> MutableRead m (UpdateReader updateA)
    -> [updateA]
    -> (forall t. MonadTransUnlift t => MutableRead (t m) (UpdateReader updateB) -> [updateB] -> t m a)
    -> m a
mapUpdates (MkCloseUnlift (unlift :: Unlift t) ef@MkAnUpdateFunction {..}) (mrA :: MutableRead m (UpdateReader updateA)) editsA call =
    runUnlift unlift $
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
mapReceiveUpdates (MkCloseUnlift (unlift :: Unlift t) ef@MkAnUpdateFunction {..}) call (mrA :: MutableRead m (UpdateReader updateA)) editsA =
    runUnlift unlift $
    withTransConstraintTM @MonadUnliftIO $ do
        editsB <- ufUpdates ef editsA mrA
        let
            mrB :: MutableRead (t m) (UpdateReader updateB)
            mrB = ufGet mrA
        call mrB editsB

mapReceiveUpdatesT ::
       forall t updateA updateB. MonadTransUnlift t
    => UpdateFunction updateA updateB
    -> ReceiveUpdatesT t updateB
    -> ReceiveUpdatesT t updateA
mapReceiveUpdatesT (MkCloseUnlift (unlift :: Unlift tlens) ef@MkAnUpdateFunction {..}) call (mrA :: MutableRead m (UpdateReader updateA)) editsA =
    withTransConstraintTM @MonadUnliftIO $
    runUnlift unlift $
    withTransConstraintTM @MonadUnliftIO $
    case hasTransConstraint @MonadUnliftIO @tlens @m of
        Dict -> do
            editsB <- remonad lift $ ufUpdates ef editsA mrA
            let
                mrB :: MutableRead (tlens m) (UpdateReader updateB)
                mrB = ufGet mrA
            commuteT $ call mrB editsB
