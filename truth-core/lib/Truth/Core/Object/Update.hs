module Truth.Core.Object.Update where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read

mapUpdates ::
       forall edita editb m a. MonadUnliftIO m
    => EditFunction edita editb
    -> MutableRead m (EditReader edita)
    -> [edita]
    -> (forall t. MonadTransUnlift t => MutableRead (t m) (EditReader editb) -> [editb] -> t m a)
    -> m a
mapUpdates (MkCloseUnlift (unlift :: Unlift t) ef@MkAnEditFunction {..}) (mrA :: MutableRead m (EditReader edita)) editsA call =
    runUnlift unlift $
    withTransConstraintTM @MonadUnliftIO $ do
        editsB <- efUpdates ef editsA mrA
        let
            mrB :: MutableRead (t m) (EditReader editb)
            mrB = efGet mrA
        call mrB editsB

type ReceiveUpdatesM m edit = MutableRead m (EditReader edit) -> [edit] -> m ()

type ReceiveUpdates edit = forall m. MonadUnliftIO m => ReceiveUpdatesM m edit

type ReceiveUpdatesT t edit = forall m. MonadUnliftIO m => MutableRead m (EditReader edit) -> [edit] -> t m ()

mapReceiveUpdates :: forall edita editb. EditFunction edita editb -> ReceiveUpdates editb -> ReceiveUpdates edita
mapReceiveUpdates (MkCloseUnlift (unlift :: Unlift t) ef@MkAnEditFunction {..}) call (mrA :: MutableRead m (EditReader edita)) editsA =
    runUnlift unlift $
    withTransConstraintTM @MonadUnliftIO $ do
        editsB <- efUpdates ef editsA mrA
        let
            mrB :: MutableRead (t m) (EditReader editb)
            mrB = efGet mrA
        call mrB editsB

mapReceiveUpdatesT ::
       forall t edita editb. MonadTransUnlift t
    => EditFunction edita editb
    -> ReceiveUpdatesT t editb
    -> ReceiveUpdatesT t edita
mapReceiveUpdatesT (MkCloseUnlift (unlift :: Unlift tlens) ef@MkAnEditFunction {..}) call (mrA :: MutableRead m (EditReader edita)) editsA =
    withTransConstraintTM @MonadUnliftIO $
    runUnlift unlift $
    withTransConstraintTM @MonadUnliftIO $
    case hasTransConstraint @MonadUnliftIO @tlens @m of
        Dict -> do
            editsB <- remonad lift $ efUpdates ef editsA mrA
            let
                mrB :: MutableRead (tlens m) (EditReader editb)
                mrB = efGet mrA
            evertT $ call mrB editsB
