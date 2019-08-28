module Truth.Core.Object.Update where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.Object
import Truth.Core.Object.UnliftIO
import Truth.Core.Read

anObjectMapUpdates ::
       (MonadTransUnlift t, MonadUnliftIO m)
    => AnUpdateFunction t edita editb
    -> AnObject m edita
    -> [edita]
    -> t m [editb]
anObjectMapUpdates ef MkAnObject {..} editAs = withTransConstraintTM @MonadUnliftIO $ ufUpdates ef editAs objRead

objectMapUpdates :: UpdateFunction edita editb -> Object edita -> [edita] -> IO [editb]
objectMapUpdates (MkCloseUnlift unlift ef) (MkCloseUnliftIO objRun obj) editAs =
    runTransform objRun $ runUnlift unlift $ anObjectMapUpdates ef obj editAs

mapUpdates ::
       forall edita editb m a. MonadUnliftIO m
    => UpdateFunction edita editb
    -> MutableRead m (EditReader edita)
    -> [edita]
    -> (forall t. MonadTransUnlift t => MutableRead (t m) (EditReader editb) -> [editb] -> t m a)
    -> m a
mapUpdates (MkCloseUnlift (unlift :: Unlift t) ef@MkAnUpdateFunction {..}) (mrA :: MutableRead m (EditReader edita)) editsA call =
    runUnlift unlift $
    withTransConstraintTM @MonadUnliftIO $ do
        editsB <- ufUpdates ef editsA mrA
        let
            mrB :: MutableRead (t m) (EditReader editb)
            mrB = ufGet mrA
        call mrB editsB

type ReceiveUpdatesM m edit = MutableRead m (EditReader edit) -> [edit] -> m ()

type ReceiveUpdates edit = forall m. MonadUnliftIO m => ReceiveUpdatesM m edit

type ReceiveUpdatesT t edit = forall m. MonadUnliftIO m => MutableRead m (EditReader edit) -> [edit] -> t m ()

mapReceiveUpdates :: forall edita editb. UpdateFunction edita editb -> ReceiveUpdates editb -> ReceiveUpdates edita
mapReceiveUpdates (MkCloseUnlift (unlift :: Unlift t) ef@MkAnUpdateFunction {..}) call (mrA :: MutableRead m (EditReader edita)) editsA =
    runUnlift unlift $
    withTransConstraintTM @MonadUnliftIO $ do
        editsB <- ufUpdates ef editsA mrA
        let
            mrB :: MutableRead (t m) (EditReader editb)
            mrB = ufGet mrA
        call mrB editsB

mapReceiveUpdatesT ::
       forall t edita editb. MonadTransUnlift t
    => UpdateFunction edita editb
    -> ReceiveUpdatesT t editb
    -> ReceiveUpdatesT t edita
mapReceiveUpdatesT (MkCloseUnlift (unlift :: Unlift tlens) ef@MkAnUpdateFunction {..}) call (mrA :: MutableRead m (EditReader edita)) editsA =
    withTransConstraintTM @MonadUnliftIO $
    runUnlift unlift $
    withTransConstraintTM @MonadUnliftIO $
    case hasTransConstraint @MonadUnliftIO @tlens @m of
        Dict -> do
            editsB <- remonad lift $ ufUpdates ef editsA mrA
            let
                mrB :: MutableRead (tlens m) (EditReader editb)
                mrB = ufGet mrA
            commuteT $ call mrB editsB
