module Truth.Core.Object.Update where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.Object
import Truth.Core.Read

objectMapUpdates ::
       forall updateA updateB.
       UpdateFunction updateA updateB
    -> Object (UpdateEdit updateA)
    -> [updateA]
    -> IO [updateB]
objectMapUpdates (MkRunnable2 trunLens ef) (MkRunnable1 (trunObj :: TransStackRunner tto) MkAnObject {..}) updateAs =
    runMonoTransStackRunner @IO trunObj $ \runObj ->
        runMonoTransStackRunner @(ApplyStack tto IO) trunLens $ \runLens ->
            runObj $ runLens $ ufUpdates ef updateAs objRead

mapUpdates ::
       forall updateA updateB m a. MonadUnliftIO m
    => UpdateFunction updateA updateB
    -> MutableRead m (UpdateReader updateA)
    -> [updateA]
    -> (forall tt.
            (MonadTransStackUnliftAll tt, MonadUnliftIO (ApplyStack tt m)) =>
                    Proxy tt -> MutableRead (ApplyStack tt m) (UpdateReader updateB) -> [updateB] -> ApplyStack tt m a)
    -> m a
mapUpdates (MkRunnable2 (trun :: TransStackRunner tt) ef@MkAnUpdateFunction {..}) (mrA :: MutableRead m (UpdateReader updateA)) editsA call =
    runMonoTransStackRunner @m trun $ \run ->
        run $ do
            editsB <- ufUpdates ef editsA mrA
            let
                mrB :: MutableRead (ApplyStack tt m) (UpdateReader updateB)
                mrB = ufGet mrA
            call @tt Proxy mrB editsB

type ReceiveUpdatesM m update = MutableRead m (UpdateReader update) -> [update] -> m ()

type ReceiveUpdates update = forall m. MonadUnliftIO m => ReceiveUpdatesM m update

type ReceiveUpdatesTT tt update
     = forall m. MonadUnliftIO m => MutableRead m (UpdateReader update) -> [update] -> ApplyStack tt m ()

mapReceiveUpdates ::
       forall updateA updateB. UpdateFunction updateA updateB -> ReceiveUpdates updateB -> ReceiveUpdates updateA
mapReceiveUpdates (MkRunnable2 (trun :: TransStackRunner tt) ef@MkAnUpdateFunction {..}) call (mrA :: MutableRead m (UpdateReader updateA)) editsA =
    runMonoTransStackRunner @m trun $ \run ->
        run $ do
            editsB <- ufUpdates ef editsA mrA
            let
                mrB :: MutableRead (ApplyStack tt m) (UpdateReader updateB)
                mrB = ufGet mrA
            call mrB editsB

mapReceiveUpdatesTT ::
       forall tt updateA updateB. MonadTransStackUnliftAll tt
    => UpdateFunction updateA updateB
    -> ReceiveUpdatesTT tt updateB
    -> ReceiveUpdatesTT tt updateA
mapReceiveUpdatesTT (MkRunnable2 (trun :: TransStackRunner ttl) ef@MkAnUpdateFunction {..}) call (mrA :: MutableRead m (UpdateReader updateA)) editsA =
    case transStackRunnerUnliftAllDict trun of
        Dict ->
            case (transStackDict @MonadUnliftIO @tt @m, transStackDict @MonadUnliftIO @ttl @m) of
                (Dict, Dict) ->
                    runMonoTransStackRunner @(ApplyStack tt m) trun $ \run ->
                        run $ do
                            editsB <- stackRemonad @ttl (stackLift @tt @m) $ ufUpdates ef editsA mrA
                            let
                                mrB :: MutableRead (ApplyStack ttl m) (UpdateReader updateB)
                                mrB = ufGet mrA
                            stackCommute @tt @ttl @m $ call mrB editsB
