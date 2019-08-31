module Truth.Core.Object.Subscriber
    ( ASubscriber(..)
    , Subscriber
    , subscriberObject
    , makeReflectingSubscriber
    , makeSharedSubscriber
    , subscriberObjectMaker
    , shareObjectMaker
    , mapSubscriber
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.EditContext
import Truth.Core.Object.Object
import Truth.Core.Object.ObjectMaker
import Truth.Core.Object.UnliftIO

data ASubscriber m update = MkASubscriber
    { subAnObject :: AnObject m (UpdateEdit update)
    , subscribe :: ([update] -> EditContext -> IO ()) -> LifeCycleT m ()
    }

type Subscriber = CloseUnliftIO ASubscriber

subscriberObject :: Subscriber update -> Object (UpdateEdit update)
subscriberObject (MkCloseUnliftIO run sub) = MkCloseUnliftIO run $ subAnObject sub

type UpdateStoreEntry update = [update] -> EditContext -> IO ()

type UpdateStore update = Store (UpdateStoreEntry update)

newtype UpdateQueue update =
    MkUpdateQueue [(EditContext, [update])]

collapse1 :: (EditContext, [update]) -> [(EditContext, [update])] -> [(EditContext, [update])]
collapse1 (esa, ea) ((esb, eb):bb)
    | esa == esb = (esb, ea <> eb) : bb
collapse1 a bb = a : bb

collapse :: [(EditContext, [update])] -> [(EditContext, [update])] -> [(EditContext, [update])]
collapse [] bb = bb
collapse [a] bb = collapse1 a bb
collapse (a:aa) bb = a : collapse aa bb

instance Semigroup (UpdateQueue update) where
    MkUpdateQueue aa <> MkUpdateQueue bb = MkUpdateQueue $ collapse aa bb

singleUpdateQueue :: [update] -> EditContext -> UpdateQueue update
singleUpdateQueue edits ec = MkUpdateQueue $ pure (ec, edits)

utReceiveUpdates :: UpdateTiming -> ([update] -> EditContext -> IO ()) -> [update] -> EditContext -> IO ()
utReceiveUpdates ut recv edits ec = recv edits $ timingEditContext ut ec

getRunner :: UpdateTiming -> ([update] -> EditContext -> IO ()) -> LifeCycleIO ([update] -> EditContext -> IO ())
getRunner SynchronousUpdateTiming recv = return recv
getRunner AsynchronousUpdateTiming recv = do
    runAsync <- asyncRunner $ \(MkUpdateQueue sourcededits) -> for_ sourcededits $ \(ec, edits) -> recv edits ec
    return $ \edits ec -> runAsync $ singleUpdateQueue edits ec

subscriberObjectMaker :: Subscriber update -> a -> ObjectMaker update a
subscriberObjectMaker (MkCloseUnliftIO run MkASubscriber {..}) a update = do
    remonad (runTransform run) $ subscribe update
    return (MkCloseUnliftIO run subAnObject, a)

makeSharedSubscriber :: forall update a. UpdateTiming -> ObjectMaker update a -> LifeCycleIO (Subscriber update, a)
makeSharedSubscriber ut uobj = do
    var :: MVar (UpdateStore update) <- liftIO $ newMVar emptyStore
    let
        updateP :: [update] -> EditContext -> IO ()
        updateP edits ectxt = do
            store <- mvarRun var get
            for_ store $ \entry -> entry edits ectxt
    runAsync <- getRunner ut $ utReceiveUpdates ut updateP
    (MkCloseUnliftIO unliftC anObjectC, a) <- uobj runAsync
    let
        child :: Subscriber update
        child =
            MkCloseUnliftIO unliftC $
            MkASubscriber anObjectC $ \updateC -> do
                key <- liftIO $ mvarRun var $ addStoreStateT updateC
                lifeCycleClose $ mvarRun var $ deleteStoreStateT key
    return (child, a)

shareObjectMaker :: forall update a. ObjectMaker update a -> LifeCycleIO (ObjectMaker update a)
shareObjectMaker uobj = do
    (sub, a) <- makeSharedSubscriber mempty uobj
    return $ subscriberObjectMaker sub a

makeReflectingSubscriber ::
       IsUpdate update => UpdateTiming -> Object (UpdateEdit update) -> LifeCycleIO (Subscriber update)
makeReflectingSubscriber ut object = do
    (sub, ()) <- makeSharedSubscriber ut $ reflectingObjectMaker object
    return sub

mapSubscriber ::
       forall updateA updateB. EditLens updateA updateB -> Subscriber updateA -> LifeCycleIO (Subscriber updateB)
mapSubscriber lens subA = do
    (subB, ()) <- makeSharedSubscriber mempty $ mapObjectMaker lens $ subscriberObjectMaker subA ()
    return subB
