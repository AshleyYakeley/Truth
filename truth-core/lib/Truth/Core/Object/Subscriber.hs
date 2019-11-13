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
import Truth.Core.Resource

data ASubscriber tt update = MkASubscriber
    { subAnObject :: AnObject tt (UpdateEdit update)
    , subscribe :: ([update] -> EditContext -> IO ()) -> LifeCycleT (ApplyStack tt IO) ()
    }

type Subscriber = Runnable1 ASubscriber

subscriberObject :: Subscriber update -> Object (UpdateEdit update)
subscriberObject (MkRunnable1 run sub) = MkRunnable1 run $ subAnObject sub

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
subscriberObjectMaker (MkRunnable1 (trun :: TransStackRunner tt) MkASubscriber {..}) a update =
    runMonoTransStackRunner trun $ \run -> do
        Dict <- return $ transStackDict @MonadUnliftIO @tt @IO
        remonad run $ subscribe update
        return (MkRunnable1 trun subAnObject, a)

makeSharedSubscriber :: forall update a. UpdateTiming -> ObjectMaker update a -> LifeCycleIO (Subscriber update, a)
makeSharedSubscriber ut uobj = do
    var :: MVar (UpdateStore update) <- liftIO $ newMVar emptyStore
    let
        updateP :: [update] -> EditContext -> IO ()
        updateP edits ectxt = do
            store <- mVarRun var get
            for_ store $ \entry -> entry edits ectxt
    runAsync <- getRunner ut $ utReceiveUpdates ut updateP
    (MkRunnable1 (trunC :: TransStackRunner tt) anObjectC, a) <- uobj runAsync
    Dict <- return $ transStackRunnerUnliftAllDict trunC
    Dict <- return $ transStackDict @MonadUnliftIO @tt @IO
    let
        child :: Subscriber update
        child =
            MkRunnable1 trunC $
            MkASubscriber anObjectC $ \updateC -> do
                key <- liftIO $ mVarRun var $ addStoreStateT updateC
                lifeCycleClose $ mVarRun var $ deleteStoreStateT key
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
