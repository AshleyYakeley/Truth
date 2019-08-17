module Truth.Core.Object.Subscriber
    ( ASubscriber(..)
    , Subscriber
    , subscriberObject
    , makeObjectSubscriber
    , makeSharedSubscriber
    , subscriberUpdatingObject
    , shareUpdatingObject
    ) where

import Truth.Core.Import
import Truth.Core.Object.EditContext
import Truth.Core.Object.Object
import Truth.Core.Object.UnliftIO
import Truth.Core.Object.UpdatingObject
import Truth.Debug

data ASubscriber m edit = MkASubscriber
    { subAnObject :: AnObject m edit
    , subscribe :: ([edit] -> EditContext -> IO ()) -> LifeCycleT m ()
    }

type Subscriber = CloseUnliftIO ASubscriber

subscriberObject :: Subscriber edit -> Object edit
subscriberObject (MkCloseUnliftIO run sub) = MkCloseUnliftIO run $ subAnObject sub

type UpdateStoreEntry edit = [edit] -> EditContext -> IO ()

type UpdateStore edit = Store (UpdateStoreEntry edit)

newtype EditQueue edit =
    MkEditQueue [(EditSource, [edit])]

collapse1 :: (EditSource, [edit]) -> [(EditSource, [edit])] -> [(EditSource, [edit])]
collapse1 (esa, ea) ((esb, eb):bb)
    | esa == esb = (esb, ea <> eb) : bb
collapse1 a bb = a : bb

collapse :: [(EditSource, [edit])] -> [(EditSource, [edit])] -> [(EditSource, [edit])]
collapse [] bb = bb
collapse [a] bb = collapse1 a bb
collapse (a:aa) bb = a : collapse aa bb

instance Semigroup (EditQueue edit) where
    MkEditQueue aa <> MkEditQueue bb = MkEditQueue $ collapse aa bb

singleEditQueue :: [edit] -> EditSource -> EditQueue edit
singleEditQueue edits esrc = MkEditQueue $ pure (esrc, edits)

utReceiveUpdates :: UpdateTiming -> ([edit] -> EditContext -> IO ()) -> [edit] -> EditSource -> IO ()
utReceiveUpdates editContextTiming recv edits editContextSource = recv edits $ MkEditContext {..}

getRunner :: UpdateTiming -> ([edit] -> EditSource -> IO ()) -> LifeCycleIO ([edit] -> EditSource -> IO ())
getRunner SynchronousUpdateTiming recv = return recv
getRunner AsynchronousUpdateTiming recv = do
    runAsync <- asyncRunner $ \(MkEditQueue sourcededits) -> traceBracket "getRunner:action" $ for_ sourcededits $ \(esrc, edits) -> recv edits esrc
    return $ \edits esrc -> traceBracket "getRunner:runAsync" $ runAsync $ singleEditQueue edits esrc

subscriberUpdatingObject :: Subscriber edit -> a -> UpdatingObject edit a
subscriberUpdatingObject (MkCloseUnliftIO run MkASubscriber {..}) a update = do
    remonad (runTransform run) $ subscribe $ \edits ec -> update edits $ editContextSource ec
    return (MkCloseUnliftIO run subAnObject, a)

makeSharedSubscriber :: forall edit a. UpdateTiming -> UpdatingObject edit a -> LifeCycleIO (Subscriber edit, a)
makeSharedSubscriber ut uobj = traceBracket "makeSharedSubscriber:run" $ do
    var :: MVar (UpdateStore edit) <- liftIO $ newMVar emptyStore
    let
        updateP :: [edit] -> EditContext -> IO ()
        updateP edits ectxt = traceBracket ("makeSharedSubscriber:updateP (" ++ show ut ++ ")") $ do
            store <- traceBarrier "makeSharedSubscriber:updateP.get.mvarRun" (mvarRun var) get
            for_ store $ \entry -> traceBracket "makeSharedSubscriber:updateP.entry" $ entry edits ectxt
    runAsync <- getRunner ut $ utReceiveUpdates ut updateP
    (MkCloseUnliftIO unliftC anObjectC, a) <- uobj runAsync
    let
        child :: Subscriber edit
        child =
            MkCloseUnliftIO unliftC $
            MkASubscriber anObjectC $ \updateC -> do
                key <- liftIO $ traceBarrier "makeSharedSubscriber:child.addStoreState" (mvarRun var) $ addStoreStateT updateC
                lifeCycleClose $ traceBarrier "makeSharedSubscriber:child.deleteStoreStateT" (mvarRun var) $ deleteStoreStateT key
    return (child, a)

shareUpdatingObject :: forall edit a. UpdateTiming -> UpdatingObject edit a -> LifeCycleIO (UpdatingObject edit a)
shareUpdatingObject ut uobj = do
    (sub, a) <- makeSharedSubscriber ut uobj
    return $ subscriberUpdatingObject sub a

makeObjectSubscriber :: UpdateTiming -> Object edit -> LifeCycleIO (Subscriber edit)
makeObjectSubscriber ut object = do
    (sub, ()) <- makeSharedSubscriber ut $ updatingObject object
    return sub
