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
    MkEditQueue [(EditContext, [edit])]

collapse1 :: (EditContext, [edit]) -> [(EditContext, [edit])] -> [(EditContext, [edit])]
collapse1 (esa, ea) ((esb, eb):bb)
    | esa == esb = (esb, ea <> eb) : bb
collapse1 a bb = a : bb

collapse :: [(EditContext, [edit])] -> [(EditContext, [edit])] -> [(EditContext, [edit])]
collapse [] bb = bb
collapse [a] bb = collapse1 a bb
collapse (a:aa) bb = a : collapse aa bb

instance Semigroup (EditQueue edit) where
    MkEditQueue aa <> MkEditQueue bb = MkEditQueue $ collapse aa bb

singleEditQueue :: [edit] -> EditContext -> EditQueue edit
singleEditQueue edits ec = MkEditQueue $ pure (ec, edits)

utReceiveUpdates :: UpdateTiming -> ([edit] -> EditContext -> IO ()) -> [edit] -> EditContext -> IO ()
utReceiveUpdates ut recv edits ec = recv edits $ timingEditContext ut ec

getRunner :: UpdateTiming -> ([edit] -> EditContext -> IO ()) -> LifeCycleIO ([edit] -> EditContext -> IO ())
getRunner SynchronousUpdateTiming recv = return recv
getRunner AsynchronousUpdateTiming recv = do
    runAsync <- asyncRunner $ \(MkEditQueue sourcededits) -> for_ sourcededits $ \(ec, edits) -> recv edits ec
    return $ \edits ec -> runAsync $ singleEditQueue edits ec

subscriberObjectMaker :: Subscriber edit -> a -> ObjectMaker edit a
subscriberObjectMaker (MkCloseUnliftIO run MkASubscriber {..}) a update = do
    remonad (runTransform run) $ subscribe update
    return (MkCloseUnliftIO run subAnObject, a)

makeSharedSubscriber :: forall edit a. UpdateTiming -> ObjectMaker edit a -> LifeCycleIO (Subscriber edit, a)
makeSharedSubscriber ut uobj = do
    var :: MVar (UpdateStore edit) <- liftIO $ newMVar emptyStore
    let
        updateP :: [edit] -> EditContext -> IO ()
        updateP edits ectxt = do
            store <- mvarRun var get
            for_ store $ \entry -> entry edits ectxt
    runAsync <- getRunner ut $ utReceiveUpdates ut updateP
    (MkCloseUnliftIO unliftC anObjectC, a) <- uobj runAsync
    let
        child :: Subscriber edit
        child =
            MkCloseUnliftIO unliftC $
            MkASubscriber anObjectC $ \updateC -> do
                key <- liftIO $ mvarRun var $ addStoreStateT updateC
                lifeCycleClose $ mvarRun var $ deleteStoreStateT key
    return (child, a)

shareObjectMaker :: forall edit a. ObjectMaker edit a -> LifeCycleIO (ObjectMaker edit a)
shareObjectMaker uobj = do
    (sub, a) <- makeSharedSubscriber mempty uobj
    return $ subscriberObjectMaker sub a

makeReflectingSubscriber :: UpdateTiming -> Object edit -> LifeCycleIO (Subscriber edit)
makeReflectingSubscriber ut object = do
    (sub, ()) <- makeSharedSubscriber ut $ reflectingObjectMaker object
    return sub

mapSubscriber :: forall edita editb. EditLens edita editb -> Subscriber edita -> LifeCycleIO (Subscriber editb)
mapSubscriber lens subA = do
    (subB, ()) <- makeSharedSubscriber mempty $ mapObjectMaker lens $ subscriberObjectMaker subA ()
    return subB
