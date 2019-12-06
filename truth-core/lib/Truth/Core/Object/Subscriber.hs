module Truth.Core.Object.Subscriber
    ( ASubscriber(..)
    , Subscriber
    , subscriberObject
    , makeReflectingSubscriber
    , makeSharedSubscriber
    , subscriberObjectMaker
    , shareObjectMaker
    , mapSubscriber
    , mapPureSubscriber
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.EditContext
import Truth.Core.Object.Object
import Truth.Core.Object.ObjectMaker
import Truth.Core.Resource

data ASubscriber update tt = MkASubscriber
    { subAnObject :: AnObject (UpdateEdit update) tt
    , subscribe :: ([update] -> EditContext -> IO ()) -> LifeCycleT (ApplyStack tt IO) ()
    }

instance MapResource (ASubscriber update) where
    mapResource ::
           forall tt1 tt2. (MonadTransStackUnliftAll tt1, MonadTransStackUnliftAll tt2)
        => TransListFunction tt1 tt2
        -> ASubscriber update tt1
        -> ASubscriber update tt2
    mapResource tlf (MkASubscriber obj1 sub1) =
        case transStackDict @Monad @tt1 @IO of
            Dict ->
                case transStackDict @Monad @tt2 @IO of
                    Dict -> let
                        obj2 = mapResource tlf obj1
                        sub2 recv = remonad (tlfFunction tlf $ Proxy @IO) $ sub1 recv
                        in MkASubscriber obj2 sub2

type Subscriber update = Resource (ASubscriber update)

subscriberObject :: Subscriber update -> Object (UpdateEdit update)
subscriberObject (MkResource run sub) = MkResource run $ subAnObject sub

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
subscriberObjectMaker (MkResource rr MkASubscriber {..}) a update =
    runResourceRunnerWith rr $ \run -> do
        remonad run $ subscribe update
        return (MkResource rr subAnObject, a)

makeSharedSubscriber :: forall update a. UpdateTiming -> ObjectMaker update a -> LifeCycleIO (Subscriber update, a)
makeSharedSubscriber ut uobj = do
    var :: MVar (UpdateStore update) <- liftIO $ newMVar emptyStore
    let
        updateP :: [update] -> EditContext -> IO ()
        updateP edits ectxt = do
            store <- mVarRun var get
            for_ store $ \entry -> entry edits ectxt
    runAsync <- getRunner ut $ utReceiveUpdates ut updateP
    (MkResource (trunC :: ResourceRunner tt) anObjectC, a) <- uobj runAsync
    Dict <- return $ resourceRunnerUnliftAllDict trunC
    Dict <- return $ transStackDict @MonadUnliftIO @tt @IO
    let
        child :: Subscriber update
        child =
            MkResource trunC $
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
       forall updateA updateB.
       LifeCycleIO (EditLens updateA updateB)
    -> Subscriber updateA
    -> LifeCycleIO (Subscriber updateB)
mapSubscriber getlens subA = do
    lens <- getlens
    (subB, ()) <- makeSharedSubscriber mempty $ mapObjectMaker lens $ subscriberObjectMaker subA ()
    return subB

mapPureSubscriber :: forall updateA updateB. EditLens updateA updateB -> Subscriber updateA -> Subscriber updateB
mapPureSubscriber lens (MkResource rr (MkASubscriber objA subA)) =
    runResourceRunnerWith rr $ \run -> let
        objB = mapAnObject lens objA
        subB recvB = let
            recvA updatesA ec = do
                updatesB <- run $ ufUpdates (elFunction lens) updatesA (objRead objA)
                recvB updatesB ec
            in subA recvA
        in MkResource rr $ MkASubscriber objB subB
