module Truth.Core.Object.Subscriber
    ( ASubscriber(..)
    , subRead
    , subEdit
    , Subscriber
    , subscriberUpdatesTask
    , subscriberCommitTask
    , makeReflectingSubscriber
    , makeSharedSubscriber
    , subscriberObjectMaker
    , shareObjectMaker
    , floatMapSubscriber
    , mapSubscriber
    , unitSubscriber
    , constantSubscriber
    , subscriberToReadOnly
    , makeMemorySubscriber
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Lens
import Truth.Core.Object.EditContext
import Truth.Core.Object.Object
import Truth.Core.Object.ObjectMaker
import Truth.Core.Read
import Truth.Core.Resource
import Truth.Core.Types

data ASubscriber update tt = MkASubscriber
    { subAnObject :: AnObject (UpdateEdit update) tt
    , subscribe :: Task () -> (ResourceContext -> NonEmpty update -> EditContext -> IO ()) -> ApplyStack tt LifeCycleIO ()
    , subUpdatesTask :: Task ()
    }

subRead :: ASubscriber update tt -> MutableRead (ApplyStack tt IO) (UpdateReader update)
subRead = objRead . subAnObject

subEdit ::
       ASubscriber update tt
    -> NonEmpty (UpdateEdit update)
    -> ApplyStack tt IO (Maybe (EditSource -> ApplyStack tt IO ()))
subEdit = objEdit . subAnObject

instance MapResource (ASubscriber update) where
    mapResource ::
           forall tt1 tt2. (MonadTransStackUnliftAll tt1, MonadTransStackUnliftAll tt2)
        => TransListFunction tt1 tt2
        -> ASubscriber update tt1
        -> ASubscriber update tt2
    mapResource tlf (MkASubscriber obj1 sub1 utask) =
        case transStackDict @Monad @tt1 @IO of
            Dict ->
                case transStackDict @Monad @tt2 @IO of
                    Dict -> let
                        obj2 = mapResource tlf obj1
                        sub2 recv task = tlfFunction tlf (Proxy @LifeCycleIO) $ sub1 recv task
                        in MkASubscriber obj2 sub2 utask

type Subscriber update = Resource (ASubscriber update)

type UpdateStoreEntry update = (Task (), ResourceContext -> NonEmpty update -> EditContext -> IO ())

type UpdateStore update = Store (UpdateStoreEntry update)

subscriberUpdatesTask :: Subscriber update -> Task ()
subscriberUpdatesTask (MkResource _ asub) = subUpdatesTask asub

subscriberCommitTask :: Subscriber update -> Task ()
subscriberCommitTask (MkResource _ asub) = objCommitTask $ subAnObject asub

newtype UpdateQueue update =
    MkUpdateQueue [(EditContext, NonEmpty update)]

collapse1 :: (EditContext, NonEmpty update) -> [(EditContext, NonEmpty update)] -> [(EditContext, NonEmpty update)]
collapse1 (esa, ea) ((esb, eb):bb)
    | esa == esb = (esb, ea <> eb) : bb
collapse1 a bb = a : bb

collapse :: [(EditContext, NonEmpty update)] -> [(EditContext, NonEmpty update)] -> [(EditContext, NonEmpty update)]
collapse [] bb = bb
collapse [a] bb = collapse1 a bb
collapse (a:aa) bb = a : collapse aa bb

instance Semigroup (UpdateQueue update) where
    MkUpdateQueue aa <> MkUpdateQueue bb = MkUpdateQueue $ collapse aa bb

singleUpdateQueue :: NonEmpty update -> EditContext -> UpdateQueue update
singleUpdateQueue updates ec = MkUpdateQueue $ pure (ec, updates)

getRunner ::
       (ResourceContext -> NonEmpty update -> EditContext -> IO ())
    -> LifeCycleIO (Task (), ResourceContext -> NonEmpty update -> EditContext -> IO ())
getRunner recv = do
    (runAsync, utask) <-
        asyncRunner $ \(MkUpdateQueue sourcedupdates) ->
            for_ sourcedupdates $ \(ec, updates) -> recv emptyResourceContext updates ec
    let recvAsync _ updates ec = runAsync $ singleUpdateQueue updates ec
    return (utask, recvAsync)

subscriberObjectMaker :: ResourceContext -> Subscriber update -> a -> ObjectMaker update a
subscriberObjectMaker rc (MkResource rr MkASubscriber {..}) val update utask = do
    runResourceRunner rc rr $ subscribe update utask
    return $ MkObjectMakerResult (MkResource rr subAnObject) subUpdatesTask val

makeSharedSubscriber :: forall update a. ObjectMaker update a -> LifeCycleIO (Subscriber update, a)
makeSharedSubscriber om = do
    var :: MVar (UpdateStore update) <- liftIO $ newMVar emptyStore
    let
        updateP :: ResourceContext -> NonEmpty update -> EditContext -> IO ()
        updateP rc updates ectxt = do
            store <- mVarRun var get
            for_ store $ \(_, entry) -> entry rc updates ectxt
    (utaskRunner, updatePAsync) <- getRunner updateP
    let
        getTasks :: IO ([Task ()])
        getTasks = do
            store <- mVarRun var get
            return $ fmap fst $ toList store
        utaskP :: Task ()
        utaskP = utaskRunner <> ioTask (fmap mconcat getTasks)
    MkObjectMakerResult {..} <- om utaskP updatePAsync
    MkResource (trunC :: ResourceRunner tt) subAnObject <- return omrObject
    Dict <- return $ resourceRunnerUnliftAllDict trunC
    Dict <- return $ transStackDict @MonadUnliftIO @tt @IO
    let
        subscribe ::
               Task () -> (ResourceContext -> NonEmpty update -> EditContext -> IO ()) -> ApplyStack tt LifeCycleIO ()
        subscribe taskC updateC =
            stackLift @tt $ do
                key <- liftIO $ mVarRun var $ addStoreStateT (taskC, updateC)
                lifeCycleClose @IO $ mVarRun var $ deleteStoreStateT key
        subUpdatesTask = omrUpdatesTask
        child :: Subscriber update
        child = MkResource trunC $ MkASubscriber {..}
    return (child, omrValue)

shareObjectMaker :: forall update a. ObjectMaker update a -> LifeCycleIO (ResourceContext -> ObjectMaker update a)
shareObjectMaker uobj = do
    (sub, a) <- makeSharedSubscriber uobj
    return $ \rc -> subscriberObjectMaker rc sub a

makeReflectingSubscriber ::
       forall update. IsUpdate update
    => Object (UpdateEdit update)
    -> LifeCycleIO (Subscriber update)
makeReflectingSubscriber object = do
    (sub, ()) <- makeSharedSubscriber $ reflectingObjectMaker object
    return sub

floatMapSubscriber ::
       forall updateA updateB.
       ResourceContext
    -> FloatingEditLens updateA updateB
    -> Subscriber updateA
    -> LifeCycleIO (Subscriber updateB)
floatMapSubscriber rc lens subA = do
    (subB, ()) <- makeSharedSubscriber $ mapObjectMaker rc lens $ subscriberObjectMaker rc subA ()
    return subB

mapSubscriber :: forall updateA updateB. EditLens updateA updateB -> Subscriber updateA -> Subscriber updateB
mapSubscriber plens (MkResource rr (MkASubscriber objA subA utaskA)) =
    case resourceRunnerUnliftAllDict rr of
        Dict -> let
            objB = mapAnObject plens objA
            subB utask recvB = let
                recvA rc updatesA ec = do
                    updatessB <-
                        runResourceRunner rc rr $ for updatesA $ \updateA -> elUpdate plens updateA (objRead objA)
                    case nonEmpty $ mconcat $ toList updatessB of
                        Nothing -> return ()
                        Just updatesB' -> recvB rc updatesB' ec
                in subA utask recvA
            in MkResource rr $ MkASubscriber objB subB utaskA

anobjSubscriber :: AnObject (UpdateEdit update) '[] -> Subscriber update
anobjSubscriber anobj = MkResource nilResourceRunner $ MkASubscriber anobj (\_ _ -> return ()) mempty

unitSubscriber :: Subscriber (WholeUpdate ())
unitSubscriber = anobjSubscriber $ MkAnObject (\ReadWhole -> return ()) (\_ -> return Nothing) mempty

constantSubscriber ::
       forall update. SubjectReader (UpdateReader update)
    => UpdateSubject update
    -> Subscriber (ReadOnlyUpdate update)
constantSubscriber subj = anobjSubscriber $ immutableAnObject $ subjectToMutableRead subj

subscriberToReadOnly :: Subscriber update -> Subscriber (ReadOnlyUpdate update)
subscriberToReadOnly = mapSubscriber toReadOnlyEditLens

makeMemorySubscriber :: forall a. a -> LifeCycleIO (Subscriber (WholeUpdate a))
makeMemorySubscriber initial = do
    obj <- liftIO $ makeMemoryObject initial $ \_ -> True
    makeReflectingSubscriber obj
