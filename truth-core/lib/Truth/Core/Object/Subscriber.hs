module Truth.Core.Object.Subscriber
    ( ASubscriber(..)
    , subRead
    , subEdit
    , Subscriber
    , subscriberUpdatesTask
    , OpenSubscriber
    , makeReflectingSubscriber
    , makeSharedSubscriber
    , subscriberObjectMaker
    , shareObjectMaker
    , floatMapSubscriber
    , mapSubscriber
    , unitSubscriber
    , constantSubscriber
    , subscriberToReadOnly
    , mapOpenSubscriber
    , mapReadOnlyWholeOpenSubscriber
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
    , subscribe :: Task () -> (NonEmpty update -> EditContext -> IO ()) -> ApplyStack tt LifeCycleIO ()
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

type OpenSubscriber update = OpenResource (ASubscriber update)

type UpdateStoreEntry update = (Task (), NonEmpty update -> EditContext -> IO ())

type UpdateStore update = Store (UpdateStoreEntry update)

subscriberUpdatesTask :: Subscriber update -> Task ()
subscriberUpdatesTask (MkResource _ asub) = subUpdatesTask asub

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

getRunner :: (NonEmpty update -> EditContext -> IO ()) -> LifeCycleIO (Task (), NonEmpty update -> EditContext -> IO ())
getRunner recv = do
    (runAsync, utask) <-
        asyncRunner $ \(MkUpdateQueue sourcedupdates) -> for_ sourcedupdates $ \(ec, updates) -> recv updates ec
    let recvAsync updates ec = runAsync $ singleUpdateQueue updates ec
    return (utask, recvAsync)

subscriberObjectMaker :: Subscriber update -> a -> ObjectMaker update a
subscriberObjectMaker (MkResource rr MkASubscriber {..}) a update utask =
    runResourceRunnerWith rr $ \run -> do
        run $ subscribe update utask
        return (MkResource rr subAnObject, a)

makeSharedSubscriber :: forall update a. ObjectMaker update a -> LifeCycleIO (Subscriber update, a)
makeSharedSubscriber om = do
    var :: MVar (UpdateStore update) <- liftIO $ newMVar emptyStore
    let
        updateP :: NonEmpty update -> EditContext -> IO ()
        updateP updates ectxt = do
            store <- mVarRun var get
            for_ store $ \(_, entry) -> entry updates ectxt
    (utaskP, updatePAsync) <- getRunner updateP
    let
        getTasks :: IO ([Task ()])
        getTasks = do
            store <- mVarRun var get
            return $ fmap fst $ toList store
        utaskC :: Task ()
        utaskC = utaskP <> ioTask (fmap mconcat getTasks)
    (MkResource (trunC :: ResourceRunner tt) anObjectC, a) <- om utaskC updatePAsync
    Dict <- return $ resourceRunnerUnliftAllDict trunC
    Dict <- return $ transStackDict @MonadUnliftIO @tt @IO
    let
        subscribeC :: forall . Task () -> (NonEmpty update -> EditContext -> IO ()) -> ApplyStack tt LifeCycleIO ()
        subscribeC taskC updateC =
            stackLift @tt $ do
                key <- liftIO $ mVarRun var $ addStoreStateT (taskC, updateC)
                lifeCycleClose @IO $ mVarRun var $ deleteStoreStateT key
        child :: Subscriber update
        child = MkResource trunC $ MkASubscriber anObjectC subscribeC utaskC
    return (child, a)

shareObjectMaker :: forall update a. ObjectMaker update a -> LifeCycleIO (ObjectMaker update a)
shareObjectMaker uobj = do
    (sub, a) <- makeSharedSubscriber uobj
    return $ subscriberObjectMaker sub a

makeReflectingSubscriber ::
       forall update. IsUpdate update
    => Object (UpdateEdit update)
    -> LifeCycleIO (Subscriber update)
makeReflectingSubscriber object = do
    (sub, ()) <- makeSharedSubscriber $ reflectingObjectMaker object
    return sub

floatMapSubscriber ::
       forall updateA updateB.
       FloatingEditLens updateA updateB
    -> Subscriber updateA
    -> LifeCycleIO (Subscriber updateB)
floatMapSubscriber lens subA = do
    (subB, ()) <- makeSharedSubscriber $ mapObjectMaker lens $ subscriberObjectMaker subA ()
    return subB

mapSubscriber :: forall updateA updateB. EditLens updateA updateB -> Subscriber updateA -> Subscriber updateB
mapSubscriber plens (MkResource rr (MkASubscriber objA subA flush)) =
    runResourceRunnerWith rr $ \run -> let
        objB = mapAnObject plens objA
        subB utask recvB = let
            recvA updatesA ec = do
                updatessB <- run $ for updatesA $ \updateA -> elUpdate plens updateA (objRead objA)
                case nonEmpty $ mconcat $ toList updatessB of
                    Nothing -> return ()
                    Just updatesB' -> recvB updatesB' ec
            in subA utask recvA
        in MkResource rr $ MkASubscriber objB subB flush

anobjSubscriber :: AnObject (UpdateEdit update) '[] -> Subscriber update
anobjSubscriber anobj = MkResource nilResourceRunner $ MkASubscriber anobj (\_ _ -> return ()) (pure ())

unitSubscriber :: Subscriber (WholeUpdate ())
unitSubscriber = anobjSubscriber $ MkAnObject (\ReadWhole -> return ()) $ \_ -> return Nothing

mapOpenSubscriber ::
       forall updateA updateB. EditLens updateA updateB -> OpenSubscriber updateA -> OpenSubscriber updateB
mapOpenSubscriber plens (MkOpenResource rr unlift (MkASubscriber objA subA utaskA)) =
    runResourceRunnerWith rr $ \run -> let
        objB = mapAnObject plens objA
        subB task recvB = let
            recvA updatesA ec = do
                -- note: use run rather than unlift here, because unlift will be out of the run at this point
                updatessB <- run $ for updatesA $ \updateA -> elUpdate plens updateA (objRead objA)
                case nonEmpty $ mconcat $ toList updatessB of
                    Nothing -> return ()
                    Just updatesB' -> recvB updatesB' ec
            in subA task recvA
        in MkOpenResource rr unlift $ MkASubscriber objB subB utaskA

constantSubscriber ::
       forall update. SubjectReader (UpdateReader update)
    => UpdateSubject update
    -> Subscriber (ReadOnlyUpdate update)
constantSubscriber subj = anobjSubscriber $ immutableAnObject $ subjectToMutableRead subj

subscriberToReadOnly :: Subscriber update -> Subscriber (ReadOnlyUpdate update)
subscriberToReadOnly = mapSubscriber toReadOnlyEditLens

mapReadOnlyWholeOpenSubscriber :: forall a b. (a -> b) -> OpenSubscriber (ROWUpdate a) -> OpenSubscriber (ROWUpdate b)
mapReadOnlyWholeOpenSubscriber ab (MkOpenResource rr run (MkASubscriber objA subA utaskA)) = let
    lens :: EditLens (ROWUpdate a) (ROWUpdate b)
    lens = liftReadOnlyEditLens $ funcEditLens ab
    objB = mapAnObject lens objA
    subB task recvB = let
        mapUpdate (MkReadOnlyUpdate (MkWholeUpdate a)) = MkReadOnlyUpdate $ MkWholeUpdate $ ab a
        recvA updatesA ec = recvB (fmap mapUpdate updatesA) ec
        in subA task recvA
    in MkOpenResource rr run $ MkASubscriber objB subB utaskA

makeMemorySubscriber :: forall a. a -> LifeCycleIO (Subscriber (WholeUpdate a))
makeMemorySubscriber initial = do
    obj <- liftIO $ makeMemoryObject initial $ \_ -> True
    makeReflectingSubscriber obj
