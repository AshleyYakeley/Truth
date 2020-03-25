module Truth.Core.Object.Model
    ( AModel(..)
    , aModelRead
    , aModelEdit
    , Model
    , modelUpdatesTask
    , modelCommitTask
    , makeReflectingModel
    , makeSharedModel
    , modelObjectMaker
    , shareObjectMaker
    , floatMapModel
    , mapModel
    , unitModel
    , constantModel
    , modelToReadOnly
    , makeMemoryModel
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

data AModel update tt = MkAModel
    { aModelAnObject :: AnObject (UpdateEdit update) tt
    , aModelSubscribe :: Task () -> (ResourceContext -> NonEmpty update -> EditContext -> IO ()) -> ApplyStack tt LifeCycleIO ()
    , aModelUpdatesTask :: Task ()
    }

aModelRead :: AModel update tt -> Readable (ApplyStack tt IO) (UpdateReader update)
aModelRead = objRead . aModelAnObject

aModelEdit ::
       AModel update tt -> NonEmpty (UpdateEdit update) -> ApplyStack tt IO (Maybe (EditSource -> ApplyStack tt IO ()))
aModelEdit = objEdit . aModelAnObject

instance MapResource (AModel update) where
    mapResource ::
           forall tt1 tt2. (MonadTransStackUnliftAll tt1, MonadTransStackUnliftAll tt2)
        => TransListFunction tt1 tt2
        -> AModel update tt1
        -> AModel update tt2
    mapResource tlf (MkAModel obj1 sub1 utask) =
        case transStackDict @Monad @tt1 @IO of
            Dict ->
                case transStackDict @Monad @tt2 @IO of
                    Dict -> let
                        obj2 = mapResource tlf obj1
                        sub2 recv task = tlfFunction tlf (Proxy @LifeCycleIO) $ sub1 recv task
                        in MkAModel obj2 sub2 utask

type Model update = Resource (AModel update)

type UpdateStoreEntry update = (Task (), ResourceContext -> NonEmpty update -> EditContext -> IO ())

type UpdateStore update = Store (UpdateStoreEntry update)

modelUpdatesTask :: Model update -> Task ()
modelUpdatesTask (MkResource _ asub) = aModelUpdatesTask asub

modelCommitTask :: Model update -> Task ()
modelCommitTask (MkResource _ asub) = objCommitTask $ aModelAnObject asub

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

modelObjectMaker :: ResourceContext -> Model update -> a -> ObjectMaker update a
modelObjectMaker rc (MkResource rr MkAModel {..}) val update utask = do
    runResourceRunner rc rr $ aModelSubscribe update utask
    return $ MkObjectMakerResult (MkResource rr aModelAnObject) aModelUpdatesTask val

makeSharedModel :: forall update a. ObjectMaker update a -> LifeCycleIO (Model update, a)
makeSharedModel om = do
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
    MkResource (trunC :: ResourceRunner tt) aModelAnObject <- return omrObject
    Dict <- return $ resourceRunnerUnliftAllDict trunC
    Dict <- return $ transStackDict @MonadUnliftIO @tt @IO
    let
        aModelSubscribe ::
               Task () -> (ResourceContext -> NonEmpty update -> EditContext -> IO ()) -> ApplyStack tt LifeCycleIO ()
        aModelSubscribe taskC updateC =
            stackLift @tt $ do
                key <- liftIO $ mVarRun var $ addStoreStateT (taskC, updateC)
                lifeCycleClose @IO $ mVarRun var $ deleteStoreStateT key
        aModelUpdatesTask = omrUpdatesTask
        child :: Model update
        child = MkResource trunC $ MkAModel {..}
    return (child, omrValue)

shareObjectMaker :: forall update a. ObjectMaker update a -> LifeCycleIO (ResourceContext -> ObjectMaker update a)
shareObjectMaker uobj = do
    (sub, a) <- makeSharedModel uobj
    return $ \rc -> modelObjectMaker rc sub a

makeReflectingModel ::
       forall update. IsUpdate update
    => Object (UpdateEdit update)
    -> LifeCycleIO (Model update)
makeReflectingModel object = do
    (sub, ()) <- makeSharedModel $ reflectingObjectMaker object
    return sub

floatMapModel ::
       forall updateA updateB.
       ResourceContext
    -> FloatingChangeLens updateA updateB
    -> Model updateA
    -> LifeCycleIO (Model updateB)
floatMapModel rc lens subA = do
    (subB, ()) <- makeSharedModel $ mapObjectMaker rc lens $ modelObjectMaker rc subA ()
    return subB

mapModel :: forall updateA updateB. ChangeLens updateA updateB -> Model updateA -> Model updateB
mapModel plens (MkResource rr (MkAModel objA subA utaskA)) =
    case resourceRunnerUnliftAllDict rr of
        Dict -> let
            objB = mapAnObject plens objA
            subB utask recvB = let
                recvA rc updatesA ec = do
                    updatessB <-
                        runResourceRunner rc rr $ for updatesA $ \updateA -> clUpdate plens updateA (objRead objA)
                    case nonEmpty $ mconcat $ toList updatessB of
                        Nothing -> return ()
                        Just updatesB' -> recvB rc updatesB' ec
                in subA utask recvA
            in MkResource rr $ MkAModel objB subB utaskA

anObjectModel :: AnObject (UpdateEdit update) '[] -> Model update
anObjectModel anobj = MkResource nilResourceRunner $ MkAModel anobj (\_ _ -> return ()) mempty

unitModel :: Model (WholeUpdate ())
unitModel = anObjectModel $ MkAnObject (\ReadWhole -> return ()) (\_ -> return Nothing) mempty

constantModel ::
       forall update. SubjectReader (UpdateReader update)
    => UpdateSubject update
    -> Model (ReadOnlyUpdate update)
constantModel subj = anObjectModel $ immutableAnObject $ subjectToReadable subj

modelToReadOnly :: Model update -> Model (ReadOnlyUpdate update)
modelToReadOnly = mapModel toReadOnlyChangeLens

makeMemoryModel :: forall a. a -> LifeCycleIO (Model (WholeUpdate a))
makeMemoryModel initial = do
    obj <- liftIO $ makeMemoryObject initial $ \_ -> True
    makeReflectingModel obj
