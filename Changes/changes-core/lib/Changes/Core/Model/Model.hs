module Changes.Core.Model.Model
    ( AModel (..)
    , aModelRead
    , aModelEdit
    , Model
    , modelReference
    , modelUpdatesTask
    , modelCommitsTask
    , makeReflectingModel
    , makeSharedModel
    , modelPremodel
    , sharePremodel
    , floatMapModel
    , mapModel
    , unitModel
    , constantModel
    , modelToReadOnly
    , makeMemoryModel
    )
where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Model.EditContext
import Changes.Core.Model.Premodel
import Changes.Core.Model.Reference
import Changes.Core.Read
import Changes.Core.Resource
import Changes.Core.Types

data AModel update tt = MkAModel
    { aModelAReference :: AReference (UpdateEdit update) tt
    , aModelSubscribe :: Task IO () -> (ResourceContext -> NonEmpty update -> EditContext -> IO ()) -> ApplyStack tt Lifecycle ()
    , aModelUpdatesTask :: Task IO ()
    }

aModelRead :: AModel update tt -> Readable (ApplyStack tt IO) (UpdateReader update)
aModelRead amodel = refRead $ aModelAReference amodel

aModelEdit ::
    AModel update tt -> NonEmpty (UpdateEdit update) -> ApplyStack tt IO (Maybe (EditSource -> ApplyStack tt IO ()))
aModelEdit = refEdit . aModelAReference

instance MapResource (AModel update) where
    mapResource ::
        forall tt1 tt2.
        (MonadTransStackUnlift tt1, MonadTransStackUnlift tt2) =>
        TransListFunction tt1 tt2 ->
        AModel update tt1 ->
        AModel update tt2
    mapResource tlf (MkAModel obj1 sub1 utask) =
        case transStackDict @Monad @tt1 @IO of
            Dict ->
                case transStackDict @Monad @tt2 @IO of
                    Dict -> let
                        obj2 = mapResource tlf obj1
                        sub2 recv task = tlfFunction tlf (Proxy @Lifecycle) $ sub1 recv task
                        in MkAModel obj2 sub2 utask

type Model update = Resource (AModel update)

data UpdateStoreEntry update = MkUpdateStoreEntry
    { useUpdate :: NonEmpty update -> EditContext -> IO ()
    , useTask :: Task IO ()
    , useLifeState :: LifeState
    }

type UpdateStore update = Store (UpdateStoreEntry update)

modelReference :: Model update -> Reference (UpdateEdit update)
modelReference (MkResource rr amodel) = MkResource rr $ aModelAReference amodel

modelUpdatesTask :: Model update -> Task IO ()
modelUpdatesTask (MkResource _ asub) = aModelUpdatesTask asub

modelCommitsTask :: Model update -> Task IO ()
modelCommitsTask (MkResource _ asub) = refCommitTask $ aModelAReference asub

newtype UpdateQueue update
    = MkUpdateQueue [(EditContext, NonEmpty update)]

collapse1 :: (EditContext, NonEmpty update) -> [(EditContext, NonEmpty update)] -> [(EditContext, NonEmpty update)]
collapse1 (esa, ea) ((esb, eb) : bb)
    | esa == esb = (esb, ea <> eb) : bb
collapse1 a bb = a : bb

collapse :: [(EditContext, NonEmpty update)] -> [(EditContext, NonEmpty update)] -> [(EditContext, NonEmpty update)]
collapse [] bb = bb
collapse [a] bb = collapse1 a bb
collapse (a : aa) bb = a : collapse aa bb

instance Semigroup (UpdateQueue update) where
    MkUpdateQueue aa <> MkUpdateQueue bb = MkUpdateQueue $ collapse aa bb

singleUpdateQueue :: NonEmpty update -> EditContext -> UpdateQueue update
singleUpdateQueue updates ec = MkUpdateQueue $ pure (ec, updates)

modelPremodel :: ResourceContext -> Model update -> a -> Premodel update a
modelPremodel rc (MkResource rr MkAModel{..}) val update utask = do
    runResourceRunner rc rr $ aModelSubscribe update utask
    return $ MkPremodelResult (MkResource rr aModelAReference) aModelUpdatesTask val

makeSharedModel :: forall update a. Premodel update a -> Lifecycle (Model update, a)
makeSharedModel premodel = do
    var :: MVar (UpdateStore update) <- liftIO $ newMVar emptyStore
    let
        updateP :: ResourceContext -> NonEmpty update -> EditContext -> IO ()
        updateP _ updates ectxt = do
            store <- mVarRunStateT var get
            for_ store $ \entry -> useUpdate entry updates ectxt
        getTasks :: IO ([Task IO ()])
        getTasks = do
            store <- mVarRunStateT var get
            return $ fmap useTask $ toList store
        utaskP :: Task IO ()
        utaskP = ioTask (fmap mconcat getTasks)
    MkPremodelResult{..} <- premodel utaskP updateP
    MkResource (trunC :: ResourceRunner tt) aModelAReference <- return pmrReference
    Dict <- return $ resourceRunnerUnliftDict trunC
    Dict <- return $ transStackDict @MonadTunnelIO @tt @IO
    let
        aModelSubscribe ::
            Task IO () -> (ResourceContext -> NonEmpty update -> EditContext -> IO ()) -> ApplyStack tt Lifecycle ()
        aModelSubscribe taskC updateC =
            stackLift @tt @Lifecycle $ do
                key <-
                    liftIO
                        $ mVarRunStateT var
                        $ do
                            ((updateAsync, runnertask), useLifeState) <-
                                lift
                                    $ getLifeState
                                    $ asyncRunner @(UpdateQueue update) "model-update"
                                    $ \(MkUpdateQueue sourcedupdates) ->
                                        for_ sourcedupdates $ \(ec, updates) -> updateC emptyResourceContext updates ec
                            let
                                useUpdate updates ectxt = updateAsync $ singleUpdateQueue updates ectxt
                                useTask = taskC <> runnertask
                            addStoreStateT $ MkUpdateStoreEntry{..}
                lifecycleOnClose $ do
                    mentry <-
                        mVarRunStateT var $ do
                            mentry <- lookupStoreStateT key
                            deleteStoreStateT key
                            return mentry
                    for_ mentry $ \entry -> closeLifeState $ useLifeState entry
        aModelUpdatesTask = pmrUpdatesTask
        child :: Model update
        child = MkResource trunC $ MkAModel{..}
    return (child, pmrValue)

sharePremodel :: forall update a. Premodel update a -> Lifecycle (ResourceContext -> Premodel update a)
sharePremodel uobj = do
    (sub, a) <- makeSharedModel uobj
    return $ \rc -> modelPremodel rc sub a

makeReflectingModel ::
    forall update.
    IsUpdate update =>
    Reference (UpdateEdit update) ->
    Lifecycle (Model update)
makeReflectingModel reference = do
    (sub, ()) <- makeSharedModel $ reflectingPremodel reference
    return sub

floatMapModel ::
    forall updateA updateB.
    ResourceContext ->
    FloatingChangeLens updateA updateB ->
    Model updateA ->
    Lifecycle (Model updateB)
floatMapModel rc lens modelA = do
    (modelB, ()) <- makeSharedModel $ mapPremodel rc lens $ modelPremodel rc modelA ()
    return modelB

mapModel :: forall updateA updateB. ChangeLens updateA updateB -> Model updateA -> Model updateB
mapModel plens (MkResource rr (MkAModel objA subA utaskA)) =
    case resourceRunnerUnliftDict rr of
        Dict -> let
            objB = mapAReference plens objA
            subB utask recvB = let
                recvA rc updatesA ec = do
                    updatessB <-
                        runResourceRunner rc rr $ for updatesA $ \updateA -> clUpdate plens updateA (refRead objA)
                    case nonEmpty $ mconcat $ toList updatessB of
                        Nothing -> return ()
                        Just updatesB' -> recvB rc updatesB' ec
                in subA utask recvA
            in MkResource rr $ MkAModel objB subB utaskA

aReferenceModel :: AReference (UpdateEdit update) '[] -> Model update
aReferenceModel anobj = MkResource nilResourceRunner $ MkAModel anobj (\_ _ -> return ()) mempty

unitModel :: Model (WholeUpdate ())
unitModel = aReferenceModel $ MkAReference (\ReadWhole -> return ()) (\_ -> return Nothing) mempty

constantModel ::
    forall update.
    SubjectReader (UpdateReader update) =>
    UpdateSubject update ->
    Model (ReadOnlyUpdate update)
constantModel subj = aReferenceModel $ immutableAReference $ subjectToReadable subj

modelToReadOnly :: Model update -> Model (ReadOnlyUpdate update)
modelToReadOnly = mapModel toReadOnlyChangeLens

makeMemoryModel :: forall a. a -> Lifecycle (Model (WholeUpdate a))
makeMemoryModel initial = do
    obj <- liftIO $ makeMemoryReference initial $ \_ -> True
    makeReflectingModel obj
