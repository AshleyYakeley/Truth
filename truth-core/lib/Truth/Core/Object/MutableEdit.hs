module Truth.Core.Object.MutableEdit where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types.None
import Truth.Core.Types.Pair

data MutableEdit m edit = MkMutableEdit
    { mutableRead :: MutableRead m (EditReader edit)
    , mutableEdit :: [edit] -> m (Maybe (m ()))
    }

instance Floating edit edit => Floating edit (MutableEdit m edit) where
    floatingUpdate edit (MkMutableEdit mr me) = MkMutableEdit mr $ \edits -> me $ fmap (floatingUpdate edit) edits

pushMutableEdit :: Monad m => MutableEdit m edit -> [edit] -> m ()
pushMutableEdit me edits = do
    mmu <- mutableEdit me edits
    case mmu of
        Just mu -> mu
        Nothing -> return ()

mutableReadEdit :: MonadIO m => MutableEdit m edit -> Readable (EditReader edit) [edit] -> m (Maybe (m ()))
mutableReadEdit MkMutableEdit {..} MkReadable {..} = do
    edits <- unReadable mutableRead
    mutableEdit edits

mutableAllowed :: Functor m => MutableEdit m edit -> edit -> m Bool
mutableAllowed me edit = mutableAlloweds me [edit]

mutableAlloweds :: Functor m => MutableEdit m edit -> [edit] -> m Bool
mutableAlloweds me edits = fmap isJust $ mutableEdit me edits

alwaysEditAction :: Applicative m => m () -> m (Maybe (m ()))
alwaysEditAction action = pure $ Just $ action

testEditAction :: Functor m => m Bool -> m () -> m (Maybe (m ()))
testEditAction test action =
    fmap
        (\ok ->
             if ok
                 then Just action
                 else Nothing)
        test

singleMutableEdit :: Applicative m => (edit -> m (Maybe (m ()))) -> [edit] -> m (Maybe (m ()))
singleMutableEdit mutableEdit' edits = fmap (fmap (fmap (\_ -> ()) . sequenceA) . sequenceA) $ for edits mutableEdit'

singleAlwaysMutableEdit :: Applicative m => (edit -> m ()) -> [edit] -> m (Maybe (m ()))
singleAlwaysMutableEdit mutableEdit' edits = alwaysEditAction $ for_ edits mutableEdit'

remonadMutableEdit :: Functor m1 => (forall a. m1 a -> m2 a) -> MutableEdit m1 edit -> MutableEdit m2 edit
remonadMutableEdit mf (MkMutableEdit r e) = MkMutableEdit (fmap mf r) (fmap (mf . (fmap (fmap mf))) e)

liftMutableEdit :: (MonadTrans t, Monad m) => MutableEdit m edit -> MutableEdit (t m) edit
liftMutableEdit = remonadMutableEdit lift

constantMutableEdit ::
       forall m edit. (SubjectReader (EditReader edit), Monad m)
    => EditSubject edit
    -> MutableEdit m edit
constantMutableEdit subj =
    let mutableRead :: MutableRead m (EditReader edit)
        mutableRead = readFromSubjectM $ pure subj
        mutableEdit _ = pure Nothing
    in MkMutableEdit {..}

readOnlyMutableEdit :: Applicative m => MutableRead m reader -> MutableEdit m (NoEdit reader)
readOnlyMutableEdit mutableRead =
    let mutableEdit [] = pure $ Just $ pure ()
        mutableEdit (e:_) = never e
    in MkMutableEdit {..}

stateMutableEdit :: (MonadIO m, Edit edit) => MutableEdit (StateT (MutableReadW m (EditReader edit)) m) edit
stateMutableEdit =
    let mutableRead = stateMutableRead
        mutableEdit edits = return $ Just $ modify $ mapMutableReadW $ applyEdits edits
    in MkMutableEdit {..}

efMapMutableRead ::
       forall m lensstate edita editb. MonadIO m
    => EditFunction lensstate edita editb
    -> MutableRead m (EditReader edita)
    -> MutableRead (StateT lensstate m) (EditReader editb)
efMapMutableRead MkEditFunction {..} readA rt = do
    st <- get
    lift $ mapMutableRead (editGet st) readA rt

mapMutableEdit ::
       forall m lensstate edita editb. (MonadIO m, Edit edita)
    => EditLens lensstate edita editb
    -> MutableEdit m edita
    -> MutableEdit (StateT lensstate m) editb
mapMutableEdit lens@MkEditLens {..} mutedA =
    let MkEditFunction {..} = editLensFunction
        readA :: MutableRead m (EditReader edita)
        pushEditA :: [edita] -> m (Maybe (m ()))
        MkMutableEdit readA pushEditA = mutedA
        readB :: MutableRead (StateT lensstate m) (EditReader editb)
        readB = efMapMutableRead editLensFunction readA
        convertEdit :: [editb] -> (StateT lensstate m) (Maybe [edita])
        convertEdit editBs = do
            oldstate <- get
            fstateeditA :: Maybe (state, [edita]) <- lift $ unReadable (editLensPutEdits lens oldstate editBs) readA
            case fstateeditA of
                Just (newstate, editAs) -> do
                    put newstate
                    return $ Just editAs
                Nothing -> return Nothing
        pushEditB :: [editb] -> (StateT lensstate m) (Maybe (StateT lensstate m ()))
        pushEditB editB = do
            meditA <- convertEdit editB
            case meditA of
                Nothing -> return Nothing
                Just editAs -> do
                    mstates <- lift $ pushEditA editAs
                    return $ fmap lift mstates
    in MkMutableEdit readB pushEditB

withMapMutableRead ::
       (IsStateIO m)
    => GeneralFunction edita editb
    -> MutableRead m (EditReader edita)
    -> (forall m'. IsStateIO m' =>
                       MutableRead m' (EditReader editb) -> m' r)
    -> m r
withMapMutableRead (MkCloseState func) mr call = editAccess func $ call $ efMapMutableRead func mr

withMapMutableEdit ::
       (IsStateIO m, Edit edita)
    => GeneralLens edita editb
    -> MutableEdit m edita
    -> (forall m'. IsStateIO m' =>
                       MutableEdit m' editb -> m' r)
    -> m r
withMapMutableEdit (MkCloseState lens) mutedA call =
    editAccess (editLensFunction lens) $ call $ mapMutableEdit lens mutedA

fixedMapMutableEdit ::
       forall m edita editb. (MonadIO m, Edit edita)
    => PureEditLens edita editb
    -> MutableEdit m edita
    -> MutableEdit m editb
fixedMapMutableEdit lens muted = remonadMutableEdit runUnitStateT $ mapMutableEdit lens muted

noneMutableEdit :: Applicative m => MutableEdit m (NoEdit (NoReader t))
noneMutableEdit = readOnlyMutableEdit never

pairMutableEdit :: Applicative m => MutableEdit m ea -> MutableEdit m eb -> MutableEdit m (PairEdit ea eb)
pairMutableEdit mea meb =
    MkMutableEdit
    { mutableRead = pairMutableRead (mutableRead mea) (mutableRead meb)
    , mutableEdit =
          \edits ->
              let (eas, ebs) = partitionPairEdits edits
              in liftA2 (liftA2 (liftA2 mappend)) (mutableEdit mea eas) (mutableEdit meb ebs)
    }
