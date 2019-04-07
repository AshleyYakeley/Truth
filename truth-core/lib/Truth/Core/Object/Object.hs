module Truth.Core.Object.Object where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.EditSource
import Truth.Core.Read
import Truth.Core.Types.None
import Truth.Core.Types.Whole

data Object edit = forall m. MonadStackIO m =>
                                 MkObject
    { objRun :: UnliftIO m
    , objRead :: MutableRead m (EditReader edit)
    , objEdit :: [edit] -> m (Maybe (EditSource -> m ()))
    }

instance Show (Object edit) where
    show (MkObject _ _ _) = "object"

noneObject :: Object (NoEdit (NoReader t))
noneObject = let
    objRun :: UnliftIO IO
    objRun = MkTransform id
    objRead :: MutableRead IO (NoReader t)
    objRead = never
    objEdit :: [NoEdit (NoReader t)] -> IO (Maybe (EditSource -> IO ()))
    objEdit [] = return $ Just $ \_ -> return ()
    objEdit (e:_) = never e
    in MkObject {..}

mvarObject :: forall a. MVar a -> (a -> Bool) -> Object (WholeEdit a)
mvarObject var allowed = let
    objRun :: UnliftIO (StateT a IO)
    objRun = mvarUnliftIO var
    objRead :: MutableRead (StateT a IO) (WholeReader a)
    objRead ReadWhole = get
    objEdit :: [WholeEdit a] -> StateT a IO (Maybe (EditSource -> StateT a IO ()))
    objEdit edits = do
        na <- applyEdits edits (mSubjectToMutableRead get) ReadWhole
        return $
            if allowed na
                then Just $ \_ -> put na
                else Nothing
    in MkObject {..}

freeIOObject :: forall a. a -> (a -> Bool) -> IO (Object (WholeEdit a))
freeIOObject firsta allowed = do
    var <- newMVar firsta
    return $ mvarObject var allowed

pushEdit :: Monad m => EditSource -> m (Maybe (EditSource -> m ())) -> m Bool
pushEdit esrc mmmu = do
    mmu <- mmmu
    case mmu of
        Just mu -> do
            mu esrc
            return True
        Nothing -> return False

pushOrFail :: MonadFail m => String -> EditSource -> m (Maybe (EditSource -> m ())) -> m ()
pushOrFail s esrc mmmu = do
    mmu <- mmmu
    case mmu of
        Just mu -> mu esrc
        Nothing -> fail s

mapObject :: forall edita editb. EditLens edita editb -> Object edita -> Object editb
mapObject = lensObject False

lensObject :: forall edita editb. Bool -> EditLens edita editb -> Object edita -> Object editb
lensObject discard (MkCloseUnlift (MkUnlift lensRun :: Unlift tl) MkAnEditLens {..}) (MkObject (MkTransform objRunA :: UnliftIO mr) objReadA objEditA)
    | Dict <- hasTransConstraint @MonadUnliftIO @tl @mr = let
        MkAnEditFunction {..} = elFunction
        objRunBFull :: UnliftIO (tl mr)
        objRunBFull = MkTransform $ \tmr -> objRunA $ lensRun $ liftWithUnlift $ \(MkUnlift unlift) -> unlift tmr
        objRunBDiscard :: UnliftIO (tl mr)
        objRunBDiscard =
            MkTransform $ \tmr ->
                objRunA $ do
                    MkUnlift du <- lensRun $ getDiscardingUnlift
                    du tmr -- discard lens effects: all these effects will be replayed by the update
        objRunB :: UnliftIO (tl mr)
        objRunB =
            if discard
                then objRunBDiscard
                else objRunBFull
        objReadB :: MutableRead (tl mr) (EditReader editb)
        objReadB = efGet objReadA
        objEditB :: [editb] -> tl mr (Maybe (EditSource -> tl mr ()))
        objEditB editbs = do
            meditas <- elPutEdits editbs objReadA
            case meditas of
                Nothing -> return Nothing
                Just editas -> do
                    mmu <- lift $ objEditA editas
                    case mmu of
                        Nothing -> return Nothing
                        Just mu -> return $ Just $ \esrc -> lift $ mu esrc
        in MkObject @editb @(tl mr) objRunB objReadB objEditB

readConstantObject :: MutableRead IO (EditReader edit) -> Object edit
readConstantObject mr =
    MkObject (MkTransform id) mr $ 
    -- must allow empty edit list
    \case
        [] -> return $ Just $ \_ -> return ()
        _ -> return Nothing

constantObject :: SubjectReader (EditReader edit) => EditSubject edit -> Object edit
constantObject subj = readConstantObject $ subjectToMutableRead subj

alwaysEdit :: Monad m => ([edit] -> EditSource -> m ()) -> [edit] -> m (Maybe (EditSource -> m ()))
alwaysEdit em edits = return $ Just $ em edits

singleAlwaysEdit :: Monad m => (edit -> EditSource -> m ()) -> [edit] -> m (Maybe (EditSource -> m ()))
singleAlwaysEdit em = alwaysEdit $ \edits esrc -> for_ edits $ \edit -> em edit esrc

testEditAction :: IO Bool -> (EditSource -> IO ()) -> IO (Maybe (EditSource -> IO ()))
testEditAction test action = do
    ok <- test
    return $
        if ok
            then Just action
            else Nothing

singleEdit :: Monad m => (edit -> m (Maybe (EditSource -> m ()))) -> [edit] -> m (Maybe (EditSource -> m ()))
singleEdit call edits =
    getComposeM $ do
        actions <- for edits $ \edit -> MkComposeM $ call edit
        return $ \esrc -> for_ actions $ \action -> action esrc

convertObject ::
       forall edita editb. (EditSubject edita ~ EditSubject editb, FullEdit edita, SubjectMapEdit editb)
    => Object edita
    -> Object editb
convertObject (MkObject (objRun :: UnliftIO m) mra pe) = let
    objRead :: MutableRead m (EditReader editb)
    objRead = mSubjectToMutableRead $ mutableReadToSubject mra
    objEdit :: [editb] -> m (Maybe (EditSource -> m ()))
    objEdit ebs = do
        oldsubj <- mutableReadToSubject mra
        newsubj <- mapSubjectEdits ebs oldsubj
        eas <- getReplaceEditsFromSubject newsubj
        pe eas
    in MkObject {..}

-- | Combines all the edits made in each call to the object.
cacheWholeObject ::
       forall t. Eq t
    => Object (WholeEdit t)
    -> Object (WholeEdit t)
cacheWholeObject (MkObject (MkTransform run :: UnliftIO m) rd push) = let
    run' :: UnliftIO (StateT (t, Maybe EditSource) m)
    run' =
        MkTransform $ \ma ->
            run $ do
                oldval :: t <- rd ReadWhole
                (r, (newval, mesrc)) <- runStateT ma (oldval, Nothing)
                case mesrc of
                    Just esrc ->
                        if oldval == newval
                            then return ()
                            else do
                                maction <- push [MkWholeEdit newval]
                                case maction of
                                    Just action -> action esrc
                                    Nothing -> liftIO $ fail "disallowed cached edit"
                    Nothing -> return ()
                return r
    rd' :: MutableRead (StateT (t, Maybe EditSource) m) (WholeReader t)
    rd' ReadWhole = do
        (t, _) <- get
        return t
    push' :: [WholeEdit t] -> StateT (t, Maybe EditSource) m (Maybe (EditSource -> StateT (t, Maybe EditSource) m ()))
    push' = singleAlwaysEdit $ \(MkWholeEdit t) esrc -> put (t, Just esrc)
    in MkObject run' rd' push'

copyObject :: FullEdit edit => EditSource -> Object edit -> Object edit -> IO ()
copyObject esrc (MkObject (runSrc :: UnliftIO ms) readSrc _) (MkObject (runDest :: UnliftIO md) _ pushDest) =
    case isCombineMonadIO @ms @md of
        Dict ->
            runTransform (combineUnliftIOs runSrc runDest) $
            replaceEdit (remonadMutableRead (combineLiftFst @ms @md) readSrc) $ \edit ->
                combineLiftSnd @ms @md $ pushOrFail "failed to copy object" esrc $ pushDest [edit]

exclusiveObject :: forall edit. Object edit -> With (Object edit)
exclusiveObject (MkObject (run :: UnliftIO m) rd push) call =
    runTransform run $ liftIOWithUnlift $ \unlift -> call $ MkObject unlift rd push

getObjectSubject :: FullSubjectReader (EditReader edit) => Object edit -> IO (EditSubject edit)
getObjectSubject (MkObject unlift rd _) = runTransform unlift $ mutableReadToSubject rd
