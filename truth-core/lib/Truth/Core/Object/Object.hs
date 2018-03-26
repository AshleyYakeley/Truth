module Truth.Core.Object.Object where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types.None
import Truth.Core.Types.Whole

data Object edit = forall m. MonadStackIO m =>
                             MkObject
    { objRun :: UnliftIO m
    , objRead :: MutableRead m (EditReader edit)
    , objEdit :: [edit] -> m (Maybe (m ()))
    }

instance Show (Object edit) where
    show (MkObject _ _ _) = "object"

noneObject :: Object (NoEdit (NoReader t))
noneObject = let
    objRun :: UnliftIO IO
    objRun = MkUnliftIO id
    objRead :: MutableRead IO (NoReader t)
    objRead = never
    objEdit :: [NoEdit (NoReader t)] -> IO (Maybe (IO ()))
    objEdit [] = return $ Just $ return ()
    objEdit (e:_) = never e
    in MkObject {..}

mvarObject :: forall a. MVar a -> (a -> Bool) -> Object (WholeEdit a)
mvarObject var allowed = let
    objRun :: UnliftIO (StateT a IO)
    objRun = mvarUnliftIO var
    objRead :: MutableRead (StateT a IO) (WholeReader a)
    objRead ReadWhole = get
    objEdit :: [WholeEdit a] -> StateT a IO (Maybe (StateT a IO ()))
    objEdit edits = do
        na <- applyEdits edits (mSubjectToMutableRead get) ReadWhole
        return $
            if allowed na
                then Just $ put na
                else Nothing
    in MkObject {..}

freeIOObject :: forall a. a -> (a -> Bool) -> IO (Object (WholeEdit a))
freeIOObject firsta allowed = do
    var <- newMVar firsta
    return $ mvarObject var allowed

pushEdit :: Monad m => m (Maybe (m ())) -> m ()
pushEdit mmmu = do
    mmu <- mmmu
    case mmu of
        Just mu -> mu
        Nothing -> return ()

pushOrFail :: MonadFail m => String -> m (Maybe (m ())) -> m ()
pushOrFail s mmmu = do
    mmu <- mmmu
    case mmu of
        Just mu -> mu
        Nothing -> fail s

mapObject :: forall edita editb. EditLens edita editb -> Object edita -> Object editb
mapObject = lensObject False

lensObject :: forall edita editb. Bool -> EditLens edita editb -> Object edita -> Object editb
lensObject discard (MkCloseUnlift (MkUnlift lensRun :: Unlift tl) MkAnEditLens {..}) (MkObject (MkUnliftIO objRunA :: UnliftIO mr) objReadA objEditA)
    | Dict <- hasTransConstraint @MonadUnliftIO @tl @mr = let
        MkAnEditFunction {..} = elFunction
        objRunBFull :: UnliftIO (tl mr)
        objRunBFull = MkUnliftIO $ \tmr -> objRunA $ lensRun $ liftWithUnlift $ \(MkUnlift unlift) -> unlift tmr
        objRunBDiscard :: UnliftIO (tl mr)
        objRunBDiscard =
            MkUnliftIO $ \tmr ->
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
        objEditB :: [editb] -> tl mr (Maybe (tl mr ()))
        objEditB editbs = do
            meditas <- elPutEdits editbs objReadA
            case meditas of
                Nothing -> return Nothing
                Just editas -> do
                    mmu <- lift $ objEditA editas
                    case mmu of
                        Nothing -> return Nothing
                        Just mu -> return $ Just $ lift mu
        in MkObject @editb @(tl mr) objRunB objReadB objEditB

constantObject :: SubjectReader (EditReader edit) => EditSubject edit -> Object edit
constantObject subj = MkObject (MkUnliftIO id) (subjectToMutableRead subj) $ \_ -> return Nothing

alwaysEdit :: Monad m => ([edit] -> m ()) -> [edit] -> m (Maybe (m ()))
alwaysEdit em edits = return $ Just $ em edits

singleAlwaysEdit :: Monad m => (edit -> m ()) -> [edit] -> m (Maybe (m ()))
singleAlwaysEdit em = alwaysEdit $ \edits -> for_ edits em

testEditAction :: IO Bool -> IO () -> IO (Maybe (IO ()))
testEditAction test action = do
    ok <- test
    return $
        if ok
            then Just action
            else Nothing

singleEdit :: Monad m => (edit -> m (Maybe (m ()))) -> [edit] -> m (Maybe (m ()))
singleEdit call edits =
    getCompose $ do
        actions <- for edits $ \edit -> Compose $ call edit
        return $ for_ actions id

convertObject :: (EditSubject edita ~ EditSubject editb, FullEdit edita, FullEdit editb) => Object edita -> Object editb
convertObject = mapObject convertEditLens

-- | Combines all the edits made in each call to the object.
cacheObject ::
       forall t. Eq t
    => Object (WholeEdit t)
    -> Object (WholeEdit t)
cacheObject (MkObject (MkUnliftIO run :: UnliftIO m) rd push) = let
    run' :: UnliftIO (StateT t m)
    run' =
        MkUnliftIO $ \ma ->
            run $ do
                oldval <- rd ReadWhole
                (r, newval) <- runStateT ma oldval
                if oldval == newval
                    then return ()
                    else do
                        maction <- push [MkWholeEdit newval]
                        case maction of
                            Just action -> action
                            Nothing -> liftIO $ fail "disallowed cached edit"
                return r
    rd' :: MutableRead (StateT t m) (WholeReader t)
    rd' ReadWhole = get
    push' :: [WholeEdit t] -> StateT t m (Maybe (StateT t m ()))
    push' = singleAlwaysEdit $ \(MkWholeEdit t) -> put t
    in MkObject run' rd' push'

copyObject :: FullEdit edit => Object edit -> Object edit -> IO ()
copyObject (MkObject (runSrc :: UnliftIO ms) readSrc _) (MkObject (runDest :: UnliftIO md) _ pushDest) =
    case isCombineMonadIO @ms @md of
        Dict ->
            runUnliftIO (combineUnliftIOs runSrc runDest) $
            replaceEdit (remonadMutableRead (combineLiftFst @ms @md) readSrc) $ \edit ->
                combineLiftSnd @ms @md $ pushOrFail "failed to copy object" $ pushDest [edit]
