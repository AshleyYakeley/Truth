module Truth.Core.Object.Object where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types.None
import Truth.Core.Types.Pair
import Truth.Core.Types.Tuple
import Truth.Core.Types.Whole

data Object edit = forall m. MonadStackIO m =>
                             MkObject
    { objRun :: UnliftIO m
    , objRead :: MutableRead m (EditReader edit)
    , objEdit :: [edit] -> m (Maybe (m ()))
    }

pushEdit :: Monad m => m (Maybe (m ())) -> m ()
pushEdit mmmu = do
    mmu <- mmmu
    case mmu of
        Just mu -> mu
        Nothing -> return ()

mapObject ::
       forall edita editb. Edit edita
    => EditLens edita editb
    -> Object edita
    -> Object editb
mapObject (MkCloseUnlift (lensRun :: Unlift tl) lens@MkAnEditLens {..}) (MkObject (objRunA :: UnliftIO mr) objReadA objEditA)
    | Dict <- hasTransConstraint @MonadUnliftIO @tl @mr = let
        MkAnEditFunction {..} = elFunction
        objRunB :: UnliftIO (tl mr)
        objRunB = objRunA . lensRun . impotent -- revert lens effects: all these effects will be replayed by the update
        objReadB :: MutableRead (tl mr) (EditReader editb)
        objReadB = efGet objReadA
        objEditB :: [editb] -> tl mr (Maybe (tl mr ()))
        objEditB editbs = do
            meditas <- elPutEdits lens editbs objReadA
            case meditas of
                Nothing -> return Nothing
                Just editas -> do
                    mmu <- lift $ objEditA editas
                    case mmu of
                        Nothing -> return Nothing
                        Just mu -> return $ Just $ lift mu
        in MkObject @editb @(tl mr) objRunB objReadB objEditB

pairObjects :: forall edita editb. Object edita -> Object editb -> Object (PairEdit edita editb)
pairObjects (MkObject (runA :: UnliftIO ma) readA editA) (MkObject (runB :: UnliftIO mb) readB editB) =
    case isCombineMonadIO @ma @mb of
        Dict -> let
            runAB :: UnliftIO (CombineMonadIO ma mb)
            runAB = combineUnliftIOs runA runB
            readAB :: MutableRead (CombineMonadIO ma mb) (PairEditReader edita editb)
            readAB (MkTupleEditReader EditFirst r) = combineLiftFst @ma @mb $ readA r
            readAB (MkTupleEditReader EditSecond r) = combineLiftSnd @ma @mb $ readB r
            editAB :: [PairEdit edita editb] -> CombineMonadIO ma mb (Maybe (CombineMonadIO ma mb ()))
            editAB edits = let
                (eas, ebs) = partitionPairEdits edits
                in liftA2
                       (liftA2 $ \mau mbu -> (>>) (combineLiftFst @ma @mb mau) (combineLiftSnd @ma @mb mbu))
                       (combineLiftFst @ma @mb $ editA eas)
                       (combineLiftSnd @ma @mb $ editB ebs)
            in MkObject runAB readAB editAB

constantObject :: SubjectReader (EditReader edit) => EditSubject edit -> Object edit
constantObject subj = MkObject identityUnlift (subjectToMutableRead subj) $ \_ -> return Nothing

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

{-
nonlockingObject :: MutableEdit IO edit -> Object edit
nonlockingObject muted = MkObject $ \call -> call muted
-}
noneObject :: Object (NoEdit (NoReader t))
noneObject = let
    objRun :: UnliftIO IO
    objRun = id
    objRead :: MutableRead IO (NoReader t)
    objRead = never
    objEdit :: [NoEdit (NoReader t)] -> IO (Maybe (IO ()))
    objEdit [] = return $ Just $ return ()
    objEdit (e:_) = never e
    in MkObject {..}

mvarObject :: forall a. MVar a -> (a -> Bool) -> Object (WholeEdit a)
mvarObject var allowed = let
    objRun :: UnliftIO (StateT a IO)
    objRun = mvarUnlift var
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

convertObject :: (EditSubject edita ~ EditSubject editb, FullEdit edita, FullEdit editb) => Object edita -> Object editb
convertObject = mapObject convertEditLens

-- | Combines all the edits made in each call to the object.
cacheObject ::
       forall t. Eq t
    => Object (WholeEdit t)
    -> Object (WholeEdit t)
cacheObject (MkObject (run :: UnliftIO m) rd push) = let
    run' :: UnliftIO (StateT t m)
    run' ma =
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