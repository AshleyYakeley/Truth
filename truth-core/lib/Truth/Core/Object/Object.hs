module Truth.Core.Object.Object where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.EditContext
import Truth.Core.Object.UnliftIO
import Truth.Core.Read
import Truth.Core.Types.None
import Truth.Core.Types.Whole
import Truth.Debug

data AnObject m edit = MkAnObject
    { objRead :: MutableRead m (EditReader edit)
    , objEdit :: [edit] -> m (Maybe (EditSource -> m ()))
    }

type Object = CloseUnliftIO AnObject

instance Show (Object edit) where
    show (MkCloseUnliftIO _ _) = "object"

noneObject :: Object (NoEdit (NoReader t))
noneObject = let
    objRead :: MutableRead IO (NoReader t)
    objRead = never
    objEdit :: [NoEdit (NoReader t)] -> IO (Maybe (EditSource -> IO ()))
    objEdit [] = return $ Just $ \_ -> return ()
    objEdit (e:_) = never e
    in MkCloseUnliftIO (MkTransform id) $ MkAnObject {..}

mvarObject :: forall a. MVar a -> (a -> Bool) -> Object (WholeEdit a)
mvarObject var allowed = let
    objRead :: MutableRead (StateT a IO) (WholeReader a)
    objRead ReadWhole = get
    objEdit :: [WholeEdit a] -> StateT a IO (Maybe (EditSource -> StateT a IO ()))
    objEdit edits = do
        na <- applyEdits edits (mSubjectToMutableRead get) ReadWhole
        return $
            if allowed na
                then Just $ \_ -> put na
                else Nothing
    in MkCloseUnliftIO (traceThing "mvarObject.mvarUnliftIO" $ mvarUnliftIO var) $ MkAnObject {..}

freeIOObject :: forall a. a -> (a -> Bool) -> IO (Object (WholeEdit a))
freeIOObject firsta allowed = do
    var <- newMVar firsta
    return $ mvarObject var allowed

pushEdit :: MonadIO m => EditSource -> m (Maybe (EditSource -> m ())) -> m Bool
pushEdit esrc mmmu = traceBracket "pushEdit.examine" $ do
    mmu <- mmmu
    case mmu of
        Just mu -> traceBracket "pushEdit.do" $ do
            mu esrc
            return True
        Nothing -> traceBracket "pushEdit.ignore" $ return False

pushOrFail :: (MonadIO m, MonadFail m) => String -> EditSource -> m (Maybe (EditSource -> m ())) -> m ()
pushOrFail s esrc mmmu = traceBracket "pushOrFail.examine" $ do
    mmu <- mmmu
    case mmu of
        Just mu -> traceBracket "pushOrFail.do" $ mu esrc
        Nothing -> traceBracket "pushOrFail.fail" $ fail s

mapObject :: forall edita editb. EditLens edita editb -> Object edita -> Object editb
mapObject = lensObject False

lensAnObject ::
       forall t m edita editb. (MonadTrans t, MonadIO (t m), MonadIO m)
    => AnEditLens t edita editb
    -> AnObject m edita
    -> AnObject (t m) editb
lensAnObject MkAnEditLens {..} (MkAnObject objReadA objEditA) = let
    MkAnEditFunction {..} = elFunction
    objReadB :: MutableRead (t m) (EditReader editb)
    objReadB = traceThing "lensObject.run.read" $ efGet objReadA
    objEditB :: [editb] -> t m (Maybe (EditSource -> t m ()))
    objEditB editbs = traceBracket "lensObject.edit" $ do
        meditas <- traceBracket "lensObject.edit lens" $ elPutEdits editbs objReadA
        case meditas of
            Nothing -> do
                traceIOM "lensObject.edit Nothing"
                return Nothing
            Just editas -> do
                mmu <- traceBracket "lensObject.edit objEditA" $ lift $ objEditA editas
                case mmu of
                    Nothing -> traceBracket "lensObject.edit.examine: no action" $ return Nothing
                    Just mu -> traceBracket "lensObject.edit.examine: action" $ return $ Just $ \esrc -> traceBracket "lensObject.edit.run" $ lift $ mu esrc
    in MkAnObject objReadB objEditB

lensObject :: forall edita editb. Bool -> EditLens edita editb -> Object edita -> Object editb
lensObject discard (MkCloseUnlift (lensUnlift :: Unlift t) alens) (MkCloseUnliftIO (objUnlift :: UnliftIO m) aobj)
    | Dict <- hasTransConstraint @MonadUnliftIO @t @m = let
        objRunB = lensObjectUnlift discard lensUnlift objUnlift
        in MkCloseUnliftIO objRunB $ lensAnObject alens aobj

immutableAnObject :: Monad m => MutableRead m (EditReader edit) -> AnObject m edit
immutableAnObject mr =
    MkAnObject mr $
    -- must allow empty edit list
    \case
        [] -> return $ Just $ \_ -> return ()
        _ -> return Nothing

readConstantObject :: MutableRead IO (EditReader edit) -> Object edit
readConstantObject mr = MkCloseUnliftIO (MkTransform id) $ immutableAnObject mr

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
convertObject (MkCloseUnliftIO (objRun :: UnliftIO m) (MkAnObject mra pe)) = let
    objRead :: MutableRead m (EditReader editb)
    objRead = mSubjectToMutableRead $ mutableReadToSubject mra
    objEdit :: [editb] -> m (Maybe (EditSource -> m ()))
    objEdit ebs = do
        oldsubj <- mutableReadToSubject mra
        newsubj <- mapSubjectEdits ebs oldsubj
        eas <- getReplaceEditsFromSubject newsubj
        pe eas
    in MkCloseUnliftIO objRun MkAnObject {..}

-- | Combines all the edits made in each call to the object.
cacheWholeObject ::
       forall t. Eq t
    => Object (WholeEdit t)
    -> Object (WholeEdit t)
cacheWholeObject (MkCloseUnliftIO (MkTransform run :: UnliftIO m) (MkAnObject rd push)) = let
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
    in MkCloseUnliftIO run' $ MkAnObject rd' push'

copyObject :: FullEdit edit => EditSource -> Object edit -> Object edit -> IO ()
copyObject esrc (MkCloseUnliftIO (runSrc :: UnliftIO ms) (MkAnObject readSrc _)) (MkCloseUnliftIO (runDest :: UnliftIO md) (MkAnObject _ pushDest)) =
    case isCombineMonadIO @ms @md of
        Dict ->
            runTransform (combineUnliftIOs runSrc runDest) $
            replaceEdit (remonadMutableRead (combineLiftFst @ms @md) readSrc) $ \edit ->
                combineLiftSnd @ms @md $ pushOrFail "failed to copy object" esrc $ pushDest [edit]

exclusiveObject :: forall edit. Object edit -> With IO (Object edit)
exclusiveObject (MkCloseUnliftIO (run :: UnliftIO m) (MkAnObject rd push)) call =
    runTransform (traceThing "exclusiveObject:back" run) $ liftIOWithUnlift $ \unlift -> call $ MkCloseUnliftIO (traceThing "exclusiveObject:front" unlift) $ MkAnObject rd push

getObjectSubject :: FullSubjectReader (EditReader edit) => Object edit -> IO (EditSubject edit)
getObjectSubject (MkCloseUnliftIO unlift (MkAnObject rd _)) = runTransform unlift $ mutableReadToSubject rd
