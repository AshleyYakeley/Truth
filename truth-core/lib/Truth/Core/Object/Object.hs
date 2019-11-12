module Truth.Core.Object.Object where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.EditContext
import Truth.Core.Read
import Truth.Core.Types.None
import Truth.Core.Types.Whole

data AnObject (tt :: [TransKind]) edit = MkAnObject
    { objRead :: MutableRead (ApplyStack tt IO) (EditReader edit)
    , objEdit :: [edit] -> ApplyStack tt IO (Maybe (EditSource -> ApplyStack tt IO ()))
    }

type Object = Runnable1 AnObject

instance RunnableMap AnObject where
    mapRunnable ::
           forall tt1 tt2. (MonadTransStackUnliftAll tt1, MonadTransStackUnliftAll tt2)
        => TransListFunction tt1 tt2
        -> NestedMorphism (->) (AnObject tt1) (AnObject tt2)
    mapRunnable MkTransListFunction {..} =
        MkNestedMorphism $ \(MkAnObject r e) -> let
            r' :: MutableRead (ApplyStack tt2 IO) _
            r' rd = tlfFunction (Proxy @IO) $ r rd
            e' :: [_] -> ApplyStack tt2 IO (Maybe (EditSource -> ApplyStack tt2 IO ()))
            e' edits =
                case transStackDict @MonadIO @tt2 @IO of
                    Dict -> (fmap $ fmap $ fmap $ tlfFunction (Proxy @IO)) $ tlfFunction (Proxy @IO) $ e edits
            in MkAnObject r' e'

instance Show (Object edit) where
    show (MkRunnable1 _ _) = "object"

noneObject :: Object (NoEdit (NoReader t))
noneObject = let
    objRead :: MutableRead IO (NoReader t)
    objRead = never
    objEdit :: [NoEdit (NoReader t)] -> IO (Maybe (EditSource -> IO ()))
    objEdit [] = return $ Just $ \_ -> return ()
    objEdit (e:_) = never e
    in MkRunnable1 cmEmpty $ MkAnObject {..}

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
    anobj :: AnObject '[ StateT a] (WholeEdit a)
    anobj = MkAnObject {..}
    in MkRunnable1 (MkTransStackRunner $ mVarRun var) anobj

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

mapObject ::
       forall updateA updateB. EditLens updateA updateB -> Object (UpdateEdit updateA) -> Object (UpdateEdit updateB)
mapObject = lensObject False

lensAnObject ::
       forall ttl tto updateA updateB. (MonadTransStackUnliftAll ttl, MonadTransStackUnliftAll tto)
    => AnEditLens ttl updateA updateB
    -> AnObject tto (UpdateEdit updateA)
    -> AnObject (Concat ttl tto) (UpdateEdit updateB)
lensAnObject MkAnEditLens {..} (MkAnObject objReadA objEditA) =
    case transStackConcatRefl @ttl @tto @IO of
        Refl ->
            case transStackDict @MonadIO @tto @IO of
                Dict ->
                    case transStackDict @MonadIO @ttl @(ApplyStack tto IO) of
                        Dict -> let
                            MkAnUpdateFunction {..} = elFunction
                            objReadB :: MutableRead (ApplyStack (Concat ttl tto) IO) (UpdateReader updateB)
                            objReadB = ufGet objReadA
                            objEditB ::
                                   [UpdateEdit updateB]
                                -> ApplyStack (Concat ttl tto) IO (Maybe (EditSource -> ApplyStack (Concat ttl tto) IO ()))
                            objEditB editbs = do
                                meditas <- elPutEdits editbs objReadA
                                case meditas of
                                    Nothing -> return Nothing
                                    Just editas -> do
                                        mmu <- stackLift @ttl $ objEditA editas
                                        case mmu of
                                            Nothing -> return Nothing
                                            Just mu -> return $ Just $ \esrc -> stackLift @ttl $ mu esrc
                            in MkAnObject objReadB objEditB

lensObjectUnliftFull ::
       forall ttl tto. TransStackRunner ttl -> TransStackRunner tto -> TransStackRunner (Concat ttl tto)
lensObjectUnliftFull lensRun objRun = cmAppend lensRun objRun

lensObjectUnliftDiscard ::
       forall ttl tto. TransStackRunner ttl -> TransStackRunner tto -> TransStackRunner (Concat ttl tto)
lensObjectUnliftDiscard (MkTransStackRunner lensRun) (MkTransStackRunner objRun) = let
    lensObjRun ::
           forall m. MonadUnliftIO m
        => MFunction (ApplyStack (Concat ttl tto) m) m
    lensObjRun tmr =
        case transStackConcatRefl @ttl @tto @m of
            Refl ->
                case transStackDict @MonadUnliftIO @tto @m of
                    Dict ->
                        objRun $
                        unStackT @tto @m $ do
                            MkWUnliftAll du <- lift $ lensRun $ unStackT @ttl @m getDiscardingUnliftAll
                            MkStackT $ du $ MkStackT tmr -- discard lens effects: all these effects will be replayed by the update
    in case concatMonadTransStackUnliftAllDict @ttl @tto of
           Dict -> MkTransStackRunner lensObjRun

lensObjectUnlift ::
       forall ttl tto. Bool -> TransStackRunner ttl -> TransStackRunner tto -> TransStackRunner (Concat ttl tto)
lensObjectUnlift False = lensObjectUnliftFull
lensObjectUnlift True = lensObjectUnliftDiscard

lensObject ::
       forall updateA updateB.
       Bool
    -> EditLens updateA updateB
    -> Object (UpdateEdit updateA)
    -> Object (UpdateEdit updateB)
lensObject discard (MkRunnable2 (lensRun :: TransStackRunner ttl) alens) (MkRunnable1 (objRun :: TransStackRunner tto) aobj) =
    case transStackRunnerUnliftAllDict objRun of
        Dict ->
            case transStackRunnerUnliftAllDict lensRun of
                Dict -> let
                    objRunB :: TransStackRunner (Concat ttl tto)
                    objRunB = lensObjectUnlift discard lensRun objRun
                    in MkRunnable1 objRunB $ lensAnObject alens aobj

immutableAnObject ::
       forall tt edit. MonadTransStackUnliftAll tt
    => MutableRead (ApplyStack tt IO) (EditReader edit)
    -> AnObject tt edit
immutableAnObject mr =
    case transStackDict @Monad @tt @IO of
        Dict ->
            MkAnObject mr $ 
        -- must allow empty edit list
            \case
                [] -> return $ Just $ \_ -> return ()
                _ -> return Nothing

readConstantObject :: MutableRead IO (EditReader edit) -> Object edit
readConstantObject mr = MkRunnable1 cmEmpty $ immutableAnObject mr

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
convertObject (MkRunnable1 (trun :: TransStackRunner tt) (MkAnObject mra pe)) =
    case transStackRunnerUnliftAllDict trun of
        Dict ->
            case transStackDict @MonadIO @tt @IO of
                Dict -> let
                    objRead :: MutableRead (ApplyStack tt IO) (EditReader editb)
                    objRead = mSubjectToMutableRead $ mutableReadToSubject mra
                    objEdit :: [editb] -> ApplyStack tt IO (Maybe (EditSource -> ApplyStack tt IO ()))
                    objEdit ebs = do
                        oldsubj <- mutableReadToSubject mra
                        newsubj <- mapSubjectEdits ebs oldsubj
                        eas <- getReplaceEditsFromSubject newsubj
                        pe eas
                    in MkRunnable1 trun MkAnObject {..}

-- | Combines all the edits made in each call to the object.
cacheWholeObject ::
       forall a. Eq a
    => Object (WholeEdit a)
    -> Object (WholeEdit a)
cacheWholeObject (MkRunnable1 (MkTransStackRunner run :: TransStackRunner tt) (MkAnObject rd push)) =
    case transStackDict @MonadIO @tt @IO of
        Dict -> let
            run' ::
                   forall m r. MonadUnliftIO m
                => StateT (a, Maybe EditSource) (ApplyStack tt m) r
                -> m r
            run' sma =
                case transStackDict @MonadUnliftIO @tt @m of
                    Dict -> let
                        sma' :: ApplyStack tt m r
                        sma' = do
                            oldval <- stackUnderliftIO @tt @m $ rd ReadWhole
                            (r, (newval, mesrc)) <- runStateT sma (oldval, Nothing)
                            stackUnderliftIO @tt @m $
                                case mesrc of
                                    Just esrc ->
                                        if oldval == newval
                                            then return ()
                                            else do
                                                maction <- push [MkWholeReaderEdit newval]
                                                case maction of
                                                    Just action -> action esrc
                                                    Nothing -> liftIO $ fail "disallowed cached edit"
                                    Nothing -> return ()
                            return r
                        in run sma'
            rd' :: MutableRead (StateT (a, Maybe EditSource) (ApplyStack tt IO)) (WholeReader a)
            rd' ReadWhole = do
                (a, _) <- get
                return a
            push' ::
                   [WholeEdit a]
                -> StateT (a, Maybe EditSource) (ApplyStack tt IO) (Maybe (EditSource -> StateT (a, Maybe EditSource) (ApplyStack tt IO) ()))
            push' = singleAlwaysEdit $ \(MkWholeReaderEdit a) esrc -> put (a, Just esrc)
            in MkRunnable1 (MkTransStackRunner run' :: TransStackRunner (StateT (a, Maybe EditSource) ': tt)) $
               MkAnObject rd' push'

copyObject ::
       forall edit. FullEdit edit
    => EditSource
    -> Object edit
    -> Object edit
    -> IO ()
copyObject esrc =
    joinRunnable1Maps_ $ \(MkAnObject readSrc _) (MkAnObject _ pushDest) (trun :: TransStackRunner tt) ->
        runMonoTransStackRunner trun $ \run ->
            case transStackDict @MonadFail @tt @IO of
                Dict ->
                    case transStackDict @MonadIO @tt @IO of
                        Dict ->
                            run $
                            replaceEdit @edit readSrc $ \edit ->
                                pushOrFail "failed to copy object" esrc $ pushDest [edit]

exclusiveObject :: forall edit. Object edit -> With IO (Object edit)
exclusiveObject (MkRunnable1 (trun :: TransStackRunner tt) anobj) call =
    runMonoTransStackRunner trun $ \run ->
        run $ unStackT $ liftWithUnliftAll $ \unlift -> call $ MkRunnable1 (unliftStackTransStackRunner unlift) anobj

getObjectSubject :: FullSubjectReader (EditReader edit) => Object edit -> IO (EditSubject edit)
getObjectSubject (MkRunnable1 (trun :: TransStackRunner tt) (MkAnObject rd _)) =
    runMonoTransStackRunner trun $ \run ->
        case transStackDict @MonadIO @tt @IO of
            Dict -> run $ mutableReadToSubject rd
