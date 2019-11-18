module Truth.Core.Object.Object where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.EditContext
import Truth.Core.Read
import Truth.Core.Resource
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
    in MkRunnable1 (mVarTransStackRunner var) anobj

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
                    lensRun' =
                        if discard
                            then discardingTransStackRunner lensRun
                            else lensRun
                    objRunB :: TransStackRunner (Concat ttl tto)
                    objRunB = cmAppend lensRun' objRun
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

getObjectSubject :: FullSubjectReader (EditReader edit) => Object edit -> IO (EditSubject edit)
getObjectSubject (MkRunnable1 (trun :: TransStackRunner tt) (MkAnObject rd _)) =
    runMonoTransStackRunner trun $ \run ->
        case transStackDict @MonadIO @tt @IO of
            Dict -> run $ mutableReadToSubject rd
