module Truth.Core.Object.Object where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.EditContext
import Truth.Core.Read
import Truth.Core.Resource
import Truth.Core.Types.None
import Truth.Core.Types.Whole

data AnObject edit (tt :: [TransKind]) = MkAnObject
    { objRead :: MutableRead (ApplyStack tt IO) (EditReader edit)
    , objEdit :: NonEmpty edit -> ApplyStack tt IO (Maybe (EditSource -> ApplyStack tt IO ()))
    }

type Object edit = Resource (AnObject edit)

instance MapResource (AnObject edit) where
    mapResource ::
           forall tt1 tt2. (MonadTransStackUnliftAll tt1, MonadTransStackUnliftAll tt2)
        => TransListFunction tt1 tt2
        -> AnObject edit tt1
        -> AnObject edit tt2
    mapResource MkTransListFunction {..} (MkAnObject r e) = let
        r' :: MutableRead (ApplyStack tt2 IO) _
        r' rd = tlfFunction (Proxy @IO) $ r rd
        e' :: _ -> ApplyStack tt2 IO (Maybe (EditSource -> ApplyStack tt2 IO ()))
        e' edits =
            case transStackDict @MonadIO @tt2 @IO of
                Dict -> (fmap $ fmap $ fmap $ tlfFunction (Proxy @IO)) $ tlfFunction (Proxy @IO) $ e edits
        in MkAnObject r' e'

instance Show (Object edit) where
    show (MkResource _ _) = "object"

noneObject :: Object (NoEdit (NoReader t))
noneObject = let
    objRead :: MutableRead IO (NoReader t)
    objRead = never
    objEdit :: NonEmpty (NoEdit (NoReader t)) -> IO (Maybe (EditSource -> IO ()))
    objEdit = never
    in MkResource nilResourceRunner $ MkAnObject {..}

mvarObject :: forall a. IOWitness (StateT a) -> MVar a -> (a -> Bool) -> Object (WholeEdit a)
mvarObject iow var allowed = let
    objRead :: MutableRead (StateT a IO) (WholeReader a)
    objRead ReadWhole = get
    objEdit :: NonEmpty (WholeEdit a) -> StateT a IO (Maybe (EditSource -> StateT a IO ()))
    objEdit edits = do
        na <- applyEdits (toList edits) (mSubjectToMutableRead get) ReadWhole
        return $
            if allowed na
                then Just $ \_ -> put na
                else Nothing
    anobj :: AnObject (WholeEdit a) '[ StateT a]
    anobj = MkAnObject {..}
    in MkResource (mvarResourceRunner iow var) anobj

freeIOObject :: forall a. a -> (a -> Bool) -> IO (Object (WholeEdit a))
freeIOObject firsta allowed = do
    iow <- newIOWitness
    var <- newMVar firsta
    return $ mvarObject iow var allowed

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
mapObject = lensObject

mapAnObject ::
       forall tt updateA updateB. MonadTransStackUnliftAll tt
    => EditLens updateA updateB
    -> AnObject (UpdateEdit updateA) tt
    -> AnObject (UpdateEdit updateB) tt
mapAnObject MkEditLens {..} (MkAnObject objReadA objEditA) =
    case transStackDict @MonadIO @tt @IO of
        Dict -> let
            MkUpdateFunction {..} = elFunction
            objReadB :: MutableRead (ApplyStack tt IO) (UpdateReader updateB)
            objReadB = ufGet objReadA
            objEditB :: NonEmpty (UpdateEdit updateB) -> ApplyStack tt IO (Maybe (EditSource -> ApplyStack tt IO ()))
            objEditB editbs = do
                meditas <- elPutEdits (toList editbs) objReadA
                case meditas of
                    Nothing -> return Nothing
                    Just [] -> return $ Just $ \_ -> return ()
                    Just (ea:editas) -> do
                        mmu <- objEditA $ ea :| editas
                        case mmu of
                            Nothing -> return Nothing
                            Just mu -> return $ Just $ \esrc -> mu esrc
            in MkAnObject objReadB objEditB

lensObject ::
       forall updateA updateB. EditLens updateA updateB -> Object (UpdateEdit updateA) -> Object (UpdateEdit updateB)
lensObject alens (MkResource rr aobj) =
    case resourceRunnerUnliftAllDict rr of
        Dict -> MkResource rr $ mapAnObject alens aobj

immutableAnObject ::
       forall tt edit. MonadTransStackUnliftAll tt
    => MutableRead (ApplyStack tt IO) (EditReader edit)
    -> AnObject edit tt
immutableAnObject mr =
    case transStackDict @Monad @tt @IO of
        Dict -> MkAnObject mr $ \_ -> return Nothing

readConstantObject :: MutableRead IO (EditReader edit) -> Object edit
readConstantObject mr = MkResource nilResourceRunner $ immutableAnObject mr

constantObject :: SubjectReader (EditReader edit) => EditSubject edit -> Object edit
constantObject subj = readConstantObject $ subjectToMutableRead subj

alwaysEdit :: Monad m => (NonEmpty edit -> EditSource -> m ()) -> NonEmpty edit -> m (Maybe (EditSource -> m ()))
alwaysEdit em edits = return $ Just $ em edits

singleAlwaysEdit :: Monad m => (edit -> EditSource -> m ()) -> NonEmpty edit -> m (Maybe (EditSource -> m ()))
singleAlwaysEdit em = alwaysEdit $ \edits esrc -> for_ edits $ \edit -> em edit esrc

testEditAction :: IO Bool -> (EditSource -> IO ()) -> IO (Maybe (EditSource -> IO ()))
testEditAction test action = do
    ok <- test
    return $
        if ok
            then Just action
            else Nothing

singleEdit :: Monad m => (edit -> m (Maybe (EditSource -> m ()))) -> NonEmpty edit -> m (Maybe (EditSource -> m ()))
singleEdit call edits =
    getComposeM $ do
        actions <- for edits $ \edit -> MkComposeM $ call edit
        return $ \esrc -> for_ actions $ \action -> action esrc

convertObject ::
       forall edita editb. (EditSubject edita ~ EditSubject editb, FullEdit edita, SubjectMapEdit editb)
    => Object edita
    -> Object editb
convertObject (MkResource (trun :: ResourceRunner tt) (MkAnObject mra pe)) =
    case resourceRunnerUnliftAllDict trun of
        Dict ->
            case transStackDict @MonadIO @tt @IO of
                Dict -> let
                    objRead :: MutableRead (ApplyStack tt IO) (EditReader editb)
                    objRead = mSubjectToMutableRead $ mutableReadToSubject mra
                    objEdit :: NonEmpty editb -> ApplyStack tt IO (Maybe (EditSource -> ApplyStack tt IO ()))
                    objEdit ebs = do
                        oldsubj <- mutableReadToSubject mra
                        newsubj <- mapSubjectEdits (toList ebs) oldsubj
                        eas <- getReplaceEditsFromSubject newsubj
                        case nonEmpty eas of
                            Nothing -> return $ Just $ \_ -> return ()
                            Just eaa -> pe eaa
                    in MkResource trun MkAnObject {..}

copyObject ::
       forall edit. FullEdit edit
    => EditSource
    -> Object edit
    -> Object edit
    -> IO ()
copyObject esrc =
    joinResource_ $ \rr (MkAnObject readSrc _) (MkAnObject _ pushDest) ->
        runResourceRunnerWith rr $ \run ->
            runLifeCycle $ do
                liftIO $
                    run $
                    replaceEdit @edit readSrc $ \edit -> pushOrFail "failed to copy object" esrc $ pushDest $ pure edit

getObjectSubject :: FullSubjectReader (EditReader edit) => Object edit -> IO (EditSubject edit)
getObjectSubject (MkResource rr (MkAnObject rd _)) =
    runResourceRunnerWith rr $ \run -> runLifeCycle $ liftIO $ run $ mutableReadToSubject rd
