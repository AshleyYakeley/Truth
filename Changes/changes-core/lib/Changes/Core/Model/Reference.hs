module Changes.Core.Model.Reference where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Model.EditContext
import Changes.Core.Read
import Changes.Core.Resource
import Changes.Core.Types.None
import Changes.Core.Types.Whole

data AReference edit (m :: Type -> Type) = MkAReference
    { refRead :: Readable m (EditReader edit)
    , refEdit :: NonEmpty edit -> m (Maybe (EditSource -> m ()))
    , refCommitTask :: Task IO ()
    }

type Reference edit = Resource (AReference edit)

referenceCommitTask :: Reference edit -> Task IO ()
referenceCommitTask (MkResource _ anobj) = refCommitTask anobj

instance MapResource (AReference edit) where
    mapResource ::
        forall m1 m2.
        (Monad m1, Monad m2) =>
        (m1 --> m2) ->
        AReference edit m1 ->
        AReference edit m2
    mapResource f (MkAReference r e ct) = let
        r' :: Readable m2 _
        r' rd = f $ r rd
        e' :: _ -> m2 (Maybe (EditSource -> m2 ()))
        e' edits = (fmap $ fmap $ fmap f) $ f $ e edits
        in MkAReference r' e' ct

instance Show (Reference edit) where
    show (MkResource _ _) = "reference"

noneReference :: Reference (ConstEdit (NoReader t))
noneReference = let
    refRead :: Readable IO (NoReader t)
    refRead = never
    refEdit :: NonEmpty (ConstEdit (NoReader t)) -> IO (Maybe (EditSource -> IO ()))
    refEdit = never
    refCommitTask = mempty
    in MkResource nilResourceRunner $ mapResource liftIO $ MkAReference{..}

mvarReference :: forall a. IOWitness (MVar a) -> MVar a -> (a -> Bool) -> Reference (WholeEdit a)
mvarReference iow var allowed = let
    refRead :: Readable (StateT a IO) (WholeReader a)
    refRead ReadWhole = get
    refEdit :: NonEmpty (WholeEdit a) -> StateT a IO (Maybe (EditSource -> StateT a IO ()))
    refEdit edits = do
        na <- applyEdits (toList edits) (mSubjectToReadable get) ReadWhole
        return
            $ if allowed na
                then Just $ \_ -> put na
                else Nothing
    refCommitTask = mempty
    anobj :: AReference (WholeEdit a) (StateT a IO)
    anobj = MkAReference{..}
    in MkResource (mvarResourceRunner iow var) $ mapResource runResourceStateT anobj

makeMemoryReference :: forall a. a -> (a -> Bool) -> IO (Reference (WholeEdit a))
makeMemoryReference firsta allowed = do
    iow <- newIOWitness
    var <- newMVar firsta
    return $ mvarReference iow var allowed

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
    success <- pushEdit esrc mmmu
    if success
        then return ()
        else fail s

mapAReference ::
    forall m updateA updateB.
    MonadIO m =>
    ChangeLens updateA updateB ->
    AReference (UpdateEdit updateA) m ->
    AReference (UpdateEdit updateB) m
mapAReference MkChangeLens{..} (MkAReference refReadA refEditA objCT) = let
    refReadB :: Readable m (UpdateReader updateB)
    refReadB = clRead refReadA
    refEditB :: NonEmpty (UpdateEdit updateB) -> m (Maybe (EditSource -> m ()))
    refEditB editbs = do
        meditas <- clPutEdits (toList editbs) refReadA
        case meditas of
            Nothing -> return Nothing
            Just [] -> return $ Just $ \_ -> return ()
            Just (ea : editas) -> do
                mmu <- refEditA $ ea :| editas
                case mmu of
                    Nothing -> return Nothing
                    Just mu -> return $ Just $ \esrc -> mu esrc
    in MkAReference refReadB refEditB objCT

mapReference ::
    forall updateA updateB.
    ChangeLens updateA updateB ->
    Reference (UpdateEdit updateA) ->
    Reference (UpdateEdit updateB)
mapReference plens (MkResource rr anobjA) =
    MkResource rr $ mapAReference plens anobjA

floatMapAReference ::
    forall m updateA updateB.
    MonadIO m =>
    FloatingChangeLens updateA updateB ->
    AReference (UpdateEdit updateA) m ->
    m (AReference (UpdateEdit updateB) m)
floatMapAReference (MkFloatingChangeLens finit rlens) anobj = do
    r <- runFloatInit finit $ refRead anobj
    return $ mapAReference (rlens r) anobj

floatMapReference ::
    forall updateA updateB.
    ResourceContext ->
    FloatingChangeLens updateA updateB ->
    Reference (UpdateEdit updateA) ->
    IO (Reference (UpdateEdit updateB))
floatMapReference rc lens (MkResource rr anobjA) = do
    anobjB <- runResourceRunner rc rr $ runReaderT $ floatMapAReference lens anobjA
    return $ MkResource rr anobjB

immutableAReference ::
    forall m reader.
    Monad m =>
    Readable m reader ->
    AReference (ConstEdit reader) m
immutableAReference mr = MkAReference mr (\_ -> return Nothing) mempty

readConstantReference :: forall reader. Readable IO reader -> Reference (ConstEdit reader)
readConstantReference mr = MkResource nilResourceRunner $ mapResource liftIO $ immutableAReference mr

constantReference ::
    forall reader.
    SubjectReader reader =>
    ReaderSubject reader ->
    Reference (ConstEdit reader)
constantReference subj = readConstantReference $ subjectToReadable subj

alwaysEdit :: Monad m => (NonEmpty edit -> EditSource -> m ()) -> NonEmpty edit -> m (Maybe (EditSource -> m ()))
alwaysEdit em edits = return $ Just $ em edits

singleAlwaysEdit :: Monad m => (edit -> EditSource -> m ()) -> NonEmpty edit -> m (Maybe (EditSource -> m ()))
singleAlwaysEdit em = alwaysEdit $ \edits esrc -> for_ edits $ \edit -> em edit esrc

testEditAction :: IO Bool -> (EditSource -> IO ()) -> IO (Maybe (EditSource -> IO ()))
testEditAction test action = do
    ok <- test
    return
        $ if ok
            then Just action
            else Nothing

singleEdit :: Monad m => (edit -> m (Maybe (EditSource -> m ()))) -> NonEmpty edit -> m (Maybe (EditSource -> m ()))
singleEdit call edits =
    unComposeInner $ do
        actions <- for edits $ \edit -> MkComposeInner $ call edit
        return $ \esrc -> for_ actions $ \action -> action esrc

convertReference ::
    forall edita editb.
    (EditSubject edita ~ EditSubject editb, FullEdit edita, SubjectMapEdit editb) =>
    Reference edita ->
    Reference editb
convertReference (MkResource (trun :: ResourceRunner tt) (MkAReference mra pe refCommitTask)) = let
    refRead :: Readable (ReaderT (ListProduct tt) IO) (EditReader editb)
    refRead = mSubjectToReadable $ readableToSubject mra
    refEdit ::
        NonEmpty editb ->
        ReaderT (ListProduct tt) IO (Maybe (EditSource -> ReaderT (ListProduct tt) IO ()))
    refEdit ebs = do
        oldsubj <- readableToSubject mra
        newsubj <- mapSubjectEdits (toList ebs) oldsubj
        eas <- getReplaceEditsFromSubject newsubj
        case nonEmpty eas of
            Nothing -> return $ Just $ \_ -> return ()
            Just eaa -> pe eaa
    in MkResource trun MkAReference{..}

copyReference ::
    forall edit.
    FullEdit edit =>
    ResourceContext ->
    EditSource ->
    Reference edit ->
    Reference edit ->
    IO (Task IO ())
copyReference rc esrc =
    joinResource_ $ \rr (MkAReference readSrc _ _) (MkAReference _ pushDest ctask) ->
        runLifecycle $ do
            liftIO
                $ runResourceRunner rc rr
                $ runReaderT
                $ replaceEdit @edit readSrc
                $ \edit -> pushOrFail "failed to copy reference" esrc $ pushDest $ pure edit
            return ctask

getReferenceSubject :: ResourceContext -> FullSubjectReader (EditReader edit) => Reference edit -> IO (EditSubject edit)
getReferenceSubject rc obj = runResource rc obj $ \(MkAReference rd _ _) -> readableToSubject rd
