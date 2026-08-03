module Changes.Core.Model.Reference where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Model.EditContext
import Changes.Core.Read
import Changes.Core.Resource
import Changes.Core.Types.None
import Changes.Core.Types.Whole

data AReference edit (t :: Type) = MkAReference
    { refRead :: Readable (ReaderT t IO) (EditReader edit)
    , refEdit :: NonEmpty edit -> ReaderT t IO (Maybe (EditSource -> ReaderT t IO ()))
    , refCommitTask :: Task IO ()
    }

type Reference edit = Resource (AReference edit)

referenceCommitTask :: Reference edit -> Task IO ()
referenceCommitTask (MkResource _ anobj) = refCommitTask anobj

instance Contravariant (AReference edit) where
    contramap :: forall a b. (a -> b) -> AReference edit b -> AReference edit a
    contramap f (MkAReference r e ct) = let
        r' :: Readable (ReaderT a IO) _
        r' rd = withReaderT f $ r rd
        e' :: _ -> ReaderT a IO (Maybe (EditSource -> ReaderT a IO ()))
        e' edits = (fmap $ fmap $ fmap $ withReaderT f) $ withReaderT f $ e edits
        in MkAReference r' e' ct

instance Show (Reference edit) where
    show (MkResource _ _) = "reference"

noneReference :: Reference (ConstEdit (NoReader t))
noneReference = let
    refRead :: Readable (ReaderT () IO) (NoReader t)
    refRead = never
    refEdit :: NonEmpty (ConstEdit (NoReader t)) -> ReaderT () IO (Maybe (EditSource -> ReaderT () IO ()))
    refEdit = never
    refCommitTask = mempty
    in MkResource nilResourceRunner MkAReference{..}

mvarReference :: forall a. IOWitness (MVar a) -> MVar a -> (a -> Bool) -> Reference (WholeEdit a)
mvarReference iow var allowed = let
    refRead :: Readable (ReaderT (MVar a, ()) IO) (WholeReader a)
    refRead ReadWhole = runResourceStateT get
    refEdit ::
        NonEmpty (WholeEdit a) ->
        ReaderT
            (MVar a, ())
            IO
            (Maybe (EditSource -> ReaderT (MVar a, ()) IO ()))
    refEdit edits = do
        na <- runResourceStateT $ applyEdits (toList edits) (mSubjectToReadable get) ReadWhole
        return
            $ if allowed na
                then Just $ \_ -> runResourceStateT $ put na
                else Nothing
    refCommitTask = mempty
    in MkResource (mvarResourceRunner iow var) MkAReference{..}

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
    forall t updateA updateB.
    ChangeLens updateA updateB ->
    AReference (UpdateEdit updateA) t ->
    AReference (UpdateEdit updateB) t
mapAReference MkChangeLens{..} (MkAReference refReadA refEditA objCT) = let
    refReadB :: Readable (ReaderT t IO) (UpdateReader updateB)
    refReadB = clRead refReadA
    refEditB ::
        NonEmpty (UpdateEdit updateB) ->
        ReaderT t IO (Maybe (EditSource -> ReaderT t IO ()))
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
    forall t updateA updateB.
    FloatingChangeLens updateA updateB ->
    AReference (UpdateEdit updateA) t ->
    ReaderT t IO (AReference (UpdateEdit updateB) t)
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
    forall t reader.
    Readable (ReaderT t IO) reader ->
    AReference (ConstEdit reader) t
immutableAReference mr = MkAReference mr (\_ -> return Nothing) mempty

readConstantReference :: forall reader. Readable IO reader -> Reference (ConstEdit reader)
readConstantReference mr = MkResource nilResourceRunner $ immutableAReference $ \r -> liftIO $ mr r

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
convertReference (MkResource (trun :: ResourceRunner t) (MkAReference mra pe refCommitTask)) = let
    refRead :: Readable (ReaderT t IO) (EditReader editb)
    refRead = mSubjectToReadable $ readableToSubject mra
    refEdit ::
        NonEmpty editb ->
        ReaderT t IO (Maybe (EditSource -> ReaderT t IO ()))
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
