module Changes.Core.Model.Reference where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Lens
import Changes.Core.Model.EditContext
import Changes.Core.Read
import Changes.Core.Resource
import Changes.Core.Types.None
import Changes.Core.Types.Whole

data AReference edit (tt :: [TransKind]) = MkAReference
    { refRead :: Readable (ApplyStack tt IO) (EditReader edit)
    , refEdit :: NonEmpty edit -> ApplyStack tt IO (Maybe (EditSource -> ApplyStack tt IO ()))
    , refCommitTask :: Task ()
    }

type Reference edit = Resource (AReference edit)

referenceCommitTask :: Reference edit -> Task ()
referenceCommitTask (MkResource _ anobj) = refCommitTask anobj

instance MapResource (AReference edit) where
    mapResource ::
           forall tt1 tt2. (MonadTransStackUnliftAll tt1, MonadTransStackUnliftAll tt2)
        => TransListFunction tt1 tt2
        -> AReference edit tt1
        -> AReference edit tt2
    mapResource MkTransListFunction {..} (MkAReference r e ct) = let
        r' :: Readable (ApplyStack tt2 IO) _
        r' rd = tlfFunction (Proxy @IO) $ r rd
        e' :: _ -> ApplyStack tt2 IO (Maybe (EditSource -> ApplyStack tt2 IO ()))
        e' edits =
            case transStackDict @MonadIO @tt2 @IO of
                Dict -> (fmap $ fmap $ fmap $ tlfFunction (Proxy @IO)) $ tlfFunction (Proxy @IO) $ e edits
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
    in MkResource nilResourceRunner $ MkAReference {..}

mvarReference :: forall a. IOWitness (StateT a) -> MVar a -> (a -> Bool) -> Reference (WholeEdit a)
mvarReference iow var allowed = let
    refRead :: Readable (StateT a IO) (WholeReader a)
    refRead ReadWhole = get
    refEdit :: NonEmpty (WholeEdit a) -> StateT a IO (Maybe (EditSource -> StateT a IO ()))
    refEdit edits = do
        na <- applyEdits (toList edits) (mSubjectToReadable get) ReadWhole
        return $
            if allowed na
                then Just $ \_ -> put na
                else Nothing
    refCommitTask = mempty
    anobj :: AReference (WholeEdit a) '[ StateT a]
    anobj = MkAReference {..}
    in MkResource (mvarResourceRunner iow var) anobj

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
       forall tt updateA updateB. MonadTransStackUnliftAll tt
    => ChangeLens updateA updateB
    -> AReference (UpdateEdit updateA) tt
    -> AReference (UpdateEdit updateB) tt
mapAReference MkChangeLens {..} (MkAReference refReadA refEditA objCT) =
    case transStackDict @MonadIO @tt @IO of
        Dict -> let
            refReadB :: Readable (ApplyStack tt IO) (UpdateReader updateB)
            refReadB = clRead refReadA
            refEditB :: NonEmpty (UpdateEdit updateB) -> ApplyStack tt IO (Maybe (EditSource -> ApplyStack tt IO ()))
            refEditB editbs = do
                meditas <- clPutEdits (toList editbs) refReadA
                case meditas of
                    Nothing -> return Nothing
                    Just [] -> return $ Just $ \_ -> return ()
                    Just (ea:editas) -> do
                        mmu <- refEditA $ ea :| editas
                        case mmu of
                            Nothing -> return Nothing
                            Just mu -> return $ Just $ \esrc -> mu esrc
            in MkAReference refReadB refEditB objCT

mapReference ::
       forall updateA updateB.
       ChangeLens updateA updateB
    -> Reference (UpdateEdit updateA)
    -> Reference (UpdateEdit updateB)
mapReference plens (MkResource rr anobjA) =
    case resourceRunnerUnliftAllDict rr of
        Dict -> MkResource rr $ mapAReference plens anobjA

floatMapAReference ::
       forall tt updateA updateB. MonadTransStackUnliftAll tt
    => FloatingChangeLens updateA updateB
    -> AReference (UpdateEdit updateA) tt
    -> ApplyStack tt IO (AReference (UpdateEdit updateB) tt)
floatMapAReference (MkFloatingChangeLens init rlens) anobj =
    case transStackDict @MonadIO @tt @IO of
        Dict -> do
            r <- runFloatInit init $ refRead anobj
            return $ mapAReference (rlens r) anobj

floatMapReference ::
       forall updateA updateB.
       ResourceContext
    -> FloatingChangeLens updateA updateB
    -> Reference (UpdateEdit updateA)
    -> IO (Reference (UpdateEdit updateB))
floatMapReference rc lens (MkResource rr anobjA) = do
    anobjB <- runResourceRunner rc rr $ floatMapAReference lens anobjA
    return $ MkResource rr anobjB

immutableAReference ::
       forall tt reader. MonadTransStackUnliftAll tt
    => Readable (ApplyStack tt IO) reader
    -> AReference (ConstEdit reader) tt
immutableAReference mr =
    case transStackDict @Monad @tt @IO of
        Dict -> MkAReference mr (\_ -> return Nothing) mempty

readConstantReference :: Readable IO reader -> Reference (ConstEdit reader)
readConstantReference mr = MkResource nilResourceRunner $ immutableAReference mr

constantReference :: SubjectReader reader => ReaderSubject reader -> Reference (ConstEdit reader)
constantReference subj = readConstantReference $ subjectToReadable subj

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

convertReference ::
       forall edita editb. (EditSubject edita ~ EditSubject editb, FullEdit edita, SubjectMapEdit editb)
    => Reference edita
    -> Reference editb
convertReference (MkResource (trun :: ResourceRunner tt) (MkAReference mra pe refCommitTask)) =
    case resourceRunnerUnliftAllDict trun of
        Dict ->
            case transStackDict @MonadIO @tt @IO of
                Dict -> let
                    refRead :: Readable (ApplyStack tt IO) (EditReader editb)
                    refRead = mSubjectToReadable $ readableToSubject mra
                    refEdit :: NonEmpty editb -> ApplyStack tt IO (Maybe (EditSource -> ApplyStack tt IO ()))
                    refEdit ebs = do
                        oldsubj <- readableToSubject mra
                        newsubj <- mapSubjectEdits (toList ebs) oldsubj
                        eas <- getReplaceEditsFromSubject newsubj
                        case nonEmpty eas of
                            Nothing -> return $ Just $ \_ -> return ()
                            Just eaa -> pe eaa
                    in MkResource trun MkAReference {..}

copyReference ::
       forall edit. FullEdit edit
    => ResourceContext
    -> EditSource
    -> Reference edit
    -> Reference edit
    -> IO (Task ())
copyReference rc esrc =
    joinResource_ $ \rr (MkAReference readSrc _ _) (MkAReference _ pushDest ctask) ->
        runLifeCycle $ do
            liftIO $
                runResourceRunner rc rr $
                replaceEdit @edit readSrc $ \edit -> pushOrFail "failed to copy reference" esrc $ pushDest $ pure edit
            return ctask

getReferenceSubject :: ResourceContext -> FullSubjectReader (EditReader edit) => Reference edit -> IO (EditSubject edit)
getReferenceSubject rc obj = runResource rc obj $ \(MkAReference rd _ _) -> readableToSubject rd
