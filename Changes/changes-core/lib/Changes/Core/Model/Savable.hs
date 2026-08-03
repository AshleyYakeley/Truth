module Changes.Core.Model.Savable
    ( SaveActions (..)
    , saveBufferReference
    )
where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Model.DeferActionT
import Changes.Core.Model.EditContext
import Changes.Core.Model.Premodel
import Changes.Core.Model.Reference
import Changes.Core.Read
import Changes.Core.Resource
import Changes.Core.Types

data SaveBuffer a = MkSaveBuffer
    { saveBuffer :: a
    , _saveBufferChanged :: Bool
    }

newtype SaveActions
    = MkSaveActions (IO (Maybe (ResourceContext -> EditSource -> IO Bool, ResourceContext -> EditSource -> IO Bool)))

saveBufferReference ::
    forall update.
    (IsUpdate update, FullEdit (UpdateEdit update)) =>
    ResourceContext ->
    Reference (WholeEdit (UpdateSubject update)) ->
    Premodel update SaveActions
saveBufferReference rc objP pmrUpdatesTask update = do
    firstVal <- liftIO $ runResource rc objP $ \anobj -> refRead anobj ReadWhole
    sbVar <- liftIO $ newMVar $ MkSaveBuffer firstVal False
    iow <- liftIO $ newIOWitness
    deferRunner <- deferActionResourceRunner
    let
        saveAction :: ResourceContext -> EditSource -> IO Bool
        saveAction urc esrc =
            runResource urc objP $ \anobj -> do
                MkSaveBuffer buf _ <- mVarRunStateT sbVar get
                maction <- refEdit anobj $ pure $ MkWholeReaderEdit buf
                case maction of
                    Nothing -> return False
                    Just action -> do
                        action esrc
                        mVarRunStateT sbVar $ put $ MkSaveBuffer buf False
                        return True
        revertAction :: ResourceContext -> EditSource -> IO Bool
        revertAction urc esrc = do
            edits <-
                runResource urc objP $ \anobj -> do
                    buf <- refRead anobj ReadWhole
                    mVarRunStateT sbVar $ put $ MkSaveBuffer buf False
                    getReplaceEditsFromSubject buf
            case nonEmpty edits of
                Nothing -> return ()
                Just edits' -> liftIO $ update urc (fmap editUpdate edits') $ editSourceContext esrc
            return False
        pmrValue :: SaveActions
        pmrValue =
            MkSaveActions $ do
                MkSaveBuffer _ changed <- mVarRunStateT sbVar get
                return
                    $ if changed
                        then Just (saveAction, revertAction)
                        else Nothing
    return
        $ combineResourceRunners (mvarResourceRunner iow sbVar) deferRunner
        $ \(rrC :: ResourceRunner tt) liftState liftDefer -> let
            runS :: StateT (SaveBuffer (UpdateSubject update)) IO --> ReaderT (ListProduct tt) IO
            runS ma = do
                params <- ask
                liftIO $ mVarRunStateT (fst $ liftState params) ma
            readC :: Readable (ReaderT (ListProduct tt) IO) (UpdateReader update)
            readC rt = runS $ mSubjectToReadable (fmap saveBuffer get) rt
            pushC ::
                NonEmpty (UpdateEdit update) ->
                ReaderT (ListProduct tt) IO (Maybe (EditSource -> ReaderT (ListProduct tt) IO ()))
            pushC edits =
                return
                    $ Just
                    $ \esrc -> do
                        runS $ do
                            newbuf <-
                                readableToSubject
                                    $ applyEdits (toList edits)
                                    $ mSubjectToReadable
                                    $ do
                                        MkSaveBuffer oldbuf _ <- get
                                        return oldbuf
                            put $ MkSaveBuffer newbuf True
                        defer <- asks $ fst . liftDefer
                        liftIO
                            $ deferAction defer
                            $ update emptyResourceContext (fmap editUpdate edits)
                            $ editSourceContext esrc
            pmrReference = MkResource rrC $ MkAReference readC pushC mempty
            in MkPremodelResult{..}
