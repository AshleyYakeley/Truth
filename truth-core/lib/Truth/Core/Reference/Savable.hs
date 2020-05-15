module Truth.Core.Reference.Savable
    ( SaveActions(..)
    , saveBufferReference
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Reference.DeferActionT
import Truth.Core.Reference.EditContext
import Truth.Core.Reference.Premodel
import Truth.Core.Reference.Reference
import Truth.Core.Resource
import Truth.Core.Types

data SaveBuffer a = MkSaveBuffer
    { saveBuffer :: a
    , _saveBufferChanged :: Bool
    }

newtype SaveActions =
    MkSaveActions (IO (Maybe (ResourceContext -> EditSource -> IO Bool, ResourceContext -> EditSource -> IO Bool)))

saveBufferReference ::
       forall update. (IsUpdate update, FullEdit (UpdateEdit update))
    => ResourceContext
    -> Reference (WholeEdit (UpdateSubject update))
    -> Premodel update SaveActions
saveBufferReference rc objP pmrUpdatesTask update = do
    firstVal <- liftIO $ runResource rc objP $ \anobj -> refRead anobj ReadWhole
    sbVar <- liftIO $ newMVar $ MkSaveBuffer firstVal False
    iow <- liftIO $ newIOWitness
    deferRunner <- deferActionResourceRunner
    let rrC = combineIndependentResourceRunners (mvarResourceRunner iow sbVar) deferRunner
    Dict <- return $ resourceRunnerUnliftAllDict rrC
    let
        pmrReference :: Reference (UpdateEdit update)
        pmrReference = let
            readC :: Readable (StateT (SaveBuffer (UpdateSubject update)) (DeferActionT IO)) (UpdateReader update)
            readC = mSubjectToReadable $ fmap saveBuffer get
            pushC ::
                   NonEmpty (UpdateEdit update)
                -> StateT (SaveBuffer (UpdateSubject update)) (DeferActionT IO) (Maybe (EditSource -> StateT (SaveBuffer (UpdateSubject update)) (DeferActionT IO) ()))
            pushC edits =
                return $
                Just $ \esrc -> do
                    newbuf <-
                        readableToSubject $
                        applyEdits (toList edits) $
                        mSubjectToReadable $ do
                            MkSaveBuffer oldbuf _ <- get
                            return oldbuf
                    put (MkSaveBuffer newbuf True)
                    lift $ deferAction $ update emptyResourceContext (fmap editUpdate edits) $ editSourceContext esrc
            in MkResource rrC $ MkAReference readC pushC mempty
        saveAction :: ResourceContext -> EditSource -> IO Bool
        saveAction urc esrc =
            runResource urc objP $ \anobj -> do
                MkSaveBuffer buf _ <- mVarRun sbVar get
                maction <- refEdit anobj $ pure $ MkWholeReaderEdit buf
                case maction of
                    Nothing -> return False
                    Just action -> do
                        action esrc
                        mVarRun sbVar $ put $ MkSaveBuffer buf False
                        return True
        revertAction :: ResourceContext -> EditSource -> IO Bool
        revertAction urc esrc = do
            edits <-
                runResource urc objP $ \anobj -> do
                    buf <- refRead anobj ReadWhole
                    mVarRun sbVar $ put $ MkSaveBuffer buf False
                    getReplaceEditsFromSubject buf
            case nonEmpty edits of
                Nothing -> return ()
                Just edits' -> liftIO $ update urc (fmap editUpdate edits') $ editSourceContext esrc
            return False
        pmrValue :: SaveActions
        pmrValue =
            MkSaveActions $ do
                MkSaveBuffer _ changed <- mVarRun sbVar get
                return $
                    if changed
                        then Just (saveAction, revertAction)
                        else Nothing
    return MkPremodelResult {..}