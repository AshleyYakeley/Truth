module Truth.Core.Object.Savable
    ( SaveActions(..)
    , saveBufferObject
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.DeferActionT
import Truth.Core.Object.EditContext
import Truth.Core.Object.Object
import Truth.Core.Object.Subscriber
import Truth.Core.Object.UnliftIO
import Truth.Core.Read
import Truth.Core.Types.Whole
import Truth.Debug

data SaveBuffer a = MkSaveBuffer
    { saveBuffer :: a
    , _saveBufferChanged :: Bool
    }

newtype SaveActions =
    MkSaveActions (IO (Maybe (EditSource -> IO Bool, EditSource -> IO Bool)))

saveBufferObject ::
       forall edit. FullEdit edit
    => Object (WholeEdit (EditSubject edit))
    -> UpdatingObject edit SaveActions
saveBufferObject (MkCloseUnliftIO (unliftP :: UnliftIO mp) (MkAnObject readP pushP)) update = traceThing "saveBufferObject" $ do
    firstVal <- liftIO $ runTransform unliftP $ readP ReadWhole
    sbVar <- liftIO $ newMVar $ MkSaveBuffer firstVal False
    let
        objC = let
            runC :: UnliftIO (StateT (SaveBuffer (EditSubject edit)) (DeferActionT IO))
            runC = composeUnliftTransform (mvarUnlift sbVar) $ composeUnliftTransform runDeferActionT id
            readC :: MutableRead (StateT (SaveBuffer (EditSubject edit)) (DeferActionT IO)) (EditReader edit)
            readC = mSubjectToMutableRead $ fmap saveBuffer get
            pushC ::
                   [edit]
                -> StateT (SaveBuffer (EditSubject edit)) (DeferActionT IO) (Maybe (EditSource -> StateT (SaveBuffer (EditSubject edit)) (DeferActionT IO) ()))
            pushC edits =
                return $
                Just $ \esrc -> do
                    newbuf <-
                        mutableReadToSubject $
                        applyEdits edits $
                        mSubjectToMutableRead $ do
                            MkSaveBuffer oldbuf _ <- get
                            return oldbuf
                    put (MkSaveBuffer newbuf True)
                    lift $ deferActionT $ update edits esrc
            in MkCloseUnliftIO runC $ MkAnObject readC pushC
        saveAction :: EditSource -> IO Bool
        saveAction esrc =
            runTransform unliftP $ do
                MkSaveBuffer buf _ <- mvarRun sbVar get
                maction <- pushP [MkWholeEdit buf]
                case maction of
                    Nothing -> return False
                    Just action -> do
                        action esrc
                        mvarRun sbVar $ put $ MkSaveBuffer buf False
                        return True
        revertAction :: EditSource -> IO Bool
        revertAction esrc = do
            edits <-
                runTransform unliftP $ do
                    buf <- readP ReadWhole
                    mvarRun sbVar $ put $ MkSaveBuffer buf False
                    getReplaceEditsFromSubject buf
            liftIO $ update edits esrc
            return False
        saveActions :: SaveActions
        saveActions =
            MkSaveActions $
            runTransform unliftP $ do
                MkSaveBuffer _ changed <- mvarRun sbVar get
                return $
                    if changed
                        then Just (saveAction, revertAction)
                        else Nothing
    return (objC, saveActions)
