module Truth.Core.Object.Savable
    ( SaveActions(..)
    , saveBufferObject
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.DeferActionT
import Truth.Core.Object.EditContext
import Truth.Core.Object.Object
import Truth.Core.Object.ObjectMaker
import Truth.Core.Object.Run
import Truth.Core.Read
import Truth.Core.Types.Whole

data SaveBuffer a = MkSaveBuffer
    { saveBuffer :: a
    , _saveBufferChanged :: Bool
    }

newtype SaveActions =
    MkSaveActions (IO (Maybe (EditSource -> IO Bool, EditSource -> IO Bool)))

saveBufferObject ::
       forall update. (IsUpdate update, FullEdit (UpdateEdit update))
    => Object (WholeEdit (UpdateSubject update))
    -> ObjectMaker update SaveActions
saveBufferObject (MkRunnableIO (unliftP :: WIOFunction mp) (MkAnObject readP pushP)) update = do
    firstVal <- liftIO $ runWMFunction unliftP $ readP ReadWhole
    sbVar <- liftIO $ newMVar $ MkSaveBuffer firstVal False
    let
        objC = let
            runC :: WIOFunction (StateT (SaveBuffer (UpdateSubject update)) (DeferActionT IO))
            runC = composeUntransFunction (wMVarRun sbVar) $ composeUntransFunction runDeferActionT id
            readC :: MutableRead (StateT (SaveBuffer (UpdateSubject update)) (DeferActionT IO)) (UpdateReader update)
            readC = mSubjectToMutableRead $ fmap saveBuffer get
            pushC ::
                   [UpdateEdit update]
                -> StateT (SaveBuffer (UpdateSubject update)) (DeferActionT IO) (Maybe (EditSource -> StateT (SaveBuffer (UpdateSubject update)) (DeferActionT IO) ()))
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
                    lift $ deferAction $ update (fmap editUpdate edits) $ editSourceContext esrc
            in MkRunnableIO runC $ MkAnObject readC pushC
        saveAction :: EditSource -> IO Bool
        saveAction esrc =
            runWMFunction unliftP $ do
                MkSaveBuffer buf _ <- mVarRun sbVar get
                maction <- pushP [MkWholeReaderEdit buf]
                case maction of
                    Nothing -> return False
                    Just action -> do
                        action esrc
                        mVarRun sbVar $ put $ MkSaveBuffer buf False
                        return True
        revertAction :: EditSource -> IO Bool
        revertAction esrc = do
            edits <-
                runWMFunction unliftP $ do
                    buf <- readP ReadWhole
                    mVarRun sbVar $ put $ MkSaveBuffer buf False
                    getReplaceEditsFromSubject buf
            liftIO $ update (fmap editUpdate edits) $ editSourceContext esrc
            return False
        saveActions :: SaveActions
        saveActions =
            MkSaveActions $
            runWMFunction unliftP $ do
                MkSaveBuffer _ changed <- mVarRun sbVar get
                return $
                    if changed
                        then Just (saveAction, revertAction)
                        else Nothing
    return (objC, saveActions)
