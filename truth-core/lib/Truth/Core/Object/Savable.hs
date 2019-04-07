module Truth.Core.Object.Savable
    ( SaveActions(..)
    , saveBufferObject
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.DeferActionT
import Truth.Core.Object.Object
import Truth.Core.Object.Subscriber
import Truth.Core.Read
import Truth.Core.Types.Whole

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
saveBufferObject (MkObject (unliftP :: UnliftIO mp) readP pushP) update = do
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
            in MkObject runC readC pushC
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
{-
    let
        subC = MkSubscriber objB $ \updateB -> do
            let
                initA :: Object (WholeEdit (EditSubject edit)) -> IO (editorB, Object edit, SaveActions)
                initA (MkObject (unliftA :: UnliftIO ma) readA pushA) =
                    runTransform unliftA $ do
                        firstBuf <- readA ReadWhole
                        mvarRun sbVar $ put $ MkSaveBuffer firstBuf False
                        let
                            runB :: UnliftIO (StateT (SaveBuffer (EditSubject edit)) (DeferActionT IO))
                            runB = composeUnliftTransform (mvarUnlift sbVar) $ composeUnliftTransform runDeferActionT id
                            readB ::
                                MutableRead (StateT (SaveBuffer (EditSubject edit)) (DeferActionT IO)) (EditReader edit)
                            readB = mSubjectToMutableRead $ fmap saveBuffer get
                        rec
                            let
                                pushB ::
                                    [edit]
                                    -> StateT (SaveBuffer (EditSubject edit)) (DeferActionT IO) (Maybe (StateT (SaveBuffer (EditSubject edit)) (DeferActionT IO) ()))
                                pushB edits =
                                    return $
                                    Just $ do
                                        newbuf <-
                                            mutableReadToSubject $
                                            applyEdits edits $
                                            mSubjectToMutableRead $ do
                                                MkSaveBuffer oldbuf _ <- get
                                                return oldbuf
                                        put (MkSaveBuffer newbuf True)
                                        lift $ deferActionT $ updateB edits
                                objB :: Object edit
                                objB = MkObject runB readB pushB
                        let
                            saveAction :: IO Bool
                            saveAction =
                                runTransform unliftA $ do
                                    MkSaveBuffer buf _ <- mvarRun sbVar get
                                    maction <- pushA [MkWholeEdit buf]
                                    case maction of
                                        Nothing -> return False
                                        Just action -> do
                                            action
                                            mvarRun sbVar $ put $ MkSaveBuffer buf False
                                            return True
                            revertAction :: IO Bool
                            revertAction = do
                                edits <-
                                    runTransform unliftA $ do
                                        buf <- readA ReadWhole
                                        mvarRun sbVar $ put $ MkSaveBuffer buf False
                                        getReplaceEditsFromSubject buf
                                liftIO $ updateB edB objB edits
                                return False
                            saveActions :: SaveActions
                            saveActions =
                                MkSaveActions $
                                runTransform unliftA $ do
                                    MkSaveBuffer _ changed <- mvarRun sbVar get
                                    return $
                                        if changed
                                            then Just (saveAction, revertAction)
                                            else Nothing
                            edA = (edB, objB, saveActions)
                        return edA
                updateA ::
                    (editorB, Object edit, SaveActions)
                    -> Object (WholeEdit (EditSubject edit))
                    -> [WholeEdit (EditSubject edit)]
                    -> IO ()
                updateA (edB, objB, _) _ edits = do
                    MkSaveBuffer oldbuffer changed <- mvarRun sbVar get
                    if changed
                        then return ()
                        else do
                            newbuffer <- mutableReadToSubject $ applyEdits edits $ subjectToMutableRead oldbuffer
                            newedits <- getReplaceEditsFromSubject newbuffer
                            updateB edB objB newedits
                            mvarRun sbVar $ put $ MkSaveBuffer newbuffer False
            (edA, _, actionA) <- subscribe subA initA updateA
            let
                (edB, objB, saveActions) = edA
                actionB = (actionA, saveActions)
            return (edB, objB, actionB)
    return (subC,saveActions)
-}
