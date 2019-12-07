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
import Truth.Core.Read
import Truth.Core.Resource
import Truth.Core.Types

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
saveBufferObject (MkResource rrP (MkAnObject readP pushP)) update =
    runResourceRunnerWith rrP $ \runP -> do
        firstVal <- liftIO $ runP $ readP ReadWhole
        sbVar <- liftIO $ newMVar $ MkSaveBuffer firstVal False
        iow <- liftIO $ newIOWitness
        deferRunner <- deferActionResourceRunner
        let rrC = combineIndependentResourceRunners (mvarResourceRunner iow sbVar) deferRunner
        Dict <- return $ resourceRunnerUnliftAllDict rrC
        let
            objC = let
                readC ::
                       MutableRead (StateT (SaveBuffer (UpdateSubject update)) (DeferActionT IO)) (UpdateReader update)
                readC = mSubjectToMutableRead $ fmap saveBuffer get
                pushC ::
                       NonEmpty (UpdateEdit update)
                    -> StateT (SaveBuffer (UpdateSubject update)) (DeferActionT IO) (Maybe (EditSource -> StateT (SaveBuffer (UpdateSubject update)) (DeferActionT IO) ()))
                pushC edits =
                    return $
                    Just $ \esrc -> do
                        newbuf <-
                            mutableReadToSubject $
                            applyEdits (toList edits) $
                            mSubjectToMutableRead $ do
                                MkSaveBuffer oldbuf _ <- get
                                return oldbuf
                        put (MkSaveBuffer newbuf True)
                        lift $ deferAction $ update (fmap editUpdate edits) $ editSourceContext esrc
                in MkResource rrC $ MkAnObject readC pushC
            saveAction :: EditSource -> IO Bool
            saveAction esrc =
                runP $ do
                    MkSaveBuffer buf _ <- mVarRun sbVar get
                    maction <- pushP $ pure $ MkWholeReaderEdit buf
                    case maction of
                        Nothing -> return False
                        Just action -> do
                            action esrc
                            mVarRun sbVar $ put $ MkSaveBuffer buf False
                            return True
            revertAction :: EditSource -> IO Bool
            revertAction esrc = do
                edits <-
                    runP $ do
                        buf <- readP ReadWhole
                        mVarRun sbVar $ put $ MkSaveBuffer buf False
                        getReplaceEditsFromSubject buf
                case nonEmpty edits of
                    Nothing -> return ()
                    Just edits' -> liftIO $ update (fmap editUpdate edits') $ editSourceContext esrc
                return False
            saveActions :: SaveActions
            saveActions =
                MkSaveActions $
                runP $ do
                    MkSaveBuffer _ changed <- mVarRun sbVar get
                    return $
                        if changed
                            then Just (saveAction, revertAction)
                            else Nothing
        return (objC, saveActions)
