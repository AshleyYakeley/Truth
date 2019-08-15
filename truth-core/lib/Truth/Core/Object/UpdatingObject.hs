module Truth.Core.Object.UpdatingObject
    ( UpdatingObject
    , updatingObject
    , lensUpdatingObject
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.DeferActionT
import Truth.Core.Object.EditContext
import Truth.Core.Object.Object
import Truth.Core.Object.UnliftIO
import Truth.Core.Read

type UpdatingObject edit a = ([edit] -> EditSource -> IO ()) -> LifeCycleIO (Object edit, a)

updatingObject :: forall edit. Object edit -> UpdatingObject edit ()
updatingObject (MkCloseUnliftIO (run :: UnliftIO m) (MkAnObject r e)) update =
    return $ let
        run' :: UnliftIO (DeferActionT m)
        run' = composeUnliftTransformCommute runDeferActionT run
        r' :: MutableRead (DeferActionT m) (EditReader edit)
        r' = liftMutableRead r
        e' :: [edit] -> DeferActionT m (Maybe (EditSource -> DeferActionT m ()))
        e' edits = do
            maction <- lift $ e edits
            case maction of
                Nothing -> return Nothing
                Just action ->
                    return $
                    Just $ \esrc -> do
                        lift $ action esrc
                        deferAction $ update edits esrc
        in (MkCloseUnliftIO run' $ MkAnObject r' e', ())

mapUpdates :: forall edita editb. EditLens edita editb -> Object edita -> [edita] -> IO [editb]
mapUpdates (MkCloseUnlift unlift (MkAnEditLens (MkAnEditFunction _ update) _)) (MkCloseUnliftIO unliftIO (MkAnObject mr _)) eas =
    runTransform unliftIO $
    runUnlift unlift $ withTransConstraintTM @MonadIO $ fmap mconcat $ for eas $ \ea -> update ea mr

lensUpdatingObject :: EditLens edita editb -> UpdatingObject edita a -> UpdatingObject editb a
lensUpdatingObject lens uobja recvb = do
    rec
        let
            recva [] _esrc = return ()
            recva eas esrc = do
                ebs <- mapUpdates lens obja eas
                recvb ebs esrc
        (obja, a) <- uobja recva
    let objb = lensObject True lens obja
    return (objb, a)
