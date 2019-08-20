module Truth.Core.Object.ObjectMaker
    ( ObjectMaker
    , reflectingObjectMaker
    , mapObjectMaker
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.DeferActionT
import Truth.Core.Object.EditContext
import Truth.Core.Object.Object
import Truth.Core.Object.UnliftIO
import Truth.Core.Read

type ObjectMaker edit a = ([edit] -> EditContext -> IO ()) -> LifeCycleIO (Object edit, a)

reflectingObjectMaker :: forall edit. Object edit -> ObjectMaker edit ()
reflectingObjectMaker (MkCloseUnliftIO (run :: UnliftIO m) (MkAnObject r e)) update =
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
                        deferAction $ update edits $ editSourceContext esrc
        in (MkCloseUnliftIO run' $ MkAnObject r' e', ())

mapUpdates :: forall edita editb. EditLens edita editb -> Object edita -> [edita] -> IO [editb]
mapUpdates (MkCloseUnlift unlift (MkAnEditLens (MkAnEditFunction _ update) _)) (MkCloseUnliftIO unliftIO (MkAnObject mr _)) eas =
    runTransform unliftIO $
    runUnlift unlift $ withTransConstraintTM @MonadIO $ fmap mconcat $ for eas $ \ea -> update ea mr

mapObjectMaker :: EditLens edita editb -> ObjectMaker edita a -> ObjectMaker editb a
mapObjectMaker lens uobja recvb = do
    rec
        let
            recva [] _esrc = return ()
            recva eas esrc = do
                ebs <- mapUpdates lens obja eas
                recvb ebs esrc
        (obja, a) <- uobja recva
    let objb = lensObject True lens obja
    return (objb, a)