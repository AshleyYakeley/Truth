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
import Truth.Core.Object.Run
import Truth.Core.Read

type ObjectMaker update a = ([update] -> EditContext -> IO ()) -> LifeCycleIO (Object (UpdateEdit update), a)

reflectingObjectMaker ::
       forall update. IsUpdate update
    => Object (UpdateEdit update)
    -> ObjectMaker update ()
reflectingObjectMaker (MkRunnableIO (run :: IOFunction m) (MkAnObject r e)) recv =
    return $ let
        run' :: IOFunction (DeferActionT m)
        run' = composeUntransFunctionCommute runDeferActionT run
        r' :: MutableRead (DeferActionT m) (UpdateReader update)
        r' = liftMutableRead r
        e' :: [UpdateEdit update] -> DeferActionT m (Maybe (EditSource -> DeferActionT m ()))
        e' edits = do
            maction <- lift $ e edits
            case maction of
                Nothing -> return Nothing
                Just action ->
                    return $
                    Just $ \esrc -> do
                        lift $ action esrc
                        deferAction $ recv (fmap editUpdate edits) $ editSourceContext esrc
        in (MkRunnableIO run' $ MkAnObject r' e', ())

mapUpdates ::
       forall updateA updateB. EditLens updateA updateB -> Object (UpdateEdit updateA) -> [updateA] -> IO [updateB]
mapUpdates (MkRunnableT2 unlift (MkAnEditLens (MkAnUpdateFunction _ update) _)) (MkRunnableIO unliftIO (MkAnObject mr _)) eas =
    unliftIO $ unlift $ withTransConstraintTM @MonadIO $ fmap mconcat $ for eas $ \ea -> update ea mr

mapObjectMaker :: EditLens updateA updateB -> ObjectMaker updateA a -> ObjectMaker updateB a
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
