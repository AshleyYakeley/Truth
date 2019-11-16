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
import Truth.Core.Read
import Truth.Core.Resource

type ObjectMaker update a = ([update] -> EditContext -> IO ()) -> LifeCycleIO (Object (UpdateEdit update), a)

reflectingObjectMaker ::
       forall update. IsUpdate update
    => Object (UpdateEdit update)
    -> ObjectMaker update ()
reflectingObjectMaker (MkRunnable1 (MkTransStackRunner run :: TransStackRunner tt) (MkAnObject r e)) recv =
    return $
    case transStackDict @MonadUnliftIO @tt @IO of
        Dict -> let
            run' ::
                   forall m. MonadUnliftIO m
                => MFunction (DeferActionT (ApplyStack tt m)) m
            run' =
                case transStackDict @MonadUnliftIO @tt @m of
                    Dict -> composeUnliftAllFunctionCommute runDeferActionT run
            r' :: MutableRead (DeferActionT (ApplyStack tt IO)) (UpdateReader update)
            r' = liftMutableRead r
            e' :: [UpdateEdit update]
               -> DeferActionT (ApplyStack tt IO) (Maybe (EditSource -> DeferActionT (ApplyStack tt IO) ()))
            e' edits = do
                maction <- lift $ e edits
                case maction of
                    Nothing -> return Nothing
                    Just action ->
                        return $
                        Just $ \esrc -> do
                            lift $ action esrc
                            deferAction $ recv (fmap editUpdate edits) $ editSourceContext esrc
            in (MkRunnable1 (MkTransStackRunner @(DeferActionT ': tt) run') $ MkAnObject r' e', ())

mapUpdates ::
       forall updateA updateB. EditLens updateA updateB -> Object (UpdateEdit updateA) -> [updateA] -> IO [updateB]
mapUpdates (MkRunnable2 (trunLens :: TransStackRunner ttl) (MkAnEditLens (MkAnUpdateFunction _ update) _)) (MkRunnable1 (trunObj :: TransStackRunner tto) (MkAnObject mr _)) eas =
    case transStackRunnerUnliftAllDict trunLens of
        Dict ->
            case transStackRunnerUnliftAllDict trunObj of
                Dict ->
                    case transStackConcatRefl @ttl @tto @IO of
                        Refl ->
                            case transStackDict @MonadUnliftIO @tto @IO of
                                Dict ->
                                    case transStackDict @MonadUnliftIO @ttl @(ApplyStack tto IO) of
                                        Dict ->
                                            runMonoTransStackRunner @IO (cmAppend trunLens trunObj) $ \run ->
                                                run $ fmap mconcat $ for eas $ \ea -> update ea mr

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
