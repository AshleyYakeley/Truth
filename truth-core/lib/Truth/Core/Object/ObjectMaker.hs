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
reflectingObjectMaker (MkRunnable1 (trun :: TransStackRunner tt) (MkAnObject r e)) recv =
    return $
    case transStackRunnerUnliftAllDict trun of
        Dict ->
            case transStackDict @MonadUnliftIO @tt @(DeferActionT IO) of
                Dict ->
                    case transStackConcatRefl @tt @'[ DeferActionT] @IO of
                        Refl -> let
                            trun' :: TransStackRunner (Concat tt '[ DeferActionT])
                            trun' = cmAppend trun (singleTransStackRunner runDeferActionT)
                            r' :: MutableRead (ApplyStack tt (DeferActionT IO)) (UpdateReader update)
                            r' rt = stackUnderliftIO @tt @(DeferActionT IO) $ r rt
                            e' :: [UpdateEdit update]
                               -> ApplyStack tt (DeferActionT IO) (Maybe (EditSource -> ApplyStack tt (DeferActionT IO) ()))
                            e' edits = do
                                maction <- stackUnderliftIO @tt @(DeferActionT IO) $ e edits
                                case maction of
                                    Nothing -> return Nothing
                                    Just action ->
                                        return $
                                        Just $ \esrc -> do
                                            stackUnderliftIO @tt @(DeferActionT IO) $ action esrc
                                            stackLift @tt $
                                                deferAction @IO $ recv (fmap editUpdate edits) $ editSourceContext esrc
                            in (MkRunnable1 trun' $ MkAnObject r' e', ())

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
