module Truth.Core.Object.UpdatingObject where

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
                        deferActionT $ update edits esrc
        in (MkCloseUnliftIO run' $ MkAnObject r' e', ())
