module Truth.Core.Object.AsyncPush
    ( asyncPushObject
    , asyncPushWithinObject
    , protectObject
    ) where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Object.Object
import Truth.Core.Read

protectObject :: forall edit. Object edit -> LifeCycle (Object edit)
protectObject (MkObject (MkTransform run :: UnliftIO m) rd push) =
    MkLifeCycle $ do
        var <- newMVar ()
        let
            run' :: forall a. m a -> IO a
            run' ma = withMVar var $ \() -> run ma
        return (MkObject (MkTransform run') rd push, return ())

-- | pushes complete within an object run
asyncPushWithinObject :: forall edit. Object edit -> Object edit
asyncPushWithinObject (MkObject (MkTransform run :: UnliftIO m) rd push) = let
    run' :: UnliftIO (ReaderT (IO () -> IO ()) m)
    run' =
        MkTransform $ \rma ->
            run $ do
                (request, closer) <- liftIO $ runLifeCycle deferrer
                a <- runReaderT rma request
                liftIO closer
                return a
    push' :: [edit] -> ReaderT (IO () -> IO ()) m (Maybe (ReaderT (IO () -> IO ()) m ()))
    push' edits = do
        maction <- lift $ push edits
        request <- ask
        return $
            case maction of
                Nothing -> Nothing
                Just action -> Just $ lift $ liftIOWithUnlift $ \(MkTransform unlift) -> request $ unlift action
    rd' :: MutableRead (ReaderT (IO () -> IO ()) m) (EditReader edit)
    rd' = liftMutableRead rd
    in MkObject run' rd' push'

-- | pushes outside object runs, as separate runs
asyncPushObject :: forall edit. LifeCycle (Object edit) -> LifeCycle (Object edit)
asyncPushObject ocObject = let
    ff :: Object edit -> (IO () -> IO ()) -> Object edit
    ff (MkObject (run :: UnliftIO m) rd push) request = let
        push' :: [edit] -> m (Maybe (m ()))
        push' edits = do
            maction <- push edits
            return $
                case maction of
                    Nothing -> Nothing
                    Just action -> Just $ liftIO $ request $ runTransform run action
        in MkObject run rd push'
    in ff <$> ocObject <*> deferrer
