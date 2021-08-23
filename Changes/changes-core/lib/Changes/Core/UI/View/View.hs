module Changes.Core.UI.View.View
    ( ViewContext
    , ViewT
    , View
    , liftIOViewAsync
    , viewRunResource
    , viewRunResourceContext
    , viewLocalResourceContext
    , viewGetResourceContext
    , viewWithoutLock
    , viewWaitUpdates
    , wModelWaitUpdates
    , runView
    , viewExit
    ) where

import Changes.Core.Import
import Changes.Core.Model
import Changes.Core.Resource
import Changes.Core.UI.View.Context
import Changes.Debug.Reference

type ViewT = ReaderT ViewContext

type View = ViewT IO

liftIOViewAsync :: forall a. ((forall r. View r -> IO r) -> IO a) -> View a
liftIOViewAsync call =
    liftIOWithUnlift $ \unlift -> call $ \vr -> unlift $ viewLocalResourceContext emptyResourceContext vr

viewRunResource ::
       forall m f r. (MonadIO m)
    => Resource f
    -> (forall tt. (MonadTransStackUnliftAll tt, MonadUnliftIO (ApplyStack tt IO)) => f tt -> ApplyStack tt IO r)
    -> ViewT m r
viewRunResource resource call = do
    rc <- viewGetResourceContext
    liftIO $ runResource rc resource $ \ftt -> call ftt

viewRunResourceContext ::
       forall m f r. MonadUnliftIO m
    => Resource f
    -> (forall tt.
            (MonadTransStackUnliftAll tt, MonadUnliftIO (ApplyStack tt (ViewT m))) =>
                    StackUnliftAll tt -> f tt -> ViewT m r)
    -> ViewT m r
viewRunResourceContext resource call = do
    rc <- viewGetResourceContext
    runResourceContext rc resource $ \rc' unlift ftt -> viewLocalResourceContext rc' $ call unlift ftt

viewWithContext :: (ViewContext -> ViewContext) -> ViewT m a -> ViewT m a
viewWithContext f ma = withReaderT f ma

viewGetResourceContext :: Monad m => ViewT m ResourceContext
viewGetResourceContext = asks vcResourceContext

viewLocalResourceContext :: ResourceContext -> ViewT m a -> ViewT m a
viewLocalResourceContext rc = viewWithContext (\vc -> vc {vcResourceContext = rc})

viewWithoutLock :: MonadIO m => IO a -> ViewT m a
viewWithoutLock ioa = do
    withoutLock <- asks vcWithoutUILock
    liftIO $ withoutLock ioa

viewWaitUpdates :: Model update -> View ()
viewWaitUpdates model = viewWithoutLock $ taskWait $ modelUpdatesTask model

wModelWaitUpdates :: WModel update -> View ()
wModelWaitUpdates (MkWModel ref) = viewWaitUpdates ref

runView ::
       forall m a. MonadUnliftIO m
    => ViewContext
    -> ViewT m a
    -> m a
runView vc (ReaderT view) = liftIOWithUnlift $ \unlift -> vcWithUILock vc $ unlift $ view vc

-- | Stop the UI loop. This does not throw any kind of exception.
viewExit :: MonadIO m => ViewT m ()
viewExit = traceBracket "viewExit" $ do
    exit <- asks vcExit
    liftIO exit
