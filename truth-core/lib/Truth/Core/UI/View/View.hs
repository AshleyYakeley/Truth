module Truth.Core.UI.View.View
    ( ViewContext
    , ViewT
    , View
    , liftIOViewAsync
    , viewRunResource
    , viewRunResourceContext
    , viewRequest
    , viewLocalResourceContext
    , viewGetResourceContext
    , runView
    ) where

import Truth.Core.Import
import Truth.Core.Resource
import Truth.Core.UI.View.Context

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

viewRequest :: Monad m => IOWitness t -> ViewT m (Maybe t)
viewRequest wit = asks (\vc -> vcRequest vc wit)

viewWithContext :: (ViewContext -> ViewContext) -> ViewT m a -> ViewT m a
viewWithContext f ma = withReaderT f ma

viewGetResourceContext :: Monad m => ViewT m ResourceContext
viewGetResourceContext = asks vcResourceContext

viewLocalResourceContext :: ResourceContext -> ViewT m a -> ViewT m a
viewLocalResourceContext rc = viewWithContext (\vc -> vc {vcResourceContext = rc})

runView :: forall m w. ResourceContext -> (IO () -> IO ()) -> ViewT m w -> (forall t. IOWitness t -> Maybe t) -> m w
runView vcResourceContext vcWithUILock (ReaderT view) vcRequest = view MkViewContext {..}
