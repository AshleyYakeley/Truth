module Pinafore.Base.Action
    ( Action
    , unAction
    , actionLiftView
    , actionLiftViewKnow
    , actionLiftViewKnowWithUnlift
    , actionTunnelView
    , actionHoistView
    , actionLiftLifecycle
    , actionGetCreateViewUnlift
    , actionResourceContext
    , actionFlushModelUpdates
    , actionFlushModelCommits
    , actionModelGet
    , actionModelPush
    , actionUndoHandler
    , actionKnow
    , knowAction
    , actionOnClose
    , actionEarlyCloser
    , actionFloatMap
    , actionFloatMapReadOnly
    ) where

import Changes.Core
import Pinafore.Base.Know
import Shapes

data ActionContext = MkActionContext
    { acUndoHandler :: UndoHandler
    }

newtype Action a =
    MkAction (ReaderT ActionContext (ComposeInner Know View) a)
    deriving ( Functor
             , Applicative
             , Monad
             , Alternative
             , MonadPlus
             , MonadFix
             , MonadFail
             , MonadIO
             , MonadHoistIO
             , MonadTunnelIO
             , MonadException
             , RepresentationalRole
             )

unAction :: forall a. UndoHandler -> Action a -> View (Know a)
unAction acUndoHandler (MkAction action) = unComposeInner $ runReaderT action MkActionContext {..}

actionLiftView :: View --> Action
actionLiftView va = MkAction $ lift $ lift va

actionLiftViewKnow :: View (Know a) -> Action a
actionLiftViewKnow va = MkAction $ lift $ MkComposeInner va

actionLiftViewKnowWithUnlift :: ((forall r. Action r -> View (Know r)) -> View (Know a)) -> Action a
actionLiftViewKnowWithUnlift call =
    MkAction $ liftWithUnlift $ \unlift -> MkComposeInner $ call $ \(MkAction rma) -> unComposeInner $ unlift rma

actionTunnelView :: forall r. (forall f. Functor f => (forall a. Action a -> View (f a)) -> View (f r)) -> Action r
actionTunnelView call =
    MkAction $
    tunnel $ \unlift1 ->
        tunnel $ \unlift2 -> fmap unComposeInner $ call $ \(MkAction rx) -> fmap MkComposeInner $ unlift2 $ unlift1 rx

actionHoistView :: (View --> View) -> Action --> Action
actionHoistView vv (MkAction ma) = MkAction $ hoist (hoist vv) ma

actionGetCreateViewUnlift :: Action (WRaised Action (ComposeInner Know View))
actionGetCreateViewUnlift =
    MkAction $ do
        MkWUnlift unlift <- askUnlift
        return $ MkWRaised $ \(MkAction ra) -> unlift ra

actionResourceContext :: Action ResourceContext
actionResourceContext = actionLiftView viewGetResourceContext

actionFlushModelUpdates :: WModel update -> Action ()
actionFlushModelUpdates (MkWModel model) = liftIO $ taskWait $ modelUpdatesTask model

actionFlushModelCommits :: WModel update -> Action ()
actionFlushModelCommits (MkWModel model) = liftIO $ taskWait $ modelCommitTask model

actionModelGet :: WModel update -> ReadM (UpdateReader update) t -> Action t
actionModelGet model rm = do
    actionFlushModelUpdates model
    rc <- actionResourceContext
    liftIO $ wModelGet rc model rm

actionModelPush :: WModel update -> NonEmpty (UpdateEdit update) -> Action ()
actionModelPush model edits = do
    actionFlushModelUpdates model
    rc <- actionResourceContext
    ok <- liftIO $ wModelPush rc model edits
    if ok
        then return ()
        else empty

actionLiftLifecycle :: Lifecycle --> Action
actionLiftLifecycle la = actionLiftView $ viewLiftLifecycle la

actionUndoHandler :: Action UndoHandler
actionUndoHandler = do
    MkActionContext {..} <- MkAction ask
    return acUndoHandler

actionKnow :: forall a. Know a -> Action a
actionKnow (Known a) = pure a
actionKnow Unknown = empty

knowAction :: forall a. Action a -> Action (Know a)
knowAction (MkAction (ReaderT rka)) = MkAction $ ReaderT $ \r -> MkComposeInner $ fmap Known $ unComposeInner $ rka r

actionOnClose :: Action () -> Action ()
actionOnClose closer = do
    MkWRaised unlift <- actionGetCreateViewUnlift
    actionLiftView $
        viewOnClose $ do
            _ <- unComposeInner $ unlift closer
            return ()

actionEarlyCloser :: Action a -> Action (a, IO ())
actionEarlyCloser ra = do
    MkWRaised unlift <- actionGetCreateViewUnlift
    MkAction $
        lift $
        MkComposeInner $ do
            (ka, closer) <- viewGetCloser $ unComposeInner $ unlift ra
            return $ fmap (\a -> (a, closer)) ka

actionFloatMap ::
       forall f updateA updateB. FloatingEditApplicative f
    => FloatingChangeLens updateA updateB
    -> f updateA
    -> Action (f updateB)
actionFloatMap flens fa = do
    rc <- actionResourceContext
    actionLiftLifecycle $ eaFloatMap rc flens fa

actionFloatMapReadOnly ::
       forall f updateA updateB. FloatingEditApplicative f
    => FloatingChangeLens updateA (ReadOnlyUpdate updateB)
    -> f (ReadOnlyUpdate updateA)
    -> Action (f (ReadOnlyUpdate updateB))
actionFloatMapReadOnly flens = actionFloatMap $ liftReadOnlyFloatingChangeLens flens
