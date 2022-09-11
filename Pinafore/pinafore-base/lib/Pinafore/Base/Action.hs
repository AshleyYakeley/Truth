module Pinafore.Base.Action
    ( PinaforeAction
    , unPinaforeAction
    , actionLiftView
    , actionLiftViewKnow
    , actionLiftViewKnowWithUnlift
    , actionTunnelView
    , actionHoistView
    , actionLiftLifecycle
    , pinaforeGetCreateViewUnlift
    , pinaforeResourceContext
    , pinaforeFlushModelUpdates
    , pinaforeFlushModelCommits
    , pinaforeRefGet
    , pinaforeRefPush
    , pinaforeUndoHandler
    , pinaforeActionKnow
    , knowPinaforeAction
    , actionOnClose
    , pinaforeEarlyCloser
    , pinaforeFloatMap
    , pinaforeFloatMapReadOnly
    ) where

import Changes.Core
import Pinafore.Base.Know
import Shapes

data ActionContext = MkActionContext
    { acUndoHandler :: UndoHandler
    }

newtype PinaforeAction a =
    MkPinaforeAction (ReaderT ActionContext (ComposeInner Know View) a)
    deriving ( Functor
             , Applicative
             , Monad
             , Alternative
             , MonadPlus
             , MonadFix
             , MonadFail
             , MonadIO
             , MonadException
             , RepresentationalRole
             )

unPinaforeAction :: forall a. UndoHandler -> PinaforeAction a -> View (Know a)
unPinaforeAction acUndoHandler (MkPinaforeAction action) = unComposeInner $ runReaderT action MkActionContext {..}

actionLiftView :: View --> PinaforeAction
actionLiftView va = MkPinaforeAction $ lift $ lift va

actionLiftViewKnow :: View (Know a) -> PinaforeAction a
actionLiftViewKnow va = MkPinaforeAction $ lift $ MkComposeInner va

actionLiftViewKnowWithUnlift :: ((forall r. PinaforeAction r -> View (Know r)) -> View (Know a)) -> PinaforeAction a
actionLiftViewKnowWithUnlift call =
    MkPinaforeAction $
    liftWithUnlift $ \unlift -> MkComposeInner $ call $ \(MkPinaforeAction rma) -> unComposeInner $ unlift rma

actionTunnelView ::
       forall r. (forall f. Functor f => (forall a. PinaforeAction a -> View (f a)) -> View (f r)) -> PinaforeAction r
actionTunnelView call =
    MkPinaforeAction $
    tunnel $ \unlift1 ->
        tunnel $ \unlift2 ->
            fmap unComposeInner $ call $ \(MkPinaforeAction rx) -> fmap MkComposeInner $ unlift2 $ unlift1 rx

actionHoistView :: (View --> View) -> PinaforeAction --> PinaforeAction
actionHoistView vv (MkPinaforeAction ma) = MkPinaforeAction $ hoist (hoist vv) ma

pinaforeGetCreateViewUnlift :: PinaforeAction (WRaised PinaforeAction (ComposeInner Know View))
pinaforeGetCreateViewUnlift =
    MkPinaforeAction $ do
        MkWUnlift unlift <- askUnlift
        return $ MkWRaised $ \(MkPinaforeAction ra) -> unlift ra

pinaforeResourceContext :: PinaforeAction ResourceContext
pinaforeResourceContext = actionLiftView viewGetResourceContext

pinaforeFlushModelUpdates :: WModel update -> PinaforeAction ()
pinaforeFlushModelUpdates (MkWModel model) = liftIO $ taskWait $ modelUpdatesTask model

pinaforeFlushModelCommits :: WModel update -> PinaforeAction ()
pinaforeFlushModelCommits (MkWModel model) = liftIO $ taskWait $ modelCommitTask model

pinaforeRefGet :: WModel update -> ReadM (UpdateReader update) t -> PinaforeAction t
pinaforeRefGet model rm = do
    pinaforeFlushModelUpdates model
    rc <- pinaforeResourceContext
    liftIO $ wModelGet rc model rm

pinaforeRefPush :: WModel update -> NonEmpty (UpdateEdit update) -> PinaforeAction ()
pinaforeRefPush model edits = do
    pinaforeFlushModelUpdates model
    rc <- pinaforeResourceContext
    ok <- liftIO $ wModelPush rc model edits
    if ok
        then return ()
        else empty

actionLiftLifecycle :: Lifecycle --> PinaforeAction
actionLiftLifecycle la = actionLiftView $ viewLiftLifecycle la

pinaforeUndoHandler :: PinaforeAction UndoHandler
pinaforeUndoHandler = do
    MkActionContext {..} <- MkPinaforeAction ask
    return acUndoHandler

pinaforeActionKnow :: forall a. Know a -> PinaforeAction a
pinaforeActionKnow (Known a) = pure a
pinaforeActionKnow Unknown = empty

knowPinaforeAction :: forall a. PinaforeAction a -> PinaforeAction (Know a)
knowPinaforeAction (MkPinaforeAction (ReaderT rka)) =
    MkPinaforeAction $ ReaderT $ \r -> MkComposeInner $ fmap Known $ unComposeInner $ rka r

actionOnClose :: PinaforeAction () -> PinaforeAction ()
actionOnClose closer = do
    MkWRaised unlift <- pinaforeGetCreateViewUnlift
    actionLiftView $
        viewOnClose $ do
            _ <- unComposeInner $ unlift closer
            return ()

pinaforeEarlyCloser :: PinaforeAction a -> PinaforeAction (a, IO ())
pinaforeEarlyCloser ra = do
    MkWRaised unlift <- pinaforeGetCreateViewUnlift
    MkPinaforeAction $
        lift $
        MkComposeInner $ do
            (ka, closer) <- viewGetCloser $ unComposeInner $ unlift ra
            return $ fmap (\a -> (a, closer)) ka

pinaforeFloatMap ::
       forall f updateA updateB. FloatingEditApplicative f
    => FloatingChangeLens updateA updateB
    -> f updateA
    -> PinaforeAction (f updateB)
pinaforeFloatMap flens fa = do
    rc <- pinaforeResourceContext
    actionLiftLifecycle $ eaFloatMap rc flens fa

pinaforeFloatMapReadOnly ::
       forall f updateA updateB. FloatingEditApplicative f
    => FloatingChangeLens updateA (ReadOnlyUpdate updateB)
    -> f (ReadOnlyUpdate updateA)
    -> PinaforeAction (f (ReadOnlyUpdate updateB))
pinaforeFloatMapReadOnly flens = pinaforeFloatMap $ liftReadOnlyFloatingChangeLens flens
