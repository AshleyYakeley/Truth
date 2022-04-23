module Pinafore.Base.Action
    ( PinaforeAction
    , unPinaforeAction
    , actionLiftView
    , actionHoistView
    , actionLiftLifeCycle
    , pinaforeGetCreateViewUnlift
    , pinaforeResourceContext
    , pinaforeFlushModelUpdates
    , pinaforeFlushModelCommits
    , pinaforeRefGet
    , pinaforeRefPush
    , pinaforeGetExitOnClose
    , actionExitUI
    , pinaforeUndoHandler
    , pinaforeActionKnow
    , knowPinaforeAction
    , pinaforeOnClose
    , pinaforeEarlyCloser
    , pinaforeFloatMap
    , pinaforeFloatMapReadOnly
    ) where

import Changes.Core
import Pinafore.Base.Know
import Shapes

data ActionContext = MkActionContext
    { acExitOnClosed :: WMFunction View View
    , acUndoHandler :: UndoHandler
    }

newtype PinaforeAction a =
    MkPinaforeAction (ReaderT ActionContext (ComposeInner Know View) a)
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadFix, MonadFail, MonadIO, RepresentationalRole)

unPinaforeAction :: forall a. ChangesContext -> UndoHandler -> PinaforeAction a -> View (Know a)
unPinaforeAction MkChangesContext {..} acUndoHandler (MkPinaforeAction action) = let
    acExitOnClosed = MkWMFunction ccExitOnClosed
    in getComposeInner $ runReaderT action MkActionContext {..}

actionLiftView :: View --> PinaforeAction
actionLiftView cva = MkPinaforeAction $ lift $ lift cva

actionHoistView :: (View --> View) -> PinaforeAction --> PinaforeAction
actionHoistView vv (MkPinaforeAction ma) = MkPinaforeAction $ hoist (hoist vv) ma

pinaforeGetCreateViewUnlift :: PinaforeAction (WMFunction PinaforeAction (ComposeInner Know View))
pinaforeGetCreateViewUnlift =
    MkPinaforeAction $ do
        MkWUnlift unlift <- askUnlift
        return $ MkWMFunction $ \(MkPinaforeAction ra) -> unlift ra

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

pinaforeGetExitOnClose :: PinaforeAction (WMFunction View View)
pinaforeGetExitOnClose = MkPinaforeAction $ asks acExitOnClosed

actionLiftLifeCycle :: LifeCycle --> PinaforeAction
actionLiftLifeCycle la = actionLiftView $ viewLiftLifeCycle la

actionExitUI :: PinaforeAction ()
actionExitUI = actionLiftView viewExitUI

pinaforeUndoHandler :: PinaforeAction UndoHandler
pinaforeUndoHandler = do
    MkActionContext {..} <- MkPinaforeAction ask
    return acUndoHandler

pinaforeActionKnow :: forall a. Know a -> PinaforeAction a
pinaforeActionKnow (Known a) = pure a
pinaforeActionKnow Unknown = empty

knowPinaforeAction :: forall a. PinaforeAction a -> PinaforeAction (Know a)
knowPinaforeAction (MkPinaforeAction (ReaderT rka)) =
    MkPinaforeAction $ ReaderT $ \r -> MkComposeInner $ fmap Known $ getComposeInner $ rka r

pinaforeOnClose :: PinaforeAction () -> PinaforeAction ()
pinaforeOnClose closer = do
    MkWMFunction unlift <- pinaforeGetCreateViewUnlift
    actionLiftView $
        viewCloser $ do
            _ <- getComposeInner $ unlift closer
            return ()

pinaforeEarlyCloser :: PinaforeAction a -> PinaforeAction (a, IO ())
pinaforeEarlyCloser ra = do
    MkWMFunction unlift <- pinaforeGetCreateViewUnlift
    MkPinaforeAction $
        lift $
        MkComposeInner $ do
            (ka, closer) <- viewGetCloser $ getComposeInner $ unlift ra
            return $ fmap (\a -> (a, closer)) ka

pinaforeFloatMap ::
       forall f updateA updateB. FloatingEditApplicative f
    => FloatingChangeLens updateA updateB
    -> f updateA
    -> PinaforeAction (f updateB)
pinaforeFloatMap flens fa = do
    rc <- pinaforeResourceContext
    actionLiftLifeCycle $ eaFloatMap rc flens fa

pinaforeFloatMapReadOnly ::
       forall f updateA updateB. FloatingEditApplicative f
    => FloatingChangeLens updateA (ReadOnlyUpdate updateB)
    -> f (ReadOnlyUpdate updateA)
    -> PinaforeAction (f (ReadOnlyUpdate updateB))
pinaforeFloatMapReadOnly flens = pinaforeFloatMap $ liftReadOnlyFloatingChangeLens flens
