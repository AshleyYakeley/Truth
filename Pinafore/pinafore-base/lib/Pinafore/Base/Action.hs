module Pinafore.Base.Action
    ( PinaforeAction
    , unPinaforeAction
    , createViewPinaforeAction
    , pinaforeGetCreateViewUnlift
    , viewPinaforeAction
    , pinaforeResourceContext
    , pinaforeFlushModelUpdates
    , pinaforeFlushModelCommits
    , pinaforeRefGet
    , pinaforeRefPush
    , pinaforeGetExitOnClose
    , pinaforeExit
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
import Changes.Debug.Reference

data ActionContext = MkActionContext
    { acChangesContext :: ChangesContext
    , acUndoHandler :: UndoHandler
    }

newtype PinaforeAction a =
    MkPinaforeAction (ReaderT ActionContext (ComposeInner Know CreateView) a)
    deriving ( Functor
             , Applicative
             , Monad
             , Alternative
             , MonadPlus
             , MonadFix
             , MonadFail
             , MonadIO
             , MonadLifeCycleIO
             , RepresentationalRole
             )

unPinaforeAction :: forall a. ChangesContext -> UndoHandler -> PinaforeAction a -> CreateView (Know a)
unPinaforeAction acChangesContext acUndoHandler (MkPinaforeAction action) =
    getComposeInner $ runReaderT action MkActionContext {..}

createViewPinaforeAction :: CreateView a -> PinaforeAction a
createViewPinaforeAction cva = MkPinaforeAction $ lift $ lift cva

pinaforeGetCreateViewUnlift :: PinaforeAction (WMFunction PinaforeAction (ComposeInner Know CreateView))
pinaforeGetCreateViewUnlift =
    MkPinaforeAction $ do
        MkWUnlift unlift <- askUnlift
        return $ MkWMFunction $ \(MkPinaforeAction ra) -> unlift ra

viewPinaforeAction :: View a -> PinaforeAction a
viewPinaforeAction va = createViewPinaforeAction $ lift va

pinaforeResourceContext :: PinaforeAction ResourceContext
pinaforeResourceContext = viewPinaforeAction viewGetResourceContext

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

pinaforeGetExitOnClose :: PinaforeAction (WMFunction CreateView CreateView)
pinaforeGetExitOnClose =
    MkPinaforeAction $ do
        tc <- asks acChangesContext
        return $ MkWMFunction $ ccExitOnClosed tc

pinaforeExit :: PinaforeAction ()
pinaforeExit = traceBracket "pinaforeExit" $ viewPinaforeAction viewExit

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
    createViewPinaforeAction $
        lifeCycleClose $
        runLifeCycleT $ do
            _ <- getComposeInner $ unlift closer
            return ()

pinaforeEarlyCloser :: PinaforeAction a -> PinaforeAction (a, IO ())
pinaforeEarlyCloser ra = do
    MkWMFunction unlift <- pinaforeGetCreateViewUnlift
    MkPinaforeAction $
        lift $
        MkComposeInner $ do
            (ka, closer) <- lifeCycleEarlyCloser $ getComposeInner $ unlift ra
            return $ fmap (\a -> (a, closer)) ka

pinaforeFloatMap ::
       forall f updateA updateB. FloatingEditApplicative f
    => FloatingChangeLens updateA updateB
    -> f updateA
    -> PinaforeAction (f updateB)
pinaforeFloatMap flens fa = do
    rc <- pinaforeResourceContext
    liftLifeCycle $ eaFloatMap rc flens fa

pinaforeFloatMapReadOnly ::
       forall f updateA updateB. FloatingEditApplicative f
    => FloatingChangeLens updateA (ReadOnlyUpdate updateB)
    -> f (ReadOnlyUpdate updateA)
    -> PinaforeAction (f (ReadOnlyUpdate updateB))
pinaforeFloatMapReadOnly flens = pinaforeFloatMap $ liftReadOnlyFloatingChangeLens flens
