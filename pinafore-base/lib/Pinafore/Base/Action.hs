module Pinafore.Base.Action
    ( PinaforeAction
    , unPinaforeAction
    , createViewPinaforeAction
    , pinaforeGetCreateViewUnlift
    , viewPinaforeAction
    , pinaforeResourceContext
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

data ActionContext = MkActionContext
    { acChangesContext :: ChangesContext
    , acUndoHandler :: UndoHandler
    }

newtype PinaforeAction a =
    MkPinaforeAction (ReaderT ActionContext (ComposeM Know CreateView) a)
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadFix, MonadIO, RepresentationalRole)

instance MonadFail PinaforeAction where
    fail s = liftIO $ fail s

instance MonadLifeCycleIO PinaforeAction where
    liftLifeCycle la = MkPinaforeAction $ lift $ lift $ lift la
    subLifeCycle (MkPinaforeAction ra) = MkPinaforeAction $ (remonad $ remonad $ remonad $ subLifeCycle) ra

unPinaforeAction :: forall a. ChangesContext -> UndoHandler -> PinaforeAction a -> CreateView (Know a)
unPinaforeAction acChangesContext acUndoHandler (MkPinaforeAction action) =
    getComposeM $ runReaderT action MkActionContext {..}

createViewPinaforeAction :: CreateView a -> PinaforeAction a
createViewPinaforeAction cva = MkPinaforeAction $ lift $ lift cva

pinaforeGetCreateViewUnlift :: PinaforeAction (WMFunction PinaforeAction (ComposeM Know CreateView))
pinaforeGetCreateViewUnlift =
    MkPinaforeAction $ do
        MkWUnliftAll unlift <- askUnlift
        return $ MkWMFunction $ \(MkPinaforeAction ra) -> unlift ra

viewPinaforeAction :: View a -> PinaforeAction a
viewPinaforeAction va = createViewPinaforeAction $ liftToLifeCycle va

pinaforeResourceContext :: PinaforeAction ResourceContext
pinaforeResourceContext = viewPinaforeAction viewGetResourceContext

pinaforeRefGet :: WModel update -> ReadM (UpdateReader update) t -> PinaforeAction t
pinaforeRefGet model rm = do
    rc <- pinaforeResourceContext
    liftIO $ wModelGet rc model rm

pinaforeRefPush :: WModel update -> NonEmpty (UpdateEdit update) -> PinaforeAction ()
pinaforeRefPush model edits = do
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
pinaforeExit = viewPinaforeAction viewExit

pinaforeUndoHandler :: PinaforeAction UndoHandler
pinaforeUndoHandler = do
    MkActionContext {..} <- MkPinaforeAction ask
    return acUndoHandler

pinaforeActionKnow :: forall a. Know a -> PinaforeAction a
pinaforeActionKnow (Known a) = pure a
pinaforeActionKnow Unknown = empty

knowPinaforeAction :: forall a. PinaforeAction a -> PinaforeAction (Know a)
knowPinaforeAction (MkPinaforeAction (ReaderT rka)) =
    MkPinaforeAction $ ReaderT $ \r -> MkComposeM $ fmap Known $ getComposeM $ rka r

pinaforeOnClose :: PinaforeAction () -> PinaforeAction ()
pinaforeOnClose closer = do
    MkWMFunction unlift <- pinaforeGetCreateViewUnlift
    createViewPinaforeAction $
        lifeCycleCloseInner $
        runLifeCycle $ do
            _ <- getComposeM $ unlift closer
            return ()

pinaforeEarlyCloser :: PinaforeAction a -> PinaforeAction (a, IO ())
pinaforeEarlyCloser ra = do
    MkWMFunction unlift <- pinaforeGetCreateViewUnlift
    MkPinaforeAction $
        lift $
        MkComposeM $ do
            (ka, closer) <- lifeCycleEarlyCloser $ getComposeM $ unlift ra
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
