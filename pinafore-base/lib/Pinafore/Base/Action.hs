module Pinafore.Base.Action
    ( PinaforeAction
    , unPinaforeAction
    , viewPinaforeAction
    , createViewPinaforeAction
    , pinaforeResourceContext
    , pinaforeFunctionValueGet
    , pinaforeRefPushAction
    , pinaforeGetExitOnClose
    , pinaforeExit
    , pinaforeUndoHandler
    , pinaforeActionKnow
    , knowPinaforeAction
    , pinaforeLiftLifeCycleIO
    ) where

import Pinafore.Base.Know
import Pinafore.Base.Ref
import Shapes
import Truth.Core

data ActionContext = MkActionContext
    { acTruthContext :: TruthContext
    , acUndoHandler :: UndoHandler
    }

newtype PinaforeAction a =
    MkPinaforeAction (ReaderT ActionContext (ComposeM Know View) a)
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadFix, MonadIO)

instance MonadFail PinaforeAction where
    fail s = liftIO $ fail s

instance RepresentationalRole PinaforeAction where
    representationalCoercion MkCoercion = MkCoercion

unPinaforeAction :: forall a. TruthContext -> UndoHandler -> PinaforeAction a -> View (Know a)
unPinaforeAction acTruthContext acUndoHandler (MkPinaforeAction action) =
    getComposeM $ runReaderT action MkActionContext {..}

viewPinaforeAction :: View a -> PinaforeAction a
viewPinaforeAction va = MkPinaforeAction $ lift $ lift va

createViewPinaforeAction :: CreateView a -> PinaforeAction a
createViewPinaforeAction cva = do
    unlift <- MkPinaforeAction $ lift $ MkComposeM $ fmap Known askUnlift
    pinaforeLiftLifeCycleIO $ runWUnliftAll unlift cva

pinaforeResourceContext :: PinaforeAction ResourceContext
pinaforeResourceContext = viewPinaforeAction viewGetResourceContext

pinaforeRefPushAction :: PinaforeRef update -> NonEmpty (UpdateEdit update) -> PinaforeAction ()
pinaforeRefPushAction lv edits = do
    rc <- pinaforeResourceContext
    ok <- liftIO $ pinaforeRefPush rc lv edits
    if ok
        then return ()
        else empty

pinaforeGetExitOnClose :: PinaforeAction (WMFunction CreateView LifeCycleIO)
pinaforeGetExitOnClose =
    MkPinaforeAction $ do
        tc <- asks acTruthContext
        unlift <- lift $ MkComposeM $ fmap Known askUnlift
        return $ MkWMFunction $ runWUnliftAll unlift . tcExitOnClosed tc

-- | Closing will be done at end of session.
pinaforeLiftLifeCycleIO :: LifeCycleIO a -> PinaforeAction a
pinaforeLiftLifeCycleIO la = do
    MkActionContext {..} <- MkPinaforeAction ask
    liftIO $ tcUnliftLifeCycle acTruthContext la

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
