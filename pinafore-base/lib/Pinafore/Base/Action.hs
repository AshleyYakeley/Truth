module Pinafore.Base.Action
    ( PinaforeAction
    , unPinaforeAction
    , viewPinaforeAction
    , pinaforeResourceContext
    , pinaforeFunctionValueGet
    , pinaforeRefPushAction
    , pinaforeGetExitOnClose
    , pinaforeExit
    , pinaforeUndoHandler
    , pinaforeActionKnow
    , knowPinaforeAction
    , pinaforeLiftLifeCycleIO
    , pinaforeLifeCycle
    , pinaforeOnClose
    , pinaforeEarlyCloser
    ) where

import Changes.Core
import Pinafore.Base.Know
import Pinafore.Base.Ref
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

unPinaforeAction :: forall a. ChangesContext -> UndoHandler -> PinaforeAction a -> CreateView (Know a)
unPinaforeAction acChangesContext acUndoHandler (MkPinaforeAction action) =
    getComposeM $ runReaderT action MkActionContext {..}

viewPinaforeAction :: View a -> PinaforeAction a
viewPinaforeAction va = MkPinaforeAction $ lift $ lift $ cvLiftView va

pinaforeResourceContext :: PinaforeAction ResourceContext
pinaforeResourceContext = viewPinaforeAction viewGetResourceContext

pinaforeRefPushAction :: WModel update -> NonEmpty (UpdateEdit update) -> PinaforeAction ()
pinaforeRefPushAction lv edits = do
    rc <- pinaforeResourceContext
    ok <- liftIO $ wModelPush rc lv edits
    if ok
        then return ()
        else empty

pinaforeGetExitOnClose :: PinaforeAction (WMFunction CreateView LifeCycleIO)
pinaforeGetExitOnClose =
    MkPinaforeAction $ do
        tc <- asks acChangesContext
        unlift <- lift $ MkComposeM $ fmap Known askUnlift
        return $ MkWMFunction $ runWUnliftAll unlift . tcExitOnClosed tc

pinaforeLiftLifeCycleIO :: LifeCycleIO a -> PinaforeAction a
pinaforeLiftLifeCycleIO la = MkPinaforeAction $ lift $ lift $ lift la

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

pinaforeLifeCycle :: PinaforeAction a -> PinaforeAction a
pinaforeLifeCycle (MkPinaforeAction ra) = MkPinaforeAction $ (remonad $ remonad $ remonad $ lift . runLifeCycle) ra

cvOnClose :: CreateView () -> CreateView ()
cvOnClose closer = liftWithUnlift $ \unlift -> lifeCycleClose $ runLifeCycle $ unlift closer

pinaforeOnClose :: PinaforeAction () -> PinaforeAction ()
pinaforeOnClose (MkPinaforeAction closer) =
    MkPinaforeAction $ do
        MkWUnliftAll unlift <- askUnlift
        lift $
            liftOuter $
            cvOnClose $ do
                _ <- getComposeM $ unlift closer
                return ()

pinaforeEarlyCloser :: PinaforeAction a -> PinaforeAction (a, PinaforeAction ())
pinaforeEarlyCloser (MkPinaforeAction ra) =
    MkPinaforeAction $ do
        MkWUnliftAll unliftA <- askUnlift
        lift $
            MkComposeM $
            liftWithUnlift $ \unliftV ->
                fmap (\(ka, closer) -> fmap (\a -> (a, liftIO closer)) ka) $
                lifeCycleEarlyCloser $ unliftV $ getComposeM $ unliftA ra
