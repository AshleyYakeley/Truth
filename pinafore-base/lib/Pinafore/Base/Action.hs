module Pinafore.Base.Action
    ( PinaforeAction
    , unPinaforeAction
    , createViewPinaforeAction
    , pinaforeGetCreateViewUnlift
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

createViewPinaforeAction :: CreateView a -> PinaforeAction a
createViewPinaforeAction cva = MkPinaforeAction $ lift $ lift cva

pinaforeGetCreateViewUnlift :: PinaforeAction (WMFunction PinaforeAction (ComposeM Know CreateView))
pinaforeGetCreateViewUnlift =
    MkPinaforeAction $ do
        MkWUnliftAll unlift <- askUnlift
        return $ MkWMFunction $ \(MkPinaforeAction ra) -> unlift ra

viewPinaforeAction :: View a -> PinaforeAction a
viewPinaforeAction va = createViewPinaforeAction $ cvLiftView va

pinaforeResourceContext :: PinaforeAction ResourceContext
pinaforeResourceContext = viewPinaforeAction viewGetResourceContext

pinaforeRefPushAction :: WModel update -> NonEmpty (UpdateEdit update) -> PinaforeAction ()
pinaforeRefPushAction lv edits = do
    rc <- pinaforeResourceContext
    ok <- liftIO $ wModelPush rc lv edits
    if ok
        then return ()
        else empty

pinaforeGetExitOnClose :: PinaforeAction (WMFunction CreateView CreateView)
pinaforeGetExitOnClose =
    MkPinaforeAction $ do
        tc <- asks acChangesContext
        return $ MkWMFunction $ tcExitOnClosed tc

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
pinaforeOnClose closer = do
    MkWMFunction unlift <- pinaforeGetCreateViewUnlift
    createViewPinaforeAction $
        cvOnClose $ do
            _ <- getComposeM $ unlift closer
            return ()

pinaforeEarlyCloser :: PinaforeAction a -> PinaforeAction (a, IO ())
pinaforeEarlyCloser ra = do
    MkWMFunction unlift <- pinaforeGetCreateViewUnlift
    MkPinaforeAction $
        lift $
        MkComposeM $ do
            (ka, closer) <- cvEarlyCloser $ getComposeM $ unlift ra
            return $ fmap (\a -> (a, closer)) ka
