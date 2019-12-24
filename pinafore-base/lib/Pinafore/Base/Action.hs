module Pinafore.Base.Action
    ( PinaforeAction
    , unPinaforeAction
    , pinaforeFunctionValueGet
    , pinaforeValuePushAction
    , PinaforeWindow(..)
    , pinaforeNewWindow
    , pinaforeExit
    , pinaforeUndoActions
    , pinaforeActionKnow
    , knowPinaforeAction
    , pinaforeLiftLifeCycleIO
    ) where

import Pinafore.Base.Know
import Pinafore.Base.Value
import Shapes
import Truth.Core

data ActionContext = MkActionContext
    { acUIToolkit :: UIToolkit
    , acUndoActions :: UndoActions
    }

newtype PinaforeAction a =
    MkPinaforeAction (ReaderT (ActionContext) (ComposeM Know IO) a)
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadFix, MonadIO)

instance MonadFail PinaforeAction where
    fail s = liftIO $ fail s

instance RepresentationalRole PinaforeAction where
    representationalCoercion MkCoercion = MkCoercion

unPinaforeAction :: forall a. UIToolkit -> UndoActions -> PinaforeAction a -> IO (Know a)
unPinaforeAction acUIToolkit acUndoActions (MkPinaforeAction action) =
    getComposeM $ runReaderT action MkActionContext {..}

pinaforeValuePushAction :: PinaforeValue update -> NonEmpty (UpdateEdit update) -> PinaforeAction ()
pinaforeValuePushAction lv edits = do
    ok <- liftIO $ pinaforeValuePush lv edits
    if ok
        then return ()
        else empty

data PinaforeWindow = MkPinaforeWindow
    { pwClose :: IO ()
    , pwWindow :: UIWindow
    }

-- | Closing will be done at end of session.
pinaforeLiftLifeCycleIO :: LifeCycleIO a -> PinaforeAction a
pinaforeLiftLifeCycleIO la = do
    MkActionContext {..} <- MkPinaforeAction ask
    let MkUIToolkit {..} = acUIToolkit
    liftIO $ uitUnliftLifeCycle la

pinaforeNewWindow :: WindowSpec -> PinaforeAction PinaforeWindow
pinaforeNewWindow uiw = do
    MkActionContext {..} <- MkPinaforeAction ask
    let MkUIToolkit {..} = acUIToolkit
    (pwWindow, pwClose) <- pinaforeLiftLifeCycleIO $ lifeCycleEarlyCloser $ uitCreateWindow uiw
    return $ MkPinaforeWindow {..}

pinaforeExit :: PinaforeAction ()
pinaforeExit = do
    MkActionContext {..} <- MkPinaforeAction ask
    let MkUIToolkit {..} = acUIToolkit
    liftIO uitExit

pinaforeUndoActions :: PinaforeAction UndoActions
pinaforeUndoActions = do
    MkActionContext {..} <- MkPinaforeAction ask
    return acUndoActions

pinaforeActionKnow :: forall a. Know a -> PinaforeAction a
pinaforeActionKnow (Known a) = pure a
pinaforeActionKnow Unknown = empty

knowPinaforeAction :: forall a. PinaforeAction a -> PinaforeAction (Know a)
knowPinaforeAction (MkPinaforeAction (ReaderT rka)) =
    MkPinaforeAction $ ReaderT $ \r -> MkComposeM $ fmap Known $ getComposeM $ rka r
