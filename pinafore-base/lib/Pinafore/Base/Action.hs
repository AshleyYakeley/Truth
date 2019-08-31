module Pinafore.Base.Action
    ( PinaforeAction
    , unPinaforeAction
    , pinaforeActionSubscriber
    , pinaforeActionObject
    , pinaforeFunctionValueGet
    , pinaforeLensPush
    , PinaforeWindow(..)
    , pinaforeNewWindow
    , pinaforeExit
    , pinaforeUndoActions
    , pinaforeActionKnow
    , knowPinaforeAction
    ) where

import Pinafore.Base.Know
import Pinafore.Base.Morphism
import Shapes
import Truth.Core

data ActionContext baseupdate = MkActionContext
    { acUIToolkit :: UIToolkit
    , acSubscriber :: Subscriber baseupdate
    , acUndoActions :: UndoActions
    }

newtype PinaforeAction baseupdate a =
    MkPinaforeAction (ReaderT (ActionContext baseupdate) (ComposeM Know IO) a)
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadFix, MonadIO)

instance MonadFail (PinaforeAction baseupdate) where
    fail s = liftIO $ fail s

instance RepresentationalRole (PinaforeAction baseupdate) where
    representationalCoercion MkCoercion = MkCoercion

pinaforeActionSubscriber :: PinaforeAction baseupdate (Subscriber baseupdate)
pinaforeActionSubscriber = MkPinaforeAction $ asks acSubscriber

pinaforeActionObject :: PinaforeAction baseupdate (Object (UpdateEdit baseupdate))
pinaforeActionObject = do
    sub <- pinaforeActionSubscriber
    return $ subscriberObject sub

unPinaforeAction ::
       forall baseupdate a.
       UIToolkit
    -> Subscriber baseupdate
    -> UndoActions
    -> PinaforeAction baseupdate a
    -> IO (Know a)
unPinaforeAction acUIToolkit acSubscriber acUndoActions (MkPinaforeAction action) =
    getComposeM $ runReaderT action MkActionContext {..}

pinaforeFunctionValueGet :: PinaforeFunctionValue baseupdate t -> PinaforeAction baseupdate t
pinaforeFunctionValueGet fval = do
    MkCloseUnliftIO objRun MkAnObject {..} <- pinaforeActionObject
    liftIO $ runTransform objRun $ updateFunctionRead fval objRead ReadWhole

pinaforeLensPush :: PinaforeLensValue baseupdate update -> [UpdateEdit update] -> PinaforeAction baseupdate ()
pinaforeLensPush lens edits = do
    obj <- pinaforeActionObject
    case mapObject lens obj of
        MkCloseUnliftIO objRun MkAnObject {..} -> do
            ok <- liftIO $ runTransform objRun $ pushEdit noEditSource $ objEdit edits
            if ok
                then return ()
                else empty

data PinaforeWindow = MkPinaforeWindow
    { pwClose :: IO ()
    , pwWindow :: UIWindow
    }

pinaforeNewWindow :: WindowSpec baseupdate -> PinaforeAction baseupdate PinaforeWindow
pinaforeNewWindow uiw = do
    MkActionContext {..} <- MkPinaforeAction ask
    let MkUIToolkit {..} = acUIToolkit
    (pwWindow, pwClose) <- liftIO $ uitUnliftLifeCycle $ lifeCycleEarlyCloser $ uitCreateWindow acSubscriber uiw
    return $ MkPinaforeWindow {..}

pinaforeExit :: PinaforeAction baseupdate ()
pinaforeExit = do
    MkActionContext {..} <- MkPinaforeAction ask
    let MkUIToolkit {..} = acUIToolkit
    liftIO uitExit

pinaforeUndoActions :: PinaforeAction baseupdate UndoActions
pinaforeUndoActions = do
    MkActionContext {..} <- MkPinaforeAction ask
    return acUndoActions

pinaforeActionKnow :: forall baseupdate a. Know a -> PinaforeAction baseupdate a
pinaforeActionKnow (Known a) = pure a
pinaforeActionKnow Unknown = empty

knowPinaforeAction :: forall baseupdate a. PinaforeAction baseupdate a -> PinaforeAction baseupdate (Know a)
knowPinaforeAction (MkPinaforeAction (ReaderT rka)) =
    MkPinaforeAction $ ReaderT $ \r -> MkComposeM $ fmap Known $ getComposeM $ rka r
