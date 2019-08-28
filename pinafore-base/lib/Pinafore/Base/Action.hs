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

data ActionContext baseedit = MkActionContext
    { acUIToolkit :: UIToolkit
    , acSubscriber :: Subscriber baseedit
    , acUndoActions :: UndoActions
    }

newtype PinaforeAction baseedit a =
    MkPinaforeAction (ReaderT (ActionContext baseedit) (ComposeM Know IO) a)
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadFix, MonadIO)

instance MonadFail (PinaforeAction baseedit) where
    fail s = liftIO $ fail s

instance RepresentationalRole (PinaforeAction baseedit) where
    representationalCoercion MkCoercion = MkCoercion

pinaforeActionSubscriber :: PinaforeAction baseedit (Subscriber baseedit)
pinaforeActionSubscriber = MkPinaforeAction $ asks acSubscriber

pinaforeActionObject :: PinaforeAction baseedit (Object baseedit)
pinaforeActionObject = do
    sub <- pinaforeActionSubscriber
    return $ subscriberObject sub

unPinaforeAction ::
       forall baseedit a. UIToolkit -> Subscriber baseedit -> UndoActions -> PinaforeAction baseedit a -> IO (Know a)
unPinaforeAction acUIToolkit acSubscriber acUndoActions (MkPinaforeAction action) =
    getComposeM $ runReaderT action MkActionContext {..}

pinaforeFunctionValueGet :: PinaforeFunctionValue baseedit t -> PinaforeAction baseedit t
pinaforeFunctionValueGet fval = do
    MkCloseUnliftIO objRun MkAnObject {..} <- pinaforeActionObject
    liftIO $ runTransform objRun $ updateFunctionRead fval objRead ReadWhole

pinaforeLensPush :: PinaforeLensValue baseedit edit -> [edit] -> PinaforeAction baseedit ()
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

pinaforeNewWindow :: WindowSpec baseedit -> PinaforeAction baseedit PinaforeWindow
pinaforeNewWindow uiw = do
    MkActionContext {..} <- MkPinaforeAction ask
    let MkUIToolkit {..} = acUIToolkit
    (pwWindow, pwClose) <- liftIO $ uitUnliftLifeCycle $ lifeCycleEarlyCloser $ uitCreateWindow acSubscriber uiw
    return $ MkPinaforeWindow {..}

pinaforeExit :: PinaforeAction baseedit ()
pinaforeExit = do
    MkActionContext {..} <- MkPinaforeAction ask
    let MkUIToolkit {..} = acUIToolkit
    liftIO uitExit

pinaforeUndoActions :: PinaforeAction baseedit UndoActions
pinaforeUndoActions = do
    MkActionContext {..} <- MkPinaforeAction ask
    return acUndoActions

pinaforeActionKnow :: forall baseedit a. Know a -> PinaforeAction baseedit a
pinaforeActionKnow (Known a) = pure a
pinaforeActionKnow Unknown = empty

knowPinaforeAction :: forall baseedit a. PinaforeAction baseedit a -> PinaforeAction baseedit (Know a)
knowPinaforeAction (MkPinaforeAction (ReaderT rka)) =
    MkPinaforeAction $ ReaderT $ \r -> MkComposeM $ fmap Known $ getComposeM $ rka r
