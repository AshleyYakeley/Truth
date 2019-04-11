module Pinafore.Base.Action
    ( PinaforeAction
    , unPinaforeAction
    , pinaforeActionSubscriber
    , pinaforeFunctionValueGet
    , pinaforeLensPush
    , PinaforeWindow(..)
    , pinaforeNewWindow
    , pinaforeCloseAllWindows
    , pinaforeActionKnow
    , knowPinaforeAction
    ) where

import Language.Expression.Dolan
import Pinafore.Base.Know
import Pinafore.Base.Morphism
import Shapes
import Truth.Core

data ActionContext baseedit = MkActionContext
    { acSubscriber :: Subscriber baseedit
    , acUIToolkit :: UIToolkit
    }

newtype PinaforeAction baseedit a =
    MkPinaforeAction (ReaderT (ActionContext baseedit) (ComposeM Know IO) a)
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadFix, MonadIO)

instance MonadFail (PinaforeAction baseedit) where
    fail s = liftIO $ fail s

instance HasDolanVary '[ 'Covariance] (PinaforeAction baseedit) where
    dolanVary = ConsDolanVarianceMap fmap $ NilDolanVarianceMap

pinaforeActionSubscriber :: PinaforeAction baseedit (Subscriber baseedit)
pinaforeActionSubscriber = do
    ac <- MkPinaforeAction ask
    return $ acSubscriber ac

unPinaforeAction :: forall baseedit a. UIToolkit -> Subscriber baseedit -> PinaforeAction baseedit a -> IO (Know a)
unPinaforeAction acUIToolkit acSubscriber (MkPinaforeAction action) =
    getComposeM $ runReaderT action MkActionContext {..}

pinaforeFunctionValueGet :: PinaforeFunctionValue baseedit t -> PinaforeAction baseedit t
pinaforeFunctionValueGet fval = do
    (subObject -> MkObject {..}) <- pinaforeActionSubscriber
    liftIO $ runTransform objRun $ editFunctionRead fval objRead ReadWhole

pinaforeLensPush :: PinaforeLensValue baseedit edit -> [edit] -> PinaforeAction baseedit ()
pinaforeLensPush lens edits = do
    sub <- pinaforeActionSubscriber
    case lensObject True lens $ subObject sub of
        MkObject {..} -> do
            ok <- liftIO $ runTransform objRun $ pushEdit noEditSource $ objEdit edits
            if ok
                then return ()
                else empty

data PinaforeWindow = MkPinaforeWindow
    { pwWindow :: UIWindow
    }

pinaforeNewWindow :: WindowSpec baseedit -> PinaforeAction baseedit PinaforeWindow
pinaforeNewWindow uiw = do
    MkActionContext {..} <- MkPinaforeAction ask
    let MkUIToolkit {..} = acUIToolkit
    w <- liftIO $ uitCreateWindow acSubscriber uiw
    return $ MkPinaforeWindow w

pinaforeCloseAllWindows :: PinaforeAction baseedit ()
pinaforeCloseAllWindows = do
    MkActionContext {..} <- MkPinaforeAction ask
    let MkUIToolkit {..} = acUIToolkit
    liftIO $ uitCloseAllWindows

pinaforeActionKnow :: forall baseedit a. Know a -> PinaforeAction baseedit a
pinaforeActionKnow (Known a) = pure a
pinaforeActionKnow Unknown = empty

knowPinaforeAction :: forall baseedit a. PinaforeAction baseedit a -> PinaforeAction baseedit (Know a)
knowPinaforeAction (MkPinaforeAction (ReaderT rka)) =
    MkPinaforeAction $ ReaderT $ \r -> MkComposeM $ fmap Known $ getComposeM $ rka r
