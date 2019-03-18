module Pinafore.Base.Action where

import Language.Expression.Dolan
import Pinafore.Base.Know
import Pinafore.Base.Morphism
import Shapes
import Truth.Core

newtype PinaforeAction baseedit a =
    MkPinaforeAction (ReaderT (WindowSpec baseedit -> IO UIWindow, Object baseedit) (ComposeM Know IO) a)
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadFix, MonadIO)

instance MonadFail (PinaforeAction baseedit) where
    fail s = liftIO $ fail s

instance HasDolanVary '[ 'Covariance] (PinaforeAction baseedit) where
    dolanVary = ConsDolanVarianceMap fmap $ NilDolanVarianceMap

resultTextToM :: MonadFail m => Result Text a -> m a
resultTextToM = resultToM . mapResultFailure unpack

pinaforeFunctionValueGet :: PinaforeFunctionValue baseedit t -> PinaforeAction baseedit t
pinaforeFunctionValueGet fval = do
    (_, MkObject {..}) <- MkPinaforeAction ask
    liftIO $ runTransform objRun $ editFunctionRead fval objRead ReadWhole

pinaforeLensPush :: PinaforeLensValue baseedit edit -> [edit] -> PinaforeAction baseedit ()
pinaforeLensPush lens edits = do
    (_, object) <- MkPinaforeAction ask
    case lensObject True lens object of
        MkObject {..} -> do
            ok <- liftIO $ runTransform objRun $ pushEdit $ objEdit edits
            if ok
                then return ()
                else empty

data PinaforeWindow = MkPinaforeWindow
    { pwWindow :: UIWindow
    }

pinaforeNewWindow :: WindowSpec baseedit -> PinaforeAction baseedit PinaforeWindow
pinaforeNewWindow uiw = do
    (neww, _) <- MkPinaforeAction ask
    w <- liftIO $ neww uiw
    return $ MkPinaforeWindow w

pinaforeActionKnow :: forall baseedit a. Know a -> PinaforeAction baseedit a
pinaforeActionKnow (Known a) = pure a
pinaforeActionKnow Unknown = empty

knowPinaforeAction :: forall baseedit a. PinaforeAction baseedit a -> PinaforeAction baseedit (Know a)
knowPinaforeAction (MkPinaforeAction (ReaderT rka)) =
    MkPinaforeAction $ ReaderT $ \r -> MkComposeM $ fmap Known $ getComposeM $ rka r
