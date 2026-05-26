module Control.Concurrent.TPieceVar where

import Shapes.Import

newtype PushPull m a = MkPushPull (PairType (Op (Ap m ())) (Ap m) a)
    deriving newtype (Invariant, Summable, Productable, Riggable)

ppPush :: PushPull m a -> a -> m ()
ppPush (MkPushPull (MkPairType (Op am) _)) a = getAp $ am a

ppPull :: PushPull m a -> m a
ppPull (MkPushPull (MkPairType _ (Ap ma))) = ma

mkPushPull :: (a -> m ()) -> m a -> PushPull m a
mkPushPull push pull = MkPushPull $ MkPairType (Op $ \a -> Ap $ push a) (Ap pull)

type TPieceVar = PushPull STM

mvarTPieceVar :: TMVar a -> TPieceVar a
mvarTPieceVar tmvar =
    mkPushPull (putTMVar tmvar) (takeTMVar tmvar)

mkWholeTPieceVar :: forall a. STM (TPieceVar a)
mkWholeTPieceVar = do
    var <- newEmptyTMVar
    return $ mvarTPieceVar var
