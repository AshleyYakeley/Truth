module Control.Concurrent.TPieceVar where

import Shapes.Import

data TPieceVar a = MkTPieceVar
    { putTPieceVar :: a -> STM ()
    , takeTPieceVar :: STM a
    }

instance Invariant TPieceVar where
    invmap ab ba (MkTPieceVar p t) = MkTPieceVar (\b -> p $ ba b) (fmap ab t)

instance Summable TPieceVar where
    rVoid = MkTPieceVar never mzero
    MkTPieceVar ap at <+++> MkTPieceVar bp bt = let
        abp = \case
            Left a -> ap a
            Right b -> bp b
        abt = fmap Left at <|> fmap Right bt
        in MkTPieceVar abp abt

mvarTPieceVar :: TMVar a -> TPieceVar a
mvarTPieceVar tmvar =
    MkTPieceVar
        { putTPieceVar = putTMVar tmvar
        , takeTPieceVar = takeTMVar tmvar
        }

mkWholeTPieceVar :: forall a. STM (TPieceVar a)
mkWholeTPieceVar = do
    var <- newEmptyTMVar
    return $ mvarTPieceVar var
