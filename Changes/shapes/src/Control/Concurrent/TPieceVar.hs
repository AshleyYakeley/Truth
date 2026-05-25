module Control.Concurrent.TPieceVar where

import Shapes.Import

data PushPull m a = MkPushPull
    { ppPush :: a -> m ()
    , ppPull :: m a
    }

instance Functor m => Invariant (PushPull m) where
    invmap ab ba (MkPushPull p t) = MkPushPull (\b -> p $ ba b) (fmap ab t)

instance Alternative m => Summable (PushPull m) where
    rVoid = MkPushPull never empty
    MkPushPull ap at <+++> MkPushPull bp bt = let
        abp = \case
            Left a -> ap a
            Right b -> bp b
        abt = fmap Left at <|> fmap Right bt
        in MkPushPull abp abt

instance Applicative m => Productable (PushPull m) where
    rUnit = MkPushPull (\() -> pure ()) (pure ())
    MkPushPull ap at <***> MkPushPull bp bt = let
        abp (a, b) = ap a *> bp b
        abt = liftA2 (,) at bt
        in MkPushPull abp abt

instance Alternative m => Riggable (PushPull m) where
    rOptional (MkPushPull p t) = let
        mp = \case
            Just a -> p a
            Nothing -> pure ()
        mt = fmap Just t <|> pure Nothing
        in MkPushPull mp mt
    rList1 fa@(MkPushPull p t) = let
        MkPushPull lp lt = rList fa
        np (a :| aa) = p a *> lp aa
        nt = liftA2 (:|) t lt
        in MkPushPull np nt
    rList fa = let
        MkPushPull np nt = rList1 fa
        lp = \case
            a : aa -> np $ a :| aa
            [] -> pure ()
        lt = fmap toList nt <|> pure []
        in MkPushPull lp lt

type TPieceVar = PushPull STM

mvarTPieceVar :: TMVar a -> TPieceVar a
mvarTPieceVar tmvar =
    MkPushPull
        { ppPush = putTMVar tmvar
        , ppPull = takeTMVar tmvar
        }

mkWholeTPieceVar :: forall a. STM (TPieceVar a)
mkWholeTPieceVar = do
    var <- newEmptyTMVar
    return $ mvarTPieceVar var
