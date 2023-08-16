module Debug.ThreadTrace.Lookup
    ( lookupMVar
    , lookupTVar
    ) where

import Control.Concurrent
import Control.Monad.IO.Class
import GHC.Conc
import Prelude
import System.IO.Unsafe
import Unsafe.Coerce

type MVarDict a = MVar [a]

findItem :: Eq a => a -> [a] -> ([a], Int)
findItem k [] = ([k], 0)
findItem k aa@(a:_)
    | k == a = (aa, 0)
findItem k (a:ar) =
    case findItem k ar of
        (ar', i) -> (a : ar', succ i)

mVarDictLookup :: Eq a => MVarDict a -> a -> IO Int
mVarDictLookup mvar k = modifyMVar mvar $ \old -> return $ findItem k old

newtype Same f =
    MkSame (f ())

instance Eq (f ()) => Eq (Same f) where
    MkSame a == MkSame b = a == b

toSame :: forall f a. f a -> Same f
toSame fa = MkSame $ unsafeCoerce fa

lookupMVarVar :: MVarDict (Same MVar)
{-# NOINLINE lookupMVarVar #-}
lookupMVarVar = unsafePerformIO $ newMVar mempty

lookupMVar :: MonadIO m => MVar a -> m String
lookupMVar x = do
    i <- liftIO $ mVarDictLookup lookupMVarVar $ toSame x
    return $ "mvar-" <> show i

lookupTVarVar :: MVarDict (Same TVar)
{-# NOINLINE lookupTVarVar #-}
lookupTVarVar = unsafePerformIO $ newMVar mempty

lookupTVar :: MonadIO m => TVar a -> m String
lookupTVar x = do
    i <- liftIO $ mVarDictLookup lookupTVarVar $ toSame x
    return $ "tvar-" <> show i
