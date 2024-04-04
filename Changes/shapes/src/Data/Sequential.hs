module Data.Sequential where

import Shapes.Import
import Shapes.Numeric

class Sequential a where
    szero :: a
    default szero :: Num a => a
    szero = 0
    ssucc :: a -> a
    default ssucc :: Enum a => a -> a
    ssucc = succ

instance Sequential PeanoNat where
    szero = Zero
    ssucc = Succ

instance Sequential Natural

instance Sequential Int

instance Sequential Integer

refSucc :: (Monad m, Sequential a) => Ref m a -> m a
refSucc ref = do
    x <- refGet ref
    refPut ref $ ssucc x
    return x
