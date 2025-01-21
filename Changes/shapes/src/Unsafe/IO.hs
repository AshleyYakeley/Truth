module Unsafe.IO
    ( unsafePerformIO
    , unsafeInterleaveIO
    , module Unsafe.IO
    )
where

import System.IO.Unsafe

import Shapes.Import

{-# NOINLINE unsafePerformIOM #-}
unsafePerformIOM :: Applicative m => IO a -> m a
unsafePerformIOM ioa = pure $ unsafePerformIO ioa
