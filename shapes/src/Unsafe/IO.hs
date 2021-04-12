module Unsafe.IO
    ( unsafePerformIO
    , module Unsafe.IO
    ) where

import Shapes.Import
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE unsafePerformIOM #-}
unsafePerformIOM :: Applicative m => IO a -> m a
unsafePerformIOM ioa = pure $ unsafePerformIO ioa
