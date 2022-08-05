module Unsafe.IO
    ( unsafePerformIO
    , unsafeInterleaveIO
    , module Unsafe.IO
    ) where

import Shapes.Import
import System.IO.Unsafe

{-# NOINLINE unsafePerformIOM #-}
unsafePerformIOM :: Applicative m => IO a -> m a
unsafePerformIOM ioa = pure $ unsafePerformIO ioa
