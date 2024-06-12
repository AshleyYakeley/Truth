{-# LANGUAGE CPP #-}
module Flags where

import Shapes

flag_TestX11 :: Bool
flag_TestX11 =
#ifdef TEST_X11
    True
#else
    False
#endif
