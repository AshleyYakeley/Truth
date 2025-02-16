{-# OPTIONS -fno-warn-orphans #-}
module Changes.World.GNOME.GI.Dynamic where

import Import
import Import.GI qualified as GI
import Data.Dynamic
import Foreign



-- | Get the value in the GValue, checking that the type is
-- `gtypeHValue`. Will return NULL and print a warning if the GValue
-- is of the wrong type.
foreign import ccall "haskell_gi_safe_get_boxed_haskell_value" safe_get_boxed_hvalue :: Ptr GI.GValue -> IO (Ptr b)
foreign import ccall g_value_take_boxed :: Ptr GI.GValue -> Ptr (a) -> IO ()

instance GI.IsGValue Dynamic where
    gvalueGType_ = GI.gvalueGType_ @(GI.HValue ())
    gvalueSet_ gvPtr v = do
        sPtr <- newStablePtr v
        g_value_take_boxed gvPtr (castStablePtrToPtr sPtr)
    gvalueGet_ gvPtr = do
        hvaluePtr <- safe_get_boxed_hvalue gvPtr
        if hvaluePtr == nullPtr
            then return $ error "GValue is not Dynamic"
            else deRefStablePtr (castPtrToStablePtr hvaluePtr)

