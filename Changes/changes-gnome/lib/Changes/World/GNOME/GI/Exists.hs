module Changes.World.GNOME.GI.Exists
    ( Exists
    , gvNewExists
    , gvIfExists
    , gvRunLockedIf
    ) where

import Changes.World.GNOME.GI.GView
import Changes.World.GNOME.GI.LockState
import Data.IORef
import Shapes

newtype Exists =
    MkExists (IORef Bool)

gvNewExists :: GView 'Locked Exists
gvNewExists = do
    ref <- liftIO $ newIORef True
    gvOnClose $ gvLiftIO $ writeIORef ref False
    return $ MkExists ref

gvIfExists :: Monoid a => Exists -> GView 'Locked a -> GView 'Locked a
gvIfExists (MkExists ref) ma = do
    b <- liftIO $ readIORef ref
    if b
        then ma
        else return mempty

gvRunLockedIf :: Monoid a => Exists -> GView 'Locked a -> GView 'Unlocked a
gvRunLockedIf e mu = gvRunLocked $ gvIfExists e mu
