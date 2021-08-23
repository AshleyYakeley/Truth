module Changes.UI.GTK.Image
    ( createImage
    ) where

import Changes.Core
import Changes.UI.GTK.Useful
import Data.Media.Image
import Data.Vector.Storable
import Foreign.Ptr
import GI.GdkPixbuf
import qualified GI.Gtk as GI
import GI.Gtk hiding (Image)
import Shapes

class Pixel px => GTKPixelType px where
    pixbufNewFrom :: Int32 -> Int32 -> Ptr (PixelBaseComponent px) -> IO Pixbuf

instance GTKPixelType PixelRGB8 where
    pixbufNewFrom w h pixelPtr = pixbufNewFromData pixelPtr ColorspaceRgb False 8 w h (w * 3) Nothing

instance GTKPixelType PixelRGBA8 where
    pixbufNewFrom w h pixelPtr = pixbufNewFromData pixelPtr ColorspaceRgb True 8 w h (w * 4) Nothing

pixbufNewFromImage ::
       forall px. GTKPixelType px
    => Image px
    -> IO Pixbuf
pixbufNewFromImage (Image w h pixelVec) =
    unsafeWith pixelVec $ \pixelPtr -> pixbufNewFrom @px (fromIntegral w) (fromIntegral h) pixelPtr

createImage :: GTKPixelType px => Model (ROWUpdate (Image px)) -> CreateView Widget
createImage lmod = do
    widget <- cvNew GI.Image []
    cvBindReadOnlyWholeModel lmod $ \image -> do
        pixbuf <- liftIO $ pixbufNewFromImage image
        set widget [#pixbuf := pixbuf]
    toWidget widget
