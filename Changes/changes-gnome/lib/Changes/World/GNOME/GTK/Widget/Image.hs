module Changes.World.GNOME.GTK.Widget.Image
    ( createImage
    )
where

import Changes.Core
import Data.GI.Base.Attributes
import Data.Media.Image
import Data.Vector.Storable
import Foreign.Ptr
import GI.GdkPixbuf
import GI.Gtk hiding (Image)
import GI.Gtk qualified as GI
import Shapes

import Changes.World.GNOME.GI

class Pixel px => GTKPixelType px where
    pixbufNewFrom :: Int32 -> Int32 -> Ptr (PixelBaseComponent px) -> IO Pixbuf

instance GTKPixelType PixelRGB8 where
    pixbufNewFrom w h pixelPtr = pixbufNewFromData pixelPtr ColorspaceRgb False 8 w h (w * 3) Nothing

instance GTKPixelType PixelRGBA8 where
    pixbufNewFrom w h pixelPtr = pixbufNewFromData pixelPtr ColorspaceRgb True 8 w h (w * 4) Nothing

pixbufNewFromImage ::
    forall px.
    GTKPixelType px =>
    Image px ->
    IO Pixbuf
pixbufNewFromImage (Image w h pixelVec) =
    unsafeWith pixelVec $ \pixelPtr -> pixbufNewFrom @px (fromIntegral w) (fromIntegral h) pixelPtr

createImage :: Model (ROWUpdate (Maybe (SomeFor Image True8PixelType))) -> GView 'Unlocked Widget
createImage lmod = do
    (imageW, widget) <- gvRunLocked $ gvNewWidget GI.Image []
    gvBindReadOnlyWholeModel lmod $ \case
        Just (MkSomeFor px image) -> do
            pixbuf :: Pixbuf <-
                gvLiftIONoUI
                    $ case px of
                        NoAlphaTrue8PixelType -> pixbufNewFromImage image
                        AlphaTrue8PixelType -> pixbufNewFromImage image
            gvRunLocked $ set imageW [#pixbuf := pixbuf]
        Nothing -> gvRunLocked $ clear imageW #pixbuf
    return widget
