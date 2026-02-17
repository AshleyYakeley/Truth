module Changes.World.GNOME.GTK.Widget.Image
    ( createImage
    )
where

-- import Data.GI.Base.Attributes
import Data.Media.Image
import Data.Vector.Storable
import Foreign.Ptr

import Changes.World.GNOME.GI
import Import
import Import.GI qualified as GI

class Pixel px => GTKPixelType px where
    pixbufNewFrom :: Int32 -> Int32 -> Ptr (PixelBaseComponent px) -> IO GI.Pixbuf

instance GTKPixelType PixelRGB8 where
    pixbufNewFrom w h pixelPtr = GI.pixbufNewFromData pixelPtr GI.ColorspaceRgb False 8 w h (w * 3) Nothing

instance GTKPixelType PixelRGBA8 where
    pixbufNewFrom w h pixelPtr = GI.pixbufNewFromData pixelPtr GI.ColorspaceRgb True 8 w h (w * 4) Nothing

pixbufNewFromImage ::
    forall px.
    GTKPixelType px =>
    Image px ->
    IO GI.Pixbuf
pixbufNewFromImage (Image w h pixelVec) =
    unsafeWith pixelVec $ \pixelPtr -> pixbufNewFrom @px (fromIntegral w) (fromIntegral h) pixelPtr

createImage :: Model (ROWUpdate (Maybe (SomeFor Image True8PixelType))) -> GView 'Unlocked GI.Widget
createImage lmod = do
    (imageW, widget) <- gvRunLocked $ gvNewWidget GI.Image []
    gvBindReadOnlyWholeModel lmod $ \case
        Just (MkSomeFor px image) -> do
            pixbuf :: GI.Pixbuf <-
                gvLiftIONoUI
                    $ case px of
                        NoAlphaTrue8PixelType -> pixbufNewFromImage image
                        AlphaTrue8PixelType -> pixbufNewFromImage image
            gvRunLocked $ GI.imageSetFromPixbuf imageW $ Just pixbuf
        Nothing -> gvRunLocked $ GI.imageClear imageW
    return widget
