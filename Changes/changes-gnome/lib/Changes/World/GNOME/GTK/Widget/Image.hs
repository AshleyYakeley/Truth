module Changes.World.GNOME.GTK.Widget.Image
    ( createImage
    )
where

-- import Data.GI.Base.Attributes
import Data.ByteString.Internal qualified as BS
import Data.Media.Image
import Data.Vector.Storable qualified as V

import Changes.World.GNOME.GI
import Import
import Import.GI qualified as GI

class (Pixel px, PixelBaseComponent px ~ Word8) => GTKPixelType px where
    memoryFormat :: GI.MemoryFormat
    rowStride :: Int32 -> Int32

instance GTKPixelType PixelRGB8 where
    memoryFormat = GI.MemoryFormatR8g8b8
    rowStride w = w * 3

instance GTKPixelType PixelRGBA8 where
    memoryFormat = GI.MemoryFormatR8g8b8a8
    rowStride w = w * 4

textureNewFromImage ::
    forall px.
    GTKPixelType px =>
    Image px ->
    IO GI.MemoryTexture
textureNewFromImage (Image w h pixelVec) = do
    let
        imageWidth = fromIntegral w
        imageHeight = fromIntegral h
        (pixelPtr, offset, len) = V.unsafeToForeignPtr pixelVec
    bytes <- GI.bytesNew $ Just $ BS.fromForeignPtr pixelPtr offset len
    GI.memoryTextureNew imageWidth imageHeight (memoryFormat @px) bytes $ fromIntegral $ rowStride @px imageWidth

createImage :: Model (ROWUpdate (Maybe (SomeFor Image True8PixelType))) -> GView 'Unlocked GI.Widget
createImage lmod = do
    (imageW, widget) <- gvRunLocked $ gvNewWidget GI.Image []
    gvBindReadOnlyWholeModel lmod $ \case
        Just (MkSomeFor px image) -> do
            texture :: GI.MemoryTexture <-
                gvLiftIOTrustMeNoUI
                    $ case px of
                        NoAlphaTrue8PixelType -> textureNewFromImage image
                        AlphaTrue8PixelType -> textureNewFromImage image
            gvRunLocked $ GI.imageSetFromPaintable imageW $ Just texture
        Nothing -> gvRunLocked $ GI.imageClear imageW
    return widget
