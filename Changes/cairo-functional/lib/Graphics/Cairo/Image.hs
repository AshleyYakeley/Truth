module Graphics.Cairo.Image
    ( R.Format(..)
    , renderToByteString
    , renderToImage
    ) where

import Codec.Picture.Types
import Data.Vector.Storable.ByteString
import qualified GI.Cairo.Render as R
import Shapes hiding (rotate)
import Shapes.Unsafe

renderToByteString :: R.Format -> (Int, Int) -> R.Render () -> StrictByteString
renderToByteString fmt (w, h) r =
    unsafePerformIO $ do
        surface <- R.createImageSurface fmt w h
        R.renderWith surface r
        image <- R.imageSurfaceGetData surface
        R.surfaceFinish surface
        return image

data PixelCairoARGB32 =
    MkPixelCairoARGB32 !Pixel8
                       !Pixel8
                       !Pixel8
                       !Pixel8
    deriving (Eq)

instance Pixel PixelCairoARGB32 where
    type PixelBaseComponent PixelCairoARGB32 = Word8
    pixelOpacity = error "pixelOpacity @PixelCairoARGB32 unimplemented"
    mixWith = error "mixWith @PixelCairoARGB32 unimplemented"
    mixWithAlpha = error "mixWithAlpha @PixelCairoARGB32 unimplemented"
    colorMap = error "colorMap @PixelCairoARGB32 unimplemented"
    componentCount _ = 4
    pixelAt = error "pixelAt @PixelCairoARGB32 unimplemented"
    readPixel = error "readPixel @PixelCairoARGB32 unimplemented"
    writePixel = error "writePixel @PixelCairoARGB32 unimplemented"
    unsafePixelAt v i =
        MkPixelCairoARGB32 (unsafeIndex v i) (unsafeIndex v $ i + 1) (unsafeIndex v $ i + 2) (unsafeIndex v $ i + 3)
    unsafeReadPixel = error "unsafeReadPixel @PixelCairoARGB32 unimplemented"
    unsafeWritePixel = error "unsafeWritePixel @PixelCairoARGB32 unimplemented"

unmultiply :: Word8 -> Word8 -> Word8
unmultiply a c = let
    ac :: Int
    ac = div (fromEnum c * 0xFF) (fromEnum a)
    in if ac >= 0xFF
           then 0xFF
           else toEnum ac

spp :: PixelCairoARGB32 -> PixelRGBA8
spp (MkPixelCairoARGB32 b g r 0xFF) = PixelRGBA8 r g b 0xFF
spp (MkPixelCairoARGB32 b g r 0) = PixelRGBA8 r g b 0
spp (MkPixelCairoARGB32 b g r a) = PixelRGBA8 (unmultiply a r) (unmultiply a g) (unmultiply a b) a

renderToCairoImage :: (Int, Int) -> R.Render () -> Image PixelCairoARGB32
renderToCairoImage s@(w, h) r = Image w h $ byteStringToVector $ renderToByteString R.FormatARGB32 s r

renderToImage :: (Int, Int) -> R.Render () -> Image PixelRGBA8
renderToImage s r = pixelMap spp $ renderToCairoImage s r
