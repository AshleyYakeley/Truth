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

data PixelARGB8 =
    MkPixelARGB8 !Pixel8
                 !Pixel8
                 !Pixel8
                 !Pixel8
    deriving (Eq)

instance Pixel PixelARGB8 where
    type PixelBaseComponent PixelARGB8 = Word8
    pixelOpacity = error "pixelOpacity @PixelARGB8 unimplemented"
    mixWith = error "mixWith @PixelARGB8 unimplemented"
    mixWithAlpha = error "mixWithAlpha @PixelARGB8 unimplemented"
    colorMap = error "colorMap @PixelARGB8 unimplemented"
    componentCount _ = 4
    pixelAt = error "pixelAt @PixelARGB8 unimplemented"
    readPixel = error "readPixel @PixelARGB8 unimplemented"
    writePixel = error "writePixel @PixelARGB8 unimplemented"
    unsafePixelAt v i =
        MkPixelARGB8 (unsafeIndex v i) (unsafeIndex v $ i + 1) (unsafeIndex v $ i + 2) (unsafeIndex v $ i + 3)
    unsafeReadPixel = error "unsafeReadPixel @PixelARGB8 unimplemented"
    unsafeWritePixel = error "unsafeWritePixel @PixelARGB8 unimplemented"

spp :: PixelARGB8 -> PixelRGBA8
spp (MkPixelARGB8 a r g b) = PixelRGBA8 r g b a

renderToARGBImage :: (Int, Int) -> R.Render () -> Image PixelARGB8
renderToARGBImage s@(w, h) r = Image w h $ byteStringToVector $ renderToByteString R.FormatARGB32 s r

renderToImage :: (Int, Int) -> R.Render () -> Image PixelRGBA8
renderToImage s r = pixelMap spp $ renderToARGBImage s r
