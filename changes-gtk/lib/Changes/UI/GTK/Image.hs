module Changes.UI.GTK.Image
    ( createImage
    ) where

import Changes.Core
import Changes.UI.GTK.Useful
import GI.GdkPixbuf
import GI.Gio
import GI.Gtk
import Shapes

createImage :: StrictByteString -> CreateView Widget
createImage bs = do
    image <-
        subLifeCycle $ do
            istream <- memoryInputStreamNewFromData bs Nothing
            cvAcquire istream
            pixbuf <- pixbufNewFromStream istream noCancellable
            cvAcquire pixbuf
            imageNewFromPixbuf $ Just pixbuf
    cvAcquire image
    toWidget image
