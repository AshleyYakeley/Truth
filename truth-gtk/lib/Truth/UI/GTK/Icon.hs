module Truth.UI.GTK.Icon
    ( iconGetView
    ) where

import GI.Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.GView

whichSize :: StockSize -> IconSize
whichSize SizeDnD = IconSizeDnd
whichSize (SizeCustom i) = AnotherIconSize i

iconGetView :: GetGView
iconGetView =
    MkGetView $ \_ uispec -> do
        MkUIIcon icon size <- isUISpec uispec
        return $
            liftLifeCycle $ do
                image <- imageNewFromIconName (Just icon) (fromIntegral $ fromEnum $ whichSize size)
                lifeCycleClose $ widgetDestroy image
                toWidget image
