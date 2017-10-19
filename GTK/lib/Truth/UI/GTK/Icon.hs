module Truth.UI.GTK.Icon(iconGetView) where
{
    import Shapes;
    import Graphics.UI.Gtk;
    import Truth.Core;
    import Truth.UI.GTK.GView;


    whichIcon :: StockIcon -> StockId;
    whichIcon SiDnD = stockDnd;

    whichSize :: StockSize -> IconSize;
    whichSize SizeDnD = IconSizeDnd;
    whichSize (SizeCustom i) = IconSizeUser i;

    iconGetView :: GetGView;
    iconGetView = MkGetView $ \_ uispec -> do
    {
        MkUIIcon icon size <- isUISpec uispec;
        return $ ioPureView $ do
        {
            image <- imageNewFromStock (whichIcon icon) (whichSize size);
            return $ toWidget image;
        };
    };
}
