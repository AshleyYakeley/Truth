{-# OPTIONS -fno-warn-orphans #-}
module Truth.UI.GTK.GView where
{
    import Graphics.UI.Gtk;
    import Data.Reity;
    import Truth.Core;

    makeButton :: String -> IO () -> IO Button;
    makeButton name action = do
    {
        button <- buttonNew;
        set button [buttonLabel := name];
        _ <- onClicked button action;
        return button;
    };

    type GView edit = View edit Widget;
    type GViewResult edit = ViewResult edit Widget;

    instance HasInfo Widget where
    {
        info = mkSimpleInfo $(ionamedwitness[t|Widget|]) [];
    };
}
