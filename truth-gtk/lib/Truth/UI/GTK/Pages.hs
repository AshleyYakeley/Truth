module Truth.UI.GTK.Pages
    ( pagesGetView
    ) where

import Graphics.UI.Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.GView

pagesGetView :: GetGView
pagesGetView =
    MkGetView $ \getview uispec -> do
        MkUIPages pagespecs <- isUISpec uispec
        return $ do
            pages <-
                for pagespecs $ \(pname, pspec) -> do
                    pwidget <- getview pspec
                    return (pname, pwidget)
            liftIO $ makeNotebook pages

makeNotebook :: [(Text, Widget)] -> IO Widget
makeNotebook pages = do
    notebook <- notebookNew
    for_ pages $ \(pname, pwidget) -> notebookAppendPage notebook pwidget pname
    return $ toWidget notebook
