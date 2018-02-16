module Truth.UI.GTK.Pages
    ( pagesGetView
    ) where

import GI.Gtk
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
            makeNotebook pages

makeNotebook :: MonadIO m => [(Text, Widget)] -> m Widget
makeNotebook pages = do
    notebook <- new Notebook []
    for_ pages $ \(pname, pwidget) -> do
        label <- new Label [#label := pname]
        #appendPage notebook pwidget $ Just label
    toWidget notebook
