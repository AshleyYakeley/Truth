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
                for pagespecs $ \(headspec, bodyspec) -> do
                    headwidget <- getview headspec
                    bodywidget <- getview bodyspec
                    return (headwidget, bodywidget)
            makeNotebook pages

makeNotebook :: MonadIO m => [(Widget, Widget)] -> m Widget
makeNotebook pages = do
    notebook <- new Notebook []
    for_ pages $ \(headwidget, bodywidget) -> #appendPage notebook bodywidget $ Just headwidget
    toWidget notebook
