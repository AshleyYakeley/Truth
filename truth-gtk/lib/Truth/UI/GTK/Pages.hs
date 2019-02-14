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
        MkPagesUISpec pagespecs <- isUISpec uispec
        return $ do
            pages <-
                for pagespecs $ \(headspec, bodyspec) -> do
                    headwidget <- cvNoAspect $ getview headspec
                    bodywidget <- getview bodyspec
                    return (headwidget, bodywidget)
            notebook <- new Notebook []
            for_ pages $ \(headwidget, bodywidget) -> #appendPage notebook bodywidget $ Just headwidget
            toWidget notebook
