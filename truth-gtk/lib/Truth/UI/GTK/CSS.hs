module Truth.UI.GTK.CSS
    ( cssGetView
    ) where

import GI.Gtk as Gtk
import Shapes
import Truth.Core
import Truth.UI.GTK.GView
import Truth.UI.GTK.Useful

cssGetView :: GetGView
cssGetView =
    mconcat
        [ MkGetView $ \getview uispec -> do
              MkUIName name spec <- isUISpec uispec
              return $ do
                  w <- getview spec
                  #setName w name
                  return w
        , MkGetView $ \getview uispec -> do
              MkUICSSClass cssclass spec <- isUISpec uispec
              return $ do
                  w <- getview spec
                  sc <- #getStyleContext w
                  #addClass sc cssclass
                  return w
        , MkGetView $ \getview uispec -> do
              MkUICSSStyleSheet full priority css spec <- isUISpec uispec
              return $ do
                  w <- getview spec
                  provider <- new CssProvider []
                  #loadFromData provider $ encodeUtf8 css
                  children <- liftIO $ widgetGetTree full w
                  for_ children $ \child -> do
                      sc <- #getStyleContext child
                      #addProvider sc provider priority
                  return w
        ]
