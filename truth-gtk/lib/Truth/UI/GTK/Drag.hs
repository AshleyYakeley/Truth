module Truth.UI.GTK.Drag
    ( dragGetView
    ) where

import Shapes
    --import GI.Gtk;

import Truth.Core
import Truth.UI.GTK.GView

dragSourceGetView :: GetGView
dragSourceGetView =
    MkGetView $ \getview uispec -> do
        MkDragSourceUISpec _typename _lens spec <- isUISpec uispec
        return $ getview spec

dragDestinationGetView :: GetGView
dragDestinationGetView =
    MkGetView $ \getview uispec -> do
        MkDragDestinationUISpec _typename _lens spec <- isUISpec uispec
        return $ getview spec

dragGetView :: GetGView
dragGetView = dragSourceGetView <> dragDestinationGetView
