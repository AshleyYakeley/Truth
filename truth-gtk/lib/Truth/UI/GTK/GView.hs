module Truth.UI.GTK.GView where

import GI.Gtk
import Truth.Core

type GCreateView sel update = CreateView sel update Widget

type GetGView = GetView Widget
