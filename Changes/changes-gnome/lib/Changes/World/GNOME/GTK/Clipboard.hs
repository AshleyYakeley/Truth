module Changes.World.GNOME.GTK.Clipboard where

import Changes.Core
import Changes.World.GNOME.GI
import GI.GLib as GI ()
import GI.GdkPixbuf as GI
import GI.Gtk as GI ()
import Shapes

data Clip
    = TextClip Text
    | RichTextClip Text
    | ImageClip Pixbuf

getClipboard :: GView 'Locked (Model (WholeUpdate (Maybe Clip)))
getClipboard = do gvLiftViewNoUI $ viewLiftLifeCycle $ makeMemoryModel Nothing
    -- NYI #18
    --clipboard <- #get selectionClipboard
    --return $ MkResource nilResourceRunner foo
