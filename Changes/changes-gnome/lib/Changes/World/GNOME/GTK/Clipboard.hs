module Changes.World.GNOME.GTK.Clipboard where

import Changes.Core
import Changes.World.GNOME.GI
import GI.GLib as GI ()
import GI.Gdk as GI
import GI.GdkPixbuf as GI
import GI.Gtk as GI
import Shapes

data Clip
    = TextClip Text
    | RichTextClip Text
    | ImageClip Pixbuf

getClipboard :: GView 'Unlocked (Model (WholeUpdate (Maybe Clip)))
getClipboard = do
    clipboard <-
        gvRunLocked $ do
            atom <- atomIntern "CLIPBOARD" False
            clipboardGet atom
    lock <- gvGetLock
    let
        writeClipboard :: Maybe Clip -> IO ()
        writeClipboard Nothing = cbRunLocked lock $ clipboardClear clipboard
        writeClipboard (Just (TextClip text)) = cbRunLocked lock $ clipboardSetText clipboard text (-1)
        writeClipboard (Just (ImageClip image)) = cbRunLocked lock $ clipboardSetImage clipboard image
        writeClipboard _ = return ()
        readClipboard :: IO (Maybe Clip)
        readClipboard = do
            mtext <- cbRunLocked lock $ clipboardWaitForText clipboard
            return $ fmap TextClip mtext
        refRead :: Readable IO (WholeReader (Maybe Clip))
        refRead ReadWhole = readClipboard
        refEdit :: NonEmpty (WholeEdit (Maybe Clip)) -> IO (Maybe (EditSource -> IO ()))
        refEdit edits =
            case last edits of
                MkWholeReaderEdit mclip -> return $ Just $ \_ -> writeClipboard mclip
        refCommitTask :: Task IO ()
        refCommitTask = mempty
        ref :: Reference (WholeEdit (Maybe Clip))
        ref = MkResource nilResourceRunner $ MkAReference {..}
    gvLiftLifecycle $ makeReflectingModel ref
