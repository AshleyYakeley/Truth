module Changes.UI.GTK.ChooseFile
    ( chooseFile
    ) where

import GI.GLib as GI hiding (String)
import GI.Gtk as GI
import Shapes

chooseFile :: IO (Maybe FilePath)
chooseFile = do
    dialog <- new FileChooserDialog [#action := FileChooserActionOpen]
    _ <- #addButton dialog "Cancel" $ fromIntegral $ fromEnum ResponseTypeCancel
    _ <- #addButton dialog "Copy" $ fromIntegral $ fromEnum ResponseTypeOk
    res <- #run dialog
    mpath <-
        case toEnum $ fromIntegral res of
            ResponseTypeOk -> fileChooserGetFilename dialog
            _ -> return Nothing
    #destroy dialog
    return mpath
