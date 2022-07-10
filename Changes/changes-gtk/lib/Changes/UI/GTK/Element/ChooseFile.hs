module Changes.UI.GTK.Element.ChooseFile
    ( chooseFile
    ) where

import Changes.GI

--import GI.GLib as GI hiding (String)
import GI.Gtk as GI
import Shapes

chooseFile :: GView 'Locked (Maybe FilePath)
chooseFile =
    gvSubLifeCycle $ do
        dialog <- gvTopLevelNew FileChooserDialog [#action := FileChooserActionOpen]
        _ <- #addButton dialog "Cancel" $ fromIntegral $ fromEnum ResponseTypeCancel
        _ <- #addButton dialog "Copy" $ fromIntegral $ fromEnum ResponseTypeOk
        res <- #run dialog
        mpath <-
            case toEnum $ fromIntegral res of
                ResponseTypeOk -> fileChooserGetFilename dialog
                _ -> return Nothing
        return mpath
