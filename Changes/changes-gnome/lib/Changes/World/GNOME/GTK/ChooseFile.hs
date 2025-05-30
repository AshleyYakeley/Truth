module Changes.World.GNOME.GTK.ChooseFile
    ( chooseFile
    )
where

import GI.Gio as GI
import GI.Gtk as GI
import Shapes

import Changes.World.GNOME.GI

chooseFile :: FileChooserAction -> (Maybe (Text, Text) -> Bool) -> GView 'Locked (Maybe File)
chooseFile action test =
    gvSubLifecycle $ do
        dialog <- gvNew FileChooserNative [#action := action]
        ffilter <- new FileFilter [] -- don't unref
        fileFilterAddCustom ffilter [FileFilterFlagsMimeType] $ \finfo -> do
            mtype <- getFileFilterInfoMimeType finfo
            return
                $ test
                $ do
                    mediatype <- mtype
                    case splitWhen ((==) '/') mediatype of
                        [t, s] -> return (t, s)
                        _ -> Nothing
        fileChooserAddFilter dialog ffilter
        res <- #run dialog
        mpath <-
            case toEnum $ fromIntegral res of
                ResponseTypeAccept -> do
                    f <- fileChooserGetFile dialog
                    gvAcquire f
                    return $ Just f
                _ -> return Nothing
        return mpath
