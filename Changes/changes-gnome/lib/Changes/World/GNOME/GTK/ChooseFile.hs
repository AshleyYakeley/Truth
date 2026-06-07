module Changes.World.GNOME.GTK.ChooseFile
    ( chooseFile
    )
where

import Changes.World.GNOME.GI
import Import
import Import.GI qualified as GI

chooseFile :: GI.FileChooserAction -> GI.Window -> Maybe [(Text, Maybe Text)] -> GView 'Locked (StoppableTask (GView 'Unlocked) GI.File)
chooseFile action window mMimeTypes = do
    dialog <- gvNew GI.FileChooserDialog [#action GI.:= action]
    #setTransientFor dialog $ Just window
    for_ mMimeTypes $ \mimeTypes -> do
        ffilter <- GI.new GI.FileFilter [] -- don't unref
        for_ mimeTypes $ \(tt, mst) -> GI.fileFilterAddMimeType ffilter $ tt <> "/" <> (fromMaybe "*" mst)
        GI.fileChooserAddFilter dialog ffilter
    (reportTask, stoppableTaskTask) <- gvLiftIOTrustMeNoUI $ gvMkTask @(Maybe GI.File)
    _ <- gvOnSignal () dialog #response $ \res ->
        case toEnum $ fromIntegral res of
            GI.ResponseTypeAccept -> do
                mf <- GI.fileChooserGetFile dialog
                for_ mf gvBind
                gvLiftIOTrustMeNoUI $ reportTask mf
            _ -> gvLiftIOTrustMeNoUI $ reportTask Nothing
    _ <-
        gvOnSignal () dialog #close
            $ gvLiftIOTrustMeNoUI
            $ reportTask Nothing
    #present dialog
    let
        stoppableTaskStop :: GView 'Unlocked ()
        stoppableTaskStop = do
            gvRunLocked $ GI.windowClose dialog
            gvLiftIOTrustMeNoUI $ reportTask Nothing
    return MkStoppableTask{..}
