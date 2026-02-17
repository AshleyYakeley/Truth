module Changes.World.GNOME.GTK.ChooseFile
    ( chooseFile
    )
where

import Changes.World.GNOME.GI
import Import
import Import.GI qualified as GI

chooseFile :: GI.FileChooserAction -> Maybe [(Text,Maybe Text)] -> GView 'Locked (StoppableTask (GView 'Unlocked) GI.File)
chooseFile action mMimeTypes =
    gvSubLifecycle $ do
        dialog <- gvNew GI.FileChooserDialog [#action GI.:= action]
        for_ mMimeTypes $ \mimeTypes -> do
            ffilter <- GI.new GI.FileFilter [] -- don't unref
            for_ mimeTypes $ \(tt,mst) -> GI.fileFilterAddMimeType ffilter $ tt <> "/" <> (fromMaybe "*" mst)
            GI.fileChooserAddFilter dialog ffilter
        (reportTask,stoppableTaskTask) <- gvLiftIONoUI $ gvMkTask @(Maybe GI.File)
        _ <- gvOnSignal dialog #response $ \res ->
            case toEnum $ fromIntegral res of
                GI.ResponseTypeAccept -> do
                    mf <- GI.fileChooserGetFile dialog
                    for_ mf gvAcquire
                    gvLiftIONoUI $ reportTask mf
                _ -> gvLiftIONoUI $ reportTask Nothing
        _ <- gvOnSignal dialog #close $
            gvLiftIONoUI $ reportTask Nothing
        let
            stoppableTaskStop :: GView 'Unlocked ()
            stoppableTaskStop = do
                gvRunLocked $ GI.windowClose dialog
                gvLiftIONoUI $ reportTask Nothing
        return  MkStoppableTask {..}
