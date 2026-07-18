module Changes.World.GNOME.GTK.ChooseFile
    ( chooseFile
    )
where

import Changes.World.GNOME.GI
import Import
import Import.GI qualified as GI

createFileFilter :: [(Text, Maybe Text)] -> GView 'Locked GI.FileFilter
createFileFilter mimeTypes = do
    ffilter <- GI.fileFilterNew
    for_ mimeTypes $ \(tt, mst) -> GI.fileFilterAddMimeType ffilter $ tt <> "/" <> (fromMaybe "*" mst)
    return ffilter

setFileFilters :: GI.FileDialog -> Maybe [(Text, Maybe Text)] -> GView 'Locked ()
setFileFilters dialog mMimeTypes =
    for_ mMimeTypes $ \mimeTypes -> do
        ffilter <- createFileFilter mimeTypes
        fileFilterType <- gvLiftIO $ GI.glibType @GI.FileFilter
        filters <- GI.listStoreNew fileFilterType
        GI.listStoreAppend filters ffilter
        GI.fileDialogSetFilters dialog $ Just filters
        GI.fileDialogSetDefaultFilter dialog $ Just ffilter

finishFileDialog :: IO GI.File -> IO (Maybe GI.File)
finishFileDialog finish =
    GI.catchIOErrorEnum
        ( GI.catchDialogError (Just <$> finish) $ \case
            GI.DialogErrorCancelled -> const $ return Nothing
            GI.DialogErrorDismissed -> const $ return Nothing
            _ -> fail . unpack
        )
        $ \case
            GI.IOErrorEnumCancelled -> const $ return Nothing
            _ -> fail . unpack

chooseFile :: GI.FileChooserAction -> GI.Window -> Maybe [(Text, Maybe Text)] -> GView 'Locked (StoppableTask (GView 'Unlocked) GI.File)
chooseFile action window mMimeTypes = do
    dialog <- GI.fileDialogNew
    setFileFilters dialog mMimeTypes
    (reportTask, stoppableTaskTask) <- gvLiftIOTrustMeNoUI $ gvMkTask @(Maybe GI.File)
    cancellable <- GI.cancellableNew
    let
        callback :: (GI.AsyncResult -> IO GI.File) -> GTKCallbackUnlift () -> GI.AsyncReadyCallback
        callback finishAction unlift _ result =
            unlift $ do
                mf <- gvLiftIO $ finishFileDialog $ finishAction result
                for_ mf gvBind
                gvLiftIOTrustMeNoUI $ reportTask mf
        start :: Maybe GI.AsyncReadyCallback -> GView 'Locked ()
        start =
            case action of
                GI.FileChooserActionOpen -> GI.fileDialogOpen dialog (Just window) (Just cancellable)
                GI.FileChooserActionSave -> GI.fileDialogSave dialog (Just window) (Just cancellable)
                GI.FileChooserActionSelectFolder -> GI.fileDialogSelectFolder dialog (Just window) (Just cancellable)
                GI.AnotherFileChooserAction i -> const $ fail $ "Unsupported file chooser action: " <> show i
        finish :: GI.AsyncResult -> IO GI.File
        finish =
            case action of
                GI.FileChooserActionOpen -> GI.fileDialogOpenFinish dialog
                GI.FileChooserActionSave -> GI.fileDialogSaveFinish dialog
                GI.FileChooserActionSelectFolder -> GI.fileDialogSelectFolderFinish dialog
                GI.AnotherFileChooserAction i -> const $ fail $ "Unsupported file chooser action: " <> show i
    gvWithCallbackUnlift () $ \unlift -> start $ Just $ callback finish unlift
    let
        stoppableTaskStop :: GView 'Unlocked ()
        stoppableTaskStop = do
            gvRunLocked $ GI.cancellableCancel $ Just cancellable
            gvLiftIOTrustMeNoUI $ reportTask Nothing
    return MkStoppableTask{..}
