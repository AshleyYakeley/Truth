module Changes.World.GNOME.GTK.Clipboard (getTheClipboardModel) where

import Data.ByteString (packCStringLen)
import Foreign.Ptr

import Changes.World.GNOME.GI
import Import
import Import.GI qualified as GI

getFormatsMimeTypes :: GI.ContentFormats -> GView 'Locked [Text]
getFormatsMimeTypes cf = do
    (mtypes, _) <- GI.contentFormatsGetMimeTypes cf
    return $ fromMaybe [] mtypes

memoryOutputStreamGetByteString :: GI.MemoryOutputStream -> GView 'Locked StrictByteString
memoryOutputStreamGetByteString stream = do
    ptr <- GI.memoryOutputStreamGetData stream
    sz <- GI.memoryOutputStreamGetDataSize stream
    liftIO $ packCStringLen (castPtr ptr, fromIntegral sz)

mkCancellableTask :: GView 'Locked (a -> GView 'Locked (), GI.Cancellable, StoppableTask (GView 'Unlocked) a)
mkCancellableTask = do
    cancellable <- GI.cancellableNew
    (putval, stoppableTaskTask) <- gvMkTask
    let
        stoppableTaskStop :: GView 'Unlocked ()
        stoppableTaskStop = do
            gvRunLocked $ GI.cancellableCancel $ Just cancellable
            unWGViewAny $ putval Nothing
    return (\a -> unWGViewAny $ putval $ Just a, cancellable, MkStoppableTask{..})

getProviderContentsTask ::
    GI.ContentProvider -> Text -> GView 'Locked (StoppableTask (GView 'Unlocked) StrictByteString)
getProviderContentsTask provider mimeType = do
    (putVal, cancellable, stask) <- mkCancellableTask
    stream <- GI.memoryOutputStreamNewResizable
    let
        callback :: GTKCallbackUnlift () -> GI.AsyncReadyCallback
        callback unlift _ result = unlift $ do
            GI.contentProviderWriteMimeTypeFinish provider result
            bs <- memoryOutputStreamGetByteString stream
            putVal bs
    gvWithCallbackUnlift () $ \unlift -> GI.contentProviderWriteMimeTypeAsync provider mimeType stream 0 (Just cancellable) (Just $ callback unlift)
    return stask

singleProvider :: Text -> StrictByteString -> GView 'Locked GI.ContentProvider
singleProvider mimeType bs = do
    bytes <- GI.bytesNew $ Just bs
    GI.contentProviderNewForBytes mimeType bytes

unionProviders :: NonEmpty GI.ContentProvider -> GView 'Locked GI.ContentProvider
unionProviders = \case
    provider :| [] -> return provider
    providers -> GI.contentProviderNewUnion $ Just $ toList providers

mediaToProvider :: Media -> GView 'Locked GI.ContentProvider
mediaToProvider (MkMedia mediaType bs) = singleProvider (encode textMediaTypeCodec mediaType) bs

readClipboard :: GI.Clipboard -> GView 'Unlocked [Media]
readClipboard clipboard = do
    mediaTasks <- gvRunLocked $ do
        mprovider <- GI.clipboardGetContent clipboard
        case mprovider of
            Nothing -> return []
            Just provider -> do
                formats <- GI.contentProviderRefFormats provider
                mimeTypes <- getFormatsMimeTypes formats
                forf mimeTypes $ \mimeType -> do
                    for (decode textMediaTypeCodec mimeType) $ \mediaType -> do
                        bsTask <- getProviderContentsTask provider mimeType
                        return $ fmap (MkMedia mediaType) bsTask
    -- The completion callbacks need the GTK lock, so wait without holding it.
    forf mediaTasks $ \stask -> taskWait $ stoppableTaskTask stask

writeClipboard :: GI.Clipboard -> [Media] -> GView 'Locked Bool
writeClipboard clipboard medias = do
    mprovider <- for (nonEmpty medias) $ \nmedias -> do
        providers <- for nmedias mediaToProvider
        unionProviders providers
    GI.clipboardSetContent clipboard mprovider

getClipboardModel :: GI.Clipboard -> GView 'Unlocked (Model (WholeUpdate [Media]))
getClipboardModel clipboard = do
    MkWRaised unlift <- gvAskUnliftLifecycle
    let
        refReadIO :: Readable IO (WholeReader [Media])
        refReadIO ReadWhole = runLifecycle $ unlift $ readClipboard clipboard
        refEditIO :: NonEmpty (WholeEdit [Media]) -> IO (Maybe (EditSource -> IO ()))
        refEditIO edits =
            case last edits of
                MkWholeReaderEdit medias -> return $ Just $ \_ -> do
                    _ <- runLifecycle $ unlift $ gvRunLocked $ writeClipboard clipboard medias
                    return ()
        refRead :: Readable (ReaderT () IO) (WholeReader [Media])
        refRead r = liftIO $ refReadIO r
        refEdit :: NonEmpty (WholeEdit [Media]) -> ReaderT () IO (Maybe (EditSource -> ReaderT () IO ()))
        refEdit edits = do
            maction <- liftIO $ refEditIO edits
            return $ fmap (\action esrc -> liftIO $ action esrc) maction
        refCommitTask :: Task IO ()
        refCommitTask = mempty
        ref :: Reference (WholeEdit [Media])
        ref = MkResource (pure ()) MkAReference{..}
    gvLiftLifecycle $ makeReflectingModel ref

getTheClipboardModel :: GView 'Unlocked (Model (WholeUpdate [Media]))
getTheClipboardModel = do
    mdisplay <- gvRunLocked GI.displayGetDefault
    case mdisplay of
        Just display -> do
            clipboard <- gvRunLocked $ GI.displayGetClipboard display
            getClipboardModel clipboard
        Nothing -> fail "No GDK display"
