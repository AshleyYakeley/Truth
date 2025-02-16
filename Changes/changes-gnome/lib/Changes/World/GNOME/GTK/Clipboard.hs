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

mkCancellableTask :: GView 'Locked (a -> GView 'Locked (), GI.Cancellable, StoppableTask (GView 'Locked) a)
mkCancellableTask = do
    cancellable <- GI.cancellableNew
    (putval, stoppableTaskTask) <- mkTask
    let
        stoppableTaskStop :: GView 'Locked ()
        stoppableTaskStop = do
            GI.cancellableCancel $ Just cancellable
            putval Nothing
    return (putval . Just, cancellable, MkStoppableTask{..})

getProviderContentsTask ::
    GI.ContentProvider -> Text -> GView 'Locked (StoppableTask (GView 'Locked) StrictByteString)
getProviderContentsTask provider mimeType = do
    (putVal, cancellable, stask) <- mkCancellableTask
    stream <- GI.memoryOutputStreamNewResizable
    let
        callback :: (GView 'Locked --> IO) -> GI.AsyncReadyCallback
        callback unlift _ result = unlift $ do
            GI.contentProviderWriteMimeTypeFinish provider result
            bs <- memoryOutputStreamGetByteString stream
            putVal bs
    gvWithUnliftLockedAsync $ \unlift -> GI.contentProviderWriteMimeTypeAsync provider mimeType stream 0 (Just cancellable) (Just $ callback unlift)
    return stask

getProviderContents ::
    GI.ContentProvider -> Text -> GView 'Locked StrictByteString
getProviderContents provider mimeType = do
    task <- getProviderContentsTask provider mimeType
    mbs <- taskWait $ stoppableTaskTask task
    return $ fromMaybe mempty mbs

singleProvider :: Text -> StrictByteString -> GView 'Locked GI.ContentProvider
singleProvider mimeType bs = do
    bytes <- GI.bytesNew $ Just bs
    GI.contentProviderNewForBytes mimeType bytes

unionProviders :: NonEmpty GI.ContentProvider -> GView 'Locked GI.ContentProvider
unionProviders = \case
    provider :| [] -> return provider
    providers -> GI.contentProviderNewUnion $ Just $ toList providers

providerToMedia :: GI.ContentProvider -> GView 'Locked [Media]
providerToMedia provider = do
    formats <- GI.contentProviderRefFormats provider
    mimeTypes <- getFormatsMimeTypes formats
    forf mimeTypes $ \mimeType -> do
        for (decode textMediaTypeCodec mimeType) $ \mediaType -> do
            bs <- getProviderContents provider mimeType
            return $ MkMedia mediaType bs

mediaToProvider :: Media -> GView 'Locked GI.ContentProvider
mediaToProvider (MkMedia mediaType bs) = singleProvider (encode textMediaTypeCodec mediaType) bs

readClipboard :: GI.Clipboard -> GView 'Locked [Media]
readClipboard clipboard = do
    mprovider <- GI.clipboardGetContent clipboard
    case mprovider of
        Nothing -> return []
        Just provider -> providerToMedia provider

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
        refRead :: Readable IO (WholeReader [Media])
        refRead ReadWhole = runLifecycle $ unlift $ gvRunLocked $ readClipboard clipboard
        refEdit :: NonEmpty (WholeEdit [Media]) -> IO (Maybe (EditSource -> IO ()))
        refEdit edits =
            case last edits of
                MkWholeReaderEdit medias -> return $ Just $ \_ -> do
                    _ <- runLifecycle $ unlift $ gvRunLocked $ writeClipboard clipboard medias
                    return ()
        refCommitTask :: Task IO ()
        refCommitTask = mempty
        ref :: Reference (WholeEdit [Media])
        ref = MkResource nilResourceRunner $ MkAReference{..}
    gvLiftLifecycle $ makeReflectingModel ref

getTheClipboardModel :: GView 'Unlocked (Model (WholeUpdate [Media]))
getTheClipboardModel = do
    mdisplay <- gvRunLocked GI.displayGetDefault
    case mdisplay of
        Just display -> do
            clipboard <- gvRunLocked $ GI.displayGetClipboard display
            getClipboardModel clipboard
        Nothing -> fail "No GDK display"
