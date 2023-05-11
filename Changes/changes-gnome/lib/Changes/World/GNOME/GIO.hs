module Changes.World.GNOME.GIO
    ( giFileReference
    ) where

import Changes.Core
import Changes.World.GNOME.GI
import qualified GI.GLib as GI
import qualified GI.GObject as GI
import qualified GI.Gio as GI
import Shapes

noCancellable :: Maybe GI.Cancellable
noCancellable = Nothing

openGIFile :: MonadIO m => GI.File -> m (Maybe GI.FileIOStream)
openGIFile path =
    liftIO $ do
        mh <- GI.catchIOErrorEnum (fmap Just $ GI.fileOpenReadwrite path noCancellable) $ \_ _ -> return Nothing
        for_ mh GI.objectRef
        return mh

closeGIFile :: MonadIO m => GI.FileIOStream -> m ()
closeGIFile h = GI.objectUnref h

getLength :: MonadIO m => GI.FileIOStream -> GI.File -> m Int64
getLength _h path = do
    info <- GI.fileQueryInfo path GI.FILE_ATTRIBUTE_STANDARD_SIZE [GI.FileQueryInfoFlagsNone] noCancellable
    GI.fileInfoGetSize info

setLength :: MonadIO m => GI.FileIOStream -> Int64 -> m ()
setLength h len = GI.seekableTruncate h len noCancellable

seek :: MonadIO m => GI.FileIOStream -> Int64 -> m ()
seek h i = GI.seekableSeek h i GI.SeekTypeSet noCancellable

getMIMEType :: MonadIO m => GI.FileIOStream -> GI.File -> m Text
getMIMEType _h path = do
    info <- GI.fileQueryInfo path GI.FILE_ATTRIBUTE_STANDARD_CONTENT_TYPE [GI.FileQueryInfoFlagsNone] noCancellable
    mtype <- GI.fileInfoGetContentType info
    return $ giToText mtype

setMIMEType :: MonadIO m => GI.FileIOStream -> GI.File -> Text -> m ()
setMIMEType _h path val =
    GI.fileSetAttributeString path GI.FILE_ATTRIBUTE_STANDARD_CONTENT_TYPE val [GI.FileQueryInfoFlagsNone] noCancellable

type T = StateT (Maybe GI.FileIOStream)

fileWitness :: IOWitness T
fileWitness = $(iowitness [t|T|])

type M = T IO

giFileReference :: GI.File -> IO (Reference (MaybeEdit (PairUpdateEdit (WholeUpdate Text) ByteStringUpdate)))
giFileReference path = do
    mbn <- GI.fileGetBasename path
    case mbn of
        Nothing -> fail "bad File"
        Just bn ->
            return $ let
                iow :: IOWitness T
                iow = hashOpenWitness fileWitness bn
                objRun :: ResourceRunner '[ T]
                objRun =
                    mkResourceRunner iow $ \rt -> do
                        oldmh <- openGIFile path
                        (r, newmh) <- runStateT rt oldmh
                        for_ newmh closeGIFile
                        return r
                refRead :: Readable M (OneReader Maybe (PairUpdateReader (WholeUpdate Text) ByteStringUpdate))
                refRead ReadHasOne = fmap (fmap (\_ -> ())) get
                refRead (ReadOne (MkTupleUpdateReader SelectFirst ReadWhole)) = do
                    mh <- get
                    for mh $ \h -> getMIMEType h path
                refRead (ReadOne (MkTupleUpdateReader SelectSecond ReadByteStringLength)) = do
                    mh <- get
                    for mh $ \h -> getLength h path
                refRead (ReadOne (MkTupleUpdateReader SelectSecond (ReadByteStringSection start len))) = do
                    mh <- get
                    for mh $ \h -> do
                        seek h start
                        st <- GI.iOStreamGetInputStream h
                        bytes <- GI.inputStreamReadBytes st (fromIntegral len) noCancellable
                        mbs <- GI.bytesGetData bytes
                        return $ fromStrict $ fromMaybe mempty mbs
                objOneEdit :: MaybeEdit (PairUpdateEdit (WholeUpdate Text) ByteStringUpdate) -> EditSource -> M ()
                objOneEdit (SuccessFullResultOneEdit (MkTupleUpdateEdit SelectFirst (MkWholeReaderEdit val))) _ = do
                    mh <- get
                    for_ mh $ \h -> setMIMEType h path val
                objOneEdit (SuccessFullResultOneEdit (MkTupleUpdateEdit SelectSecond (ByteStringSetLength len))) _ = do
                    mh <- get
                    for_ mh $ \h -> setLength h len
                objOneEdit (SuccessFullResultOneEdit (MkTupleUpdateEdit SelectSecond (ByteStringWrite start bs))) _ = do
                    mh <- get
                    for_ mh $ \h -> do
                        oldlen <- getLength h path
                        if start > oldlen
                            then setLength h start
                            else return ()
                        seek h start
                        st <- GI.iOStreamGetOutputStream h
                        _ <- GI.outputStreamWrite st (toStrict bs) noCancellable
                        return ()
                objOneEdit (NewFullResultOneEdit Nothing) _ = do
                    mh <- get
                    case mh of
                        Just h -> do
                            closeGIFile h
                            GI.fileDelete path noCancellable
                            put Nothing
                        Nothing -> return ()
                objOneEdit (NewFullResultOneEdit (Just (mimetype, bs))) _ = do
                    mh <- get
                    h <-
                        case mh of
                            Just h -> return h
                            Nothing -> do
                                h <- GI.fileCreateReadwrite path [GI.FileCreateFlagsNone] noCancellable
                                put $ Just h
                                return h
                    seek h 0
                    st <- GI.iOStreamGetOutputStream h
                    _ <- GI.outputStreamWrite st (toStrict bs) noCancellable
                    setLength h $ fromIntegral $ olength bs
                    setMIMEType h path mimetype
                refEdit ::
                       NonEmpty (MaybeEdit (PairUpdateEdit (WholeUpdate Text) ByteStringUpdate))
                    -> M (Maybe (EditSource -> M ()))
                refEdit = singleAlwaysEdit objOneEdit
                refCommitTask :: Task IO ()
                refCommitTask = mempty
                in MkResource objRun MkAReference {..}
