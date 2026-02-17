module Changes.World.GNOME.GIO.ReferenceStream
    ( ReferenceInputStream
    , byteStringReferenceInputStream
    )
where

import Data.ByteString.Unsafe (unsafeUseAsCString)
import Data.IORef
import Foreign

import Changes.World.GNOME.GI
import Import
import Import.GI qualified as GI

data Private = MkPrivate
    { privReadable :: Readable IO ByteStringReader
    , privClose :: IO ()
    , privOffset :: IORef Int64
    }

type ReferenceInputStream = GIObject Private

instance IsGIType Private where
    type GIClassParent Private = GI.InputStream
    type GISupertypes Private = '[GI.Object, GI.InputStream, GI.Seekable]

closeFnMethod :: GI.C_InputStreamClassCloseFnFieldCallback
closeFnMethod pis _ _ =
    withPrivatePtr pis $ \MkPrivate{..} -> do
        privClose
        return 0

readFnMethod :: GI.C_InputStreamClassReadFnFieldCallback
readFnMethod pis p (fromIntegral -> n) _ _ =
    withPrivatePtr pis $ \MkPrivate{..} -> do
        offset <- readIORef privOffset
        bs <- privReadable $ ReadByteStringSection offset n
        let
            l :: Int64
            l = min n $ olength64 bs
        writeIORef privOffset $ offset + l
        unsafeUseAsCString (toStrict bs) $ \c -> copyBytes (castPtr p) c (fromIntegral l)
        return l

skipMethod :: GI.C_InputStreamClassSkipFieldCallback
skipMethod pis (fromIntegral -> n) _ _ =
    withPrivatePtr pis $ \MkPrivate{..} -> do
        offset <- readIORef privOffset
        len <- privReadable ReadByteStringLength
        let
            l :: Int64
            l = min n (len - offset)
        writeIORef privOffset $ offset + l
        return l

seekMethod :: GI.C_SeekableIfaceSeekFieldCallback
seekMethod ptr offset seekType _ _ =
    withPrivatePtr ptr $ \MkPrivate{..} -> do
        len <- privReadable ReadByteStringLength
        base <-
            case toEnum $ fromEnum seekType of
                GI.SeekTypeCur -> readIORef privOffset
                GI.SeekTypeSet -> return 0
                GI.SeekTypeEnd -> return len
                _ -> return 0
        let newpos = max 0 $ min len $ base + offset
        writeIORef privOffset newpos
        return 0

tellMethod :: GI.SeekableIfaceTellFieldCallback
tellMethod obj = do
    MkPrivate{..} <- getPrivate obj
    readIORef privOffset

instance GIImplement Private GI.InputStreamClass where
    giImplement isc = do
        closeCB <- GI.mk_InputStreamClassCloseFnFieldCallback closeFnMethod
        GI.set isc [#closeFn GI.:&= closeCB]
        skipCB <- GI.mk_InputStreamClassSkipFieldCallback skipMethod
        GI.set isc [#skip GI.:&= skipCB]
        readFnCB <- GI.mk_InputStreamClassReadFnFieldCallback readFnMethod
        GI.set isc [#readFn GI.:&= readFnCB]

instance GIImplement Private GI.SeekableIface where
    giImplement iface = do
        GI.set iface [#canSeek GI.:&= \_ -> return True]
        GI.set iface [#canTruncate GI.:&= \_ -> return False]
        seekCB <- GI.mk_SeekableIfaceSeekFieldCallback $ seekMethod
        GI.set iface [#seek GI.:&= seekCB]
        GI.set iface [#tell GI.:&= tellMethod]

instance IsGIDerived Private where
    type GIImplementClasses Private = '[GI.InputStream]
    type GIImplementInterfaces Private = '[GI.Seekable]
    giTypeName = "ReferenceInputStream"

byteStringReferenceInputStream :: Reference ByteStringEdit -> View ReferenceInputStream
byteStringReferenceInputStream ref = do
    (aref, privClose) <- viewGetCloser $ viewRunResourceLifecycle ref
    let
        privReadable :: Readable IO ByteStringReader
        privReadable = refRead aref
    privOffset <- liftIO $ newIORef 0
    liftIO $ createDerivedGIObject $ MkPrivate{..}
