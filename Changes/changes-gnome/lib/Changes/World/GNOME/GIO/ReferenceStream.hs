module Changes.World.GNOME.GIO.ReferenceStream
    ( ReferenceInputStream
    , byteStringReferenceInputStream
    ) where

import Changes.Core
import Data.ByteString.Unsafe (unsafeUseAsCString)
import qualified Data.GI.Base.GObject as GI
import qualified Data.GI.Base.Overloading as GI
import Data.IORef
import Foreign
import qualified GI.GLib as GI
import qualified GI.GObject as GI
import qualified GI.Gio as GI
import Shapes

data Private = MkPrivate
    { privReadable :: Readable IO ByteStringReader
    , privOffset :: IORef Int64
    }

newtype ReferenceInputStream =
    MkReferenceInputStream (GI.ManagedPtr ReferenceInputStream)

instance GI.ManagedPtrNewtype ReferenceInputStream where
    toManagedPtr (MkReferenceInputStream mptr) = mptr

type instance GI.ParentTypes ReferenceInputStream = '[ GI.Object, GI.InputStream, GI.Seekable]

instance GI.HasParentTypes ReferenceInputStream

readFn :: GI.C_InputStreamClassReadFnFieldCallback
readFn pis p (fromIntegral -> n) _ _ =
    GI.withNewObject pis $ \is -> do
        Just ris <- GI.castTo MkReferenceInputStream is
        MkPrivate {..} <- GI.gobjectGetPrivateData ris
        offset <- readIORef privOffset
        bs <- privReadable $ ReadByteStringSection offset n
        let
            l :: Int64
            l = min n $ olength64 bs
        writeIORef privOffset $ offset + l
        unsafeUseAsCString (toStrict bs) $ \c -> copyBytes (castPtr p) c (fromIntegral l)
        return l

instance GI.DerivedGObject ReferenceInputStream where
    type GObjectParentType ReferenceInputStream = GI.InputStream
    type GObjectPrivateData ReferenceInputStream = Private
    objectTypeName = "ReferenceInputStream"
    objectClassInit cl = do
        Just isc <- error "ISSUE #285" cl -- ISSUE #285: set up virtual methods here
        readFnCB <- GI.mk_InputStreamClassReadFnFieldCallback readFn
        GI.setInputStreamClassReadFn isc readFnCB
        return ()
    objectInstanceInit _ _ = return $ error "uninitialised ReferenceInputStream"

instance GI.TypedObject ReferenceInputStream where
    glibType = GI.registerGType MkReferenceInputStream

instance GI.GObject ReferenceInputStream

instance GI.HasAttributeList ReferenceInputStream

type instance GI.AttributeList ReferenceInputStream = GI.AttributeList GI.InputStream

byteStringReferenceInputStream :: Reference ByteStringEdit -> View ReferenceInputStream
byteStringReferenceInputStream ref = do
    o <- liftIO $ GI.constructGObject MkReferenceInputStream []
    aref <- viewRunResourceLifecycle ref
    let
        privReadable :: Readable IO ByteStringReader
        privReadable = refRead aref
    privOffset <- liftIO $ newIORef 0
    liftIO $ GI.gobjectSetPrivateData o $ MkPrivate {..}
    return o
