module Changes.World.GNOME.GIO.ReferenceStream
    ( ReferenceInputStream
    , byteStringReferenceInputStream
    ) where

import Changes.Core
import qualified Data.GI.Base.GObject as GI
import qualified Data.GI.Base.Overloading as GI
import qualified GI.GLib as GI
import qualified GI.GObject as GI
import qualified GI.Gio as GI
import Shapes

newtype Private =
    MkPrivate (Readable IO ByteStringReader)

newtype ReferenceInputStream =
    MkReferenceInputStream (GI.ManagedPtr ReferenceInputStream)

instance GI.ManagedPtrNewtype ReferenceInputStream where
    toManagedPtr (MkReferenceInputStream mptr) = mptr

type instance GI.ParentTypes ReferenceInputStream = '[ GI.Object, GI.InputStream, GI.Seekable]

instance GI.HasParentTypes ReferenceInputStream

instance GI.DerivedGObject ReferenceInputStream where
    type GObjectParentType ReferenceInputStream = GI.InputStream
    type GObjectPrivateData ReferenceInputStream = Private
    objectTypeName = "ReferenceInputStream"
    objectClassInit _cl = do
        -- ISSUE #285: set up virtual methods here
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
    liftIO $ GI.gobjectSetPrivateData o $ MkPrivate $ refRead aref
    return o
