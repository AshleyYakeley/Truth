module Changes.World.GNOME.GI.Derived where

import qualified Data.GI.Base.GObject as GI
import qualified Data.GI.Base.Overloading as GI
import Foreign
import qualified GI.GLib as GI
import Shapes

withGObjectClass ::
       forall a r. GI.ManagedPtrNewtype a
    => GI.GObjectClass
    -> (a -> IO r)
    -> IO r
withGObjectClass cl = GI.withTransient (coerce cl)

newtype GIObject (t :: Type) =
    MkGIObject (GI.ManagedPtr (GIObject t))

instance GI.ManagedPtrNewtype (GIObject t) where
    toManagedPtr (MkGIObject mptr) = mptr

class (GI.GObject (GIParent t), Is (ListType (Compose Dict GI.TypedObject)) (GIInterfaces t)) => IsGIType (t :: Type) where
    type GIParent t :: Type
    type GIInterfaces t :: [Type]
    type GIParents t :: [Type]
    giGetGType :: IO GI.GType
    default giGetGType :: IsGIDerived t => IO GI.GType
    giGetGType = GI.registerGType $ MkGIObject @t

type instance GI.ParentTypes (GIObject t) = GIParents t

instance IsGIType t => GI.HasParentTypes (GIObject t)

instance IsGIType t => GI.TypedObject (GIObject t) where
    glibType = giGetGType @t

instance IsGIType t => GI.GObject (GIObject t)

instance IsGIType t => GI.HasAttributeList (GIObject t)

type instance GI.AttributeList (GIObject t) = GI.AttributeList (GIParent t)

class IsGIType t => IsGIDerived (t :: Type) where
    giTypeName :: Text
    giSetupClasses :: GI.GObjectClass -> IO ()

getPrivate ::
       forall parent t. (GI.GObject parent, IsGIDerived t)
    => parent
    -> IO t
getPrivate pobj = do
    Just obj <- GI.castTo MkGIObject pobj
    GI.gobjectGetPrivateData obj

withPrivatePtr ::
       forall parent t r. (GI.GObject parent, IsGIDerived t)
    => Ptr parent
    -> (t -> IO r)
    -> IO r
withPrivatePtr ptr call =
    GI.withTransient ptr $ \pobj -> do
        private <- getPrivate pobj
        call private

instance IsGIDerived t => GI.DerivedGObject (GIObject t) where
    type GObjectParentType (GIObject t) = GIParent t
    type GObjectPrivateData (GIObject t) = t
    objectTypeName = giTypeName @t
    objectClassInit cl = giSetupClasses @t cl
    objectInstanceInit _ _ = return $ error $ "uninitialised " <> unpack (giTypeName @t)
    objectInterfaces :: [(IO GI.GType, Ptr () -> IO (), Maybe (Ptr () -> IO ()))]
    objectInterfaces = let
        rep :: ListType (Compose Dict GI.TypedObject) (GIInterfaces t)
        rep = representative
        toItem :: forall a. Compose Dict GI.TypedObject a -> (IO GI.GType, Ptr () -> IO (), Maybe (Ptr () -> IO ()))
        toItem (Compose Dict) = (GI.glibType @a, \_ -> return (), Nothing)
        in listTypeToList toItem rep

createDerivedGIObject :: IsGIDerived t => t -> IO (GIObject t)
createDerivedGIObject t = do
    o <- GI.constructGObject MkGIObject []
    GI.gobjectSetPrivateData o t
    return o
