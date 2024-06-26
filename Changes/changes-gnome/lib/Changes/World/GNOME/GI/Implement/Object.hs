module Changes.World.GNOME.GI.Implement.Object where

import qualified Data.GI.Base.GObject as GI
import qualified Data.GI.Base.Overloading as GI
import qualified GI.GLib as GI
import Shapes

newtype GIObject (t :: Type) =
    MkGIObject (GI.ManagedPtr (GIObject t))

instance GI.ManagedPtrNewtype (GIObject t) where
    toManagedPtr (MkGIObject mptr) = mptr

class (GI.GObject (GIClassParent t)) => IsGIType (t :: Type) where
    type GIClassParent t :: Type
    type GISupertypes t :: [Type]
    giGetGType :: IO GI.GType
    default giGetGType ::
        (GI.GObject (GI.GObjectParentType (GIObject t)), GI.DerivedGObject (GIObject t)) => IO GI.GType
    giGetGType = GI.registerGType $ MkGIObject @t

type instance GI.ParentTypes (GIObject t) = GISupertypes t

instance IsGIType t => GI.HasParentTypes (GIObject t)

instance IsGIType t => GI.TypedObject (GIObject t) where
    glibType = giGetGType @t

instance IsGIType t => GI.GObject (GIObject t)

instance IsGIType t => GI.HasAttributeList (GIObject t)

type instance GI.AttributeList (GIObject t) = GI.AttributeList (GIClassParent t)
