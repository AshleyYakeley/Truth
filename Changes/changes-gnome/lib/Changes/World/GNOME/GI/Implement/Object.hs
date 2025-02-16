module Changes.World.GNOME.GI.Implement.Object where

import Import
import Import.GI qualified as GI

newtype GIObject (t :: Type)
    = MkGIObject (GI.ManagedPtr (GIObject t))

instance GI.ManagedPtrNewtype (GIObject t) where
    toManagedPtr (MkGIObject mptr) = mptr

class GI.GObject (GIClassParent t) => IsGIType (t :: Type) where
    type GIClassParent t :: Type
    type GISupertypes t :: [Type]
    giGetGType :: IO GI.GType
    default giGetGType ::
        (GI.GObject (GI.GObjectParentType (GIObject t)), GI.DerivedGObject (GIObject t)) => IO GI.GType
    giGetGType = GI.registerGType $ MkGIObject @t

type instance GI.ParentTypes (GIObject t) = GISupertypes t

instance GI.HasParentTypes (GIObject t)

instance IsGIType t => GI.TypedObject (GIObject t) where
    glibType = giGetGType @t

instance IsGIType t => GI.GObject (GIObject t)

instance GI.HasAttributeList (GIObject t)

type instance GI.AttributeList (GIObject t) = GI.AttributeList (GIClassParent t)
