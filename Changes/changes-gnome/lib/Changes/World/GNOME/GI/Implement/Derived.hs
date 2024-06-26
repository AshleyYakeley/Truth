{-# OPTIONS -fno-warn-orphans #-}

module Changes.World.GNOME.GI.Implement.Derived where

import Changes.World.GNOME.GI.Implement.Object
import qualified Data.GI.Base.GObject as GI
import Foreign
import qualified GI.GLib as GI
import qualified GI.Gio as GI
import Shapes

type family GIInterface (a :: Type) :: Type

class GI.ManagedPtrNewtype intf => GIImplement (t :: Type) (intf :: Type) where
    giImplement :: intf -> IO ()

data ImplementWit t a where
    MkImplementWit
        :: forall t a. (GI.TypedObject a, GIImplement t (GIInterface a))
        => ImplementWit t a

instance Representative (ImplementWit t) where
    getRepWitness MkImplementWit = Dict

instance (GI.TypedObject a, GIImplement t (GIInterface a)) => Is (ImplementWit t) a where
    representative = MkImplementWit

getClassImplementation :: forall t a. GI.GObjectClass -> ImplementWit t a -> IO ()
getClassImplementation cl MkImplementWit = GI.withTransient (coerce cl) $ giImplement @t @(GIInterface a)

getInterfaceImplementation :: forall t a. ImplementWit t a -> (IO GI.GType, Ptr () -> IO (), Maybe (Ptr () -> IO ()))
getInterfaceImplementation MkImplementWit = let
    intfGetType :: IO GI.GType
    intfGetType = GI.glibType @a
    intfInit :: Ptr () -> IO ()
    intfInit ptr = GI.withTransient (castPtr ptr) $ giImplement @t @(GIInterface a)
    intfFinal :: Maybe (Ptr () -> IO ())
    intfFinal = Nothing
    in (intfGetType, intfInit, intfFinal)

class ( IsGIType t
      , Is (ListType (ImplementWit t)) (GIImplementClasses t)
      , Is (ListType (ImplementWit t)) (GIImplementInterfaces t)
      ) => IsGIDerived (t :: Type) where
    type GIImplementClasses t :: [Type]
    type GIImplementInterfaces t :: [Type]
    giTypeName :: Text

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
    type GObjectParentType (GIObject t) = GIClassParent t
    type GObjectPrivateData (GIObject t) = t
    objectTypeName = giTypeName @t
    objectClassInit cl = let
        rep :: ListType (ImplementWit t) (GIImplementClasses t)
        rep = representative
        in listTypeFor_ rep (getClassImplementation cl)
    objectInstanceInit _ _ = return $ error $ "uninitialised " <> unpack (giTypeName @t)
    objectInterfaces :: [(IO GI.GType, Ptr () -> IO (), Maybe (Ptr () -> IO ()))]
    objectInterfaces = let
        rep :: ListType (ImplementWit t) (GIImplementInterfaces t)
        rep = representative
        in listTypeToList getInterfaceImplementation rep

createDerivedGIObject :: IsGIDerived t => t -> IO (GIObject t)
createDerivedGIObject t = do
    o <- GI.constructGObject MkGIObject []
    GI.gobjectSetPrivateData o t
    return o

--
type instance GIInterface GI.InputStream = GI.InputStreamClass

type instance GIInterface GI.Seekable = GI.SeekableIface
