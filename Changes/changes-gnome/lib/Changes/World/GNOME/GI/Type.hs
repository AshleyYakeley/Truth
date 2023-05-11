module Changes.World.GNOME.GI.Type where

import Data.GI.Base
import Data.GI.Base.GObject
import Data.GI.Base.GType
import GI.GObject
import Shapes

-- workaround for GNOME version hell
class GIToText a where
    giToText :: a -> Text

instance GIToText Text where
    giToText t = t

instance GIToText (Maybe Text) where
    giToText mt = fromMaybe "" mt

getObjectType :: (MonadIO m, GObject a) => a -> m GType
getObjectType v = liftIO $ gtypeFromInstance v

getTypeParent :: MonadIO m => GType -> m (Maybe GType)
getTypeParent t = do
    p <- typeParent t
    return $
        if p == gtypeInvalid
            then Nothing
            else Just p

getTypeName :: MonadIO m => GType -> m Text
getTypeName t = fmap giToText $ typeName t

getTypeAncestry :: MonadIO m => GType -> m [GType]
getTypeAncestry t = do
    mp <- getTypeParent t
    case mp of
        Nothing -> return [t]
        Just p -> do
            rest <- getTypeAncestry p
            return $ t : rest

getObjectTypeAncestry :: (MonadIO m, GObject a) => a -> m [GType]
getObjectTypeAncestry v = do
    t <- getObjectType v
    getTypeAncestry t

getObjectTypeName :: (MonadIO m, GObject a) => a -> m Text
getObjectTypeName a = do
    t <- getObjectType a
    getTypeName t
