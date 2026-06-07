module Changes.World.GNOME.GI.Type where

import Import
import Import.GI qualified as GI

-- workaround for GNOME version hell
class GIToText a where
    giToText :: a -> Text

instance GIToText Text where
    giToText t = t

instance GIToText (Maybe Text) where
    giToText mt = fromMaybe "" mt

getObjectType :: (MonadIO m, GI.GObject a) => a -> m GI.GType
getObjectType v = liftIO $ GI.gtypeFromInstance v

getTypeParent :: MonadIO m => GI.GType -> m (Maybe GI.GType)
getTypeParent t = do
    p <- GI.typeParent t
    return
        $ if p == GI.gtypeInvalid
            then Nothing
            else Just p

getTypeName :: MonadIO m => GI.GType -> m Text
getTypeName t = fmap giToText $ GI.typeName t

getTypeAncestry :: MonadIO m => GI.GType -> m [GI.GType]
getTypeAncestry t = do
    mp <- getTypeParent t
    case mp of
        Nothing -> return [t]
        Just p -> do
            rest <- getTypeAncestry p
            return $ t : rest

getObjectTypeAncestry :: (MonadIO m, GI.GObject a) => a -> m [GI.GType]
getObjectTypeAncestry v = do
    t <- getObjectType v
    getTypeAncestry t

getObjectTypeName :: (MonadIO m, GI.GObject a) => a -> m Text
getObjectTypeName a = do
    t <- getObjectType a
    getTypeName t
