module Changes.World.GNOME.GI.Error where

import Import
import Import.GI qualified as GI

data GTKError = MkGTKError
    { gtkerrDomain :: Word32
    , gtkerrCode :: Int32
    , gtkerrMessage :: Text
    }

instance Show GTKError where
    show MkGTKError{..} = (unpack gtkerrMessage) <> " (" <> show gtkerrDomain <> ": " <> show gtkerrCode <> ")"

getGTKError :: GI.GError -> IO GTKError
getGTKError err = do
    gtkerrDomain <- GI.gerrorDomain err
    gtkerrCode <- GI.gerrorCode err
    gtkerrMessage <- GI.gerrorMessage err
    return MkGTKError{..}

catchGTKNull :: IO a -> IO (Maybe a)
catchGTKNull ioa = catch (fmap Just ioa) $ \(_ :: GI.UnexpectedNullPointerReturn) -> return Nothing
