module Changes.World.GNOME.GI.Error where

import Data.GI.Base
import Shapes

data GTKError = MkGTKError
    { gtkerrDomain :: Word32
    , gtkerrCode :: Int32
    , gtkerrMessage :: Text
    }

instance Show GTKError where
    show MkGTKError{..} = (unpack gtkerrMessage) <> " (" <> show gtkerrDomain <> ": " <> show gtkerrCode <> ")"

getGTKError :: GError -> IO GTKError
getGTKError err = do
    gtkerrDomain <- gerrorDomain err
    gtkerrCode <- gerrorCode err
    gtkerrMessage <- gerrorMessage err
    return MkGTKError{..}

catchGTKNull :: IO a -> IO (Maybe a)
catchGTKNull ioa = catch (fmap Just ioa) $ \(_ :: UnexpectedNullPointerReturn) -> return Nothing
