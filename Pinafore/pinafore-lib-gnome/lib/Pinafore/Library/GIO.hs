{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Library.GIO
    ( LangFile
    , gioStuff
    )
where

import Changes.Core
import Changes.World.GNOME.GIO.File
import GI.Gio qualified as GI
import Pinafore.API
import Pinafore.Library.Media
import Shapes
import Shapes.Unsafe

-- File
type LangFile = GI.File

fileGroundType :: QGroundType '[] LangFile
fileGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangFile)|]) "File.GIO."

instance HasQGroundType '[] LangFile where
    qGroundType = fileGroundType

uriToFile :: Text -> LangFile
uriToFile t = unsafePerformIO $ GI.fileNewForUri t

fileToURI :: LangFile -> Text
fileToURI f = unsafePerformIO $ GI.fileGetUri f

pathToFile :: Text -> LangFile
pathToFile t = unsafePerformIO $ GI.fileNewForPath $ unpack t

fileToPath :: LangFile -> Maybe Text
fileToPath f = fmap pack $ unsafePerformIO $ GI.fileGetPath f

literalConv :: Bijection (Maybe (Text, LazyByteString)) (Maybe Media)
literalConv =
    MkIsomorphism
        { isoForwards =
            \mtb -> do
                (mediatype, b) <- mtb
                case splitWhen ((==) '/') mediatype of
                    [t, s] -> return $ MkMedia (MkMediaType t s []) $ toStrict b
                    _ -> Nothing
        , isoBackwards =
            \ml -> do
                MkMedia (MkMediaType t s _) b <- ml
                return (t <> "/" <> s, fromStrict b)
        }

fileMakeRef :: LangFile -> Action (LangWholeModel '(Media, Media))
fileMakeRef f = do
    fref <- liftIO $ giFileReference f
    (model :: Model (MaybeUpdate (PairUpdate (WholeUpdate Text) ByteStringUpdate)), ()) <-
        actionLiftLifecycle $ makeSharedModel $ reflectingPremodel fref
    return $ wModelToWholeModel $ eaMap (bijectionWholeChangeLens $ invert knowMaybe . literalConv) $ MkWModel model

gioStuff :: LibraryStuff
gioStuff =
    headingBDS "GIO" "GNOME file access."
        $ pure
        $ namespaceBDS
            "GIO"
            [ typeBDS
                "File"
                "A file."
                (MkSomeGroundType fileGroundType)
                [ valPatBDS "URI" "Construct a file from a URI." uriToFile $ PureFunction $ pure $ \f -> (fileToURI f, ())
                , valPatBDS "Path" "Construct a file from a local path." pathToFile
                    $ ImpureFunction
                    $ pure
                    $ \f -> do
                        p <- fileToPath f
                        return (p, ())
                ]
            , namespaceBDS "File" [valBDS "makeRef" "Make a reference from a file." fileMakeRef]
            ]
