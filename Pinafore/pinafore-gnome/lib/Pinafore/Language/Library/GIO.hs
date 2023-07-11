{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.GIO
    ( LangFile
    , gioStuff
    ) where

import Changes.Core
import Changes.World.GNOME.GIO
import Changes.World.MIME
import qualified GI.Gio as GI
import Pinafore.Base
import Pinafore.Language.API
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

literalConv :: Bijection (Maybe (Text, LazyByteString)) (Maybe MIME)
literalConv =
    MkIsomorphism
        { isoForwards =
              \mtb -> do
                  (mimetype, b) <- mtb
                  case splitWhen ((==) '/') mimetype of
                      [t, s] -> return $ MkMIME (MkMIMEContentType t s []) $ toStrict b
                      _ -> Nothing
        , isoBackwards =
              \ml -> do
                  MkMIME (MkMIMEContentType t s _) b <- ml
                  return (t <> "/" <> s, fromStrict b)
        }

fileMakeRef :: LangFile -> Action (LangWholeModel '( MIME, MIME))
fileMakeRef f = do
    fref <- liftIO $ giFileReference f
    (model :: Model (MaybeUpdate (PairUpdate (WholeUpdate Text) ByteStringUpdate)), ()) <-
        actionLiftLifecycle $ makeSharedModel $ reflectingPremodel fref
    return $ wModelToWholeModel $ eaMap (bijectionWholeChangeLens $ invert knowMaybe . literalConv) $ MkWModel model

gioStuff :: BindDocStuff ()
gioStuff =
    headingBDS "GIO" "GNOME file access." $
    pure $
    namespaceBDS
        "GIO"
        ""
        [ typeBDS
              "File"
              "A file."
              (MkSomeGroundType fileGroundType)
              [ valPatBDS "URI" "Construct a file from a URI." uriToFile $ PureFunction $ \f -> (fileToURI f, ())
              , valPatBDS "Path" "Construct a file from a local path." pathToFile $
                ImpureFunction $ \f -> do
                    p <- fileToPath f
                    return (p, ())
              ]
        , namespaceBDS "File" "" [valBDS "makeRef" "Make a reference from a file." fileMakeRef]
        ]
