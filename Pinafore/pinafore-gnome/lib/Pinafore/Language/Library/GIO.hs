{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.GIO
    ( File
    , gioStuff
    ) where

import Changes.Core
import Changes.World.GNOME.GIO
import Changes.World.MIME
import GI.Gio as GI hiding (Action)
import Pinafore.Base
import Pinafore.Language.API
import Shapes
import Shapes.Unsafe

-- File
fileGroundType :: QGroundType '[] File
fileGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily File)|]) "File.GIO."

instance HasQGroundType '[] File where
    qGroundType = fileGroundType

fileToParseName :: File -> Text
fileToParseName f = unsafePerformIO $ fileGetParseName f

parseNameToFile :: Text -> File
parseNameToFile t = unsafePerformIO $ fileParseName t

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

fileMakeRef :: File -> Action (LangWholeModel '( MIME, MIME))
fileMakeRef f = do
    fref <- liftIO $ giFileReference f
    (model :: Model (MaybeUpdate (PairUpdate (WholeUpdate Text) ByteStringUpdate)), ()) <-
        actionLiftLifecycle $ makeSharedModel $ reflectingPremodel fref
    return $ wModelToWholeModel $ eaMap (bijectionWholeChangeLens $ invert knowMaybe . literalConv) $ MkWModel model

gioStuff :: BindDocTree ()
gioStuff =
    headingBDT "GIO" "GNOME file access." $
    pure $
    namespaceBDT
        "GIO"
        ""
        [ typeBDT
              "File"
              "A file."
              (MkSomeGroundType fileGroundType)
              [ valPatBDT "FileParseName" "Construct a file from its parse name." parseNameToFile $
                PureFunction $ \f -> (fileToParseName f, ())
              ]
        , valBDT "fileMakeRef" "Make a reference from a file." fileMakeRef
        ]
