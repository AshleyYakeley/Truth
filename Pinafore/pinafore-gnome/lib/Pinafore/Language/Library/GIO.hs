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
fileGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily File)|]) "File"

instance HasQGroundType '[] File where
    qGroundType = fileGroundType

fileToParseName :: File -> Text
fileToParseName f = unsafePerformIO $ fileGetParseName f

parseNameToFile :: Text -> File
parseNameToFile t = unsafePerformIO $ fileParseName t

literalConv :: Bijection (Maybe (Text, LazyByteString)) (Maybe Literal)
literalConv =
    MkIsomorphism
        { isoForwards =
              \mtb -> do
                  (mimetype, b) <- mtb
                  case splitWhen ((==) '/') mimetype of
                      [t, s] -> return $ MkMIMELiteral (MkMIMEContentType t s []) $ toStrict b
                      _ -> Nothing
        , isoBackwards =
              \ml -> do
                  l <- ml
                  case l of
                      MkMIMELiteral (MkMIMEContentType t s _) b -> return (t <> "/" <> s, fromStrict b)
                      _ -> Nothing
        }

fileMakeRef :: File -> Action (LangWholeModel '( Literal, Literal))
fileMakeRef f = do
    fref <- liftIO $ giFileReference f
    (model :: Model (MaybeUpdate (PairUpdate (WholeUpdate Text) ByteStringUpdate)), ()) <-
        actionLiftLifecycle $ makeSharedModel $ reflectingPremodel fref
    return $ wModelToWholeModel $ eaMap (bijectionWholeChangeLens $ invert knowMaybe . literalConv) $ MkWModel model

gioStuff :: DocTreeEntry BindDoc
gioStuff =
    docTreeEntry "GIO" "GNOME file access." $
    namespaceRelative
        "GIO"
        [ mkTypeEntry "File" "A file." $ MkSomeGroundType fileGroundType
        , mkValPatEntry "FileParseName" "Construct a file from its parse name." parseNameToFile $
          PureFunction $ \f -> (fileToParseName f, ())
        , mkValEntry "fileMakeRef" "Make a reference from a file." fileMakeRef
        ]
