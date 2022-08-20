{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.GIO
    ( File
    , gioLibraryModule
    ) where

import Changes.Core
import Changes.World.GNOME.GIO
import Changes.World.MIME
import GI.Gio as GI
import Pinafore.Base
import Pinafore.Language.API
import Shapes
import Shapes.Unsafe

-- File
fileGroundType :: PinaforeGroundType '[] File
fileGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily File)|]) "File"

instance HasPinaforeGroundType '[] File where
    pinaforeGroundType = fileGroundType

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

fileMakeRef :: File -> PinaforeAction (LangWholeRef '( Literal, Literal))
fileMakeRef f = do
    fref <- liftIO $ giFileReference f
    (model :: Model (MaybeUpdate (PairUpdate (WholeUpdate Text) ByteStringUpdate)), ()) <-
        actionLiftLifeCycle $ makeSharedModel $ reflectingPremodel fref
    return $ pinaforeRefToWholeRef $ eaMap (bijectionWholeChangeLens $ invert knowMaybe . literalConv) $ MkWModel model

gioLibraryModule :: LibraryModule
gioLibraryModule =
    MkDocTree
        "GIO"
        "GNOME file access."
        [ mkTypeEntry "File" "A file." $ MkBoundType fileGroundType
        , mkValPatEntry "FileParseName" "Construct a file from its parse name." parseNameToFile $ \f ->
              Just (fileToParseName f, ())
        , mkValEntry "fileMakeRef" "Make a reference from a file." fileMakeRef
        ]