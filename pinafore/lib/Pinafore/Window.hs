module Pinafore.Window where

import Pinafore.Edit
import Pinafore.Morphism
import Pinafore.Query
import Pinafore.SQLite
import Shapes
import Truth.Core

type FilePinaforeType = [(PinaforeFunctionValue (Maybe Text), UISpec PinaforeEdit)]

filePinaforeType :: Text
filePinaforeType = qTypeDescriptionFrom @FilePinaforeType

sqlitePinaforeWindow :: FilePath -> (FilePath, Text) -> IO [UIWindow ()]
sqlitePinaforeWindow sqlitepath (puipath, puitext) = do
    sub <- makeObjectSubscriber $ sqlitePinaforeObject sqlitepath
    windows :: FilePinaforeType <- resultToM $ mapResultFailure unpack $ parseValue puipath puitext
    return $
        fmap (\(title, spec) -> MkUIWindow (editLensFunction (maybeNothingEditLens mempty) . title) spec sub) windows
