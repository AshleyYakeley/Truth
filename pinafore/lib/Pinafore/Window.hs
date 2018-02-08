module Pinafore.Window where

import Pinafore.Edit
import Pinafore.Query
import Pinafore.SQLite
import Shapes
import Truth.Core
import Truth.Debug.Object

pinaforeEditShower :: EditShower PinaforeEdit
pinaforeEditShower = blankEditShower {
    showRead = show,
    showEdit = show
}

sqlitePinaforeWindow :: FilePath -> (FilePath, String) -> IO [UIWindow ()]
sqlitePinaforeWindow sqlitepath (puipath, puitext) = do
    sub <- makeObjectSubscriber $ traceObject "pinafore" pinaforeEditShower $ sqlitePinaforeObject sqlitepath
    windows <- resultToM $ parseValue puipath puitext
    return $ fmap (\(title :: Text, spec) -> MkUIWindow (unpack title) spec sub) windows
