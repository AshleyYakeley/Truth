module Pinafore.Language.Library.Debug
    ( debugStuff
    ) where

import Pinafore.Base
import Pinafore.Language.Debug
import Pinafore.Language.DocTree
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Std ()
import Pinafore.Language.Name
import Shapes

debugCheckEntity :: Text -> Entity -> IO ()
debugCheckEntity t e = do
    _ <- evaluate $ checkEntity (unpack t) e
    return ()

debugLiteralLength :: Literal -> Int
debugLiteralLength = olength . unLiteral

debugLiteralIsEmbedded :: Literal -> Bool
debugLiteralIsEmbedded = isJust . entityToLiteral . literalToEntity

debugStuff :: DocTreeEntry (BindDoc context)
debugStuff =
    docTreeEntry "Debug" "Functions for debugging." $
    namespaceRelative
        "Debug"
        [ mkValEntry "message" "Debug message to std error." debugMessage
        , mkValEntry "checkEntity" "debugCheckEntity" debugCheckEntity
        , mkValEntry "literalLength" "Byte length of a Literal" debugLiteralLength
        , mkValEntry "literalIsEmbedded" "Is this Literal embeddable in an Entity?" debugLiteralIsEmbedded
        ]
