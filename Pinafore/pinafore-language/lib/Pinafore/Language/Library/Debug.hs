module Pinafore.Language.Library.Debug
    ( debugLibSection
    ) where

import Import
import Pinafore.Language.Debug
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Entity ()
import Pinafore.Language.Library.LibraryModule

debugCheckEntity :: Text -> Entity -> IO ()
debugCheckEntity t e = do
    _ <- evaluate $ checkEntity (unpack t) e
    return ()

debugLiteralLength :: Literal -> Int
debugLiteralLength = olength . unLiteral

debugLiteralIsEmbedded :: Literal -> Bool
debugLiteralIsEmbedded = isJust . entityToLiteral . literalToEntity

debugLibSection :: LibraryStuff context
debugLibSection =
    headingBDS "Debug" "Functions for debugging." $
    pure $
    namespaceBDS
        "Debug"
        [ valBDS "message" "Debug message to std error." debugMessage
        , valBDS "checkEntity" "debugCheckEntity" debugCheckEntity
        , valBDS "literalLength" "Byte length of a Literal" debugLiteralLength
        , valBDS "literalIsEmbedded" "Is this Literal embeddable in an Entity?" debugLiteralIsEmbedded
        ]
