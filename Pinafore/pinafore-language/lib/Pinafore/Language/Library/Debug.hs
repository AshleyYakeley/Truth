module Pinafore.Language.Library.Debug
    ( debugLibSection
    ) where

import Pinafore.Base
import Pinafore.Language.Debug
import Pinafore.Language.Library.Base ()
import Pinafore.Language.Library.Defs
import Shapes

debugCheckEntity :: Text -> Entity -> IO ()
debugCheckEntity t e = do
    _ <- evaluate $ checkEntity (unpack t) e
    return ()

debugLiteralLength :: Literal -> Int
debugLiteralLength = olength . unLiteral

debugLiteralIsEmbedded :: Literal -> Bool
debugLiteralIsEmbedded = isJust . entityToLiteral . literalToEntity

debugLibSection :: BindDocTree context
debugLibSection =
    headingBDT "Debug" "Functions for debugging." $
    pure $
    namespaceBDT
        "Debug"
        ""
        [ valBDT "message" "Debug message to std error." debugMessage
        , valBDT "checkEntity" "debugCheckEntity" debugCheckEntity
        , valBDT "literalLength" "Byte length of a Literal" debugLiteralLength
        , valBDT "literalIsEmbedded" "Is this Literal embeddable in an Entity?" debugLiteralIsEmbedded
        ]
