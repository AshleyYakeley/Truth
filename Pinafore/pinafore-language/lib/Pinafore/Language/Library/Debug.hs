module Pinafore.Language.Library.Debug
    ( debugLibraryModule
    ) where

import Pinafore.Base
import Pinafore.Language.DocTree
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Std ()
import Shapes

debugMessage :: Text -> IO ()
debugMessage t = hPutStrLn stderr $ unpack t

debugCheckEntity :: Text -> Entity -> IO ()
debugCheckEntity t e = do
    _ <- evaluate $ checkEntity (unpack t) e
    return ()

debugLiteralLength :: Literal -> Int
debugLiteralLength = olength . unLiteral

debugLiteralIsEmbedded :: Literal -> Bool
debugLiteralIsEmbedded = isJust . entityToLiteral . literalToEntity

debugLibraryModule :: LibraryModule
debugLibraryModule =
    MkDocTree
        "Debug"
        "Functions for debugging."
        [ mkValEntry "message" "Debug message to std error." debugMessage
        , mkValEntry "checkEntity" "debugCheckEntity" debugCheckEntity
        , mkValEntry "literalLength" "Byte length of a Literal" debugLiteralLength
        , mkValEntry "literalIsEmbedded" "Is this Literal embeddable in an Entity?" debugLiteralIsEmbedded
        ]
