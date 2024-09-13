module Main
    ( main
    ) where

import Pinafore.Base
import Pinafore.Syntax
import Shapes
import Shapes.Test

testParseAccept :: Text -> TestTree
testParseAccept t =
    testTree (unpack t) $
    case evalStateT (runParser readExpression t) (initialPos "<test>") of
        SuccessResult _ -> return () :: IO ()
        FailureResult err -> fail $ unpack $ showText err

main :: IO ()
main =
    testMainNoSignalHandler $
    testTree
        "pinafore-syntax"
        [ testParseAccept "do {r}"
        , testParseAccept "do {a = x; r}"
        , testParseAccept "do {a = x; r;}"
        , testParseAccept "do {a <- x; r}"
        , testParseAccept "do {a <- x; r;}"
        , testParseAccept "do {let {a = x}; r}"
        , testParseAccept "do {let {a = x}; r;}"
        , testParseAccept "do {let {a = x;}; r;}"
        , testParseAccept "do {let {a = x} y; r;}"
        ]
