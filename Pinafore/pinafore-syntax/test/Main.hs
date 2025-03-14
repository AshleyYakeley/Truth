module Main
    ( main
    )
where

import Pinafore.Base
import Shapes
import Shapes.Test

import Pinafore.Syntax

testParseAccept :: Text -> TestTree
testParseAccept t =
    testTree (unpack t)
        $ case evalStateT (runParser readExpression t) (initialPos "<test>") of
            SuccessResult _ -> return () :: IO ()
            FailureResult err -> fail $ unpack $ showText err

testParseFail :: Text -> (ParseErrorType -> Bool) -> TestTree
testParseFail t errtest =
    testTree (unpack t)
        $ case evalStateT (runParser readExpression t) (initialPos "<test>") of
            SuccessResult _ -> fail "accepted bad text"
            FailureResult (MkLocated _ _ err)
                | errtest err -> return () :: IO ()
            FailureResult err -> fail $ unpack $ showText err

main :: IO ()
main =
    testMainNoSignalHandler
        $ testTree
            "pinafore-syntax"
            [ testTree
                "decls"
                [ testParseAccept "let {a = x} m"
                , testParseAccept "let {a = x} let {b = y} m"
                , testParseAccept "let {let {b = y} a = x} m"
                ]
            , testTree
                "do"
                [ testParseAccept "do {r}"
                , testParseAccept "do {a = x; r}"
                , testParseAccept "do {a = x; r;}"
                , testParseAccept "do {a <- x; r}"
                , testParseAccept "do {a <- x; r;}"
                , testParseAccept "do {let {a = x}; r}"
                , testParseAccept "do {let {a = x}; r;}"
                , testParseAccept "do {let {a = x;}; r;}"
                , testParseAccept "do {let {a = x} m; r;}"
                , testParseAccept "do {let {a = x} p = q; r;}"
                , testParseAccept "do {let {a = x} let {b = y}; r;}"
                , testParseAccept "do {let {a = x} let {b = y} m; r;}"
                , testParseFail "do {a <- {}; r;}" $ \case
                    SyntaxErrorType _ -> True
                    _ -> False
                , testParseFail "do {{}}" $ \case
                    SyntaxErrorType _ -> True
                    _ -> False
                ]
            ]
