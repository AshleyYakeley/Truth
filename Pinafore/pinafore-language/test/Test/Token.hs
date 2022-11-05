module Test.Token
    ( testToken
    ) where

import Pinafore
import Pinafore.Test
import Shapes
import Shapes.Test

testTokens :: Text -> Text -> TestTree
testTokens input expected =
    testTree (unpack input) $ do
        pairs <- fromInterpretResult $ evalStateT (parseTokens input) $ initialPos "<input>"
        let found = pack $ intercalate " " $ fmap (show . snd) pairs
        assertEqual "tokens" expected found

testToken :: TestTree
testToken =
    testTree
        "token"
        [ testTokens "A" "unames"
        , testTokens "a" "lnames"
        , testTokens "." "infix"
        , testTokens "a ." "lnames infix"
        , testTokens "a." "lnames infix"
        , testTokens "A.B" "unames"
        , testTokens "A.b" "lnames"
        , testTokens ".A.B" "unames"
        , testTokens ".A.b" "lnames"
        , testTokens ".B" "unames"
        , testTokens ".b" "lnames"
        , testTokens
              "let x : rec a. Maybe a = Nothing in x"
              "\"let\" lnames \":\" \"rec\" lnames infix unames lnames \"=\" unames \"in\" lnames"
        ]
