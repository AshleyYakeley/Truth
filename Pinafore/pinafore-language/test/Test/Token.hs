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
        [ testTokens "A" "unames(A)"
        , testTokens "a" "lnames(a)"
        , testTokens "." "infix(.)"
        , testTokens "a ." "lnames(a) infix(.)"
        , testTokens "a." "lnames(a.)"
        , testTokens "A.B" "unames(A.B)"
        , testTokens "a.B" "lnames(a.B)"
        , testTokens "A.B." "unames(A.B.)"
        , testTokens "a.B." "lnames(a.B.)"
        , testTokens "B." "unames(B.)"
        , testTokens "b." "lnames(b.)"
        , testTokens "$" "infix($)"
        , testTokens "$." "infix($.)"
        , testTokens "$.B" "infix($.B)"
        , testTokens "." "infix(.)"
        , testTokens ".." "infix(..)"
        , testTokens "..B" "infix(..B)"
        , testTokens "..B." "infix(..B.)"
        , testTokens
              "let x : rec a, Maybe a = Nothing in x"
              "\"let\" lnames(x) \":\" \"rec\" lnames(a) \",\" unames(Maybe) lnames(a) \"=\" unames(Nothing) \"in\" lnames(x)"
        ]
