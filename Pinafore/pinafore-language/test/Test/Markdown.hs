module Test.Markdown
    ( testMarkdown
    ) where

import Pinafore
import Shapes
import Shapes.Test

testText :: (Show t, ToText t) => Text -> t -> TestTree
testText expected input =
    testTree (show input) $ let
        found = toText input
        in assertEqual "" expected found

testMarkdown :: TestTree
testMarkdown =
    testTree
        "markdown"
        [ let
              test e i = testText e (plainText i :: MarkdownText)
              in testTree
                     "plain"
                     [ test "abc" "abc"
                     , test "\\<" "<"
                     , test "\\>" ">"
                     , test "\\&" "&"
                     , test "\\\\" "\\"
                     , test "\\*\\*" "**"
                     , test "\\<\\*\\*\\>" "<**>"
                     ]
        , let
              test e i = testText e $ boldMarkdown $ plainText i
              in testTree
                     "bold"
                     [ test "**abc**" "abc"
                     , test "**\\<**" "<"
                     , test "**\\>**" ">"
                     , test "**\\&**" "&"
                     , test "**\\\\**" "\\"
                     , test "**\\*\\***" "**"
                     , test "**\\<\\*\\*\\>**" "<**>"
                     ]
        , let
              test e i = testText e $ codeMarkdown $ plainText i
              in testTree
                     "code"
                     [ test "<code>abc</code>" "abc"
                     , test "<code>\\<</code>" "<"
                     , test "<code>\\></code>" ">"
                     , test "<code>\\&</code>" "&"
                     , test "<code>\\&lt;</code>" "&lt;"
                     , test "<code>\\\\</code>" "\\"
                     , test "<code>\\*\\*</code>" "**"
                     , test "<code>\\<\\*\\*\\></code>" "<**>"
                     , test "<code>\\<:\\\\\\></code>" "<:\\>"
                     ]
        , let
              test i = let
                  e = "**" <> (toText $ codeMarkdown $ plainText i) <> "**"
                  in testText e $ boldMarkdown $ codeMarkdown $ plainText i
              in testTree "bold-code" [test "abc", test "<", test ">", test "&", test "\\", test "**", test "<**>"]
        , let
              test i = let
                  e = "**" <> (toText $ codeMarkdown $ plainText i) <> "**"
                  in testText e $ boldMarkdown $ codeMarkdown $ plainText i
              in testTree "code-bold" [test "abc", test "<", test ">", test "&", test "\\", test "**", test "<**>"]
        ]
