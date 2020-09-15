module Test.ReadType
    ( testReadTypes
    ) where

import Data.Shim
import Pinafore
import Pinafore.Test
import Shapes
import Test.Tasty
import Test.Tasty.HUnit

testReadType :: Text -> TestTree
testReadType text =
    testCase (unpack text) $
    throwResult $ do
        _ <- runTestPinaforeSourceScoped $ parseType @'Positive text
        return ()

testReadTypes :: TestTree
testReadTypes =
    testGroup
        "read-type"
        [ testReadType "()"
        , testReadType "Boolean"
        , testReadType "Literal"
        , testReadType "Entity"
        , testReadType "a"
        , testReadType "a -> b"
        , testReadType "a -> b -> c"
        , testReadType "a -> b ~> c"
        , testReadType "Entity -> Entity -> Entity"
        , testReadType "Maybe a -> Maybe b"
        , testReadType "Maybe a -> Maybe b -> Maybe c"
        , testReadType "Maybe (a -> Maybe b)"
        , testReadType "a -> Maybe b"
        , testReadType "Maybe (Maybe a)"
        , testReadType "a ~> b"
        , testReadType "Entity -> Entity"
        , testReadType "Entity ~> Entity"
        , testReadType "+Entity ~> Entity"
        , testReadType "Entity ~> +Entity"
        , testReadType "{Entity} ~> Entity"
        , testReadType "Entity ~> {Entity}"
        , testReadType "+Entity ~> +Entity"
        , testReadType "-Entity ~> -Entity"
        , testReadType "{-Entity,+a} ~> {-Entity,a}"
        , testReadType "WholeRef a"
        , testReadType "WholeRef +a"
        , testReadType "WholeRef Entity"
        , testReadType "WholeRef +Entity"
        , testReadType "WholeRef {a}"
        , testReadType "WholeRef {+a}"
        , testReadType "WholeRef {Entity}"
        , testReadType "WholeRef {+Entity}"
        , testReadType "WholeRef {a,b}"
        , testReadType "WholeRef {+a,-b}"
        , testReadType "WholeRef {+a,b,-Entity}"
        , testReadType "WholeRef a -> WholeRef b"
        , testReadType "WholeRef +a -> WholeRef +b"
        , testReadType "Maybe ()"
        , testReadType "Maybe Entity"
        , testReadType "Maybe a"
        , testReadType "SetRef a"
        , testReadType "FiniteSetRef a"
        , testReadType "FiniteSetRef +a"
        , testReadType "FiniteSetRef {+a,b,-Entity}"
        , testReadType "WholeRef +Text -> (Action a -> [MenuItem]) -> UI -> Action Window"
        , testGroup
              "recursive"
              [testReadType "rec a. Maybe a", testReadType "rec a. Maybe a -> b", testReadType "rec a. rec b. (a,b)"]
        ]
