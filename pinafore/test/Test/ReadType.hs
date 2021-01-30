module Test.ReadType
    ( testReadTypes
    ) where

import Data.Shim
import Pinafore
import Pinafore.Test
import Shapes
import Shapes.Test

testReadType :: Text -> TestTree
testReadType text =
    testTree @Assertion (unpack text) $
    throwInterpretResult $ do
        _ <- runTestPinaforeSourceScoped $ parseType @'Positive text
        return ()

testReadTypes :: TestTree
testReadTypes =
    testTree
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
        , testReadType "WholeRef +Text -> (Action a -> [UI.MenuItem]) -> UI.Element -> Action UI.Window"
        , testTree
              "recursive"
              [testReadType "rec a. Maybe a", testReadType "rec a. Maybe a -> b", testReadType "rec a. rec b. (a,b)"]
        ]
