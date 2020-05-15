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
        _ <- runTestPinaforeSourceScoped $ parseType @PinaforeEntityUpdate @'Positive text
        return ()

testReadTypes :: TestTree
testReadTypes =
    testGroup
        "read-type"
        [ testReadType "()"
        , testReadType "Boolean"
        , testReadType "Literal"
        , testReadType "Entity"
        , testReadType "NewEntity"
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
        , testReadType "NewEntity -> Entity"
        , testReadType "NewEntity ~> Entity"
        , testReadType "+NewEntity ~> Entity"
        , testReadType "NewEntity ~> +Entity"
        , testReadType "{NewEntity} ~> Entity"
        , testReadType "NewEntity ~> {Entity}"
        , testReadType "+NewEntity ~> +Entity"
        , testReadType "-NewEntity ~> -Entity"
        , testReadType "{-NewEntity,+a} ~> {-Entity,a}"
        , testReadType "Ref a"
        , testReadType "Ref +a"
        , testReadType "Ref Entity"
        , testReadType "Ref +Entity"
        , testReadType "Ref {a}"
        , testReadType "Ref {+a}"
        , testReadType "Ref {Entity}"
        , testReadType "Ref {+Entity}"
        , testReadType "Ref {a,b}"
        , testReadType "Ref {+a,-b}"
        , testReadType "Ref {+a,b,-NewEntity}"
        , testReadType "Ref a -> Ref b"
        , testReadType "Ref +a -> Ref +b"
        , testReadType "Maybe ()"
        , testReadType "Maybe Entity"
        , testReadType "Maybe a"
        , testReadType "SetRef a"
        , testReadType "FiniteSetRef a"
        , testReadType "FiniteSetRef +a"
        , testReadType "FiniteSetRef {+a,b,-NewEntity}"
        , testReadType "Ref +Text -> (Action a -> [MenuItem]) -> UI -> Action Window"
        ]
