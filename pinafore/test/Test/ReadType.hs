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
    ioRunInterpretResult $ do
        _ <- runTestPinaforeSourceScoped $ parseType @PinaforeUpdate @'Positive text
        return ()

testReadTypes :: TestTree
testReadTypes =
    testGroup
        "read type"
        [ testReadType "()"
        , testReadType "Boolean"
        , testReadType "Literal"
        , testReadType "Entity"
        , testReadType "NewEntity"
        , testReadType "a"
        , testReadType "a -> b"
        , testReadType "a ~> b"
        , testReadType "NewEntity -> Entity"
        , testReadType "NewEntity ~> Entity"
        , testReadType "+NewEntity ~> +Entity"
        , testReadType "-NewEntity ~> -Entity"
        , testReadType "{-NewEntity,+a} ~> {-Entity,a}"
        , testReadType "Ref a"
        , testReadType "Ref +a"
        , testReadType "Ref {+a,b,-NewEntity}"
        , testReadType "FiniteSetRef a"
        , testReadType "FiniteSetRef +a"
        , testReadType "FiniteSetRef {+a,b,-NewEntity}"
        ]
