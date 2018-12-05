module Test.ReadType
    ( testReadTypes
    ) where

import Language.Expression.Dolan
import Pinafore
import Pinafore.Test
import Shapes
import Test.Tasty
import Test.Tasty.HUnit

testReadType :: Text -> TestTree
testReadType text =
    testCase (unpack text) $
    resultTextToM $ do
        mt <- parseType @PinaforeEdit @'PositivePolarity (initialPos "<input>") text
        _ <- runPinaforeTypeCheck mt
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
        , testReadType "Set a"
        , testReadType "Set +a"
        , testReadType "Set {+a,b,-NewEntity}"
        ]
