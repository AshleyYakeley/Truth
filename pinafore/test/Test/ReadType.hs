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
    testCase (unpack text) $ let
        r = do
            mt <- parseType @PinaforeEdit @'PositivePolarity "input" text
            _ <- runPinaforeTypeCheck mt
            return ()
        in case r of
               SuccessResult _ -> return ()
               FailureResult err -> fail $ unpack err

testReadTypes :: TestTree
testReadTypes =
    testGroup
        "read type"
        [ testReadType "()"
        , testReadType "Bool"
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
