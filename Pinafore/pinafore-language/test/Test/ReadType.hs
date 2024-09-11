module Test.ReadType
    ( testReadTypes
    ) where

import Data.Shim
import Pinafore.Test.Internal
import Shapes
import Shapes.Test

testReadType :: Text -> TestTree
testReadType text =
    testTree @Assertion (unpack text) $
    runTester defaultTester $ do
        _ <- testerLiftInterpreter $ parseType @'Positive text
        return ()

testReadTypes :: TestTree
testReadTypes =
    testTree
        "read-type"
        [ testReadType "Unit"
        , testReadType "Entity"
        , testReadType "Entity."
        , testReadType "Literal"
        , testReadType "Literal."
        , testReadType "Text"
        , testReadType "Text."
        , testReadType "Boolean"
        , testReadType "Boolean."
        , testReadType "Duration"
        , testReadType "Duration."
        , testReadType "Time"
        , testReadType "Time."
        , testReadType "Date"
        , testReadType "Date."
        , testReadType "TimeOfDay"
        , testReadType "TimeOfDay."
        , testReadType "LocalTime"
        , testReadType "LocalTime."
        , testReadType "a"
        , testReadType "a -> b"
        , testReadType "a -> b -> c"
        , testReadType "a -> Property b c"
        , testReadType "Entity -> Entity -> Entity"
        , testReadType "Maybe a -> Maybe b"
        , testReadType "Maybe a -> Maybe b -> Maybe c"
        , testReadType "Maybe (a -> Maybe b)"
        , testReadType "a -> Maybe b"
        , testReadType "Maybe (Maybe a)"
        , testReadType "Property a b"
        , testReadType "Entity -> Entity"
        , testReadType "Property Entity Entity"
        , testReadType "Property +Entity Entity"
        , testReadType "Property Entity +Entity"
        , testReadType "Property (Entity) Entity"
        , testReadType "Property Entity (Entity)"
        , testReadType "Property +Entity +Entity"
        , testReadType "Property -Entity -Entity"
        , testReadType "Property (-Entity,+a) (-Entity,a)"
        , testReadType "WholeModel a"
        , testReadType "WholeModel +a"
        , testReadType "WholeModel Entity"
        , testReadType "WholeModel +Entity"
        , testReadType "WholeModel (a)"
        , testReadType "WholeModel (+a)"
        , testReadType "WholeModel (Entity)"
        , testReadType "WholeModel (+Entity)"
        , testReadType "WholeModel ((List a))"
        , testReadType "WholeModel (List a)"
        , testReadType "WholeModel (+List q)"
        , testReadType "WholeModel (-List p)"
        , testReadType "WholeModel (-List p,+List q)"
        , testReadType "WholeModel (a,b)"
        , testReadType "WholeModel (+a,-b)"
        , testReadType "WholeModel (+a,b,-Entity)"
        , testReadType "WholeModel a -> WholeModel b"
        , testReadType "WholeModel +a -> WholeModel +b"
        , testReadType "Maybe Unit"
        , testReadType "Maybe Entity"
        , testReadType "Maybe a"
        , testReadType "SetModel a"
        , testReadType "FiniteSetModel a"
        , testReadType "FiniteSetModel +a"
        , testReadType "FiniteSetModel (+a,b,-Entity)"
        , testReadType "WholeModel. + Text. -> a -> Action. a"
        , testTree
              "recursive"
              [testReadType "rec a, Maybe a", testReadType "(rec a, Maybe a) -> b", testReadType "rec a, rec b, a *: b"]
        ]
