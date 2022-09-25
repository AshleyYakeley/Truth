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
    fromInterpretResult $ do
        _ <- runTestPinaforeSourceScoped $ parseType @'Positive text
        return ()

testReadTypes :: TestTree
testReadTypes =
    testTree
        "read-type"
        [ testReadType "Unit"
        , testReadType "Entity"
        , testReadType "Std.Entity"
        , testReadType "Literal"
        , testReadType "Std.Literal"
        , testReadType "Text"
        , testReadType "Std.Text"
        , testReadType "Boolean"
        , testReadType "Std.Boolean"
        , testReadType "Duration"
        , testReadType "Std.Duration"
        , testReadType "Time"
        , testReadType "Std.Time"
        , testReadType "Date"
        , testReadType "Std.Date"
        , testReadType "TimeOfDay"
        , testReadType "Std.TimeOfDay"
        , testReadType "LocalTime"
        , testReadType "Std.LocalTime"
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
        , testReadType "WholeModel a"
        , testReadType "WholeModel +a"
        , testReadType "WholeModel Entity"
        , testReadType "WholeModel +Entity"
        , testReadType "WholeModel {a}"
        , testReadType "WholeModel {+a}"
        , testReadType "WholeModel {Entity}"
        , testReadType "WholeModel {+Entity}"
        , testReadType "WholeModel {(List a)}"
        , testReadType "WholeModel {List a}"
        , testReadType "WholeModel {+List q}"
        , testReadType "WholeModel {-List p}"
        , testReadType "WholeModel {-List p,+List q}"
        , testReadType "WholeModel {a,b}"
        , testReadType "WholeModel {+a,-b}"
        , testReadType "WholeModel {+a,b,-Entity}"
        , testReadType "WholeModel a -> WholeModel b"
        , testReadType "WholeModel +a -> WholeModel +b"
        , testReadType "Maybe Unit"
        , testReadType "Maybe Entity"
        , testReadType "Maybe a"
        , testReadType "SetModel a"
        , testReadType "FiniteSetModel a"
        , testReadType "FiniteSetModel +a"
        , testReadType "FiniteSetModel {+a,b,-Entity}"
        , testReadType "Std.WholeModel +Std.Text -> a -> Std.Action a"
        , testTree
              "recursive"
              [testReadType "rec a. Maybe a", testReadType "rec a. Maybe a -> b", testReadType "rec a. rec b. a *: b"]
        ]
