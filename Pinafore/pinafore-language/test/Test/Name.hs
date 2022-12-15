module Test.Name
    ( testNames
    ) where

import Pinafore
import Shapes
import Shapes.Test

testName ::
       forall (a :: Type). (Eq a, Show a, IsString a, ToText a)
    => String
    -> Text
    -> TestTree
testName input expected =
    testTree (unpack input) $ do
        assertEqual "text" expected $ toText $ fromString @a input
        assertEqual "name" (fromString @a $ unpack expected) $ fromString @a input

testEqual ::
       forall (a :: Type). (Eq a, Show a)
    => a
    -> a
    -> TestTree
testEqual input expected = testTree (show input) $ assertEqual "" expected input

fullNamePair :: FullName -> (Namespace, Name)
fullNamePair (MkFullName ns n) = (ns, n)

testNames :: TestTree
testNames =
    testTree
        "name"
        [ testTree
              "structure"
              [ testEqual "." RootNamespace
              , testEqual "" RootNamespace
              , testEqual "N" (MkNamespace ["N"])
              , testEqual ".N" (MkNamespace ["N"])
              , testEqual (fullNamePair "A") (RootNamespace, "A")
              , testEqual (fullNamePair ".A") (RootNamespace, "A")
              , testEqual (fullNamePair "N.A") (MkNamespace ["N"], "A")
              , testEqual (fullNamePair ".N.A") (MkNamespace ["N"], "A")
              , testEqual "A" (RootFullName "A")
              , testEqual ".A" (RootFullName "A")
              , testEqual "N.A" (MkFullName ".N" "A")
              , testEqual ".N.A" (MkFullName ".N" "A")
              ]
        , testTree
              "text"
              [ testName @FullName ".A" ".A"
              , testName @FullName "A" ".A"
              , testName @FullName "N.A" ".N.A"
              , testName @FullName ".N.A" ".N.A"
              ]
        ]
