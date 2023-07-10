module Test.Name
    ( testNames
    ) where

import Pinafore
import Shapes
import Shapes.Test

throwPureError :: a -> IO ()
throwPureError x = do
    me <- catchPureError x
    case me of
        Just e -> throwExc e
        Nothing -> return ()

testName ::
       forall (a :: Type). (Eq a, Show a, IsString a, ShowText a)
    => String
    -> Text
    -> TestTree
testName input expected =
    testTree input $ do
        let
            found :: a
            found = fromString @a input
        throwPureError found
        assertEqual "text" expected $ showText found
        assertEqual "name" (fromString @a $ unpack expected) found

testEqual ::
       forall (a :: Type). (Eq a, Show a)
    => a
    -> a
    -> TestTree
testEqual input expected =
    testTree (show expected) $ do
        throwPureError input
        assertEqual "" expected input

fullNamePair :: FullName -> (Namespace, Name)
fullNamePair (MkFullName ns n) = (n, ns)

testNames :: TestTree
testNames =
    testTree
        "name"
        [ testTree
              "structure"
              [ testEqual "." RootNamespace
              , testEqual "" RootNamespace
              , testEqual "N" (MkNamespace ["N"])
              , testEqual "N." (MkNamespace ["N"])
              , testEqual (fullNamePair "A") (RootNamespace, "A")
              , testEqual (fullNamePair "B.") (RootNamespace, "B")
              , testEqual (fullNamePair "C.N") (MkNamespace ["N"], "C")
              , testEqual (fullNamePair "D.N.") (MkNamespace ["N"], "D")
              , testEqual "E" (RootFullName "E")
              , testEqual "F." (RootFullName "F")
              , testEqual "G.N" (MkFullName "G" "N.")
              , testEqual "H.N." (MkFullName "H" "N.")
              , testEqual "%%.N" (MkFullName "%%" "N.")
              , testEqual "%%.N." (MkFullName "%%" "N.")
              , testEqual "..N" (MkFullName "." "N.")
              , testEqual "..N." (MkFullName "." "N.")
              , testEqual "A.N" (MkFullNameRef "A" "N")
              , testEqual "B.N." (MkFullNameRef "B" "N.")
              , testEqual "..N" (MkFullNameRef "." "N")
              , testEqual "..N." (MkFullNameRef "." "N.")
              , testEqual "." (MkFullNameRef "." CurrentNamespaceRef)
              ]
        , testTree
              "text"
              [ testName @FullName "A." "A."
              , testName @FullName "B" "B."
              , testName @FullName "C.N" "C.N."
              , testName @FullName "D.N." "D.N."
              ]
        ]
