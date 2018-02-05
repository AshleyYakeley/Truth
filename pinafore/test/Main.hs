{-# OPTIONS -fno-warn-orphans #-}

module Main
    ( main
    ) where

import Pinafore
import Pinafore.Query.Expression
import Pinafore.Query.Value
import Shapes
import Test.Tasty
import Test.Tasty.HUnit
    -- for test only

instance Eq QValue where
    (MkAny QLiteral a1) == (MkAny QLiteral a2) = a1 == a2
    _ == _ = error "QValue: not comparable"

testQueryValue :: (Eq a, Show a) => String -> QExpr a -> Maybe a -> TestTree
testQueryValue name expr expected = testCase name $ assertEqual "result" expected $ qeval expr

qint :: Int -> QValue
qint = toQValue

testQueryValues :: TestTree
testQueryValues =
    testGroup
        "query values"
        [ testQueryValue "pure A" (pure "A") (Just "A")
        , testQueryValue "var a" (qvar "a") Nothing
        , testQueryValue
              "let a=1;b=2 in (a,b,a,b)"
              (qlet "a" (pure $ qint 1) $
               qlet "b" (pure $ qint 2) $ (,,,) <$> (qvar "a") <*> (qvar "b") <*> (qvar "a") <*> (qvar "b")) $
          Just (qint 1, qint 2, qint 1, qint 2)
        , testQueryValue
              "let a=1;b=2 in (b,a,b,a)"
              (qlet "a" (pure $ qint 1) $
               qlet "b" (pure $ qint 2) $ (,,,) <$> (qvar "b") <*> (qvar "a") <*> (qvar "b") <*> (qvar "a")) $
          Just (qint 2, qint 1, qint 2, qint 1)
        ]

testQuery :: String -> Maybe String -> TestTree
testQuery query expected =
    testCase query $
    case (expected, parseValue "<input>" query) of
        (Nothing, FailureResult _) -> return ()
        (Nothing, SuccessResult v) -> assertFailure $ "expected failure, found success: " ++ show v
        (Just _, FailureResult e) -> assertFailure $ "expected success, found failure: " ++ e
        (Just s, SuccessResult (v :: QValue)) -> assertEqual "result" s (show v)

testQueries :: TestTree
testQueries =
    testGroup
        "queries"
        [ testQuery "" $ Nothing
        , testQuery "x" $ Nothing
        -- constants
        , testQuery "\"\"" $ Just ""
        , testQuery "\"Hello \"" $ Just "Hello "
        , testQuery "true" $ Just "true"
        , testQuery "false" $ Just "false"
        , testQuery "\"1\"" $ Just "1"
        , testQuery "3" $ Just "3"
        , testQuery "uiTable" $ Just "<function>"
        -- list construction
        , testQuery "[]" $ Just "[]"
        , testQuery "[1]" $ Just "[1]"
        , testQuery "[1,2,3]" $ Just "[1,2,3]"
        -- functions
        , testQuery "\\x -> x" $ Just "<function>"
        , testQuery "\\x -> 1" $ Just "<function>"
        , testQuery "\\x y -> y" $ Just "<function>"
        , testQuery "\\x y z -> [x,y,z]" $ Just "<function>"
        -- let-binding
        , testQuery "let in 27" $ Just "27"
        , testQuery "let a=\"5\" in a" $ Just "5"
        , testQuery "let a=5 in a" $ Just "5"
        , testQuery "let a=1;b=2 in a" $ Just "1"
        , testQuery "let a=1;b=2 in b" $ Just "2"
        , testQuery "let a=1;b=2 in b" $ Just "2"
        , testQuery "let a=1;b=\"2\" in b" $ Just "2"
        , testQuery "let a=1 ;b=\"2\" in b" $ Just "2"
        , testQuery "let a= 1 ;b=\"2\" in b" $ Just "2"
        , testQuery "let a=7;b=a in a" $ Just "7"
        , testQuery "let a=7;b=a in b" $ Just "7"
        , testQuery "let a=2 in let b=a in b" $ Just "2"
        -- partial keywords
        , testQuery "let i=1 in i" $ Just "1"
        , testQuery "let inx=1 in inx" $ Just "1"
        , testQuery "let l=1 in l" $ Just "1"
        , testQuery "let le=1 in le" $ Just "1"
        , testQuery "let letx=1 in letx" $ Just "1"
        , testQuery "let letre=1 in letre" $ Just "1"
        , testQuery "let letrecx=1 in letrecx" $ Just "1"
        , testQuery "let tru=1 in tru" $ Just "1"
        , testQuery "let truex=1 in truex" $ Just "1"
        , testQuery "let f=1 in f" $ Just "1"
        , testQuery "let fals=1 in fals" $ Just "1"
        , testQuery "let falsex=1 in falsex" $ Just "1"
        -- recursive let-binding
        , testQuery "let a=1 in a" $ Just "1"
        , testQuery "let a=1;b=a in b" $ Just "1"
        , testQuery "let b=a;a=1 in b" $ Just "1"
        , testQuery "let a x = x in a 1" $ Just "1"
        , testQuery "let a x = x; b = a in b" $ Just "<function>"
        , testQuery "let a = \\x -> x in let b = a 1 in b" $ Just "1"
        , testQuery "let a x = x; b = a 1 in b" $ Just "1"
        , testQuery "let a x = b; b = b in a" $ Just "<function>"
        , testQuery "let a x = 1; b = b in a b" $ Just "1"
        , testQuery "let a x = 1; b = a b in b" $ Just "1"
        , testQuery "let b = a b; a x = 1 in b" $ Just "1"
        , testQuery "let a x = 1; b = a c; c=b in b" $ Just "1"
        -- duplicate bindings
        , testQuery "let a=1;a=1 in a" $ Nothing
        , testQuery "let a=1;a=2 in a" $ Nothing
        , testQuery "let a=1;b=0;a=2 in a" $ Nothing
        -- lexical scoping
        , testQuery "let a=1 in let b=a in let a=3 in a" $ Just "3"
        , testQuery "let a=1;b=a;a=3 in a" $ Nothing
        , testQuery "let a=1 in let b=a in let a=3 in b" $ Just "1"
        , testQuery "let a=1;b=a;a=3 in b" $ Nothing
        ]

tests :: TestTree
tests = localOption (mkTimeout 2000000) $ testGroup "pinafore" [testQueryValues, testQueries]

main :: IO ()
main = defaultMain tests
