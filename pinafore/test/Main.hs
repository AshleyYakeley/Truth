{-# OPTIONS -fno-warn-orphans #-}

module Main
    ( main
    ) where

import Data.Ratio
import Pinafore
import Pinafore.Query.Expression
import Pinafore.Query.Value
import Prelude (read)
import Prelude (Fractional(..))
import Shapes
import Test.Tasty
import Test.Tasty.HUnit

newtype PreciseEq t =
    MkPreciseEq t

instance Show t => Show (PreciseEq t) where
    show (MkPreciseEq a) = show a

instance Eq (PreciseEq Rational) where
    (MkPreciseEq a) == (MkPreciseEq b) = a == b

instance Eq (PreciseEq Double) where
    (MkPreciseEq a) == (MkPreciseEq b) = show a == show b

instance Eq (PreciseEq Number) where
    (MkPreciseEq (ExactNumber a)) == (MkPreciseEq (ExactNumber b)) = MkPreciseEq a == MkPreciseEq b
    (MkPreciseEq (InexactNumber a)) == (MkPreciseEq (InexactNumber b)) = MkPreciseEq a == MkPreciseEq b
    _ == _ = False

testCalc :: String -> Number -> Number -> TestTree
testCalc name expected found = testCase name $ assertEqual "" (MkPreciseEq expected) (MkPreciseEq found)

testNumbersArithemetic :: TestTree
testNumbersArithemetic =
    testGroup
        "arithmetic"
        [ testCalc "1/0" (InexactNumber $ 1 / 0) (1 / 0)
        , testCalc "-1/0" (InexactNumber $ -1 / 0) (-1 / 0)
        , testCalc "0/0" (InexactNumber $ 0 / 0) (0 / 0)
        , testCalc "2+3" (ExactNumber $ 5) $ 2 + 3
        , testCalc "2*3" (ExactNumber $ 6) $ 2 * 3
        , testCalc "2-3" (ExactNumber $ -1) $ 2 - 3
        , testCalc "2/3" (ExactNumber $ 2 % 3) $ 2 / 3
        ]

testShowRead ::
       forall t. (Show t, Eq (PreciseEq t), Read t)
    => String
    -> t
    -> TestTree
testShowRead str t =
    testGroup
        (show str)
        [ testCase "show" $ assertEqual "" str $ show t
        , testCase "read" $ assertEqual "" (MkPreciseEq t) $ MkPreciseEq $ read str
        , testCase "read-show" $ assertEqual "" str $ show $ read @t str
        ]

testRead ::
       forall t. (Show t, Eq (PreciseEq t), Read t)
    => String
    -> t
    -> TestTree
testRead str t = testCase (show str) $ assertEqual "" (MkPreciseEq t) $ MkPreciseEq $ read str

testNumbersShowRead :: TestTree
testNumbersShowRead =
    testGroup
        "show,read"
        [ testShowRead "0" $ ExactNumber 0
        , testShowRead "1" $ ExactNumber 1
        , testShowRead "-1" $ ExactNumber $ negate 1
        , testShowRead "0.5" $ ExactNumber $ 1 / 2
        , testShowRead "0._3" $ ExactNumber $ 1 / 3
        , testShowRead "-0._3" $ ExactNumber $ negate $ 1 / 3
        , testShowRead "-0.0_3" $ ExactNumber $ negate $ 1 / 30
        , testShowRead "0.3_571428" $ ExactNumber $ 5 / 14
        , testShowRead "NaN" $ InexactNumber $ 0 / 0
        , testShowRead "~0.0" $ InexactNumber 0
        , testShowRead "~1.0" $ InexactNumber 1
        , testShowRead "~-1.0" $ InexactNumber $ negate 1
        , testShowRead "~Infinity" $ InexactNumber $ 1 / 0
        , testShowRead "~-Infinity" $ InexactNumber $ -1 / 0
        , testRead "" $ InexactNumber $ 0 / 0
        , testRead " " $ InexactNumber $ 0 / 0
        , testRead "  " $ InexactNumber $ 0 / 0
        , testRead " 1" $ ExactNumber 1
        , testRead "1 " $ ExactNumber 1
        , testRead " 1 " $ ExactNumber 1
        , testRead "z" $ InexactNumber $ 0 / 0
        , testRead "ZZ" $ InexactNumber $ 0 / 0
        , testRead "~1Z" $ InexactNumber $ 0 / 0
        , testRead "~-1.1Z" $ InexactNumber $ 0 / 0
        , testRead "0" $ ExactNumber 0
        , testRead "0." $ ExactNumber 0
        , testRead "0.0" $ ExactNumber 0
        , testRead "0._" $ ExactNumber 0
        , testRead "0._0" $ ExactNumber 0
        , testRead "0.0_" $ ExactNumber 0
        , testRead "0.0_0" $ ExactNumber 0
        ]

testNumbers :: TestTree
testNumbers = testGroup "numbers" [testNumbersArithemetic, testNumbersShowRead]

-- | for test only
instance Eq (QValue baseedit) where
    (MkAny QConstant a1) == (MkAny QConstant a2) = a1 == a2
    _ == _ = error "QValue: not comparable"

testQueryValue :: (Eq a, Show a) => String -> QExpr baseedit a -> Maybe a -> TestTree
testQueryValue name expr expected = testCase name $ assertEqual "result" expected $ qeval expr

qint :: Int -> QValue baseedit
qint = toQValue

testQueryValues :: TestTree
testQueryValues =
    testGroup
        "query values"
        [ testQueryValue "pure A" (pure "A") (Just "A" :: Maybe String)
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

testQuery :: Text -> Maybe String -> TestTree
testQuery query expected =
    testCase (unpack query) $
    case (expected, parseValue @PinaforeTableEdit "<input>" query) of
        (Nothing, FailureResult _) -> return ()
        (Nothing, SuccessResult v) -> assertFailure $ "expected failure, found success: " ++ show v
        (Just _, FailureResult e) -> assertFailure $ "expected success, found failure: " ++ unpack e
        (Just s, SuccessResult (v :: QValue PinaforeTableEdit)) -> assertEqual "result" s (show v)

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
        , testQuery "ui_table" $ Just "<function>"
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
        -- operators
        , testQuery "\"abc\" ++ \"def\"" $ Just "abcdef"
        -- if/then/else
        , testQuery "if true then 3 else 4" $ Just "3"
        , testQuery "if false then 3 else 4" $ Just "4"
        , testQuery "if false then if true then 1 else 2 else if true then 3 else 4" $ Just "3"
        ]

tests :: TestTree
tests = localOption (mkTimeout 2000000) $ testGroup "pinafore" [testNumbers, testQueryValues, testQueries]

main :: IO ()
main = defaultMain tests
