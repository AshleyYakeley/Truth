{-# OPTIONS -fno-warn-orphans #-}

module Test.Language
    ( testLanguage
    ) where

import Data.Ratio
import Pinafore
import Pinafore.Language.Expression
import Prelude (read)
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

instance Eq (PreciseEq t) => Eq (PreciseEq (Maybe t)) where
    (MkPreciseEq Nothing) == (MkPreciseEq Nothing) = True
    (MkPreciseEq (Just a)) == (MkPreciseEq (Just b)) = MkPreciseEq a == MkPreciseEq b
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
    -> Maybe t
    -> TestTree
testRead str t = testCase (show str) $ assertEqual "" (MkPreciseEq t) $ MkPreciseEq $ readMaybe str

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
        , testRead "" $ Nothing @Number
        , testRead " " $ Nothing @Number
        , testRead "  " $ Nothing @Number
        , testRead "NaN" $ Just $ InexactNumber $ 0 / 0
        , testRead "~Infinity" $ Just $ InexactNumber $ 1 / 0
        , testRead "~-Infinity" $ Just $ InexactNumber $ -1 / 0
        , testRead " 1" $ Just $ ExactNumber 1
        , testRead "1 " $ Just $ ExactNumber 1
        , testRead " 1 " $ Just $ ExactNumber 1
        , testRead "z" $ Nothing @Number
        , testRead "ZZ" $ Nothing @Number
        , testRead "~1Z" $ Nothing @Number
        , testRead "~-1.1Z" $ Nothing @Number
        , testRead "0" $ Just $ ExactNumber 0
        , testRead "0." $ Just $ ExactNumber 0
        , testRead "0.0" $ Just $ ExactNumber 0
        , testRead "0._" $ Just $ ExactNumber 0
        , testRead "0._0" $ Just $ ExactNumber 0
        , testRead "0.0_" $ Just $ ExactNumber 0
        , testRead "0.0_0" $ Just $ ExactNumber 0
        ]

testNumbers :: TestTree
testNumbers = testGroup "numbers" [testNumbersArithemetic, testNumbersShowRead]

-- | for test only
instance Eq (QValue baseedit) where
    (MkAny QTConstLiteral a1) == (MkAny QTConstLiteral a2) = a1 == a2
    (MkAny QTList a1) == (MkAny QTList a2) = a1 == a2
    (MkAny t1 _) == (MkAny t2 _) = error $ "QValue: " <> show t1 <> " & " <> show t2 <> " not comparable"

testQueryValue :: String -> QTypeCheck (QExpr PinaforeEdit) -> Maybe (QValue PinaforeEdit) -> TestTree
testQueryValue name texpr expected =
    testCase name $
    case runQTypeCheck texpr of
        SuccessResult expr -> assertEqual "result" expected $ qEvalExpr expr
        FailureResult err -> fail $ unpack err

qint :: Int -> QValue baseedit
qint = toQValue

testQueryValues :: TestTree
testQueryValues =
    testGroup
        "query values"
        [ testQueryValue "pure A" (return $ opoint $ toQValue ("A" :: Text)) (Just $ toQValue ("A" :: Text))
        , testQueryValue "var a" (qVarExpr "a") Nothing
        , testQueryValue
              "let a=1;b=2 in (a,b,a,b)"
              (do
                   ae <- qVarExpr "a"
                   be <- qVarExpr "b"
                   e1 <- qLetExpr "b" (opoint $ qint 2) $ qSequenceExpr [ae, be, ae, be]
                   qLetExpr "a" (opoint $ qint 1) e1) $
          Just $ toQValue ([1, 2, 1, 2] :: [Int])
        , testQueryValue
              "let a=1;b=2 in (b,a,b,a)"
              (do
                   ae <- qVarExpr "a"
                   be <- qVarExpr "b"
                   e1 <- qLetExpr "b" (opoint $ qint 2) $ qSequenceExpr [be, ae, be, ae]
                   qLetExpr "a" (opoint $ qint 1) e1) $
          Just $ toQValue ([2, 1, 2, 1] :: [Int])
        ]

testQuery :: Text -> Maybe String -> TestTree
testQuery query expected =
    testCase (unpack query) $
    case (expected, parseValue @PinaforeEdit "<input>" query) of
        (Nothing, FailureResult _) -> return ()
        (Nothing, SuccessResult v) -> assertFailure $ "expected failure, found success: " ++ show v
        (Just _, FailureResult e) -> assertFailure $ "expected success, found failure: " ++ unpack e
        (Just s, SuccessResult (v :: QValue PinaforeEdit)) -> assertEqual "result" s (show v)

showText :: Text -> String
showText = show

testQueries :: TestTree
testQueries =
    testGroup
        "queries"
        [ testQuery "" $ Nothing
        , testQuery "x" $ Nothing
        -- constants
        , testQuery "\"\"" $ Just $ showText ""
        , testQuery "\"Hello \"" $ Just $ showText "Hello "
        , testQuery "true" $ Just $ showText "true"
        , testQuery "false" $ Just $ showText "false"
        , testQuery "\"1\"" $ Just $ showText "1"
        , testQuery "3" $ Just $ showText "3"
        , testQuery "3.2_4" $ Just $ showText "3.2_4"
        , testQuery "~1" $ Just $ showText "~1.0"
        , testQuery "~-2.4" $ Just $ showText "~-2.4"
        , testQuery "NaN" $ Just $ showText "NaN"
        , testQuery "~Infinity" $ Just $ showText "~Infinity"
        , testQuery "~-Infinity" $ Just $ showText "~-Infinity"
        , testQuery "ui_table" $ Just "<value -> value>"
        -- list construction
        , testQuery "[]" $ Just $ show @[Text] []
        , testQuery "[1]" $ Just $ show @[Text] ["1"]
        , testQuery "[1,2,3]" $ Just $ show @[Text] ["1", "2", "3"]
        -- functions
        , testQuery "\\x -> x" $ Just "<value -> value>"
        , testQuery "\\x -> 1" $ Just "<value -> value>"
        , testQuery "\\x y -> y" $ Just "<value -> value>"
        , testQuery "\\x y z -> [x,y,z]" $ Just "<value -> value>"
        -- let-binding
        , testQuery "let in 27" $ Just $ showText "27"
        , testQuery "let a=\"5\" in a" $ Just $ showText "5"
        , testQuery "let a=5 in a" $ Just $ showText "5"
        , testQuery "let a=1;b=2 in a" $ Just $ showText "1"
        , testQuery "let a=1;b=2 in b" $ Just $ showText "2"
        , testQuery "let a=1;b=2 in b" $ Just $ showText "2"
        , testQuery "let a=1;b=\"2\" in b" $ Just $ showText "2"
        , testQuery "let a=1 ;b=\"2\" in b" $ Just $ showText "2"
        , testQuery "let a= 1 ;b=\"2\" in b" $ Just $ showText "2"
        , testQuery "let a=7;b=a in a" $ Just $ showText "7"
        , testQuery "let a=7;b=a in b" $ Just $ showText "7"
        , testQuery "let a=2 in let b=a in b" $ Just $ showText "2"
        -- partial keywords
        , testQuery "let i=1 in i" $ Just $ showText "1"
        , testQuery "let inx=1 in inx" $ Just $ showText "1"
        , testQuery "let l=1 in l" $ Just $ showText "1"
        , testQuery "let le=1 in le" $ Just $ showText "1"
        , testQuery "let letx=1 in letx" $ Just $ showText "1"
        , testQuery "let letre=1 in letre" $ Just $ showText "1"
        , testQuery "let letrecx=1 in letrecx" $ Just $ showText "1"
        , testQuery "let tru=1 in tru" $ Just $ showText "1"
        , testQuery "let truex=1 in truex" $ Just $ showText "1"
        , testQuery "let f=1 in f" $ Just $ showText "1"
        , testQuery "let fals=1 in fals" $ Just $ showText "1"
        , testQuery "let falsex=1 in falsex" $ Just $ showText "1"
        -- recursive let-binding
        , testQuery "let a=1 in a" $ Just $ showText "1"
        , testQuery "let a=1;b=a in b" $ Just $ showText "1"
        , testQuery "let b=a;a=1 in b" $ Just $ showText "1"
        , testQuery "let a x = x in a 1" $ Just $ showText "1"
        , testQuery "let a x = x; b = a in b" $ Just "<value -> value>"
        , testQuery "let a = \\x -> x in let b = a 1 in b" $ Just $ showText "1"
        , testQuery "let a x = x; b = a 1 in b" $ Just $ showText "1"
        , testQuery "let a x = b; b = b in a" $ Just "<value -> value>"
        , testQuery "let a x = 1; b = b in a b" $ Just $ showText "1"
        , testQuery "let a x = 1; b = a b in b" $ Just $ showText "1"
        , testQuery "let b = a b; a x = 1 in b" $ Just $ showText "1"
        , testQuery "let a x = 1; b = a c; c=b in b" $ Just $ showText "1"
        -- duplicate bindings
        , testQuery "let a=1;a=1 in a" $ Nothing
        , testQuery "let a=1;a=2 in a" $ Nothing
        , testQuery "let a=1;b=0;a=2 in a" $ Nothing
        -- lexical scoping
        , testQuery "let a=1 in let b=a in let a=3 in a" $ Just $ showText "3"
        , testQuery "let a=1;b=a;a=3 in a" $ Nothing
        , testQuery "let a=1 in let b=a in let a=3 in b" $ Just $ showText "1"
        , testQuery "let a=1;b=a;a=3 in b" $ Nothing
        -- operators
        , testQuery "0 == 1" $ Just $ showText "false"
        , testQuery "1 == 1" $ Just $ showText "true"
        , testQuery "0 /= 1" $ Just $ showText "true"
        , testQuery "1 /= 1" $ Just $ showText "false"
        , testQuery "0 <= 1" $ Just $ showText "true"
        , testQuery "1 <= 1" $ Just $ showText "true"
        , testQuery "2 <= 1" $ Just $ showText "false"
        , testQuery "0 < 1" $ Just $ showText "true"
        , testQuery "1 < 1" $ Just $ showText "false"
        , testQuery "2 < 1" $ Just $ showText "false"
        , testQuery "0 >= 1" $ Just $ showText "false"
        , testQuery "1 >= 1" $ Just $ showText "true"
        , testQuery "2 >= 1" $ Just $ showText "true"
        , testQuery "0 >= ~1" $ Just $ showText "false"
        , testQuery "1 >= ~1" $ Just $ showText "true"
        , testQuery "2 >= ~1" $ Just $ showText "true"
        , testQuery "0 > 1" $ Just $ showText "false"
        , testQuery "1 > 1" $ Just $ showText "false"
        , testQuery "2 > 1" $ Just $ showText "true"
        , testQuery "1 == ~1" $ Just $ showText "false"
        , testQuery "0 ~== 1" $ Just $ showText "false"
        , testQuery "1 ~== 1" $ Just $ showText "true"
        , testQuery "1 ~== ~1" $ Just $ showText "true"
        , testQuery "0 ~== ~1" $ Just $ showText "false"
        , testQuery "0 ~/= 1" $ Just $ showText "true"
        , testQuery "1 ~/= 1" $ Just $ showText "false"
        , testQuery "1 ~/= ~1" $ Just $ showText "false"
        , testQuery "0 ~/= ~1" $ Just $ showText "true"
        , testQuery "7+8" $ Just $ showText "15"
        , testQuery "7 +8" $ Just $ showText "15"
        , testQuery "7+ 8" $ Just $ showText "15"
        , testQuery "7 + 8" $ Just $ showText "15"
        , testQuery "\"abc\"++\"def\"" $ Just $ showText "abcdef"
        , testQuery "\"abc\" ++\"def\"" $ Just $ showText "abcdef"
        , testQuery "\"abc\"++ \"def\"" $ Just $ showText "abcdef"
        , testQuery "\"abc\" ++ \"def\"" $ Just $ showText "abcdef"
        , testQuery "let f x = x + 2 in f -1" $ Just $ showText "1"
        , testQuery "let f = 2 in f - 1" $ Just $ showText "1"
        -- boolean
        , testQuery "true && true" $ Just $ showText "true"
        , testQuery "true && false" $ Just $ showText "false"
        , testQuery "false && true" $ Just $ showText "false"
        , testQuery "false && false" $ Just $ showText "false"
        , testQuery "true || true" $ Just $ showText "true"
        , testQuery "true || false" $ Just $ showText "true"
        , testQuery "false || true" $ Just $ showText "true"
        , testQuery "false || false" $ Just $ showText "false"
        , testQuery "not true" $ Just $ showText "false"
        , testQuery "not false" $ Just $ showText "true"
        -- operator precedence
        , testQuery "1 + 2 * 3" $ Just $ showText "7"
        , testQuery "3 * 2 + 1" $ Just $ showText "7"
        , testQuery "2 * 2 * 2" $ Just $ showText "8"
        , testQuery "12 / 2 / 2" $ Just $ showText "3"
        , testQuery "12 / 2 / 2" $ Just $ showText "3"
        , testQuery "0 == 0 == 0" $ Nothing
        -- if/then/else
        , testQuery "if true then 3 else 4" $ Just $ showText "3"
        , testQuery "if false then 3 else 4" $ Just $ showText "4"
        , testQuery "if false then if true then 1 else 2 else if true then 3 else 4" $ Just $ showText "3"
        ]

testLanguage :: TestTree
testLanguage = localOption (mkTimeout 2000000) $ testGroup "language" [testNumbers, testQueryValues, testQueries]
