{-# OPTIONS -fno-warn-orphans #-}

module Test.Language
    ( testLanguage
    )
where

import Data.Shim
import Shapes
import Shapes.Numeric
import Shapes.Test
import Prelude (read)

import Pinafore.Documentation
import Pinafore.Test.Internal

textTypeTest :: Text -> Text -> TestTree
textTypeTest text expected =
    testTree (unpack text) $ parseExpressionToType text $ \found -> assertEqual "" expected found

testOp :: Name -> TestTree
testOp n =
    testTree (show $ unpack n) $ do
        case unpack n of
            '(' : _ -> assertFailure "parenthesis"
            _ -> return ()
        case operatorFixity n of
            MkFixity AssocLeft 10 -> assertFailure "unassigned fixity"
            _ -> return ()

testInfix :: TestTree
testInfix =
    testTree "infix"
        $ fmap testOp
        $ allOperatorNames
        $ \case
            ValueDocItem{} -> True
            _ -> False

newtype PreciseEq t
    = MkPreciseEq t

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
testCalc name expected found = testTree name $ assertEqual "" (MkPreciseEq expected) (MkPreciseEq found)

testNumbersArithemetic :: TestTree
testNumbersArithemetic =
    testTree
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
    forall t.
    (Show t, Eq (PreciseEq t), Read t) =>
    String ->
    t ->
    TestTree
testShowRead str t =
    testTree
        (show str)
        [ testTree "show" $ assertEqual "" str $ show t
        , testTree "read" $ assertEqual "" (MkPreciseEq t) $ MkPreciseEq $ read str
        , testTree "read-show" $ assertEqual "" str $ show $ read @t str
        ]

testRead ::
    forall t.
    (Show t, Eq (PreciseEq t), Read t) =>
    String ->
    Maybe t ->
    TestTree
testRead str t = testTree (show str) $ assertEqual "" (MkPreciseEq t) $ MkPreciseEq $ readMaybe str

testNumbersShowRead :: TestTree
testNumbersShowRead =
    testTree
        "show,read"
        [ testShowRead "0" $ ExactNumber 0
        , testShowRead "1" $ ExactNumber 1
        , testShowRead "-1" $ ExactNumber $ negate 1
        , testShowRead "5/14" $ ExactNumber $ 5 / 14
        , testShowRead "-8/11" $ ExactNumber $ -8 / 11
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
        , testRead "z" $ Nothing @Number
        , testRead "ZZ" $ Nothing @Number
        , testRead "~1Z" $ Nothing @Number
        , testRead "~-1.1Z" $ Nothing @Number
        , testRead "0" $ Just $ ExactNumber 0
        ]

testNumbers :: TestTree
testNumbers = testTree "numbers" [testNumbersArithemetic, testNumbersShowRead]

data LangResult
    = LRCheckFail
    | LRRunError
    | LRSuccess String

goodLangResult :: Bool -> LangResult -> LangResult
goodLangResult True lr = lr
goodLangResult False _ = LRCheckFail

testNamedQuery :: String -> Text -> LangResult -> TestTree
testNamedQuery name query expected =
    testTree name
        $ runTester defaultTester
        $ do
            result <-
                testerLiftInterpreter
                    $ tryExc
                    $ do
                        v <- parseToValue query []
                        showPinaforeModel v
            liftIO
                $ case result of
                    FailureResult (Left pe) ->
                        case expected of
                            LRCheckFail -> return ()
                            _ -> assertFailure $ "check: expected success, found error: " ++ show pe
                    FailureResult (Right se) ->
                        case expected of
                            _ -> assertFailure $ "check: found exception: " ++ show se
                    SuccessResult r -> do
                        me <- catchPureError r
                        case (expected, me) of
                            (LRCheckFail, _) -> assertFailure $ "check: expected failure, found success"
                            (LRRunError, Nothing) -> assertFailure $ "run: expected error, found success: " ++ r
                            (LRRunError, Just _) -> return ()
                            (LRSuccess _, Just e) -> assertFailure $ "run: expected success, found error: " ++ show e
                            (LRSuccess s, Nothing) -> assertEqual "result" s r

testQuery :: Text -> LangResult -> TestTree
testQuery query = testNamedQuery (show query) query

testSubsumeSubtype :: Bool -> Text -> Text -> [Text] -> TestTree
testSubsumeSubtype good t1 t2 vs =
    testTree "subsume"
        $ [ testNamedQuery "plain" ("let {x : " <> t1 <> " = undefined; y : " <> t2 <> " = x} ()")
                $ goodLangResult good
                $ LRSuccess "()"
          , testTree "let-1"
                $ fmap
                    ( \v ->
                        testNamedQuery (show v) ("let {x : " <> t1 <> " = " <> v <> "} x : " <> t2)
                            $ goodLangResult good
                            $ LRSuccess
                            $ unpack v
                    )
                    vs
          , testTree "let-2"
                $ fmap
                    ( \v ->
                        testNamedQuery (show v) ("let {x : " <> t1 <> " = " <> v <> "; y : " <> t2 <> " = x} y")
                            $ goodLangResult good
                            $ LRSuccess
                            $ unpack v
                    )
                    vs
          ]

testFunctionSubtype :: Bool -> Text -> Text -> [Text] -> TestTree
testFunctionSubtype good t1 t2 vs =
    testTree "function"
        $ [ testNamedQuery "id" ("(fn x => x) : (" <> t1 <> ") -> (" <> t2 <> ")") $ goodLangResult good $ LRSuccess "<?>"
          , testNamedQuery "let" ("let {f : (" <> t1 <> ") -> (" <> t2 <> ") = fn x => x} f")
                $ goodLangResult good
                $ LRSuccess "<?>"
          , testTree "vars"
                $ fmap
                    ( \v ->
                        testNamedQuery (show v) ("let {f : (" <> t1 <> ") -> (" <> t2 <> ") = fn x => x} f (" <> v <> ")")
                            $ goodLangResult good
                            $ LRSuccess
                            $ unpack v
                    )
                    vs
          ]

testSubtype1 :: Bool -> Bool -> Text -> Text -> [Text] -> [TestTree]
testSubtype1 good b t1 t2 vs =
    [testSubsumeSubtype good t1 t2 vs]
        <> if b
            then [testFunctionSubtype good t1 t2 vs]
            else []

testSubtype :: Bool -> Text -> Text -> [Text] -> TestTree
testSubtype b t1 t2 vs =
    testTree (show $ t1 <> " <: " <> t2) $ testSubtype1 True b t1 t2 vs <> testSubtype1 False b t2 t1 vs

testSameType :: Bool -> Text -> Text -> [Text] -> TestTree
testSameType b t1 t2 vs =
    testTree (show $ t1 <> " = " <> t2) $ testSubtype1 True b t1 t2 vs <> testSubtype1 True b t2 t1 vs

testQueries :: TestTree
testQueries =
    testTree
        "queries"
        [ testTree "trivial" [testQuery "" $ LRCheckFail, testQuery "x" $ LRCheckFail]
        , testTree
            "comments"
            [ testQuery "# comment\n1" $ LRSuccess "1"
            , testQuery "1# comment\n" $ LRSuccess "1"
            , testQuery "1 # comment\n" $ LRSuccess "1"
            , testQuery "{# comment #} 1" $ LRSuccess "1"
            , testQuery "{# comment #}\n1" $ LRSuccess "1"
            , testQuery "{# comment\ncomment #}\n1" $ LRSuccess "1"
            , testQuery "{# comment\ncomment\n#}\n1" $ LRSuccess "1"
            , testQuery "{# A {# B #} C #} 1" $ LRSuccess "1"
            , testQuery "{#\nA\n{#\nB\n#}\nC\n#}\n1" $ LRSuccess "1"
            ]
        , testTree
            "constants"
            [ testTree
                "numeric"
                [ testQuery "0.5" $ LRSuccess "1/2"
                , testQuery "0._3" $ LRSuccess "1/3"
                , testQuery "-0._3" $ LRSuccess "-1/3"
                , testQuery "-0.0_3" $ LRSuccess "-1/30"
                , testQuery "0.3_571428" $ LRSuccess "5/14"
                , testQuery "0." $ LRSuccess "0"
                , testQuery "0.0" $ LRSuccess "0"
                , testQuery "0._" $ LRSuccess "0"
                , testQuery "0._0" $ LRSuccess "0"
                , testQuery "0.0_" $ LRSuccess "0"
                , testQuery "0.0_0" $ LRSuccess "0"
                , testQuery "3" $ LRSuccess "3"
                , testQuery "3.2_4" $ LRSuccess "146/45"
                , testQuery "~1" $ LRSuccess "~1.0"
                , testQuery "~-2.4" $ LRSuccess "~-2.4"
                , testQuery "NaN" $ LRSuccess "NaN"
                , testQuery "~Infinity" $ LRSuccess "~Infinity"
                , testQuery "~-Infinity" $ LRSuccess "~-Infinity"
                ]
            , testQuery "\"\"" $ LRSuccess "\"\""
            , testQuery "\"Hello \"" $ LRSuccess "\"Hello \""
            , testQuery "True" $ LRSuccess "True"
            , testQuery "False" $ LRSuccess "False"
            , testQuery "\"1\"" $ LRSuccess "\"1\""
            , testQuery "(+)" $ LRSuccess "<?>"
            , testQuery "length.Text." $ LRSuccess "<?>"
            , testQuery "let {entitytype T} !{point.OpenEntity @T !\"example\"}" $ LRSuccess "<?>"
            , testQuery "let {entitytype T} anchor.Entity $.Function !{point.OpenEntity @T !\"example\"}"
                $ LRSuccess "\"!F332D47A-3C96F533-854E5116-EC65D65E-5279826F-25EE1F57-E925B6C3-076D3BEC\""
            ]
        , testTree
            "construction"
            [ testQuery "[]" $ LRSuccess $ show @[Text] []
            , testQuery "[1]" $ LRSuccess $ "[1]"
            , testQuery "(1,2)" $ LRSuccess $ "(1,2)"
            , testQuery "(1,2,3)" $ LRSuccess $ "(1,(2,3))"
            , testQuery "Left 4" $ LRSuccess $ "Left 4"
            , testQuery "Right 5" $ LRSuccess $ "Right 5"
            , testQuery "[1,2]" $ LRSuccess $ "[1,2]"
            , testQuery "[1,2,3]" $ LRSuccess "[1,2,3]"
            ]
        , testTree
            "functions"
            [ testQuery "fn x => x" $ LRSuccess "<?>"
            , testQuery "fn x => 1" $ LRSuccess "<?>"
            , testQuery "fn x, y => y" $ LRSuccess "<?>"
            , testQuery "fn x, y, z => [x,y,z]" $ LRSuccess "<?>"
            ]
        , testTree
            "predefined"
            [ testQuery "abs.Integer" $ LRSuccess "<?>"
            , testQuery "fst.Product" $ LRSuccess "<?>"
            , testQuery "(+.Integer)" $ LRSuccess "<?>"
            , testQuery "fn a, b => a +.Integer b" $ LRSuccess "<?>"
            , testQuery "(==.Entity)" $ LRSuccess "<?>"
            , testQuery "fn a, b => a ==.Entity b" $ LRSuccess "<?>"
            ]
        , testTree
            "let-binding"
            [ testQuery "let {} 27" $ LRSuccess "27"
            , testQuery "let {a=\"5\"} a" $ LRSuccess "\"5\""
            , testQuery "let {a=5} a" $ LRSuccess "5"
            , testQuery "let {a=1} let {a=2} a" $ LRSuccess "2"
            , testQuery "let {a=1;b=2} a" $ LRSuccess "1"
            , testQuery "let {a=1;b=2} b" $ LRSuccess "2"
            , testQuery "let {a=1;b=2} b" $ LRSuccess "2"
            , testQuery "let {a=1;b=\"2\"} b" $ LRSuccess "\"2\""
            , testQuery "let {a=1 ;b=\"2\"} b" $ LRSuccess "\"2\""
            , testQuery "let {a= 1 ;b=\"2\"} b" $ LRSuccess "\"2\""
            , testQuery "let {a=7;b=a} a" $ LRSuccess "7"
            , testQuery "let {a=7;b=a} b" $ LRSuccess "7"
            , testQuery "let {a=2} let {b=a} b" $ LRSuccess "2"
            , testTree
                "operator"
                [ testQuery "(-) 9 5" $ LRSuccess "4"
                , testQuery "let {minus = (-)} minus 4 2" $ LRSuccess "2"
                , testQuery "let {(&$$&) = fn a,b => a -.Integer b} 374 &$$& 21" $ LRSuccess "353"
                , testQuery "let {(&$$&) = (-)} 7 &$$& 4" $ LRSuccess "3"
                , testQuery "let {(&$$&) = fn a,b => a -.Integer b; (**$*) = (&$$&)} 15 **$* 8" $ LRSuccess "7"
                , testQuery "let {(&$$&) = fn a,b => a -.Integer b; (**$*) = (&$$&)} 15 **$* 8" $ LRSuccess "7"
                , testQuery "let {(&$$&) = fn a,b => a -.Integer b; (**$*) = fn a,b => a &$$& b} 18 **$* 3"
                    $ LRSuccess "15"
                ]
            , testTree
                "recursive"
                [ testQuery "let rec {a=1} a" $ LRSuccess "1"
                , testQuery "let rec {a=1} let rec {a=2} a" $ LRSuccess "2"
                , testQuery "let rec {a=1;a=2} a" $ LRCheckFail
                , testQuery "let rec {a=1;b=a} b" $ LRSuccess "1"
                , testQuery "let rec {b=a;a=1} b" $ LRSuccess "1"
                , testQuery "let rec {a = fn x => x} a 1" $ LRSuccess "1"
                , testQuery "let rec {a = fn x => x; b = a} b" $ LRSuccess "<?>"
                , testQuery "let rec {a = fn x => x} let rec {b = a 1} b" $ LRSuccess "1"
                , testQuery "let rec {a = fn x => x; b = a 1} b" $ LRSuccess "1"
                , testQuery "let rec {a = fn x => b; b = b} a" $ LRSuccess "<?>"
                , testQuery "let rec {a = fn x => 1; b = b} a b" $ LRSuccess "1"
                , testQuery "let rec {a = fn x => 1; b = a b} b" $ LRSuccess "1"
                , testQuery "let rec {a = fn x => 1} let rec {b = a b} b" $ LRSuccess "1"
                , testQuery "let rec {b = (fn x => 1) b} b" $ LRSuccess "1"
                , testQuery "let rec {b = a b; a = fn x => 1} b" $ LRSuccess "1"
                , testQuery "let rec {a = fn x => 1; b = a c; c=b} b" $ LRSuccess "1"
                , testTree
                    "polymorphism"
                    [ testQuery "let rec {i = fn x => x} (succ.Integer $.Function i 1, i False)"
                        $ LRSuccess "(2,False)"
                    , testQuery "let rec {i = fn x => x; r = (succ.Integer $.Function i 1, i False)} r"
                        $ LRSuccess "(2,False)"
                    , testQuery "let rec {r = (succ.Integer $.Function i 1, i False); i = fn x => x} r"
                        $ LRSuccess "(2,False)"
                    ]
                ]
            , testTree
                "pattern"
                [ testQuery "let {(a,b) = (3,4)} a" $ LRSuccess "3"
                , testQuery "let {(a,b) = (3,4)} b" $ LRSuccess "4"
                , testQuery "let {(a,b): Integer *: Integer = (3,4)} (b,a)" $ LRSuccess "(4,3)"
                , testQuery "let rec {(a,b): Integer *: Integer = (3,a +.Integer 4)} (b,a)" $ LRSuccess "(7,3)"
                , testQuery "let rec {(a,b) = (3,a +.Integer 4)} (b,a)" $ LRSuccess "(7,3)"
                , testQuery "let rec {(a,b) = (3,a +.Integer 4); (c,d) = (8,c +.Integer 1)} (a,b,c,d)"
                    $ LRSuccess "(3,(7,(8,9)))"
                , testQuery "let rec {(a,b) = (3,a +.Integer 4); (c,d) = (b +.Integer 17,c +.Integer 1)} (a,b,c,d)"
                    $ LRSuccess "(3,(7,(24,25)))"
                , testQuery
                    "let rec {(a,b) = (3,a +.Integer 4); (c,d): Integer *: Integer = (b +.Integer 17,c +.Integer 1)} (a,b,c,d)"
                    $ LRSuccess "(3,(7,(24,25)))"
                , testQuery
                    "let rec {(a,b): Integer *: Integer = (3,a +.Integer 4); (c,d) = (b +.Integer 17,c +.Integer 1)} (a,b,c,d)"
                    $ LRSuccess "(3,(7,(24,25)))"
                , testQuery
                    "let rec {(a,b): Integer *: Integer = (3,a +.Integer 4); (c,d): Integer *: Integer = (b +.Integer 17,c +.Integer 1)} (a,b,c,d)"
                    $ LRSuccess "(3,(7,(24,25)))"
                ]
            , testTree
                "rename"
                [ testQuery "let {f: List a -> Integer -> List a = fn x => fn _ => x} 0" $ LRSuccess "0"
                , testQuery "let {f: List a -> Integer -> List a = fn x => fn p => x} 0" $ LRSuccess "0"
                , testQuery "let {f: List a -> Integer *: Integer -> List a = fn x => fn (p,q) => x} 0"
                    $ LRSuccess "0"
                ]
            , testTree
                "issue-199"
                [ testQuery "let {f = fn x => let {y = x} y} f 0" $ LRSuccess "0"
                , testQuery "let {f = fn x => let {x = x} x} f 0" $ LRSuccess "0"
                ]
            ]
        , testTree
            "scoping"
            [ testQuery "(fn b => fn a => b) a" LRCheckFail
            , testQuery "let {b=a} fn a => b" LRCheckFail
            , testQuery "let {b=a} ()" LRCheckFail
            , testQuery "let rec {b=a} ()" LRCheckFail
            , testQuery "let {a=1} let {b=a} (fn a => b) 2" $ LRSuccess "1"
            , testQuery "(fn a => let {b=a} (fn a => b) 2) 1" $ LRSuccess "1"
            ]
        , testTree
            "name shadowing"
            [ testQuery "let {a=1} (fn a => a) 2" $ LRSuccess "2"
            , testQuery "let {a=1} (fn (Just a) => a) (Just 2)" $ LRSuccess "2"
            , testQuery "let {a=1} let {a=2} a" $ LRSuccess "2"
            , testQuery "(fn a => let {a=2} a) 1" $ LRSuccess "2"
            , testQuery "(fn a => fn a => a) 1 2" $ LRSuccess "2"
            , testQuery "let {a=1} 2 >-.Function fn {a => a}" $ LRSuccess "2"
            , testQuery "let {a=1} Just 2 >-.Function fn {Just a => a}" $ LRSuccess "2"
            , testQuery "1 >-.Function fn {a => 2 >-.Function fn {a => a}}" $ LRSuccess "2"
            ]
        , testTree
            "partial keywords"
            [ testQuery "let {i=1} i" $ LRSuccess "1"
            , testQuery "let {inx=1} inx" $ LRSuccess "1"
            , testQuery "let {l=1} l" $ LRSuccess "1"
            , testQuery "let {le=1} le" $ LRSuccess "1"
            , testQuery "let {letx=1} letx" $ LRSuccess "1"
            , testQuery "let {letre=1} letre" $ LRSuccess "1"
            , testQuery "let {letrecx=1} letrecx" $ LRSuccess "1"
            , testQuery "let {tru=1} tru" $ LRSuccess "1"
            , testQuery "let {truex=1} truex" $ LRSuccess "1"
            , testQuery "let {f=1} f" $ LRSuccess "1"
            , testQuery "let {fals=1} fals" $ LRSuccess "1"
            , testQuery "let {falsex=1} falsex" $ LRSuccess "1"
            ]
        , testTree
            "duplicate bindings"
            [ testQuery "let rec {a=1;a=1} a" $ LRCheckFail
            , testQuery "let rec {a=1;a=2} a" $ LRCheckFail
            , testQuery "let rec {a=1;b=0;a=2} a" $ LRCheckFail
            ]
        , testTree
            "lexical scoping"
            [ testQuery "let {a=1} let {b=a} let {a=3} a" $ LRSuccess "3"
            , testQuery "let rec {a=1;b=a;a=3} a" $ LRCheckFail
            , testQuery "let {a=1} let {b=a} let {a=3} b" $ LRSuccess "1"
            , testQuery "let rec {a=1;b=a;a=3} b" $ LRCheckFail
            ]
        , testTree
            "implicit parameters"
            [ testQuery "let {x = ?p} x" $ LRCheckFail
            , testQuery "let {x = ?p} imply {?p = 5} x" $ LRSuccess "5"
            , testQuery "let {x : Integer = ?p} imply {?p = 43} x" $ LRSuccess "43"
            ]
        , testTree
            "operator"
            [ testQuery "0 ==.Entity 1" $ LRSuccess "False"
            , testQuery "1 ==.Entity 1" $ LRSuccess "True"
            , testQuery "0 /=.Entity 1" $ LRSuccess "True"
            , testQuery "1 /=.Entity 1" $ LRSuccess "False"
            , testQuery "0 <=.Number 1" $ LRSuccess "True"
            , testQuery "1 <=.Number 1" $ LRSuccess "True"
            , testQuery "2 <=.Number 1" $ LRSuccess "False"
            , testQuery "0 <.Number 1" $ LRSuccess "True"
            , testQuery "1 <.Number 1" $ LRSuccess "False"
            , testQuery "2 <.Number 1" $ LRSuccess "False"
            , testQuery "0 >=.Number 1" $ LRSuccess "False"
            , testQuery "1 >=.Number 1" $ LRSuccess "True"
            , testQuery "2 >=.Number 1" $ LRSuccess "True"
            , testQuery "0 >=.Number ~1" $ LRSuccess "False"
            , testQuery "1 >=.Number ~1" $ LRSuccess "True"
            , testQuery "2 >=.Number ~1" $ LRSuccess "True"
            , testQuery "0 >.Number 1" $ LRSuccess "False"
            , testQuery "1 >.Number 1" $ LRSuccess "False"
            , testQuery "2 >.Number 1" $ LRSuccess "True"
            , testQuery "1 ==.Entity ~1" $ LRSuccess "False"
            , testQuery "0 ==.Number 1" $ LRSuccess "False"
            , testQuery "1 ==.Number 1" $ LRSuccess "True"
            , testQuery "1 ==.Number ~1" $ LRSuccess "True"
            , testQuery "0 ==.Number ~1" $ LRSuccess "False"
            , testQuery "0 /=.Number 1" $ LRSuccess "True"
            , testQuery "1 /=.Number 1" $ LRSuccess "False"
            , testQuery "1 /=.Number ~1" $ LRSuccess "False"
            , testQuery "0 /=.Number ~1" $ LRSuccess "True"
            , testQuery "with Integer 7+8" $ LRSuccess "15"
            , testQuery "with Integer 7 +8" $ LRSuccess "15"
            , testQuery "with Integer 7+ 8" $ LRSuccess "15"
            , testQuery "with Integer 7 + 8" $ LRSuccess "15"
            , testQuery "\"abc\"<>.Text\"def\"" $ LRSuccess "\"abcdef\""
            , testQuery "\"abc\" <>.Text\"def\"" $ LRSuccess "\"abcdef\""
            , testQuery "\"abc\"<>.Text \"def\"" $ LRSuccess "\"abcdef\""
            , testQuery "\"abc\" <>.Text \"def\"" $ LRSuccess "\"abcdef\""
            , testQuery "with Integer let {f = fn x => x + 2} f -1" $ LRSuccess "1"
            , testQuery "with Integer let {f = 2} f - 1" $ LRSuccess "1"
            , testTree
                "precedence"
                [ testQuery "with Integer succ $.Function 2 * 3" $ LRSuccess "7"
                , testQuery "with Integer 3 * 2 + 1" $ LRSuccess "7"
                , testQuery "with Integer 2 * 2 * 2" $ LRSuccess "8"
                , testQuery "with Rational 12 / 2 / 2" $ LRSuccess "3"
                , testQuery "0 ==.Entity 0" $ LRSuccess "True"
                , testQuery "0 ==.Entity 0 ==.Entity 0" $ LRCheckFail
                ]
            ]
        , testTree
            "boolean"
            [ testQuery "True && True" $ LRSuccess "True"
            , testQuery "True && False" $ LRSuccess "False"
            , testQuery "False && True" $ LRSuccess "False"
            , testQuery "False && False" $ LRSuccess "False"
            , testQuery "True || True" $ LRSuccess "True"
            , testQuery "True || False" $ LRSuccess "True"
            , testQuery "False || True" $ LRSuccess "True"
            , testQuery "False || False" $ LRSuccess "False"
            , testQuery "not True" $ LRSuccess "False"
            , testQuery "not False" $ LRSuccess "True"
            ]
        , testTree
            "text"
            [ testQuery "\"pqrs\"" $ LRSuccess "\"pqrs\""
            , testQuery "length.Text \"abd\"" $ LRSuccess "3"
            , testQuery "section.Text 4 3 \"ABCDEFGHIJKLMN\"" $ LRSuccess "\"EFG\""
            ]
        , testTree
            "if-then-else"
            [ testQuery "if True then 3 else 4" $ LRSuccess "3"
            , testQuery "if False then 3 else 4" $ LRSuccess "4"
            , testQuery "if False then if True then 1 else 2 else if True then 3 else 4" $ LRSuccess "3"
            ]
        , testTree
            "product"
            [testQuery "fst.Product (7,9)" $ LRSuccess "7", testQuery "snd.Product (7,9)" $ LRSuccess "9"]
        , testTree
            "sum"
            [ testQuery "from.Sum (fn a => (\"Left\",a)) (fn a => (\"Right\",a)) $.Function Left \"x\""
                $ LRSuccess "(\"Left\",\"x\")"
            , testQuery "from.Sum (fn a => (\"Left\",a)) (fn a => (\"Right\",a)) $.Function Right \"x\""
                $ LRSuccess "(\"Right\",\"x\")"
            ]
        , testTree
            "type-signature"
            [ testQuery "let {i = fn x => x} i 3" $ LRSuccess "3"
            , testQuery "let {i : tvar -> tvar = fn x => x} i 3" $ LRSuccess "3"
            , testQuery "let {i : a -> a = fn x => x} i 3" $ LRSuccess "3"
            , testQuery "let {i : Number -> Number = fn x => x} i 3" $ LRSuccess "3"
            , testQuery "let {i : Text -> Text = fn x => x} i 3" $ LRCheckFail
            , testQuery "let {i : a -> a = fn x => x} i \"t\"" $ LRSuccess "\"t\""
            , testQuery "let {i : Number -> Number = fn x => x} i \"t\"" $ LRCheckFail
            , testQuery "let {i : Text -> Text = fn x => x} i \"t\"" $ LRSuccess "\"t\""
            , testQuery "let {i : a -> a = fn x => x} 0" $ LRSuccess "0"
            , testQuery "let {i : a -> Number = fn x => x} 0" $ LRCheckFail
            , testQuery "let {i : Number -> a = fn x => x} 0" $ LRCheckFail
            , testQuery "let {i : Number -> Number = fn x => x} 0" $ LRSuccess "0"
            , testQuery "let {i : Number +: Boolean = Left 5} i" $ LRSuccess "Left 5"
            , testQuery "let {i : Number +: Boolean = Right False} i" $ LRSuccess "Right False"
            , testQuery "let {i : Maybe Number = Just 5} i" $ LRSuccess "Just 5"
            , testQuery "let {i : Maybe Number = Nothing} i" $ LRSuccess "Nothing"
            , testTree
                "polar"
                [ testQuery "let {x : Text | Number = 3} x" $ LRSuccess "3"
                , testQuery "let {f : Any -> Integer = fn _ => 3} f ()" $ LRSuccess "3"
                , testQuery "(fn x => (x,x)) : ((a & Number) -> Showable *: a)" $ LRSuccess "<?>"
                , testQuery "let {f = (fn x => (x,x)) : (a & Number) -> Showable *: a} f 3" $ LRSuccess "(3,3)"
                , testQuery "let {f : (a & Number) -> Showable *: a = fn x => (x,x)} f 3" $ LRSuccess "(3,3)"
                ]
            ]
        , testTree
            "pattern"
            [ testQuery "(fn a => 5) 2" $ LRSuccess "5"
            , testQuery "(fn a => a) 2" $ LRSuccess "2"
            , testQuery "(fn _ => 5) 2" $ LRSuccess "5"
            , testQuery "(fn (a,b) => a +.Integer b) (5,6)" $ LRSuccess "11"
            , testQuery "(fn Just a => a) (Just 71)" $ LRSuccess "71"
            , testQuery "(fn {Just a => a}) (Just 72)" $ LRSuccess "72"
            , testQuery "let {Just a = Just 73} a" $ LRSuccess "73"
            , testTree
                "at"
                [ testQuery "(fn a@b => (a,b)) 2" $ LRSuccess "(2,2)"
                , testQuery "(fn (Just a)@(Just b) => (a,b)) (Just 578)" $ LRSuccess "(578,578)"
                , testQuery "(fn (Nothing)@Nothing => 345) Nothing" $ LRSuccess "345"
                , testQuery "(fn Nothing@Nothing => 345) Nothing" $ LRSuccess "345"
                ]
            ]
        , testTree
            "match-to"
            [ testTree
                "basic"
                [ testQuery "2 >-.Function fn {a => 5}" $ LRSuccess "5"
                , testQuery "2 >-.Function fn {a => 5; a => 3}" $ LRSuccess "5"
                , testQuery "2 >-.Function fn {a => 5; a => 3;}" $ LRSuccess "5"
                , testQuery "2 >-.Function fn {a => a}" $ LRSuccess "2"
                , testQuery "2 >-.Function fn {_ => 5}" $ LRSuccess "5"
                , testQuery "2 >-.Function fn {_ => 5; _ => 3}" $ LRSuccess "5"
                , testQuery "2 >-.Function fn {a@b => (a,b)}" $ LRSuccess "(2,2)"
                ]
            , testTree
                "Boolean"
                [ testQuery "True >-.Function fn {True => 5; False => 7}" $ LRSuccess "5"
                , testQuery "False >-.Function fn {True => 5; False => 7}" $ LRSuccess "7"
                , testQuery "True >-.Function fn {False => 7; True => 5}" $ LRSuccess "5"
                , testQuery "False >-.Function fn {False => 7; True => 5}" $ LRSuccess "7"
                ]
            , testTree
                "Number"
                [ testQuery "37 >-.Function fn {37 => True; _ => False}" $ LRSuccess "True"
                , testQuery "38 >-.Function fn {37 => True; _ => False}" $ LRSuccess "False"
                , testQuery "-24.3 >-.Function fn {37 => 1; -24.3 => 2; _ => 3}" $ LRSuccess "2"
                ]
            , testTree
                "String"
                [ testQuery "\"Hello\" >-.Function fn {\"Hello\" => True; _ => False}" $ LRSuccess "True"
                , testQuery "\"thing\" >-.Function fn {\"Hello\" => True; _ => False}" $ LRSuccess "False"
                , testQuery "\"thing\" >-.Function fn {\"Hello\" => 1; \"thing\" => 2; _ => 3}" $ LRSuccess "2"
                ]
            , testTree
                "Either"
                [ testQuery "Left 3 >-.Function fn {Left a => a; Right _ => 1}" $ LRSuccess "3"
                , testQuery "Right 4 >-.Function fn {Left a => succ.Integer a; Right a => a}" $ LRSuccess "4"
                , testQuery "Right 7 >-.Function fn {Right 4 => True; _ => False}" $ LRSuccess "False"
                , testQuery "Right 7 >-.Function fn {Right 4 => 1; Right 7 => 2; Left _ => 3; _ => 4}"
                    $ LRSuccess "2"
                ]
            , testTree "Unit" [testQuery "() >-.Function fn {() => 4}" $ LRSuccess "4"]
            , testTree "Pair" [testQuery "(2,True) >-.Function fn {(2,a) => a}" $ LRSuccess "True"]
            , testTree
                "Maybe"
                [ testQuery "Just 3 >-.Function fn {Just a => succ.Integer a; Nothing => 7}" $ LRSuccess "4"
                , testQuery "Nothing >-.Function fn {Just a => succ.Integer a; Nothing => 7}" $ LRSuccess "7"
                ]
            , testTree
                "List"
                [ testQuery "[] >-.Function fn {[] => True; _ => False}" $ LRSuccess "True"
                , testQuery "[] >-.Function fn {_::_ => True; _ => False}" $ LRSuccess "False"
                , testQuery "[1,2] >-.Function fn {[] => True; _ => False}" $ LRSuccess "False"
                , testQuery "[3,4] >-.Function fn {_::_ => True; _ => False}" $ LRSuccess "True"
                , testQuery "[3] >-.Function fn {a::b => (a,b)}" $ LRSuccess "(3,[])"
                , testQuery "[3,4] >-.Function fn {a::b => (a,b)}" $ LRSuccess "(3,[4])"
                , testQuery "[3,4,5] >-.Function fn {a::b => (a,b)}" $ LRSuccess "(3,[4,5])"
                , testQuery "[3] >-.Function fn {[a,b] => 1; _ => 2}" $ LRSuccess "2"
                , testQuery "[3,4] >-.Function fn {[a,b] => 1; _ => 2}" $ LRSuccess "1"
                , testQuery "[3,4,5] >-.Function fn {[a,b] => 1; _ => 2}" $ LRSuccess "2"
                , testQuery "[3,4] >-.Function fn {[a,b] => (a,b)}" $ LRSuccess "(3,4)"
                ]
            ]
        , testTree
            "match"
            [ testQuery "(fn {a => 5}) 2" $ LRSuccess "5"
            , testQuery "(fn {a => 5; a => 3}) 2" $ LRSuccess "5"
            , testQuery "(fn {a => 5; a => 3;}) 2" $ LRSuccess "5"
            , testQuery "(fn {a => a}) 2" $ LRSuccess "2"
            , testQuery "(fn {_ => 5}) 2" $ LRSuccess "5"
            , testQuery "(fn {_ => 5; _ => 3}) 2" $ LRSuccess "5"
            , testQuery "(fn {a@b => (a,b)}) 2" $ LRSuccess "(2,2)"
            ]
        , testTree
            "matches"
            [ testQuery "(fn {a => 5}) 2" $ LRSuccess "5"
            , testQuery "(fn {a, b => a +.Integer b}) 2 3" $ LRSuccess "5"
            , testQuery
                "(fn {Nothing, Nothing => 1; Nothing, Just a => a +.Integer 10; Just a, _ => a +.Integer 20;}) (Just 1) (Just 2)"
                $ LRSuccess "21"
            , testQuery
                "(fn {Nothing, Nothing => 1; Just a, Nothing => a +.Integer 10; _, Just a => a +.Integer 20;}) (Just 1) (Just 2)"
                $ LRSuccess "22"
            , testQuery
                "(fn {Nothing, Nothing => 1; Just a, Nothing => a +.Integer 10; Nothing, Just a => a +.Integer 20; Just a, Just b => a +.Integer b +.Integer 30;}) (Just 1) (Just 2)"
                $ LRSuccess "33"
            ]
        , testTree
            "type-operator"
            [ testSameType True "Unit" "Unit" ["()"]
            , testSameType True "List a" "List a" []
            , testSameType True "a *: b +: c *: d" "(a *: b) +: (c *: d)" []
            , testSameType True "a *: b *: c *: d" "a *: (b *: (c *: d))" []
            , testSameType
                True
                "Integer *: Boolean *: Integer *: Boolean"
                "Integer *: (Boolean *: (Integer *: Boolean))"
                ["(3,(True,(7,False)))"]
            ]
        , testTree
            "subtype"
            [ testQuery "let {i : Integer -> Number = fn x => x} i 3" $ LRSuccess "3"
            , testQuery "let {a : Integer = 3; b : Number = a} b" $ LRSuccess "3"
            , testQuery "let {i : FiniteSetModel -a -> SetModel a = fn x => x} 3" $ LRSuccess "3"
            , testQuery "let {i : FiniteSetModel (-a,+Integer) -> SetModel a = fn x => x} 3" $ LRSuccess "3"
            ]
        , testTree
            "subsume"
            [ testQuery "let rec {a: Unit = a} ()" $ LRSuccess "()"
            , testQuery "let rec {a: Integer = a} ()" $ LRSuccess "()"
            , testQuery "let {a: Integer|Text = error.Function \"undefined\"} ()" $ LRSuccess "()"
            , testQuery "let rec {a: Integer|Text = a} ()" $ LRSuccess "()"
            , testQuery "let {a: Integer|Text = 3} ()" $ LRSuccess "()"
            , testQuery "let {a: Integer|Text = 3; b: Integer|Text = 3} ()" $ LRSuccess "()"
            , testQuery "let {a: Integer|Text = 3; b: Integer|Text = a} ()" $ LRSuccess "()"
            , testQuery "let rec {r = r} let {a: Integer|Text = r} ()" $ LRSuccess "()"
            , testQuery "let rec {r = r} let {a: Integer|Text = r} ()" $ LRSuccess "()"
            , testQuery "let rec {r = a; a: Integer|Text = r} ()" $ LRSuccess "()"
            , testQuery "let rec {a: None = a} ()" $ LRSuccess "()"
            , testQuery "let rec {r = r} let {a : None = r} ()" $ LRSuccess "()"
            , testQuery "let rec {r = a; a: None = r} ()" $ LRSuccess "()"
            , testQuery "let {a: List (Integer|Text) = []} a" $ LRSuccess "[]"
            , testQuery "let {a: List Integer | List Text = []} a" $ LRSuccess "[]"
            , testSameType True "Integer" "Integer" ["56"]
            , testSameType False "Integer|Text" "Integer|Text" []
            , testSameType False "List (Integer|Text)" "List (Integer|Text)" ["[]"]
            , testSameType False "List Integer | List Text" "List Integer | List Text" ["[]"]
            , testSameType False "List (Integer|Text)" "List Integer | List Text" ["[]"]
            , testQuery "let {a: Integer|Text = 3; b: List Integer | List Text = [a]} b" $ LRSuccess "[3]"
            , testQuery "newMem.WholeModel >>= fn m => m := 1 >> get m >>= outputLn.Env" LRCheckFail
            , testQuery
                "newMem.WholeModel >>= fn m => let {n: WholeModel a = m: WholeModel a; n1: WholeModel Integer = n: WholeModel Integer; n2: WholeModel Text = n: WholeModel Text} n1 := 1 >> get n2 >>= outputLn.Env"
                LRCheckFail
            ]
        , testTree
            "conversion"
            [ testQuery ("((fn x => Just x): Integer -> Maybe Integer) 34 >-.Function fn {Just x => x}")
                $ LRSuccess "34"
            , testQuery ("((fn x => [x]): xy -> List1 xy: Integer -> List Integer) 79") $ LRSuccess "[79]"
            , testQuery ("((fn x => x :: []): Integer -> List Integer) 57 >-.Function fn {x::_ => x}")
                $ LRSuccess "57"
            ]
        , testTree
            "recursive"
            [ testTree
                "automaton"
                [ testQuery "Nothing: Maybe (rec a, List a)" $ LRSuccess "Nothing"
                , testQuery "Nothing: Maybe (rec a, Maybe a)" $ LRSuccess "Nothing"
                , testQuery "Nothing: rec a, Maybe a" $ LRSuccess "Nothing"
                , testQuery "[]: rec a, List a" $ LRSuccess "[]"
                ]
            , let
                atree = ["[]", "[[]]", "[[[[]]]]", "[[],[[]]]"]
                in testTree
                    "equivalence"
                    [ testSameType True "Integer" "Integer" ["0"]
                    , testSameType True "Integer" "rec a, Integer" ["0"]
                    , testSameType True "List Integer" "List (rec a, Integer)" ["[0]"]
                    , testSameType True "rec a, List a" "rec a, List a" atree
                    , testSameType True "rec a, List a" "rec a, List (List a)" atree
                    , testSubtype True "rec a, Maybe a" "rec a, (Maybe a | List a)" ["Nothing", "Just Nothing"]
                    , testSubtype False "Maybe None" "rec a, (Maybe a | List a)" ["Nothing"]
                    , testSubtype False "List None" "rec a, (Maybe a | List a)" ["[]"]
                    , testSubtype False "Maybe None | List None" "rec a, (Maybe a | List a)" ["[]", "Nothing"]
                    , testSubtype False "Maybe None" "(rec a, Maybe a) | (rec b, List b)" ["Nothing"]
                    , testSubtype False "List None" "(rec a, Maybe a) | (rec b, List b)" ["[]"]
                    , testSubtype
                        False
                        "(rec a, Maybe a) | (rec b, List b)"
                        "rec a, (Maybe a | List a)"
                        ["[]", "Nothing", "Just Nothing", "[[]]"]
                    , testSubtype True "rec a, List a" "Showable" []
                    , testSubtype True "List (rec a, List a)" "Showable" []
                    , testSubtype True "rec a, List a" "List Showable" ["[]"]
                    , testSubtype True "List (rec a, List a)" "List Showable" ["[]"]
                    , testSameType False "None" "None" []
                    , testSameType True "rec a, Integer" "Integer" ["0"]
                    , testSameType True "List (rec a, Integer)" "List Integer" ["[0]"]
                    , testTree
                        "unroll"
                        [ testSameType True "rec a, List a" "List (rec a, List a)" atree
                        , testSameType
                            False
                            "rec a, (List a|Integer)"
                            "List (rec a, (List a|Integer))|Integer"
                            ["[]"]
                        , testSameType
                            False
                            "rec a, (List a|Integer)"
                            "List (rec a, (List a|Integer))|Integer"
                            ["2"]
                        , testSameType
                            False
                            "rec a, List (a|Integer)"
                            "List ((rec a, List (a|Integer))|Integer)"
                            ["[]"]
                        , testSameType
                            False
                            "rec a, List (a|Integer)"
                            "List ((rec a, List (a|Integer))|Integer)"
                            ["[3]"]
                        , testSameType
                            False
                            "rec a, List (a|Integer)"
                            "List (rec a, (List (a|Integer)|Integer))"
                            ["[]"]
                        , testSameType
                            False
                            "rec a, List (a|Integer)"
                            "List (rec a, (List (a|Integer)|Integer))"
                            ["[3]"]
                        ]
                    ]
            , testTree
                "lazy"
                [ testQuery
                    "let {lazy: Any -> Integer -> Integer = fn _, x => x} (fn x => lazy x 1) (error.Function \"strict\")"
                    $ LRSuccess "1"
                , testQuery "let {lazy: Any -> Integer -> Integer = fn _, x => x} let rec {x = lazy x 1} x"
                    $ LRSuccess "1"
                , testQuery
                    "let {lazy: Any -> Integer -> Integer = fn _, x => x} let {x = lazy (error.Function \"strict\") 1} x"
                    $ LRSuccess "1"
                , testQuery "let {lazy: Any -> Integer -> Integer = fn _, x => x} let rec {f = lazy f} f 1"
                    $ LRSuccess "1"
                , testQuery
                    "let {lazy: Any -> Integer -> Integer = fn _, x => x} let {f = lazy (error.Function \"strict\")} f 1"
                    $ LRSuccess "1"
                ]
            , testTree
                "subsume"
                [ testQuery "let rec {rval: rec a, Maybe a = rval} ()" $ LRSuccess "()"
                , testQuery "let rec {rval: rec a, Maybe a = Just rval} ()" $ LRSuccess "()"
                , testQuery
                    "with Function, Integer let rec {rcount: (rec a, Maybe a) -> Integer = fn {Nothing => 0; Just y => succ $ rcount y}} rcount"
                    $ LRSuccess "<?>"
                , testQuery
                    "with Function, Integer let rec {rcount: (rec a, Maybe a) -> Integer = fn {Nothing => 0; Just y => succ $ rcount y}} rcount Nothing"
                    $ LRSuccess "0"
                , testQuery
                    "with Function, Integer let rec {rcount: (rec a, Maybe a) -> Integer = fn {Nothing => 0; Just y => succ $ rcount1 y}; rcount1: (rec a, Maybe a) -> Integer = fn {Nothing => 0; Just y => succ $ rcount y}} rcount"
                    $ LRSuccess "<?>"
                , testQuery
                    "with Function, Integer let rec {rcount = fn {Nothing => 0; Just y => succ $ rcount1 y}; rcount1 = fn {Nothing => 0; Just y => succ $ rcount y}} rcount $ Just Nothing"
                    $ LRSuccess "1"
                , testQuery
                    "with Function, Integer let rec {rcount = rcount1; rcount1 = fn {Nothing => 0; Just y => succ $ rcount1 y}} rcount $ Just Nothing"
                    $ LRSuccess "1"
                , testQuery
                    "with Function, Integer let rec {rcount: (rec a, Maybe a) -> Integer = rcount1; rcount1 = fn {Nothing => 0; Just y => succ $ rcount1 y}} rcount $ Just Nothing"
                    $ LRSuccess "1"
                , testQuery
                    "with Function, Integer let rec {rcount: (rec xb, Maybe xb) -> Integer = fn {Nothing => 0; Just y => succ $ rcount y}} rcount $ Just Nothing"
                    $ LRSuccess "1"
                , testQuery
                    "with Function, Integer let rec {rcount = rcount1; rcount1: (rec xb, Maybe xb) -> Integer = fn {Nothing => 0; Just y => succ $ rcount1 y}} rcount $ Just Nothing"
                    $ LRSuccess "1"
                , testQuery
                    "with Function, Integer let rec {rcount: (rec xa, Maybe xa) -> Integer = rcount1; rcount1: (rec xb, Maybe xb) -> Integer = fn {Nothing => 0; Just y => succ $ rcount1 y}} rcount $ Just Nothing"
                    $ LRSuccess "1"
                , testQuery
                    "with Function, Integer let rec {rcount: (rec x, Maybe x) -> Integer = rcount1; rcount1: (rec x, Maybe x) -> Integer = fn {Nothing => 0; Just y => succ $ rcount y}} ()"
                    $ LRSuccess "()"
                , testQuery
                    "with Function, Integer let rec {rcount = rcount1; rcount1 = fn {Nothing => 0; Just y => succ $ rcount y}} rcount $ Just Nothing"
                    $ LRSuccess "1"
                , testQuery
                    "with Function, Integer let rec {rcount = rcount1; rcount1: (rec x, Maybe x) -> Integer = fn {Nothing => 0; Just y => succ $ rcount y}} rcount $ Just Nothing"
                    $ LRSuccess "1"
                , testQuery
                    "with Function, Integer let rec {rcount: (rec xa, Maybe xa) -> Integer = fn {Nothing => 0; Just y => succ $ rcount1 y}; rcount1: (rec xb, Maybe xb) -> Integer = fn {Nothing => 0; Just y => succ $ rcount y}} rcount $ Just $ Just $ Just $ Just $ Just Nothing"
                    $ LRSuccess "5"
                , testQuery
                    "with Function, Integer let rec {rcount: (rec xc, Maybe xc) -> Integer = fn {Nothing => 0; Just y => succ $ rcount1 y}; rcount1 = fn {Nothing => 0; Just y => succ $ rcount y}} rcount $ Just $ Just $ Just $ Just $ Just Nothing"
                    $ LRSuccess "5"
                , testTree
                    "lazy"
                    [ testQuery
                        "with Function, Integer let {f: (x -> Integer) -> Maybe x -> Integer = fn rc => fn {Nothing => 0; Just y => succ $ rc y}} let rec {rcount: (rec z, Maybe z) -> Integer = rcount1; rcount1 = f rcount} rcount $ Just Nothing"
                        $ LRSuccess "1"
                    , testQuery
                        "with Function, Integer let {f: ((rec x, Maybe x) -> Integer) -> (rec x, Maybe x) -> Integer = fn rc => fn {Nothing => 0; Just y => succ $ rc y}} let rec {rcount: (rec z, Maybe z) -> Integer = rcount1; rcount1 = f rcount} rcount $ Just Nothing"
                        $ LRSuccess "1"
                    , testQuery
                        "with Function, Integer let {f: (Integer -> Integer) -> Integer -> Integer = fn rc, x => if x ==.Entity 0 then 0 else succ $ rc (x - 1)} let rec {rcount: Integer -> Integer = rcount1; rcount1 = f rcount} rcount 1"
                        $ LRSuccess "1"
                    , testQuery "with Function let {f = fn _, x => x} let rec {rcount = f rcount} rcount 1"
                        $ LRSuccess "1"
                    , testQuery
                        "with Function let {f: Any -> Integer -> Integer = fn _, x => x} let rec {rcount = f (seq (error \"strict\") rcount)} rcount 1"
                        $ LRSuccess "1"
                    , testQuery
                        "with Function let {f: Any -> Integer -> Integer = fn _, x => x} let rec {rcount = f (seq rcount (error \"strict\"))} rcount 1"
                        $ LRSuccess "1"
                    , testQuery
                        "with Function let {f: (Integer -> Integer) -> Integer -> Integer = fn _, x => x} let rec {rcount = f rcount} rcount 1"
                        $ LRSuccess "1"
                    , testQuery
                        "with Function let {f: (Integer -> Integer) -> Integer -> Integer = fn _, x => x} let rec {rcount = rcount1; rcount1 = f rcount} rcount 1"
                        $ LRSuccess "1"
                    , testQuery
                        "with Function let {f: (Integer -> Integer) -> Integer -> Integer = fn _, x => x} let rec {rcount: Integer -> Integer = rcount1; rcount1 = f rcount} rcount 1"
                        $ LRSuccess "1"
                    , testQuery
                        "with Function, Integer let rec {rcount: (rec a, Maybe a) -> Integer = rcount1; rcount1 = fn {Nothing => 0; Just y => succ $ rcount y}} rcount $ Just Nothing"
                        $ LRSuccess "1"
                    , testQuery
                        "with Function, Integer let rec {rcount: (rec a, Maybe a) -> Integer = rcount1; rcount1: (rec a, Maybe a) -> Integer = fn {Nothing => 0; Just y => succ $ rcount y}} rcount $ Just Nothing"
                        $ LRSuccess "1"
                    , testQuery
                        "with Function, Integer let rec {rcount: (rec xa, Maybe xa) -> Integer = rcount1; rcount1: (rec xb, Maybe xb) -> Integer = fn {Nothing => 0; Just y => succ $ rcount y}} rcount $ Just Nothing"
                        $ LRSuccess "1"
                    ]
                ]
            , testTree
                "match"
                [ testQuery
                    "with Function, Integer let rec {rcount = fn {Nothing => 0; Just y => succ $ rcount y}} rcount Nothing"
                    $ LRSuccess "0"
                , testQuery
                    "with Function, Integer let rec {rcount = fn {Nothing => 0; Just y => if True then 1 else succ $ rcount y}} rcount $ Just Nothing"
                    $ LRSuccess "1"
                , testQuery
                    "with Function, Integer let rec {rcount = fn {Nothing => 0; Just Nothing => 1; Just (Just y) => if True then 2 else 2 + rcount y}} rcount $ Just Nothing"
                    $ LRSuccess "1"
                , testQuery
                    "with Function, Integer let rec {rcount = fn {Nothing => 0; Just y => y >- fn {Nothing => 1; Just z => if True then 2 else succ $ rcount y}}} rcount $ Just Nothing"
                    $ LRSuccess "1"
                , testQuery
                    "with Function, Integer let rec {rcount = fn {Nothing => 0; Just y => succ $ rcount y}} rcount $ Just Nothing"
                    $ LRSuccess "1"
                , testQuery
                    "with Function, Integer let rec {rcount = fn {Nothing => 0; Just y => if True then 1 else succ $ rcount y}} rcount $ Just $ Just Nothing"
                    $ LRSuccess "1"
                , testQuery
                    "with Function, Integer let rec {rcount = fn {Nothing => 0; Just Nothing => 1; Just (Just y) => if True then 2 else 2 + rcount y}} rcount $ Just $ Just Nothing"
                    $ LRSuccess "2"
                , testQuery
                    "with Function, Integer let rec {rcount = fn {Nothing => 0; Just y => y >- fn {Nothing => 1; Just z => if True then 2 else succ $ rcount y}}} rcount $ Just $ Just Nothing"
                    $ LRSuccess "2"
                , testQuery
                    "with Function, Integer let rec {rcount = fn {Nothing => 0; Just y => y >- fn {Nothing => 1; Just z => z >- fn {Nothing => 2; Just p => if True then 3 else succ $ rcount y}}}} rcount $ Just $ Just Nothing"
                    $ LRSuccess "2"
                , testQuery
                    "with Function, Integer let rec {rcount = fn {Nothing => 0; Just y => y >- fn {Nothing => 1; Just z => z >- fn {Nothing => 2; Just p => succ $ rcount y}}}} rcount $ Just $ Just Nothing"
                    $ LRSuccess "2"
                , testQuery
                    "with Function, Integer let rec {rcount = fn {Nothing => 0; Just y => y >- fn {Nothing => 1; Just z => z >- fn {Nothing => 2; Just p => succ $ rcount y}}}; rcount1 = fn x => rcount x} rcount1 $ Just $ Just Nothing"
                    $ LRSuccess "2"
                , testQuery
                    "with Function, Integer let rec {rcount1 = fn {Nothing => 0; Just z => succ $ rcount z;}; rcount = fn {Nothing => 0; Just y => succ $ rcount1 y}} rcount $ Just $ Just $ Just $ Just Nothing"
                    $ LRSuccess "4"
                , testQuery
                    "with Function, Integer let rec {rcount1 = fn {Nothing => 0; Just z => succ $ rcount z}; rcount = fn {Nothing => 0; Just y => succ $ rcount1 y}} rcount $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just Nothing"
                    $ LRSuccess "12"
                , testQuery
                    "with Function, Integer let rec {rcount1 = fn {Nothing => 0; Just z => succ $ rcount z}; rcount : (rec a, Maybe a) -> Integer = fn {Nothing => 0; Just y => succ $ rcount1 y}} rcount $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just Nothing"
                    $ LRSuccess "12"
                , testQuery
                    "with Function, Integer let rec {rcount : (rec a, Maybe a) -> Integer = fn {Nothing => 0; Just Nothing => 1; Just (Just y) => if True then 2 else 2 + rcount y}; rval : rec a, Maybe a = Just $ Just Nothing} rcount rval"
                    $ LRSuccess "2"
                , testQuery
                    "with Function, Integer let rec {rcount : (rec a, Maybe a) -> Integer = fn {Nothing => 0; Just Nothing => 1; Just (Just Nothing) => 2; Just (Just (Just Nothing)) => 3; Just (Just (Just (Just Nothing))) => 4; Just (Just (Just (Just (Just _)))) => 5}; rval : rec a, Maybe a = Just $ Just $ Just $ Just Nothing} rcount rval"
                    $ LRSuccess "4"
                , testQuery
                    "with Function let rec {rcount : (rec a, Maybe a) -> Integer = fn {Nothing => 0; Just Nothing => 1; Just (Just Nothing) => 2; Just (Just (Just Nothing)) => 3; Just (Just (Just (Just Nothing))) => 4; Just (Just (Just (Just (Just _)))) => 5}; rval : rec a, Maybe a = Just rval} rcount rval"
                    $ LRSuccess "5"
                , testQuery
                    "with Function, Integer let rec {rcount : (rec a, Maybe a) -> Integer = fn {Nothing => 0; Just y => succ $ rcount y}; rval : rec a, Maybe a = Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just Nothing} rcount rval"
                    $ LRSuccess "10"
                , testQuery
                    "with Function, Integer let rec {rcount : (rec a, Maybe a) -> Integer = fn {Nothing => 0; Just y => succ $ rcount y}; rval = Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just Nothing} rcount rval"
                    $ LRSuccess "10"
                , testQuery
                    "with Function, Integer let {rc: (a -> Integer) -> Maybe a -> Integer = fn r => fn {Nothing => 0; Just y => succ $ r y}} fix rc $ Just Nothing"
                    $ LRSuccess "1"
                , testQuery
                    "with Function, Integer let {rc: (a -> Integer) -> Maybe a -> Integer = fn r => fn {Nothing => 0; Just y => succ $ r y}} fix rc $ Just $ Just $ Just $ Just $ Just Nothing"
                    $ LRSuccess "5"
                , testQuery
                    "with Function, Integer let rec {rcount = fn {Nothing => 0; Just y => succ $ rcount y}} rcount $ Just $ Just Nothing"
                    $ LRSuccess "2"
                , testQuery
                    "with Function, Integer let rec {rcount = fn {Nothing => 0; Just y => succ $ rcount y}} rcount $ Just $ Just $ Just Nothing"
                    $ LRSuccess "3"
                , testQuery
                    "with Function, Integer let rec {rcount = fn {Nothing => 0; Just y => succ $ rcount y}} rcount $ Just $ Just $ Just $ Just Nothing"
                    $ LRSuccess "4"
                , testQuery
                    "with Function, Integer let rec {rcount = fn {Nothing => 0; Just y => succ $ rcount y}} rcount $ Just $ Just $ Just $ Just $ Just Nothing"
                    $ LRSuccess "5"
                , testQuery
                    "with Function, Integer let rec {rcount = fn {Nothing => 0; Just y => succ $ rcount y}; rval = Just $ Just Nothing} rcount rval"
                    $ LRSuccess "2"
                , testQuery
                    "with Function, Integer let rec {rcount = fn {Nothing => 0; Just y => succ $ rcount y}; rval : Maybe (Maybe (Maybe None)) = Just $ Just Nothing} rcount rval"
                    $ LRSuccess "2"
                , testQuery
                    "with Function, Integer let rec {rcount = fn {Nothing => 0; Just y => succ $ rcount y}; rval : Maybe (Maybe (Maybe (Maybe None))) = Just $ Just Nothing} rcount rval"
                    $ LRSuccess "2"
                , testQuery "with Function let {} Just $ Just $ Just Nothing" $ LRSuccess "Just Just Just Nothing"
                , testQuery
                    "with Function, Integer let rec {rcount = fn {Nothing => 0; Just y => succ $ rcount y}; rval = Just $ Just $ Just Nothing} rcount rval"
                    $ LRSuccess "3"
                , testQuery
                    "with Function, Integer let rec {rcount = fn {Nothing => 0; Just y => succ $ rcount y}; rval : Maybe (Maybe (Maybe (Maybe None))) = Just $ Just $ Just Nothing} rcount rval"
                    $ LRSuccess "3"
                , testQuery
                    "with Function, Integer let rec {rcount = fn {Nothing => 0; Just y => succ $ rcount y}; rval : Maybe (Maybe (Maybe (Maybe (Maybe None)))) = Just $ Just $ Just Nothing} rcount rval"
                    $ LRSuccess "3"
                , testQuery
                    "with Function let rec {rcount = fn {Nothing => 0; Just Nothing => 1; Just (Just Nothing) => 2; Just (Just (Just Nothing)) => 3; _ => 4}} rcount $ Just $ Just $ Just Nothing"
                    $ LRSuccess "3"
                , testQuery
                    "with Function, Integer let rec {rcount = fn {Nothing => 0; Just y => succ $ r1count y}; r1count = fn {Nothing => 0; Just y => succ $ r1count y}} rcount $ Just $ Just $ Just Nothing"
                    $ LRSuccess "3"
                , testQuery
                    "with Function let {} (Just $ Just $ Just Nothing) >- fn {Nothing => 0; Just Nothing => 1; Just (Just Nothing) => 2; Just (Just (Just Nothing)) => 3; _ => 4}"
                    $ LRSuccess "3"
                , testQuery
                    "with Function let rec {rcount = fn {Nothing => 0; Just Nothing => 1; Just (Just Nothing) => 2; Just (Just (Just Nothing)) => 3; _ => 4}} rcount $ Just $ Just $ Just Nothing"
                    $ LRSuccess "3"
                , testQuery
                    "with Function let rec {rcount : (rec a , Maybe a) -> Integer = fn {Nothing => 0; Just Nothing => 1; Just (Just Nothing) => 2; Just (Just (Just Nothing)) => 3; _ => 4}} rcount $ Just $ Just $ Just Nothing"
                    $ LRSuccess "3"
                , testQuery
                    "with Function, Integer let rec {rcount : (rec a , Maybe a) -> Integer = fn {Nothing => 0; Just Nothing => 1; Just (Just Nothing) => 2; Just (Just (Just Nothing)) => 3; Just (Just (Just (Just y))) => 4 + rcount y}} rcount $ Just $ Just $ Just Nothing"
                    $ LRSuccess "3"
                , testQuery
                    "with Function, Integer let rec {rcount : (rec a , Maybe a) -> Integer = fn {Nothing => 0; Just Nothing => 1; Just (Just y) => 2 + rcount y}} rcount $ Just $ Just $ Just Nothing"
                    $ LRSuccess "3"
                ]
            , testTree
                "contra-var"
                [ testQuery "let {f: rec r, (r -> Integer) = fn _ => 3} 0" LRCheckFail
                , testQuery "let {f: rec r, (r -> r) = fn x => x} 0" LRCheckFail
                ]
            ]
        , let
            testSupertype :: Text -> Text -> Text -> Text -> Bool -> TestTree
            testSupertype supertype subtype val altval good = let
                result =
                    LRSuccess
                        $ unpack
                        $ if good
                            then val
                            else altval
                in testTree
                    (unpack $ supertype <> " -> " <> subtype)
                    [ testQuery
                        ( "with Rational let {x: "
                            <> supertype
                            <> " = "
                            <> val
                            <> "; y: "
                            <> subtype
                            <> " = x >-.Function fn {(z:? "
                            <> subtype
                            <> ") => z; _ => "
                            <> altval
                            <> ";}} y"
                        )
                        result
                    , testQuery
                        ( "with Rational let {x: "
                            <> supertype
                            <> " = "
                            <> val
                            <> "; y: "
                            <> subtype
                            <> " = !{check.Function @("
                            <> subtype
                            <> ")} x >-.Function fn {Just z => z; Nothing => "
                            <> altval
                            <> ";}} y"
                        )
                        result
                    , testQuery
                        ( "with Rational let {x: "
                            <> supertype
                            <> " = "
                            <> val
                            <> "; y: "
                            <> subtype
                            <> " = !{coerce.Function @("
                            <> subtype
                            <> ")} x} y"
                        )
                        $ if good
                            then result
                            else LRRunError
                    ]
            in testTree
                "supertype"
                [ testSupertype "Integer" "Integer" "3" "0" True
                , testSupertype "Rational" "Integer" "3" "0" True
                , testSupertype "Rational" "Integer" "7/2" "0" False
                , testSupertype "Integer" "Rational" "3" "0" True
                , testSupertype "Number" "Integer" "3" "0" True
                , testSupertype "Number" "Integer" "7/2" "0" False
                , testSupertype "Integer" "Number" "3" "0" True
                , testSupertype "Number" "Rational" "3" "0" True
                , testSupertype "Number" "Rational" "7/2" "0" True
                , testSupertype "Rational" "Number" "7/2" "0" True
                ]
        , let
            testLiteral :: Int -> Bool -> Text -> TestTree
            testLiteral len embedded val =
                testTree
                    (unpack val)
                    [ testQuery ("literalLength.Debug " <> val) $ LRSuccess $ show len
                    , testQuery ("literalIsEmbedded.Debug " <> val) $ LRSuccess $ show embedded
                    ]
            in testTree
                "literal"
                [ testLiteral 1 True "\"\""
                , testLiteral 2 True "\"A\""
                , testLiteral 21 True "\"12345678901234567890\""
                , testLiteral 31 True "\"123456789012345678901234567890\""
                , testLiteral 32 False "\"1234567890123456789012345678901\""
                , testLiteral 1 True "()"
                , testLiteral 2 True "True"
                , testLiteral 2 True "False"
                , testLiteral 3 True "34"
                , testLiteral 4 True "34.5"
                , testLiteral 9 True "~34"
                ]
        , testTree
            "record"
            [ testQuery "let {rf {} = 4} rf {}" $ LRSuccess "4"
            , testQuery "let {rf {x: Integer} = x} rf {x = 7}" $ LRSuccess "7"
            , testQuery "let {rf {x: Integer} = x} rf {}" $ LRCheckFail
            , testQuery "let {rf {x: Integer} : Integer = x} rf {x = 7}" $ LRSuccess "7"
            , testQuery "let {rf {x: Integer} : Unit = x} rf {x = 7}" $ LRCheckFail
            , testQuery "let {rf {x: Integer} : Number = x} rf {x = 7} : Number" $ LRSuccess "7"
            , testQuery "let {rf {x: Integer} : Number = x} rf {x = 7} : Integer" $ LRCheckFail
            , testQuery "let {rf {x: Integer} : Integer = x} rf {x = 7} : Integer" $ LRSuccess "7"
            , testQuery "let {rf {x: Integer} = x} let {x = 7} rf" $ LRSuccess "7"
            , testQuery "let {rf {x: Integer; y: Integer} = x + y} rf {x = 8; y = 12}" $ LRSuccess "20"
            , testQuery "let {rf {x: Integer; y: Integer = 2} = x + y} rf {x = 8; y = 12}" $ LRSuccess "20"
            , testQuery "let {rf {x: Integer; y: Integer = 2} = x + y} rf {x = 8}" $ LRSuccess "10"
            , testQuery "let {rf {x: Integer; y: Integer = 2} = x + y} let {x = 6} rf" $ LRSuccess "8"
            , textTypeTest "let {rf {m: a -> Maybe a} = m 3} rf {m = Just}" "{} -> Maybe. Natural."
            ]
        , testTree
            "polymorphism"
            [ textTypeTest "let {x: a -> Maybe a = Just} (x 3,x \"text\")" "{} -> Maybe. Natural. *: Maybe. Text."
            , textTypeTest
                "let {x: a -> Maybe a = Just} imply {?x = x} (?x 3,?x \"text\")"
                "{} -> Maybe. Natural. *: Maybe. Text."
            , textTypeTest "imply {?x = Just} (?x 3,?x \"text\")" "{} -> Maybe. Natural. *: Maybe. Text."
            , textTypeTest
                "imply {?x: a -> Maybe a = Just} (?x 3,?x \"text\")"
                "{} -> Maybe. Natural. *: Maybe. Text."
            , textTypeTest
                "let {rf {m: a -> Maybe a} = (m 3,m \"text\")} rf {m = Just}"
                "{} -> Maybe. Natural. *: Maybe. Text."
            , textTypeTest
                "let {datatype T {Mk {m: a -> Maybe a}}; rf = fn Mk.T => (m 3,m \"text\")} rf Mk.T {m = Just}"
                "{} -> Maybe. Natural. *: Maybe. Text."
            , textTypeTest
                "ap{let {rm = ap{Just}} (%rm 3,%rm \"text\")}"
                "{} -> WholeModel. +(Maybe. Natural. *: Maybe. Text.)"
            , textTypeTest
                "ap{let {m = %ap{Just}} (m 3,m \"text\")}" -- since this is sugar for ap, it must be monomorphic
                "{} -> WholeModel. +(Maybe. (Natural. | Text.) *: Maybe. (Natural. | Text.))"
            , testQuery "let {rf {m: a -> Maybe a} = (m 3,m \"text\")} rf {m = Just}"
                $ LRSuccess "(Just 3,Just \"text\")"
            , testQuery "let {rf {m: a -> Maybe a = Just} = (m 3,m \"text\")} rf {}"
                $ LRSuccess "(Just 3,Just \"text\")"
            , testQuery "let {rf {m: a -> Maybe a = Just} = (m 3,m \"text\")} rf" $ LRSuccess "(Just 3,Just \"text\")"
            ]
        , testTree
            "splice"
            [ textTypeTest "!expression {f x}" "{} -> Interpreter.Pinafore. Expression.Pinafore."
            , testQuery "let {expr = !expression {3}} !{expr}" $ LRSuccess "3"
            , testQuery "let {expr = !expression {x}} let {x = 5} !{expr}" $ LRSuccess "5"
            , testQuery "let {expr = !expression {x}; sc = !decl{x = 17}} let {!{sc}} !{expr}" $ LRSuccess "17"
            ]
        ]

testShim :: Text -> String -> String -> TestTree
testShim query expectedType expectedShim =
    testTree (unpack query)
        $ runTester defaultTester
        $ do
            result <- tryExc $ testerLiftInterpreter $ parseToValue query []
            liftIO
                $ case result of
                    FailureResult e -> assertFailure $ "expected success, found failure: " ++ show e
                    SuccessResult (MkSomeOf (MkPosShimWit t shim) _) -> do
                        assertEqual "type" expectedType $ exprShowShow t
                        assertEqual "shim" expectedShim $ show shim

testShims :: TestTree
testShims =
    testTree
        "shim"
        [ testShim "3" "Natural." "(join1 id)"
        , testShim "negate.Integer" "Integer. -> Integer." "(join1 (co (contra id (meet1 id)) (join1 id)))"
        , testShim "negate.Integer 3" "Integer." "(join1 id)"
        , expectFailBecause "ISSUE #63" $ testShim "id" "a -> a" "(join1 (co (contra id (meet1 id)) (join1 id)))"
        , expectFailBecause "ISSUE #63" $ testShim "id 3" "Integer." "(join1 id)"
        , expectFailBecause "ISSUE #63" $ testShim "fn x => x" "a -> a" "(join1 (co (contra id (meet1 id)) (join1 id)))"
        , expectFailBecause "ISSUE #63" $ testShim "(fn x => x) 3" "Integer." "(join1 id)"
        , expectFailBecause "ISSUE #63"
            $ testShim "fn x => 4" "Any -> Natural." "(join1 (co (contra id termf) (join1 id)))"
        , testShim "(fn x => 4) 3" "Natural." "(join1 id)"
        , expectFailBecause "ISSUE #63"
            $ testShim
                "let {rcount = fn {Nothing => 0; Just y => succ.Natural $ rcount y}} rcount"
                "(rec c, Maybe c) -> Natural."
                "(join1 id)"
        , expectFailBecause "ISSUE #63"
            $ testShim
                "let {rcount : (rec a, Maybe a) -> Integer = fn {Nothing => 0; Just y => succ $ rcount y}} rcount"
                "(rec a, Maybe a) -> Integer."
                "(join1 id)"
        ]

testLanguage :: TestTree
testLanguage = localOption (mkTimeout 2000000) $ testTree "language" [testInfix, testNumbers, testShims, testQueries]
