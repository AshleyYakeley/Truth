{-# OPTIONS -fno-warn-orphans #-}

module Test.Language
    ( testLanguage
    ) where

import Data.Shim
import Pinafore
import Pinafore.Documentation
import Pinafore.Test
import Prelude (read)
import Shapes
import Shapes.Numeric
import Shapes.Test

testOp :: Name -> TestTree
testOp n =
    testTree (show $ unpack n) $ do
        case unpack n of
            '(':_ -> assertFailure "parenthesis"
            _ -> return ()
        case operatorFixity n of
            MkFixity AssocLeft 10 -> assertFailure "unassigned fixity"
            _ -> return ()

testInfix :: TestTree
testInfix =
    testTree "infix" $
    fmap testOp $
    allOperatorNames $ \case
        ValueDocItem {} -> True
        _ -> False

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
       forall t. (Show t, Eq (PreciseEq t), Read t)
    => String
    -> t
    -> TestTree
testShowRead str t =
    testTree
        (show str)
        [ testTree "show" $ assertEqual "" str $ show t
        , testTree "read" $ assertEqual "" (MkPreciseEq t) $ MkPreciseEq $ read str
        , testTree "read-show" $ assertEqual "" str $ show $ read @t str
        ]

testRead ::
       forall t. (Show t, Eq (PreciseEq t), Read t)
    => String
    -> Maybe t
    -> TestTree
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

testQuery :: Text -> LangResult -> TestTree
testQuery query expected =
    testTree (show $ unpack query) $ do
        result <-
            withNullPinaforeContext $
            runInterpretResult $
            runPinaforeScoped (initialPos "<input>") $ do
                v <- parseValue query
                showPinaforeRef v
        case result of
            FailureResult e ->
                case expected of
                    LRCheckFail -> return ()
                    _ -> assertFailure $ "check: expected success, found failure: " ++ show e
            SuccessResult r -> do
                me <- catchPureError r
                case (expected, me) of
                    (LRCheckFail, _) -> assertFailure $ "check: expected failure, found success"
                    (LRRunError, Nothing) -> assertFailure $ "run: expected error, found success: " ++ r
                    (LRRunError, Just _) -> return ()
                    (LRSuccess _, Just e) -> assertFailure $ "run: expected success, found error: " ++ show e
                    (LRSuccess s, Nothing) -> assertEqual "result" s r

testSubsumeSubtype :: Bool -> Text -> Text -> [Text] -> [TestTree]
testSubsumeSubtype good t1 t2 vs =
    [ testQuery ("let rec r = r end; x : " <> t1 <> "; x = r; y : " <> t2 <> "; y = x in ()") $
      goodLangResult good $ LRSuccess "()"
    ] <>
    fmap
        (\v ->
             testQuery ("let x : " <> t1 <> "; x = " <> v <> " in x : " <> t2) $
             goodLangResult good $ LRSuccess $ unpack v)
        vs <>
    fmap
        (\v ->
             testQuery ("let x : " <> t1 <> "; x = " <> v <> "; y : " <> t2 <> "; y = x in y") $
             goodLangResult good $ LRSuccess $ unpack v)
        vs

testFunctionSubtype :: Bool -> Text -> Text -> [Text] -> [TestTree]
testFunctionSubtype good t1 t2 vs =
    [ testQuery ("let rec r=r end; f : (" <> t2 <> ") -> Unit; f _ = (); x : (" <> t1 <> "); x = r in f x") $
      goodLangResult good $ LRSuccess "()"
    , testQuery ("let f : (" <> t1 <> ") -> (" <> t2 <> "); f x = x in f") $ goodLangResult good $ LRSuccess "<?>"
    , testQuery ("(\\x => x) : (" <> t1 <> ") -> (" <> t2 <> ")") $ goodLangResult good $ LRSuccess "<?>"
    ] <>
    fmap
        (\v ->
             testQuery ("let f : (" <> t1 <> ") -> (" <> t2 <> "); f x = x in f " <> v) $
             goodLangResult good $ LRSuccess $ unpack v)
        vs

testSubtype1 :: Bool -> Bool -> Text -> Text -> [Text] -> [TestTree]
testSubtype1 good b t1 t2 vs =
    testSubsumeSubtype good t1 t2 vs <>
    if b
        then testFunctionSubtype good t1 t2 vs
        else []

testSubtype :: Bool -> Text -> Text -> [Text] -> TestTree
testSubtype b t1 t2 vs =
    testTree (unpack $ t1 <> " <: " <> t2) $ testSubtype1 True b t1 t2 vs <> testSubtype1 False b t2 t1 vs

testSameType :: Bool -> Text -> Text -> [Text] -> TestTree
testSameType b t1 t2 vs =
    testTree (unpack $ t1 <> " = " <> t2) $ testSubtype1 True b t1 t2 vs <> testSubtype1 True b t2 t1 vs

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
              , testQuery "Std.textLength" $ LRSuccess "<?>"
              , testQuery "let opentype T in openEntity @T !\"example\"" $ LRSuccess "<?>"
              , testQuery "let opentype T in entityAnchor $ openEntity @T !\"example\"" $
                LRSuccess "\"!61604E6E-5CD45F24-A9CEB59A-3FE58A09-5242FF8A-D0C603D0-8734E583-DC034C5F\""
              ]
        , testTree
              "list construction"
              [ testQuery "[]" $ LRSuccess $ show @[Text] []
              , testQuery "[1]" $ LRSuccess $ "[1]"
              , testQuery "[1,2,3]" $ LRSuccess "[1, 2, 3]"
              ]
        , testTree
              "functions"
              [ testQuery "\\x => x" $ LRSuccess "<?>"
              , testQuery "\\x => 1" $ LRSuccess "<?>"
              , testQuery "\\x y => y" $ LRSuccess "<?>"
              , testQuery "\\x y z => [x,y,z]" $ LRSuccess "<?>"
              ]
        , testTree
              "predefined"
              [ testQuery "abs" $ LRSuccess "<?>"
              , testQuery "fst" $ LRSuccess "<?>"
              , testQuery "(+)" $ LRSuccess "<?>"
              , testQuery "\\a b => a + b" $ LRSuccess "<?>"
              , testQuery "(==)" $ LRSuccess "<?>"
              , testQuery "\\a b => a == b" $ LRSuccess "<?>"
              ]
        , testTree
              "let-binding"
              [ testQuery "let in 27" $ LRSuccess "27"
              , testQuery "let a=\"5\" in a" $ LRSuccess "\"5\""
              , testQuery "let a=5 in a" $ LRSuccess "5"
              , testQuery "let a=1 in let a=2 in a" $ LRSuccess "2"
              , testQuery "let a=1;b=2 in a" $ LRSuccess "1"
              , testQuery "let a=1;b=2 in b" $ LRSuccess "2"
              , testQuery "let a=1;b=2 in b" $ LRSuccess "2"
              , testQuery "let a=1;b=\"2\" in b" $ LRSuccess "\"2\""
              , testQuery "let a=1 ;b=\"2\" in b" $ LRSuccess "\"2\""
              , testQuery "let a= 1 ;b=\"2\" in b" $ LRSuccess "\"2\""
              , testQuery "let a=7;b=a in a" $ LRSuccess "7"
              , testQuery "let a=7;b=a in b" $ LRSuccess "7"
              , testQuery "let a=2 in let b=a in b" $ LRSuccess "2"
              , testTree
                    "recursive"
                    [ testQuery "let rec a=1 end in a" $ LRSuccess "1"
                    , testQuery "let rec a=1 end in let rec a=2 end in a" $ LRSuccess "2"
                    , testQuery "let rec a=1;a=2 end in a" $ LRCheckFail
                    , testQuery "let rec a=1;b=a end in b" $ LRSuccess "1"
                    , testQuery "let rec b=a;a=1 end in b" $ LRSuccess "1"
                    , testQuery "let rec a x = x end in a 1" $ LRSuccess "1"
                    , testQuery "let rec a x = x; b = a end in b" $ LRSuccess "<?>"
                    , testQuery "let rec a = \\x => x end in let rec b = a 1 end in b" $ LRSuccess "1"
                    , testQuery "let rec a x = x; b = a 1 end in b" $ LRSuccess "1"
                    , testQuery "let rec a x = b; b = b end in a" $ LRSuccess "<?>"
                    , testQuery "let rec a x = 1; b = b end in a b" $ LRSuccess "1"
                    , testQuery "let rec a x = 1; b = a b end in b" $ LRSuccess "1"
                    , testQuery "let rec a x = 1 end in let rec b = a b end in b" $ LRSuccess "1"
                    , testQuery "let rec b = (\\x => 1) b end in b" $ LRSuccess "1"
                    , testQuery "let rec b = a b; a x = 1 end in b" $ LRSuccess "1"
                    , testQuery "let rec a x = 1; b = a c; c=b end in b" $ LRSuccess "1"
                    , testTree
                          "polymorphism"
                          [ testQuery "let rec i = \\x => x end in (1 + i 1, i False)" $ LRSuccess "(2, False)"
                          , testQuery "let rec i = \\x => x; r = (1 + i 1, i False) end in r" $ LRSuccess "(2, False)"
                          , testQuery "let rec r = (1 + i 1, i False); i = \\x => x end in r" $ LRSuccess "(2, False)"
                          ]
                    ]
              ]
        , testTree
              "scoping"
              [ testQuery "(\\b => \\a => b) a" LRCheckFail
              , testQuery "let b=a in \\a => b" LRCheckFail
              , testQuery "let b=a in ()" LRCheckFail
              , testQuery "let rec b=a end in ()" LRCheckFail
              , testQuery "let a=1 in let b=a in (\\a => b) 2" $ LRSuccess "1"
              , testQuery "(\\a => let b=a in (\\a => b) 2) 1" $ LRSuccess "1"
              ]
        , testTree
              "name shadowing"
              [ testQuery "let a=1 in (\\a => a) 2" $ LRSuccess "2"
              , testQuery "let a=1 in (\\(Just a) => a) (Just 2)" $ LRSuccess "2"
              , testQuery "let a=1 in let a=2 in a" $ LRSuccess "2"
              , testQuery "(\\a => let a=2 in a) 1" $ LRSuccess "2"
              , testQuery "(\\a => \\a => a) 1 2" $ LRSuccess "2"
              , testQuery "let a=1 in case 2 of a => a end" $ LRSuccess "2"
              , testQuery "let a=1 in case Just 2 of Just a => a end" $ LRSuccess "2"
              , testQuery "case 1 of a => case 2 of a => a end end" $ LRSuccess "2"
              ]
        , testTree
              "partial keywords"
              [ testQuery "let i=1 in i" $ LRSuccess "1"
              , testQuery "let inx=1 in inx" $ LRSuccess "1"
              , testQuery "let l=1 in l" $ LRSuccess "1"
              , testQuery "let le=1 in le" $ LRSuccess "1"
              , testQuery "let letx=1 in letx" $ LRSuccess "1"
              , testQuery "let letre=1 in letre" $ LRSuccess "1"
              , testQuery "let letrecx=1 in letrecx" $ LRSuccess "1"
              , testQuery "let tru=1 in tru" $ LRSuccess "1"
              , testQuery "let truex=1 in truex" $ LRSuccess "1"
              , testQuery "let f=1 in f" $ LRSuccess "1"
              , testQuery "let fals=1 in fals" $ LRSuccess "1"
              , testQuery "let falsex=1 in falsex" $ LRSuccess "1"
              ]
        , testTree
              "duplicate bindings"
              [ testQuery "let rec a=1;a=1 end in a" $ LRCheckFail
              , testQuery "let red a=1;a=2 end in a" $ LRCheckFail
              , testQuery "let rec a=1;b=0;a=2 end in a" $ LRCheckFail
              ]
        , testTree
              "lexical scoping"
              [ testQuery "let a=1 in let b=a in let a=3 in a" $ LRSuccess "3"
              , testQuery "let rec a=1;b=a;a=3 end in a" $ LRCheckFail
              , testQuery "let a=1 in let b=a in let a=3 in b" $ LRSuccess "1"
              , testQuery "let rec a=1;b=a;a=3 end in b" $ LRCheckFail
              ]
        , testTree
              "operator"
              [ testQuery "0 == 1" $ LRSuccess "False"
              , testQuery "1 == 1" $ LRSuccess "True"
              , testQuery "0 /= 1" $ LRSuccess "True"
              , testQuery "1 /= 1" $ LRSuccess "False"
              , testQuery "0 <= 1" $ LRSuccess "True"
              , testQuery "1 <= 1" $ LRSuccess "True"
              , testQuery "2 <= 1" $ LRSuccess "False"
              , testQuery "0 < 1" $ LRSuccess "True"
              , testQuery "1 < 1" $ LRSuccess "False"
              , testQuery "2 < 1" $ LRSuccess "False"
              , testQuery "0 >= 1" $ LRSuccess "False"
              , testQuery "1 >= 1" $ LRSuccess "True"
              , testQuery "2 >= 1" $ LRSuccess "True"
              , testQuery "0 >= ~1" $ LRSuccess "False"
              , testQuery "1 >= ~1" $ LRSuccess "True"
              , testQuery "2 >= ~1" $ LRSuccess "True"
              , testQuery "0 > 1" $ LRSuccess "False"
              , testQuery "1 > 1" $ LRSuccess "False"
              , testQuery "2 > 1" $ LRSuccess "True"
              , testQuery "1 == ~1" $ LRSuccess "False"
              , testQuery "0 ~== 1" $ LRSuccess "False"
              , testQuery "1 ~== 1" $ LRSuccess "True"
              , testQuery "1 ~== ~1" $ LRSuccess "True"
              , testQuery "0 ~== ~1" $ LRSuccess "False"
              , testQuery "0 ~/= 1" $ LRSuccess "True"
              , testQuery "1 ~/= 1" $ LRSuccess "False"
              , testQuery "1 ~/= ~1" $ LRSuccess "False"
              , testQuery "0 ~/= ~1" $ LRSuccess "True"
              , testQuery "7+8" $ LRSuccess "15"
              , testQuery "7 +8" $ LRSuccess "15"
              , testQuery "7+ 8" $ LRSuccess "15"
              , testQuery "7 + 8" $ LRSuccess "15"
              , testQuery "\"abc\"++\"def\" : Text" $ LRSuccess "\"abcdef\""
              , testQuery "\"abc\" ++\"def\" : Text" $ LRSuccess "\"abcdef\""
              , testQuery "\"abc\"++ \"def\" : Text" $ LRSuccess "\"abcdef\""
              , testQuery "\"abc\" ++ \"def\" : Text" $ LRSuccess "\"abcdef\""
              , testQuery "let f x = x + 2 in f -1" $ LRSuccess "1"
              , testQuery "let f = 2 in f - 1" $ LRSuccess "1"
              , testTree
                    "precedence"
                    [ testQuery "1 + 2 * 3" $ LRSuccess "7"
                    , testQuery "3 * 2 + 1" $ LRSuccess "7"
                    , testQuery "2 * 2 * 2" $ LRSuccess "8"
                    , testQuery "12 / 2 / 2" $ LRSuccess "3"
                    , testQuery "12 / 2 / 2" $ LRSuccess "3"
                    , testQuery "0 == 0 == 0" $ LRCheckFail
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
              , testQuery "textLength \"abd\"" $ LRSuccess "3"
              , testQuery "textSection 4 3 \"ABCDEFGHIJKLMN\"" $ LRSuccess "\"EFG\""
              ]
        , testTree
              "if-then-else"
              [ testQuery "if True then 3 else 4" $ LRSuccess "3"
              , testQuery "if False then 3 else 4" $ LRSuccess "4"
              , testQuery "if False then if True then 1 else 2 else if True then 3 else 4" $ LRSuccess "3"
              ]
        , testTree "product" [testQuery "fst (7,9)" $ LRSuccess "7", testQuery "snd (7,9)" $ LRSuccess "9"]
        , testTree
              "sum"
              [ testQuery "fromEither (\\a => (\"Left\",a)) (\\a => (\"Right\",a)) $ Left \"x\"" $
                LRSuccess "(\"Left\", \"x\")"
              , testQuery "fromEither (\\a => (\"Left\",a)) (\\a => (\"Right\",a)) $ Right \"x\"" $
                LRSuccess "(\"Right\", \"x\")"
              ]
        , testTree
              "type-signature"
              [ testQuery "let i x = x in i 3" $ LRSuccess "3"
              , testQuery "let i : tvar -> tvar; i x = x in i 3" $ LRSuccess "3"
              , testQuery "let i : a -> a; i x = x in i 3" $ LRSuccess "3"
              , testQuery "let i : Number -> Number; i x = x in i 3" $ LRSuccess "3"
              , testQuery "let i : Text -> Text; i x = x in i 3" $ LRCheckFail
              , testQuery "let i : a -> a; i x = x in i \"t\"" $ LRSuccess "\"t\""
              , testQuery "let i : Number -> Number; i x = x in i \"t\"" $ LRCheckFail
              , testQuery "let i : Text -> Text; i x = x in i \"t\"" $ LRSuccess "\"t\""
              , testQuery "let i : a -> a; i x = x in 0" $ LRSuccess "0"
              , testQuery "let i : a -> Number; i x = x in 0" $ LRCheckFail
              , testQuery "let i : Number -> a; i x = x in 0" $ LRCheckFail
              , testQuery "let i : Number -> Number; i x = x in 0" $ LRSuccess "0"
              , testQuery "let i : Number :+: Boolean; i = Left 5 in i" $ LRSuccess "Left 5"
              , testQuery "let i : Number :+: Boolean; i = Right False in i" $ LRSuccess "Right False"
              , testQuery "let i : Maybe Number; i = Just 5 in i" $ LRSuccess "Just 5"
              , testQuery "let i : Maybe Number; i = Nothing in i" $ LRSuccess "Nothing"
              , testTree
                    "polar"
                    [ testQuery "let x : Text | Number; x = 3 in x" $ LRSuccess "3"
                    , testQuery "let f : Any -> Integer; f _ = 3 in f ()" $ LRSuccess "3"
                    , testQuery "(\\x => (x,x)) : ((a & Number) -> Showable :*: a)" $ LRSuccess "<?>"
                    , testQuery "let f = (\\x => (x,x)) : (a & Number) -> Showable :*: a in f 3" $ LRSuccess "(3, 3)"
                    , testQuery "let f : (a & Number) -> Showable :*: a; f x = (x,x) in f 3" $ LRSuccess "(3, 3)"
                    ]
              ]
        , testTree
              "patterns"
              [ testQuery "(\\a => 5) 2" $ LRSuccess "5"
              , testQuery "(\\a => a) 2" $ LRSuccess "2"
              , testQuery "(\\_ => 5) 2" $ LRSuccess "5"
              , testQuery "(\\a@b => (a,b)) 2" $ LRSuccess "(2, 2)"
              , testQuery "(\\(a,b) => a + b) (5,6)" $ LRSuccess "11"
              ]
        , testTree
              "case"
              [ testTree
                    "basic"
                    [ testQuery "case 2 of a => 5 end" $ LRSuccess "5"
                    , testQuery "case 2 of a => 5; a => 3 end" $ LRSuccess "5"
                    , testQuery "case 2 of a => 5; a => 3; end" $ LRSuccess "5"
                    , testQuery "case 2 of a => a end" $ LRSuccess "2"
                    , testQuery "case 2 of _ => 5 end" $ LRSuccess "5"
                    , testQuery "case 2 of _ => 5; _ => 3 end" $ LRSuccess "5"
                    , testQuery "case 2 of a@b => (a,b) end" $ LRSuccess "(2, 2)"
                    ]
              , testTree
                    "Boolean"
                    [ testQuery "case True of True => 5; False => 7 end" $ LRSuccess "5"
                    , testQuery "case False of True => 5; False => 7 end" $ LRSuccess "7"
                    , testQuery "case True of False => 7; True => 5 end" $ LRSuccess "5"
                    , testQuery "case False of False => 7; True => 5 end" $ LRSuccess "7"
                    ]
              , testTree
                    "Number"
                    [ testQuery "case 37 of 37 => True; _ => False end" $ LRSuccess "True"
                    , testQuery "case 38 of 37 => True; _ => False end" $ LRSuccess "False"
                    , testQuery "case -24.3 of 37 => 1; -24.3 => 2; _ => 3 end" $ LRSuccess "2"
                    ]
              , testTree
                    "String"
                    [ testQuery "case \"Hello\" of \"Hello\" => True; _ => False end" $ LRSuccess "True"
                    , testQuery "case \"thing\" of \"Hello\" => True; _ => False end" $ LRSuccess "False"
                    , testQuery "case \"thing\" of \"Hello\" => 1; \"thing\" => 2; _ => 3 end" $ LRSuccess "2"
                    ]
              , testTree
                    "Either"
                    [ testQuery "case Left 3 of Left a => a; Right _ => 1 end" $ LRSuccess "3"
                    , testQuery "case Right 4 of Left a => a + 1; Right a => a end" $ LRSuccess "4"
                    , testQuery "case Right 7 of Right 4 => True; _ => False end" $ LRSuccess "False"
                    , testQuery "case Right 7 of Right 4 => 1; Right 7 => 2; Left _ => 3; _ => 4 end" $ LRSuccess "2"
                    ]
              , testTree "Unit" [testQuery "case () of () => 4 end" $ LRSuccess "4"]
              , testTree "Pair" [testQuery "case (2,True) of (2,a) => a end" $ LRSuccess "True"]
              , testTree
                    "Maybe"
                    [ testQuery "case Just 3 of Just a => a + 1; Nothing => 7 end" $ LRSuccess "4"
                    , testQuery "case Nothing of Just a => a + 1; Nothing => 7 end" $ LRSuccess "7"
                    ]
              , testTree
                    "List"
                    [ testQuery "case [] of [] => True; _ => False end" $ LRSuccess "True"
                    , testQuery "case [] of _::_ => True; _ => False end" $ LRSuccess "False"
                    , testQuery "case [1,2] of [] => True; _ => False end" $ LRSuccess "False"
                    , testQuery "case [3,4] of _::_ => True; _ => False end" $ LRSuccess "True"
                    , testQuery "case [3] of a::b => (a,b) end" $ LRSuccess "(3, [])"
                    , testQuery "case [3,4] of a::b => (a,b) end" $ LRSuccess "(3, [4])"
                    , testQuery "case [3,4,5] of a::b => (a,b) end" $ LRSuccess "(3, [4, 5])"
                    , testQuery "case [3] of [a,b] => 1; _ => 2 end" $ LRSuccess "2"
                    , testQuery "case [3,4] of [a,b] => 1; _ => 2 end" $ LRSuccess "1"
                    , testQuery "case [3,4,5] of [a,b] => 1; _ => 2 end" $ LRSuccess "2"
                    , testQuery "case [3,4] of [a,b] => (a,b) end" $ LRSuccess "(3, 4)"
                    ]
              ]
        , testTree
              "lambda-case"
              [ testQuery "(\\case a => 5 end) 2" $ LRSuccess "5"
              , testQuery "(\\case a => 5; a => 3 end) 2" $ LRSuccess "5"
              , testQuery "(\\case a => 5; a => 3; end) 2" $ LRSuccess "5"
              , testQuery "(\\case a => a end) 2" $ LRSuccess "2"
              , testQuery "(\\case _ => 5 end) 2" $ LRSuccess "5"
              , testQuery "(\\case _ => 5; _ => 3 end) 2" $ LRSuccess "5"
              , testQuery "(\\case a@b => (a,b) end) 2" $ LRSuccess "(2, 2)"
              ]
        , testTree
              "type-operator"
              [ testSameType True "Unit" "Unit" ["()"]
              , testSameType True "List a" "List a" []
              , testSameType True "a :*: b :+: c :*: d" "(a :*: b) :+: (c :*: d)" []
              , testSameType True "a :*: b :*: c :*: d" "a :*: (b :*: (c :*: d))" []
              , testSameType
                    True
                    "Integer :*: Boolean :*: Integer :*: Boolean"
                    "Integer :*: (Boolean :*: (Integer :*: Boolean))"
                    ["(3, (True, (7, False)))"]
              ]
        , testTree
              "subtype"
              [ testQuery "let i : Integer -> Number; i x = x in i 3" $ LRSuccess "3"
              , testQuery "let a : Integer; a = 3; b : Number; b = a in b" $ LRSuccess "3"
              , testQuery "let i : FiniteSetRef -a -> SetRef a; i x = x in 3" $ LRSuccess "3"
              , testQuery "let i : FiniteSetRef {-a,+Integer} -> SetRef a; i x = x in 3" $ LRSuccess "3"
              , testSubtype True "List1 Integer" "List Integer" []
              , testTree
                    "diamond"
                    [ testSubtype True "Monoid (List Unit)" "List Unit" []
                    , testSubtype True "Semigroup (List Unit)" "Monoid (List Unit)" []
                    , testSubtype True "Semigroup (List Unit)" "List Unit" []
                    ]
              ]
        , testTree
              "subsume"
              [ testQuery "let rec a: Unit; a = a end in ()" $ LRSuccess "()"
              , testQuery "let rec a: Integer; a = a end in ()" $ LRSuccess "()"
              , testQuery "let a: Integer|Text; a = error \"undefined\" in ()" $ LRSuccess "()"
              , testQuery "let rec a: Integer|Text; a = a end in ()" $ LRSuccess "()"
              , testQuery "let a: Integer|Text; a = 3 in ()" $ LRSuccess "()"
              , testQuery "let a: Integer|Text; a = 3; b: Integer|Text; b = 3 in ()" $ LRSuccess "()"
              , testQuery "let a: Integer|Text; a = 3; b: Integer|Text; b = a in ()" $ LRSuccess "()"
              , testQuery "let rec r = r end in let a: Integer|Text; a = r in ()" $ LRSuccess "()"
              , testQuery "let rec r = r end; a: Integer|Text; a = r in ()" $ LRSuccess "()"
              , testQuery "let rec r = a; a: Integer|Text; a = r end in ()" $ LRSuccess "()"
              , testQuery "let rec a: None; a = a end in ()" $ LRSuccess "()"
              , testQuery "let rec r = r end in let a : None; a = r in ()" $ LRSuccess "()"
              , testQuery "let rec r = r end; a: None; a = r in ()" $ LRSuccess "()"
              , testQuery "let rec r = a; a: None; a = r end in ()" $ LRSuccess "()"
              , testQuery "let a: List (Integer|Text); a = [] in a" $ LRSuccess "[]"
              , testQuery "let a: List Integer | List Text; a = [] in a" $ LRSuccess "[]"
              , testSameType True "Integer" "Integer" ["56"]
              , testSameType False "List (Integer|Text)" "List (Integer|Text)" ["[]"]
              , testSameType False "List Integer | List Text" "List Integer | List Text" ["[]"]
              , testSameType False "List (Integer|Text)" "List Integer | List Text" ["[]"]
              , testQuery "let a: Integer|Text; a = 3; b: List Integer | List Text; b = [a] in b" $ LRSuccess "[3]"
              ]
        , testTree
              "conversion"
              [ testQuery ("case ((\\x => Just x): Integer -> Maybe Integer) 34 of Just x => x end") $ LRSuccess "34"
              , testQuery ("((\\x => [x]): xy -> List1 xy: Integer -> List Integer) 79") $ LRSuccess "[79]"
              , testQuery ("case ((\\x => x :: []): Integer -> List Integer) 57 of x::_ => x end") $ LRSuccess "57"
              ]
        , testTree
              "recursive"
              [ testQuery "let x : rec a. List a; x = [] in x" $ LRSuccess "[]"
              , let
                    atree = ["[]", "[[]]", "[[[[]]]]", "[[], [[]]]"]
                    in testTree
                           "equivalence"
                           [ testSameType True "Integer" "Integer" ["0"]
                           , testSameType True "Integer" "rec a. Integer" ["0"]
                           , testSameType True "List Integer" "List (rec a. Integer)" ["[0]"]
                           , testSameType True "rec a. List a" "rec a. List a" atree
                           , testSameType True "rec a. List a" "rec a. List (List a)" atree
                           , ignoreTestBecause "ISSUE #61" $
                             testSameType
                                 False
                                 "rec a. (Maybe a | List a)"
                                 "(rec a. Maybe a) | (rec b. List b)"
                                 ["[]", "Nothing", "Just []", "[[]]"]
                           , ignoreTestBecause "ISSUE #61" $
                             testSameType
                                 False
                                 "rec a. (Maybe a | List a)"
                                 "(rec a. Maybe a) | (rec a. List a)"
                                 ["[]", "Nothing", "Just []", "[[]]"]
                           , testSubtype True "rec a. List a" "Showable" []
                           , testSubtype True "List (rec a. List a)" "Showable" []
                           , testSubtype True "rec a. List a" "List Showable" ["[]"]
                           , testSubtype True "List (rec a. List a)" "List Showable" ["[]"]
                           , testSameType False "None" "None" []
                           , testSameType False "rec a. a" "None" []
                           , testSameType False "List (rec a. a)" "List None" ["[]"]
                           , testSameType True "rec a. Integer" "Integer" ["0"]
                           , testSameType True "List (rec a. Integer)" "List Integer" ["[0]"]
                           , testTree
                                 "unroll"
                                 [ testSameType True "rec a. List a" "List (rec a. List a)" atree
                                 , testSameType
                                       False
                                       "rec a. (List a|Integer)"
                                       "List (rec a. (List a|Integer))|Integer"
                                       ["[]"]
                                 , testSameType
                                       False
                                       "rec a. (List a|Integer)"
                                       "List (rec a. (List a|Integer))|Integer"
                                       ["2"]
                                 , testSameType
                                       False
                                       "rec a. List (a|Integer)"
                                       "List ((rec a. List (a|Integer))|Integer)"
                                       ["[]"]
                                 , testSameType
                                       False
                                       "rec a. List (a|Integer)"
                                       "List ((rec a. List (a|Integer))|Integer)"
                                       ["[3]"]
                                 , testSameType
                                       False
                                       "rec a. List (a|Integer)"
                                       "List (rec a. (List (a|Integer)|Integer))"
                                       ["[]"]
                                 , testSameType
                                       False
                                       "rec a. List (a|Integer)"
                                       "List (rec a. (List (a|Integer)|Integer))"
                                       ["[3]"]
                                 ]
                           ]
              , testTree
                    "lazy"
                    [ testQuery
                          "let lazy: Any -> Integer -> Integer; lazy _ x = x in (\\x => lazy x 1) (error \"strict\")" $
                      LRSuccess "1"
                    , testQuery "let lazy: Any -> Integer -> Integer; lazy _ x = x in let rec x = lazy x 1 end in x" $
                      LRSuccess "1"
                    , testQuery
                          "let lazy: Any -> Integer -> Integer; lazy _ x = x in let x = lazy (error \"strict\") 1 in x" $
                      LRSuccess "1"
                    , testQuery "let lazy: Any -> Integer -> Integer; lazy _ x = x in let rec f = lazy f end in f 1" $
                      LRSuccess "1"
                    , testQuery
                          "let lazy: Any -> Integer -> Integer; lazy _ x = x in let f = lazy (error \"strict\") in f 1" $
                      LRSuccess "1"
                    ]
              , testTree
                    "subsume"
                    [ testQuery "let rec rval: rec a. Maybe a; rval = rval end in ()" $ LRSuccess "()"
                    , testQuery "let rec rval: rec a. Maybe a; rval = Just rval end in ()" $ LRSuccess "()"
                    , testQuery
                          "let rec rcount: (rec a. Maybe a) -> Integer; rcount x = case x of Nothing => 0; Just y => 1 + rcount y end end in rcount" $
                      LRSuccess "<?>"
                    , testQuery
                          "let rec rcount: (rec a. Maybe a) -> Integer; rcount x = case x of Nothing => 0; Just y => 1 + rcount y end end in rcount Nothing" $
                      LRSuccess "0"
                    , testQuery
                          "let rec rcount: (rec a. Maybe a) -> Integer; rcount x = case x of Nothing => 0; Just y => 1 + rcount1 y end; rcount1: (rec a. Maybe a) -> Integer; rcount1 x = case x of Nothing => 0; Just y => 1 + rcount y end end in rcount" $
                      LRSuccess "<?>"
                    , testQuery
                          "let rec rcount x = case x of Nothing => 0; Just y => 1 + rcount1 y end; rcount1 x = case x of Nothing => 0; Just y => 1 + rcount y end end in rcount $ Just Nothing" $
                      LRSuccess "1"
                    , testQuery
                          "let rec rcount = rcount1; rcount1 x = case x of Nothing => 0; Just y => 1 + rcount1 y end end in rcount $ Just Nothing" $
                      LRSuccess "1"
                    , testQuery
                          "let rec rcount: (rec a. Maybe a) -> Integer; rcount = rcount1; rcount1 x = case x of Nothing => 0; Just y => 1 + rcount1 y end end in rcount $ Just Nothing" $
                      LRSuccess "1"
                    , testQuery
                          "let rec rcount: (rec xb. Maybe xb) -> Integer; rcount x = case x of Nothing => 0; Just y => 1 + rcount y end end in rcount $ Just Nothing" $
                      LRSuccess "1"
                    , testQuery
                          "let rec rcount = rcount1; rcount1: (rec xb. Maybe xb) -> Integer; rcount1 x = case x of Nothing => 0; Just y => 1 + rcount1 y end end in rcount $ Just Nothing" $
                      LRSuccess "1"
                    , testQuery
                          "let rec rcount: (rec xa. Maybe xa) -> Integer; rcount = rcount1; rcount1: (rec xb. Maybe xb) -> Integer; rcount1 x = case x of Nothing => 0; Just y => 1 + rcount1 y end end in rcount $ Just Nothing" $
                      LRSuccess "1"
                    , testQuery
                          "let rec rcount: (rec x. Maybe x) -> Integer; rcount = rcount1; rcount1: (rec x. Maybe x) -> Integer; rcount1 x = case x of Nothing => 0; Just y => 1 + rcount y end end in ()" $
                      LRSuccess "()"
                    , testQuery
                          "let rec rcount = rcount1; rcount1 x = case x of Nothing => 0; Just y => 1 + rcount y end end in rcount $ Just Nothing" $
                      LRSuccess "1"
                    , testQuery
                          "let rec rcount = rcount1; rcount1: (rec x. Maybe x) -> Integer; rcount1 x = case x of Nothing => 0; Just y => 1 + rcount y end end in rcount $ Just Nothing" $
                      LRSuccess "1"
                    , testQuery
                          "let rec rcount: (rec xa. Maybe xa) -> Integer; rcount x = case x of Nothing => 0; Just y => 1 + rcount1 y end; rcount1: (rec xb. Maybe xb) -> Integer; rcount1 x = case x of Nothing => 0; Just y => 1 + rcount y end end in rcount $ Just $ Just $ Just $ Just $ Just Nothing" $
                      LRSuccess "5"
                    , testQuery
                          "let rec rcount: (rec xc. Maybe xc) -> Integer; rcount x = case x of Nothing => 0; Just y => 1 + rcount1 y end; rcount1 x = case x of Nothing => 0; Just y => 1 + rcount y end end in rcount $ Just $ Just $ Just $ Just $ Just Nothing" $
                      LRSuccess "5"
                    , testTree
                          "lazy"
                          [ testQuery
                                "let f: (x -> Integer) -> Maybe x -> Integer; f rc x = case x of Nothing => 0; Just y => 1 + rc y end in let rec rcount: (rec z. Maybe z) -> Integer; rcount = rcount1; rcount1 = f rcount end in rcount $ Just Nothing" $
                            LRSuccess "1"
                          , testQuery
                                "let f: ((rec x. Maybe x) -> Integer) -> (rec x. Maybe x) -> Integer; f rc x = case x of Nothing => 0; Just y => 1 + rc y end in let rec rcount: (rec z. Maybe z) -> Integer; rcount = rcount1; rcount1 = f rcount end in rcount $ Just Nothing" $
                            LRSuccess "1"
                          , testQuery
                                "let f: (Integer -> Integer) -> Integer -> Integer; f rc x = if x == 0 then 0 else 1 + rc (x - 1) in let rec rcount: Integer -> Integer; rcount = rcount1; rcount1 = f rcount end in rcount 1" $
                            LRSuccess "1"
                          , testQuery "let f _ x = x in let rec rcount = f rcount end in rcount 1" $ LRSuccess "1"
                          , testQuery
                                "let f: Any -> Integer -> Integer; f _ x = x in let rec rcount = f (seq (error \"strict\") rcount) end in rcount 1" $
                            LRSuccess "1"
                          , testQuery
                                "let f: Any -> Integer -> Integer; f _ x = x in let rec rcount = f (seq rcount (error \"strict\")) end in rcount 1" $
                            LRSuccess "1"
                          , testQuery
                                "let f: (Integer -> Integer) -> Integer -> Integer; f _ x = x in let rec rcount = f rcount end in rcount 1" $
                            LRSuccess "1"
                          , testQuery
                                "let f: (Integer -> Integer) -> Integer -> Integer; f _ x = x in let rec rcount = rcount1; rcount1 = f rcount end in rcount 1" $
                            LRSuccess "1"
                          , testQuery
                                "let f: (Integer -> Integer) -> Integer -> Integer; f _ x = x in let rec rcount: Integer -> Integer; rcount = rcount1; rcount1 = f rcount end in rcount 1" $
                            LRSuccess "1"
                          , testQuery
                                "let rec rcount: (rec a. Maybe a) -> Integer; rcount = rcount1; rcount1 x = case x of Nothing => 0; Just y => 1 + rcount y end end in rcount $ Just Nothing" $
                            LRSuccess "1"
                          , testQuery
                                "let rec rcount: (rec a. Maybe a) -> Integer; rcount = rcount1; rcount1: (rec a. Maybe a) -> Integer; rcount1 x = case x of Nothing => 0; Just y => 1 + rcount y end end in rcount $ Just Nothing" $
                            LRSuccess "1"
                          , testQuery
                                "let rec rcount: (rec xa. Maybe xa) -> Integer; rcount = rcount1; rcount1: (rec xb. Maybe xb) -> Integer; rcount1 x = case x of Nothing => 0; Just y => 1 + rcount y end end in rcount $ Just Nothing" $
                            LRSuccess "1"
                          ]
                    ]
              , testTree
                    "case"
                    [ testQuery
                          "let rec rcount x = case x of Nothing => 0; Just y => 1 + rcount y end end in rcount Nothing" $
                      LRSuccess "0"
                    , testQuery
                          "let rec rcount x = case x of Nothing => 0; Just y => if True then 1 else 1 + rcount y end end in rcount $ Just Nothing" $
                      LRSuccess "1"
                    , testQuery
                          "let rec rcount x = case x of Nothing => 0; Just Nothing => 1; Just (Just y) => if True then 2 else 2 + rcount y end end in rcount $ Just Nothing" $
                      LRSuccess "1"
                    , testQuery
                          "let rec rcount x = case x of Nothing => 0; Just y => case y of Nothing => 1; Just z => if True then 2 else 1 + rcount y end end end in rcount $ Just Nothing" $
                      LRSuccess "1"
                    , testQuery
                          "let rec rcount x = case x of Nothing => 0; Just y => 1 + rcount y end end in rcount $ Just Nothing" $
                      LRSuccess "1"
                    , testQuery
                          "let rec rcount x = case x of Nothing => 0; Just y => if True then 1 else 1 + rcount y end end in rcount $ Just $ Just Nothing" $
                      LRSuccess "1"
                    , testQuery
                          "let rec rcount x = case x of Nothing => 0; Just Nothing => 1; Just (Just y) => if True then 2 else 2 + rcount y end end in rcount $ Just $ Just Nothing" $
                      LRSuccess "2"
                    , testQuery
                          "let rec rcount x = case x of Nothing => 0; Just y => case y of Nothing => 1; Just z => if True then 2 else 1 + rcount y end end end in rcount $ Just $ Just Nothing" $
                      LRSuccess "2"
                    , testQuery
                          "let rec rcount x = case x of Nothing => 0; Just y => case y of Nothing => 1; Just z => case z of Nothing => 2; Just p => if True then 3 else 1 + rcount y end end end end in rcount $ Just $ Just Nothing" $
                      LRSuccess "2"
                    , testQuery
                          "let rec rcount x = case x of Nothing => 0; Just y => case y of Nothing => 1; Just z => case z of Nothing => 2; Just p => 1 + rcount y end end end end in rcount $ Just $ Just Nothing" $
                      LRSuccess "2"
                    , testQuery
                          "let rec rcount x = case x of Nothing => 0; Just y => case y of Nothing => 1; Just z => case z of Nothing => 2; Just p => 1 + rcount y end end end; rcount1 x = rcount x end in rcount1 $ Just $ Just Nothing" $
                      LRSuccess "2"
                    , testQuery
                          "let rec rcount1 y = case y of Nothing => 0; Just z => 1 + rcount z end; rcount x = case x of Nothing => 0; Just y => 1 + rcount1 y end end in rcount $ Just $ Just $ Just $ Just Nothing" $
                      LRSuccess "4"
                    , testQuery
                          "let rec rcount1 y = case y of Nothing => 0; Just z => 1 + rcount z end; rcount x = case x of Nothing => 0; Just y => 1 + rcount1 y end end in rcount $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just Nothing" $
                      LRSuccess "12"
                    , testQuery
                          "let rec rcount1 y = case y of Nothing => 0; Just z => 1 + rcount z end; rcount : (rec a. Maybe a) -> Integer; rcount x = case x of Nothing => 0; Just y => 1 + rcount1 y end end in rcount $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just Nothing" $
                      LRSuccess "12"
                    , testQuery
                          "let rec rcount : (rec a. Maybe a) -> Integer; rcount x = case x of Nothing => 0; Just Nothing => 1; Just (Just y) => if True then 2 else 2 + rcount y end; rval : rec a. Maybe a; rval = Just $ Just Nothing end in rcount rval" $
                      LRSuccess "2"
                    , testQuery
                          "let rec rcount : (rec a. Maybe a) -> Integer; rcount x = case x of Nothing => 0; Just Nothing => 1; Just (Just Nothing) => 2; Just (Just (Just Nothing)) => 3; Just (Just (Just (Just Nothing))) => 4; Just (Just (Just (Just (Just _)))) => 5 end; rval : rec a. Maybe a; rval = Just $ Just $ Just $ Just Nothing end in rcount rval" $
                      LRSuccess "4"
                    , testQuery
                          "let rec rcount : (rec a. Maybe a) -> Integer; rcount x = case x of Nothing => 0; Just Nothing => 1; Just (Just Nothing) => 2; Just (Just (Just Nothing)) => 3; Just (Just (Just (Just Nothing))) => 4; Just (Just (Just (Just (Just _)))) => 5 end; rval : rec a. Maybe a; rval = Just rval end in rcount rval" $
                      LRSuccess "5"
                    , testQuery
                          "let rec rcount : (rec a. Maybe a) -> Integer; rcount x = case x of Nothing => 0; Just y => 1 + rcount y end; rval : rec a. Maybe a; rval = Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just Nothing end in rcount rval" $
                      LRSuccess "10"
                    , testQuery
                          "let rec rcount : (rec a. Maybe a) -> Integer; rcount x = case x of Nothing => 0; Just y => 1 + rcount y end; rval = Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just Nothing end in rcount rval" $
                      LRSuccess "10"
                    , testQuery
                          "let fix: (a -> a) -> a; fix f = let rec x = f x end in x; rc: (a -> Integer) -> Maybe a -> Integer; rc r x = case x of Nothing => 0; Just y => 1 + r y end in fix rc $ Just $ Just $ Just $ Just $ Just Nothing" $
                      LRSuccess "5"
                    , testQuery
                          "let rec rcount x = case x of Nothing => 0; Just y => 1 + rcount y end end in rcount $ Just $ Just Nothing" $
                      LRSuccess "2"
                    , testQuery
                          "let rec rcount x = case x of Nothing => 0; Just y => 1 + rcount y end end in rcount $ Just $ Just $ Just Nothing" $
                      LRSuccess "3"
                    , testQuery
                          "let rec rcount x = case x of Nothing => 0; Just y => 1 + rcount y end end in rcount $ Just $ Just $ Just $ Just Nothing" $
                      LRSuccess "4"
                    , testQuery
                          "let rec rcount x = case x of Nothing => 0; Just y => 1 + rcount y end end in rcount $ Just $ Just $ Just $ Just $ Just Nothing" $
                      LRSuccess "5"
                    , testQuery
                          "let rec rcount x = case x of Nothing => 0; Just y => 1 + rcount y end; rval = Just $ Just Nothing end in rcount rval" $
                      LRSuccess "2"
                    , testQuery
                          "let rec rcount x = case x of Nothing => 0; Just y => 1 + rcount y end; rval : Maybe (Maybe (Maybe None)) ; rval = Just $ Just Nothing end in rcount rval" $
                      LRSuccess "2"
                    , testQuery
                          "let rec rcount x = case x of Nothing => 0; Just y => 1 + rcount y end; rval : Maybe (Maybe (Maybe (Maybe None))) ; rval = Just $ Just Nothing end in rcount rval" $
                      LRSuccess "2"
                    , testQuery "Just $ Just $ Just Nothing" $ LRSuccess "Just Just Just Nothing"
                    , testQuery
                          "let rec rcount x = case x of Nothing => 0; Just y => 1 + rcount y end; rval = Just $ Just $ Just Nothing end in rcount rval" $
                      LRSuccess "3"
                    , testQuery
                          "let rec rcount x = case x of Nothing => 0; Just y => 1 + rcount y end; rval : Maybe (Maybe (Maybe (Maybe None))) ; rval = Just $ Just $ Just Nothing end in rcount rval" $
                      LRSuccess "3"
                    , testQuery
                          "let rec rcount x = case x of Nothing => 0; Just y => 1 + rcount y end; rval : Maybe (Maybe (Maybe (Maybe (Maybe None)))) ; rval = Just $ Just $ Just Nothing end in rcount rval" $
                      LRSuccess "3"
                    , testQuery
                          "let rec rcount x = case x of Nothing => 0; Just Nothing => 1; Just (Just Nothing) => 2; Just (Just (Just Nothing)) => 3; _ => 4 end end in rcount $ Just $ Just $ Just Nothing" $
                      LRSuccess "3"
                    , testQuery
                          "let rec rcount x = case x of Nothing => 0; Just y => 1 + r1count y end; r1count x = case x of Nothing => 0; Just y => 1 + r1count y end end in rcount $ Just $ Just $ Just Nothing" $
                      LRSuccess "3"
                    , testQuery
                          "case Just $ Just $ Just Nothing of Nothing => 0; Just Nothing => 1; Just (Just Nothing) => 2; Just (Just (Just Nothing)) => 3; _ => 4 end" $
                      LRSuccess "3"
                    , testQuery
                          "let rec rcount x = case x of Nothing => 0; Just Nothing => 1; Just (Just Nothing) => 2; Just (Just (Just Nothing)) => 3; _ => 4 end end in rcount $ Just $ Just $ Just Nothing" $
                      LRSuccess "3"
                    , testQuery
                          "let rec rcount : (rec a . Maybe a) -> Integer; rcount x = case x of Nothing => 0; Just Nothing => 1; Just (Just Nothing) => 2; Just (Just (Just Nothing)) => 3; _ => 4 end end in rcount $ Just $ Just $ Just Nothing" $
                      LRSuccess "3"
                    , testQuery
                          "let rec rcount : (rec a . Maybe a) -> Integer; rcount x = case x of Nothing => 0; Just Nothing => 1; Just (Just Nothing) => 2; Just (Just (Just Nothing)) => 3; Just (Just (Just (Just y))) => 4 + rcount y end end in rcount $ Just $ Just $ Just Nothing" $
                      LRSuccess "3"
                    , testQuery
                          "let rec rcount : (rec a . Maybe a) -> Integer; rcount x = case x of Nothing => 0; Just Nothing => 1; Just (Just y) => 2 + rcount y end end in rcount $ Just $ Just $ Just Nothing" $
                      LRSuccess "3"
                    ]
              ]
        , let
              testSupertype :: Text -> Text -> Text -> Text -> Bool -> TestTree
              testSupertype supertype subtype val altval good = let
                  result =
                      LRSuccess $
                      unpack $
                      if good
                          then val
                          else altval
                  in testTree
                         (unpack $ supertype <> " -> " <> subtype)
                         [ testQuery
                               ("let x: " <>
                                supertype <>
                                "; x=" <>
                                val <>
                                "; y: " <>
                                subtype <>
                                "; y = case x of (z: " <> subtype <> ") => z; _ => " <> altval <> "; end in y")
                               result
                         , testQuery
                               ("let x: " <>
                                supertype <>
                                "; x=" <>
                                val <>
                                "; y: " <>
                                subtype <>
                                "; y = case check @(" <>
                                subtype <> ") x of Just z => z; Nothing => " <> altval <> "; end in y")
                               result
                         , testQuery
                               ("let x: " <>
                                supertype <>
                                "; x=" <> val <> "; y: " <> subtype <> "; y = coerce @(" <> subtype <> ") x in y") $
                           if good
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
                      [ testQuery ("Debug.literalLength " <> val) $ LRSuccess $ show len
                      , testQuery ("Debug.literalIsEmbedded " <> val) $ LRSuccess $ show embedded
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
                     , testLiteral 11 True "34"
                     , testLiteral 11 True "34.5"
                     , testLiteral 9 True "~34"
                     ]
        ]

testShim :: Text -> String -> String -> TestTree
testShim query expectedType expectedShim =
    testTree (unpack query) $ do
        result <-
            withNullPinaforeContext $ runInterpretResult $ runPinaforeScoped (initialPos "<input>") $ parseValue query
        case result of
            FailureResult e -> assertFailure $ "expected success, found failure: " ++ show e
            SuccessResult (MkSomeOf (MkPosShimWit t shim) _) -> do
                assertEqual "type" expectedType $ unpack $ exprShow t
                assertEqual "shim" expectedShim $ show shim

testShims :: TestTree
testShims =
    testTree
        "shim"
        [ testShim "3" "Integer" "(join1 id)"
        , testShim "negate" "Integer -> Integer" "(join1 (co (contra id (meet1 id)) (join1 id)))"
        , testShim "negate 3" "Integer" "(join1 id)"
        , expectFailBecause "ISSUE #63" $ testShim "id" "a -> a" "(join1 (co (contra id (meet1 id)) (join1 id)))"
        , expectFailBecause "ISSUE #63" $ testShim "id 3" "Integer" "(join1 id)"
        , expectFailBecause "ISSUE #63" $ testShim "\\x => x" "a -> a" "(join1 (co (contra id (meet1 id)) (join1 id)))"
        , expectFailBecause "ISSUE #63" $ testShim "(\\x => x) 3" "Integer" "(join1 id)"
        , testShim "\\x => 4" "Any -> Integer" "(join1 (co (contra id termf) (join1 id)))"
        , testShim "(\\x => 4) 3" "Integer" "(join1 id)"
        , expectFailBecause "ISSUE #63" $
          testShim
              "let rcount x = case x of Nothing => 0; Just y => 1 + rcount y end in rcount"
              "(rec c. Maybe c) -> Integer"
              "(join1 id)"
        , expectFailBecause "ISSUE #63" $
          testShim
              "let rcount : (rec a. Maybe a) -> Integer; rcount x = case x of Nothing => 0; Just y => 1 + rcount y end in rcount"
              "(rec a. Maybe a) -> Integer"
              "(join1 id)"
        ]

testLanguage :: TestTree
testLanguage = localOption (mkTimeout 2000000) $ testTree "language" [testInfix, testNumbers, testShims, testQueries]
