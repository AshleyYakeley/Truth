{-# OPTIONS -fno-warn-orphans #-}

module Test.Language
    ( testLanguage
    ) where

import Data.Shim
import Pinafore
import Pinafore.Language.Documentation
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
testInfix = let
    names = filter nameIsInfix $ fmap (MkName . docName) $ toList predefinedDoc
    in testTree "infix" $ fmap testOp names

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
        , testRead " 1" $ Just $ ExactNumber 1
        , testRead "1 " $ Just $ ExactNumber 1
        , testRead " 1 " $ Just $ ExactNumber 1
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

testQuery :: Text -> LangResult -> TestTree
testQuery query expected =
    testTree (show $ unpack query) $ do
        result <- withNullPinaforeContext $ runInterpretResult $ runPinaforeSourceScoped "<input>" $ parseValue query
        case result of
            FailureResult e ->
                case expected of
                    LRCheckFail -> return ()
                    _ -> assertFailure $ "check: expected success, found failure: " ++ show e
            SuccessResult v -> do
                let r = showPinaforeRef v
                me <- catchPureError r
                case (expected, me) of
                    (LRCheckFail, _) -> assertFailure $ "check: expected failure, found success"
                    (LRRunError, Nothing) -> assertFailure $ "run: expected error, found success: " ++ r
                    (LRRunError, Just _) -> return ()
                    (LRSuccess _, Just e) -> assertFailure $ "run: expected success, found error: " ++ show e
                    (LRSuccess s, Nothing) -> assertEqual "result" s r

testSubsumeSubtype :: Text -> Text -> [Text] -> [TestTree]
testSubsumeSubtype t1 t2 vs =
    [testQuery ("let r = r; x : " <> t1 <> "; x = r; y : " <> t2 <> "; y = x in ()") $ LRSuccess "unit"] <>
    fmap
        (\v -> testQuery ("let x : " <> t1 <> "; x = " <> v <> "; y : " <> t2 <> "; y = x in y") $ LRSuccess $ unpack v)
        vs

testFunctionSubtype :: Text -> Text -> [Text] -> [TestTree]
testFunctionSubtype t1 t2 vs =
    [testQuery ("let f : (" <> t1 <> ") -> (" <> t2 <> "); f x = x in f") $ LRSuccess "<?>"] <>
    fmap (\v -> testQuery ("let f : (" <> t1 <> ") -> (" <> t2 <> "); f x = x in f " <> v) $ LRSuccess $ unpack v) vs

testSubtype1 :: Bool -> Text -> Text -> [Text] -> [TestTree]
testSubtype1 b t1 t2 vs =
    testSubsumeSubtype t1 t2 vs <>
    if b
        then testFunctionSubtype t1 t2 vs
        else []

testSubtype :: Bool -> Text -> Text -> [Text] -> TestTree
testSubtype b t1 t2 vs = testTree (unpack $ t1 <> " <: " <> t2) $ testSubtype1 b t1 t2 vs

testSameType :: Bool -> Text -> Text -> [Text] -> TestTree
testSameType b t1 t2 vs = testTree (unpack $ t1 <> " = " <> t2) $ (testSubtype1 b t1 t2 vs) <> (testSubtype1 b t2 t1 vs)

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
              , testQuery "\"\"" $ LRSuccess ""
              , testQuery "\"Hello \"" $ LRSuccess "Hello "
              , testQuery "True" $ LRSuccess "True"
              , testQuery "False" $ LRSuccess "False"
              , testQuery "\"1\"" $ LRSuccess "1"
              , testQuery "uiListTable" $ LRSuccess "<?>"
              , testQuery "let opentype T in openEntity @T !\"example\"" $ LRSuccess "<?>"
              , testQuery "let opentype T in entityAnchor $ openEntity @T !\"example\"" $
                LRSuccess "!1AF8A5FD-24AAAF3E-3668C588-6C74D36A-70ED9618-CC874895-E4569C9F-FCD42CD3"
              ]
        , testTree
              "list construction"
              [ testQuery "[]" $ LRSuccess $ show @[Text] []
              , testQuery "[1]" $ LRSuccess $ "[1]"
              , testQuery "[1,2,3]" $ LRSuccess "[1, 2, 3]"
              ]
        , testTree
              "functions"
              [ testQuery "\\x -> x" $ LRSuccess "<?>"
              , testQuery "\\x -> 1" $ LRSuccess "<?>"
              , testQuery "\\x y -> y" $ LRSuccess "<?>"
              , testQuery "\\x y z -> [x,y,z]" $ LRSuccess "<?>"
              ]
        , testTree
              "predefined"
              [ testQuery "abs" $ LRSuccess "<?>"
              , testQuery "fst" $ LRSuccess "<?>"
              , testQuery "(+)" $ LRSuccess "<?>"
              , testQuery "\\a b -> a + b" $ LRSuccess "<?>"
              , testQuery "(==)" $ LRSuccess "<?>"
              , testQuery "\\a b -> a == b" $ LRSuccess "<?>"
              ]
        , testTree
              "let-binding"
              [ testQuery "let in 27" $ LRSuccess "27"
              , testQuery "let a=\"5\" in a" $ LRSuccess "5"
              , testQuery "let a=5 in a" $ LRSuccess "5"
              , testQuery "let a=1 in let a=2 in a" $ LRSuccess "2"
              , testQuery "let a=1;b=2 in a" $ LRSuccess "1"
              , testQuery "let a=1;b=2 in b" $ LRSuccess "2"
              , testQuery "let a=1;b=2 in b" $ LRSuccess "2"
              , testQuery "let a=1;b=\"2\" in b" $ LRSuccess "2"
              , testQuery "let a=1 ;b=\"2\" in b" $ LRSuccess "2"
              , testQuery "let a= 1 ;b=\"2\" in b" $ LRSuccess "2"
              , testQuery "let a=7;b=a in a" $ LRSuccess "7"
              , testQuery "let a=7;b=a in b" $ LRSuccess "7"
              , testQuery "let a=2 in let b=a in b" $ LRSuccess "2"
              ]
        , testTree
              "name shadowing"
              [ testQuery "let a=1 in (\\a -> a) 2" $ LRSuccess "2"
              , testQuery "let a=1 in (\\(Just a) -> a) (Just 2)" $ LRSuccess "2"
              , testQuery "let a=1 in let a=2 in a" $ LRSuccess "2"
              , testQuery "(\\a -> let a=2 in a) 1" $ LRSuccess "2"
              , testQuery "(\\a -> \\a -> a) 1 2" $ LRSuccess "2"
              , testQuery "let a=1 in case 2 of a -> a end" $ LRSuccess "2"
              , testQuery "let a=1 in case Just 2 of Just a -> a end" $ LRSuccess "2"
              , testQuery "case 1 of a -> case 2 of a -> a end end" $ LRSuccess "2"
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
              "recursive let-binding"
              [ testQuery "let a=1 in a" $ LRSuccess "1"
              , testQuery "let a=1 in let a=2 in a" $ LRSuccess "2"
              , testQuery "let a=1;a=2 in a" $ LRCheckFail
              , testQuery "let a=1;b=a in b" $ LRSuccess "1"
              , testQuery "let b=a;a=1 in b" $ LRSuccess "1"
              , testQuery "let a x = x in a 1" $ LRSuccess "1"
              , testQuery "let a x = x; b = a in b" $ LRSuccess "<?>"
              , testQuery "let a = \\x -> x in let b = a 1 in b" $ LRSuccess "1"
              , testQuery "let a x = x; b = a 1 in b" $ LRSuccess "1"
              , testQuery "let a x = b; b = b in a" $ LRSuccess "<?>"
              , testQuery "let a x = 1; b = b in a b" $ LRSuccess "1"
              , testQuery "let a x = 1; b = a b in b" $ LRSuccess "1"
              , testQuery "let a x = 1 in let b = a b in b" $ LRSuccess "1"
              , testQuery "let b = (\\x -> 1) b in b" $ LRSuccess "1"
              , testQuery "let b = a b; a x = 1 in b" $ LRSuccess "1"
              , testQuery "let a x = 1; b = a c; c=b in b" $ LRSuccess "1"
              ]
        , testTree
              "recursive let-binding polymorphism"
              [ testQuery "let i = \\x -> x in (1 + i 1, i False)" $ LRSuccess "(2, False)"
              , testQuery "let i = \\x -> x; r = (1 + i 1, i False) in r" $ LRSuccess "(2, False)"
              , testQuery "let r = (1 + i 1, i False); i = \\x -> x in r" $ LRSuccess "(2, False)"
              ]
        , testTree
              "duplicate bindings"
              [ testQuery "let a=1;a=1 in a" $ LRCheckFail
              , testQuery "let a=1;a=2 in a" $ LRCheckFail
              , testQuery "let a=1;b=0;a=2 in a" $ LRCheckFail
              ]
        , testTree
              "lexical scoping"
              [ testQuery "let a=1 in let b=a in let a=3 in a" $ LRSuccess "3"
              , testQuery "let a=1;b=a;a=3 in a" $ LRCheckFail
              , testQuery "let a=1 in let b=a in let a=3 in b" $ LRSuccess "1"
              , testQuery "let a=1;b=a;a=3 in b" $ LRCheckFail
              ]
        , testTree
              "operators"
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
              , testQuery "\"abc\"<>\"def\"" $ LRSuccess "abcdef"
              , testQuery "\"abc\" <>\"def\"" $ LRSuccess "abcdef"
              , testQuery "\"abc\"<> \"def\"" $ LRSuccess "abcdef"
              , testQuery "\"abc\" <> \"def\"" $ LRSuccess "abcdef"
              , testQuery "let f x = x + 2 in f -1" $ LRSuccess "1"
              , testQuery "let f = 2 in f - 1" $ LRSuccess "1"
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
              [ testQuery "\"pqrs\"" $ LRSuccess "pqrs"
              , testQuery "textLength \"abd\"" $ LRSuccess "3"
              , testQuery "textSection 4 3 \"ABCDEFGHIJKLMN\"" $ LRSuccess "EFG"
              ]
        , testTree
              "operator precedence"
              [ testQuery "1 + 2 * 3" $ LRSuccess "7"
              , testQuery "3 * 2 + 1" $ LRSuccess "7"
              , testQuery "2 * 2 * 2" $ LRSuccess "8"
              , testQuery "12 / 2 / 2" $ LRSuccess "3"
              , testQuery "12 / 2 / 2" $ LRSuccess "3"
              , testQuery "0 == 0 == 0" $ LRCheckFail
              ]
        , testTree
              "if/then/else"
              [ testQuery "if True then 3 else 4" $ LRSuccess "3"
              , testQuery "if False then 3 else 4" $ LRSuccess "4"
              , testQuery "if False then if True then 1 else 2 else if True then 3 else 4" $ LRSuccess "3"
              ]
        , testTree "pairs" [testQuery "fst (7,9)" $ LRSuccess "7", testQuery "snd (7,9)" $ LRSuccess "9"]
        , testTree
              "either"
              [ testQuery "fromEither (\\a -> (\"Left\",a)) (\\a -> (\"Right\",a)) $ Left \"x\"" $ LRSuccess "(Left, x)"
              , testQuery "fromEither (\\a -> (\"Left\",a)) (\\a -> (\"Right\",a)) $ Right \"x\"" $
                LRSuccess "(Right, x)"
              ]
        , testTree
              "type signature"
              [ testQuery "let i : a -> a; i x = x in i 3" $ LRSuccess "3"
              , testQuery "let i : Number -> Number; i x = x in i 3" $ LRSuccess "3"
              , testQuery "let i : Text -> Text; i x = x in i 3" $ LRCheckFail
              , testQuery "let i : a -> a; i x = x in i \"t\"" $ LRSuccess "t"
              , testQuery "let i : Number -> Number; i x = x in i \"t\"" $ LRCheckFail
              , testQuery "let i : Text -> Text; i x = x in i \"t\"" $ LRSuccess "t"
              , testQuery "let i : a -> a; i x = x in 0" $ LRSuccess "0"
              , testQuery "let i : a -> Number; i x = x in 0" $ LRCheckFail
              , testQuery "let i : Number -> a; i x = x in 0" $ LRCheckFail
              , testQuery "let i : Number -> Number; i x = x in 0" $ LRSuccess "0"
              , testQuery "let i : Either Number Boolean; i = Left 5 in i" $ LRSuccess "Left 5"
              , testQuery "let i : Either Number Boolean; i = Right False in i" $ LRSuccess "Right False"
              , testQuery "let i : Maybe Number; i = Just 5 in i" $ LRSuccess "Just 5"
              , testQuery "let i : Maybe Number; i = Nothing in i" $ LRSuccess "Nothing"
              ]
        , testTree
              "patterns"
              [ testQuery "(\\a -> 5) 2" $ LRSuccess "5"
              , testQuery "(\\a -> a) 2" $ LRSuccess "2"
              , testQuery "(\\_ -> 5) 2" $ LRSuccess "5"
              , testQuery "(\\a@b -> (a,b)) 2" $ LRSuccess "(2, 2)"
              , testQuery "(\\(a,b) -> a + b) (5,6)" $ LRSuccess "11"
              ]
        , testTree
              "case"
              [ testTree
                    "basic"
                    [ testQuery "case 2 of a -> 5 end" $ LRSuccess "5"
                    , testQuery "case 2 of a -> 5; a -> 3 end" $ LRSuccess "5"
                    , testQuery "case 2 of a -> 5; a -> 3; end" $ LRSuccess "5"
                    , testQuery "case 2 of a -> a end" $ LRSuccess "2"
                    , testQuery "case 2 of _ -> 5 end" $ LRSuccess "5"
                    , testQuery "case 2 of _ -> 5; _ -> 3 end" $ LRSuccess "5"
                    , testQuery "case 2 of a@b -> (a,b) end" $ LRSuccess "(2, 2)"
                    ]
              , testTree
                    "Boolean"
                    [ testQuery "case True of True -> 5; False -> 7 end" $ LRSuccess "5"
                    , testQuery "case False of True -> 5; False -> 7 end" $ LRSuccess "7"
                    , testQuery "case True of False -> 7; True -> 5 end" $ LRSuccess "5"
                    , testQuery "case False of False -> 7; True -> 5 end" $ LRSuccess "7"
                    ]
              , testTree
                    "Number"
                    [ testQuery "case 37 of 37 -> True; _ -> False end" $ LRSuccess "True"
                    , testQuery "case 38 of 37 -> True; _ -> False end" $ LRSuccess "False"
                    , testQuery "case -24.3 of 37 -> 1; -24.3 -> 2; _ -> 3 end" $ LRSuccess "2"
                    ]
              , testTree
                    "String"
                    [ testQuery "case \"Hello\" of \"Hello\" -> True; _ -> False end" $ LRSuccess "True"
                    , testQuery "case \"thing\" of \"Hello\" -> True; _ -> False end" $ LRSuccess "False"
                    , testQuery "case \"thing\" of \"Hello\" -> 1; \"thing\" -> 2; _ -> 3 end" $ LRSuccess "2"
                    ]
              , testTree
                    "Either"
                    [ testQuery "case Left 3 of Left a -> a; Right _ -> 1 end" $ LRSuccess "3"
                    , testQuery "case Right 4 of Left a -> a + 1; Right a -> a end" $ LRSuccess "4"
                    , testQuery "case Right 7 of Right 4 -> True; _ -> False end" $ LRSuccess "False"
                    , testQuery "case Right 7 of Right 4 -> 1; Right 7 -> 2; Left _ -> 3; _ -> 4 end" $ LRSuccess "2"
                    ]
              , testTree "Unit" [testQuery "case () of () -> 4 end" $ LRSuccess "4"]
              , testTree "Pair" [testQuery "case (2,True) of (2,a) -> a end" $ LRSuccess "True"]
              , testTree
                    "Maybe"
                    [ testQuery "case Just 3 of Just a -> a + 1; Nothing -> 7 end" $ LRSuccess "4"
                    , testQuery "case Nothing of Just a -> a + 1; Nothing -> 7 end" $ LRSuccess "7"
                    ]
              , testTree
                    "List"
                    [ testQuery "case [] of [] -> True; _ -> False end" $ LRSuccess "True"
                    , testQuery "case [] of _::_ -> True; _ -> False end" $ LRSuccess "False"
                    , testQuery "case [1,2] of [] -> True; _ -> False end" $ LRSuccess "False"
                    , testQuery "case [3,4] of _::_ -> True; _ -> False end" $ LRSuccess "True"
                    , testQuery "case [3] of a::b -> (a,b) end" $ LRSuccess "(3, [])"
                    , testQuery "case [3,4] of a::b -> (a,b) end" $ LRSuccess "(3, [4])"
                    , testQuery "case [3,4,5] of a::b -> (a,b) end" $ LRSuccess "(3, [4, 5])"
                    , testQuery "case [3] of [a,b] -> 1; _ -> 2 end" $ LRSuccess "2"
                    , testQuery "case [3,4] of [a,b] -> 1; _ -> 2 end" $ LRSuccess "1"
                    , testQuery "case [3,4,5] of [a,b] -> 1; _ -> 2 end" $ LRSuccess "2"
                    , testQuery "case [3,4] of [a,b] -> (a,b) end" $ LRSuccess "(3, 4)"
                    ]
              ]
        , testTree
              "subtype"
              [ testQuery "let i : Integer -> Number; i x = x in i 3" $ LRSuccess "3"
              , testQuery "let a : Integer; a = 3; b : Number; b = a in b" $ LRSuccess "3"
              , testQuery "let i : FiniteSetRef -a -> SetRef a; i x = x in 3" $ LRSuccess "3"
              , testQuery "let i : FiniteSetRef {-a,+Integer} -> SetRef a; i x = x in 3" $ LRSuccess "3"
              ]
        , testTree
              "subsume"
              [ testQuery "let a : (); a = a in ()" $ LRSuccess "unit"
              , testQuery "let a : Integer; a = a in ()" $ LRSuccess "unit"
              , testQuery "let a : Integer|Text; a = a in ()" $ LRSuccess "unit"
              , testQuery "let r = r in let a : Integer|Text; a = r in ()" $ LRSuccess "unit"
              , testQuery "let r = r; a : Integer|Text; a = r in ()" $ LRSuccess "unit"
              , testQuery "let r = a; a : Integer|Text; a = r in ()" $ LRSuccess "unit"
              , testQuery "let a : None; a = a in ()" $ LRSuccess "unit"
              , testQuery "let r = r in let a : None; a = r in ()" $ LRSuccess "unit"
              , testQuery "let r = r; a : None; a = r in ()" $ LRSuccess "unit"
              , testQuery "let r = a; a : None; a = r in ()" $ LRSuccess "unit"
              , testQuery "let a : [Integer|Text]; a = [] in a" $ LRSuccess "[]"
              , testQuery "let a : [Integer]|[Text]; a = [] in a" $ LRSuccess "[]"
              , testSameType True "Integer" "Integer" ["56"]
              , testSameType False "[Integer|Text]" "[Integer|Text]" ["[]"]
              , testSameType False "[Integer]|[Text]" "[Integer]|[Text]" ["[]"]
              , testSameType False "[Integer|Text]" "[Integer]|[Text]" ["[]"]
              , testQuery "let a : Integer|Text; a = 3; b : [Integer]|[Text]; b = [a] in b" $ LRSuccess "[3]"
              ]
        , testTree
              "recursive"
              [ testQuery "let x : rec a. [a]; x = [] in x" $ LRSuccess "[]"
              , let
                    atree = ["[]", "[[]]", "[[[[]]]]", "[[], [[]]]"]
                    in testTree
                           "equivalence"
                           [ testSameType True "Integer" "Integer" ["0"]
                           , testSameType True "Integer" "rec a. Integer" ["0"]
                           , testSameType True "[Integer]" "[rec a. Integer]" ["[0]"]
                           , testSameType True "rec a. [a]" "rec a. [a]" atree
                           , testSameType True "rec a. [a]" "rec a. [[a]]" atree
                           , ignoreTestBecause "ISSUE #61" $
                             testSameType
                                 False
                                 "rec a. (Maybe a | [a])"
                                 "(rec a. Maybe a) | (rec b. [b])"
                                 ["[]", "Nothing", "Just []", "[[]]"]
                           , ignoreTestBecause "ISSUE #61" $
                             testSameType
                                 False
                                 "rec a. (Maybe a | [a])"
                                 "(rec a. Maybe a) | (rec a. [a])"
                                 ["[]", "Nothing", "Just []", "[[]]"]
                           , testSubtype True "rec a. [a]" "Entity" []
                           , testSubtype True "[rec a. [a]]" "Entity" []
                           , testSubtype True "rec a. [a]" "[Entity]" ["[]"]
                           , testSubtype True "[rec a. [a]]" "[Entity]" ["[]"]
                           , testSameType False "None" "None" []
                           , testSameType False "rec a. a" "None" []
                           , testSameType False "[rec a. a]" "[None]" ["[]"]
                           , testSameType True "rec a. Integer" "Integer" ["0"]
                           , testSameType True "[rec a. Integer]" "[Integer]" ["[0]"]
                           , testTree
                                 "unroll"
                                 [ testSameType True "rec a. [a]" "[rec a. [a]]" atree
                                 , testSameType False "rec a. ([a]|Integer)" "[rec a. ([a]|Integer)]|Integer" ["[]"]
                                 , testSameType False "rec a. ([a]|Integer)" "[rec a. ([a]|Integer)]|Integer" ["2"]
                                 , testSameType False "rec a. [a|Integer]" "[rec a. [a|Integer]|Integer]" ["[]"]
                                 , testSameType False "rec a. [a|Integer]" "[rec a. [a|Integer]|Integer]" ["[3]"]
                                 ]
                           ]
              , testTree
                    "subsume"
                    [ testQuery "let rval: rec a. Maybe a; rval = rval in ()" $ LRSuccess "unit"
                    , testQuery "let rval: rec a. Maybe a; rval = Just rval in ()" $ LRSuccess "unit"
                    , testQuery
                          "let rcount: (rec a. Maybe a) -> Integer; rcount x = case x of Nothing -> 0; Just y -> 1 + rcount y end in ()" $
                      LRSuccess "unit"
                    , testQuery
                          "let rcount: (rec a. Maybe a) -> Integer; rcount x = case x of Nothing -> 0; Just y -> 1 + rcount y end in rcount Nothing" $
                      LRSuccess "0"
                    , testQuery
                          "let rcount: (rec a. Maybe a) -> Integer; rcount x = case x of Nothing -> 0; Just y -> 1 + rcount1 y end; rcount1 x = case x of Nothing -> 0; Just y -> 1 + rcount y end in rcount $ Just Nothing" $
                      LRSuccess "1"
                    ]
              , testTree
                    "case"
                    [ testQuery "let rcount x = case x of Nothing -> 0; Just y -> 1 + rcount y end in rcount Nothing" $
                      LRSuccess "0"
                    , testQuery
                          "let rcount x = case x of Nothing -> 0; Just y -> if True then 1 else 1 + rcount y end in rcount $ Just Nothing" $
                      LRSuccess "1"
                    , testQuery
                          "let rcount x = case x of Nothing -> 0; Just Nothing -> 1; Just (Just y) -> if True then 2 else 2 + rcount y end in rcount $ Just Nothing" $
                      LRSuccess "1"
                    , testQuery
                          "let rcount x = case x of Nothing -> 0; Just y -> case y of Nothing -> 1; Just z -> if True then 2 else 1 + rcount y end end in rcount $ Just Nothing" $
                      LRSuccess "1"
                    , testQuery
                          "let rcount x = case x of Nothing -> 0; Just y -> 1 + rcount y end in rcount $ Just Nothing" $
                      LRSuccess "1"
                    , testQuery
                          "let rcount x = case x of Nothing -> 0; Just y -> if True then 1 else 1 + rcount y end in rcount $ Just $ Just Nothing" $
                      LRSuccess "1"
                    , testQuery
                          "let rcount x = case x of Nothing -> 0; Just Nothing -> 1; Just (Just y) -> if True then 2 else 2 + rcount y end in rcount $ Just $ Just Nothing" $
                      LRSuccess "2"
                    , testQuery
                          "let rcount x = case x of Nothing -> 0; Just y -> case y of Nothing -> 1; Just z -> if True then 2 else 1 + rcount y end end in rcount $ Just $ Just Nothing" $
                      LRSuccess "2"
                    , testQuery
                          "let rcount x = case x of Nothing -> 0; Just y -> case y of Nothing -> 1; Just z -> case z of Nothing -> 2; Just p -> if True then 3 else 1 + rcount y end end end in rcount $ Just $ Just Nothing" $
                      LRSuccess "2"
                    , testQuery
                          "let rcount x = case x of Nothing -> 0; Just y -> case y of Nothing -> 1; Just z -> case z of Nothing -> 2; Just p -> 1 + rcount y end end end in rcount $ Just $ Just Nothing" $
                      LRSuccess "2"
                    , testQuery
                          "let rcount x = case x of Nothing -> 0; Just y -> case y of Nothing -> 1; Just z -> case z of Nothing -> 2; Just p -> 1 + rcount y end end end; rcount1 x = rcount x in rcount1 $ Just $ Just Nothing" $
                      LRSuccess "2"
                    , testQuery
                          "let rcount1 y = case y of Nothing -> 0; Just z -> 1 + rcount z end; rcount x = case x of Nothing -> 0; Just y -> 1 + rcount1 y end in rcount $ Just $ Just $ Just $ Just Nothing" $
                      LRSuccess "4"
                    , testQuery
                          "let rcount1 y = case y of Nothing -> 0; Just z -> 1 + rcount z end; rcount x = case x of Nothing -> 0; Just y -> 1 + rcount1 y end in rcount $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just Nothing" $
                      LRSuccess "12"
                    , testQuery
                          "let rcount1 y = case y of Nothing -> 0; Just z -> 1 + rcount z end; rcount : (rec a. Maybe a) -> Integer; rcount x = case x of Nothing -> 0; Just y -> 1 + rcount1 y end in rcount $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just Nothing" $
                      LRSuccess "12"
                    , testQuery
                          "let rcount : (rec a. Maybe a) -> Integer; rcount x = case x of Nothing -> 0; Just Nothing -> 1; Just (Just y) -> if True then 2 else 2 + rcount y end; rval : rec a. Maybe a; rval = Just $ Just Nothing in rcount rval" $
                      LRSuccess "2"
                    , testQuery
                          "let rcount : (rec a. Maybe a) -> Integer; rcount x = case x of Nothing -> 0; Just Nothing -> 1; Just (Just Nothing) -> 2; Just (Just (Just Nothing)) -> 3; Just (Just (Just (Just Nothing))) -> 4; Just (Just (Just (Just (Just _)))) -> 5 end; rval : rec a. Maybe a; rval = Just $ Just $ Just $ Just Nothing in rcount rval" $
                      LRSuccess "4"
                    , testQuery
                          "let rcount : (rec a. Maybe a) -> Integer; rcount x = case x of Nothing -> 0; Just Nothing -> 1; Just (Just Nothing) -> 2; Just (Just (Just Nothing)) -> 3; Just (Just (Just (Just Nothing))) -> 4; Just (Just (Just (Just (Just _)))) -> 5 end; rval : rec a. Maybe a; rval = Just rval in rcount rval" $
                      LRSuccess "5"
                    , testQuery
                          "let rcount : (rec a. Maybe a) -> Integer; rcount x = case x of Nothing -> 0; Just y -> 1 + rcount y end; rval : rec a. Maybe a; rval = Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just Nothing in rcount rval" $
                      LRSuccess "10"
                    , testQuery
                          "let rcount : (rec a. Maybe a) -> Integer; rcount x = case x of Nothing -> 0; Just y -> 1 + rcount y end; rval = Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just $ Just Nothing in rcount rval" $
                      LRSuccess "10"
                    , testQuery
                          "let fix: (a -> a) -> a; fix f = let x = f x in x; rc: (a -> Integer) -> Maybe a -> Integer; rc r x = case x of Nothing -> 0; Just y -> 1 + r y end in fix rc $ Just $ Just $ Just $ Just $ Just Nothing" $
                      LRSuccess "5"
                    , testQuery
                          "let rcount x = case x of Nothing -> 0; Just y -> 1 + rcount y end in rcount $ Just $ Just Nothing" $
                      LRSuccess "2"
                    , testQuery
                          "let rcount x = case x of Nothing -> 0; Just y -> 1 + rcount y end in rcount $ Just $ Just $ Just Nothing" $
                      LRSuccess "3"
                    , testQuery
                          "let rcount x = case x of Nothing -> 0; Just y -> 1 + rcount y end in rcount $ Just $ Just $ Just $ Just Nothing" $
                      LRSuccess "4"
                    , testQuery
                          "let rcount x = case x of Nothing -> 0; Just y -> 1 + rcount y end in rcount $ Just $ Just $ Just $ Just $ Just Nothing" $
                      LRSuccess "5"
                    , testQuery
                          "let rcount x = case x of Nothing -> 0; Just y -> 1 + rcount y end; rval = Just $ Just Nothing in rcount rval" $
                      LRSuccess "2"
                    , testQuery
                          "let rcount x = case x of Nothing -> 0; Just y -> 1 + rcount y end; rval : Maybe (Maybe (Maybe None)) ; rval = Just $ Just Nothing in rcount rval" $
                      LRSuccess "2"
                    , testQuery
                          "let rcount x = case x of Nothing -> 0; Just y -> 1 + rcount y end; rval : Maybe (Maybe (Maybe (Maybe None))) ; rval = Just $ Just Nothing in rcount rval" $
                      LRSuccess "2"
                    , testQuery "Just $ Just $ Just Nothing" $ LRSuccess "Just Just Just Nothing"
                    , testQuery
                          "let rcount x = case x of Nothing -> 0; Just y -> 1 + rcount y end; rval = Just $ Just $ Just Nothing in rcount rval" $
                      LRSuccess "3"
                    , testQuery
                          "let rcount x = case x of Nothing -> 0; Just y -> 1 + rcount y end; rval : Maybe (Maybe (Maybe (Maybe None))) ; rval = Just $ Just $ Just Nothing in rcount rval" $
                      LRSuccess "3"
                    , testQuery
                          "let rcount x = case x of Nothing -> 0; Just y -> 1 + rcount y end; rval : Maybe (Maybe (Maybe (Maybe (Maybe None)))) ; rval = Just $ Just $ Just Nothing in rcount rval" $
                      LRSuccess "3"
                    , testQuery
                          "let rcount x = case x of Nothing -> 0; Just Nothing -> 1; Just (Just Nothing) -> 2; Just (Just (Just Nothing)) -> 3; _ -> 4 end in rcount $ Just $ Just $ Just Nothing" $
                      LRSuccess "3"
                    , testQuery
                          "let rcount x = case x of Nothing -> 0; Just y -> 1 + r1count y end; r1count x = case x of Nothing -> 0; Just y -> 1 + r1count y end in rcount $ Just $ Just $ Just Nothing" $
                      LRSuccess "3"
                    , testQuery
                          "case Just $ Just $ Just Nothing of Nothing -> 0; Just Nothing -> 1; Just (Just Nothing) -> 2; Just (Just (Just Nothing)) -> 3; _ -> 4 end" $
                      LRSuccess "3"
                    , testQuery
                          "let rcount x = case x of Nothing -> 0; Just Nothing -> 1; Just (Just Nothing) -> 2; Just (Just (Just Nothing)) -> 3; _ -> 4 end in rcount $ Just $ Just $ Just Nothing" $
                      LRSuccess "3"
                    , testQuery
                          "let rcount : (rec a . Maybe a) -> Integer; rcount x = case x of Nothing -> 0; Just Nothing -> 1; Just (Just Nothing) -> 2; Just (Just (Just Nothing)) -> 3; _ -> 4 end in rcount $ Just $ Just $ Just Nothing" $
                      LRSuccess "3"
                    , testQuery
                          "let rcount : (rec a . Maybe a) -> Integer; rcount x = case x of Nothing -> 0; Just Nothing -> 1; Just (Just Nothing) -> 2; Just (Just (Just Nothing)) -> 3; Just (Just (Just (Just y))) -> 4 + rcount y end in rcount $ Just $ Just $ Just Nothing" $
                      LRSuccess "3"
                    , testQuery
                          "let rcount : (rec a . Maybe a) -> Integer; rcount x = case x of Nothing -> 0; Just Nothing -> 1; Just (Just y) -> 2 + rcount y end in rcount $ Just $ Just $ Just Nothing" $
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
                                "; y = case x of (z: " <> subtype <> ") -> z; _ -> " <> altval <> "; end in y")
                               result
                         , testQuery
                               ("let x: " <>
                                supertype <>
                                "; x=" <>
                                val <>
                                "; y: " <>
                                subtype <>
                                "; y = case check @(" <>
                                subtype <> ") x of Just z -> z; Nothing -> " <> altval <> "; end in y")
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
        ]

testShim :: Text -> String -> String -> TestTree
testShim query expectedType expectedShim =
    testTree (unpack query) $ do
        result <- withNullPinaforeContext $ runInterpretResult $ runPinaforeSourceScoped "<input>" $ parseValue query
        case result of
            FailureResult e -> assertFailure $ "expected success, found failure: " ++ show e
            SuccessResult (MkAnyValue (MkPosShimWit t shim) _) -> do
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
        , expectFailBecause "ISSUE #63" $ testShim "\\x -> x" "a -> a" "(join1 (co (contra id (meet1 id)) (join1 id)))"
        , expectFailBecause "ISSUE #63" $ testShim "(\\x -> x) 3" "Integer" "(join1 id)"
        , testShim "\\x -> 4" "Any -> Integer" "(join1 (co (contra id termf) (join1 id)))"
        , testShim "(\\x -> 4) 3" "Integer" "(join1 id)"
        , expectFailBecause "ISSUE #63" $
          testShim
              "let rcount x = case x of Nothing -> 0; Just y -> 1 + rcount y end in rcount"
              "(rec c. Maybe c) -> Integer"
              "(join1 id)"
        , expectFailBecause "ISSUE #63" $
          testShim
              "let rcount : (rec a. Maybe a) -> Integer; rcount x = case x of Nothing -> 0; Just y -> 1 + rcount y end in rcount"
              "(rec a. Maybe a) -> Integer"
              "(join1 id)"
        ]

testLanguage :: TestTree
testLanguage = localOption (mkTimeout 2000000) $ testTree "language" [testInfix, testNumbers, testShims, testQueries]
