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
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit

testOp :: Name -> TestTree
testOp n =
    testCase (show $ unpack n) $ do
        case unpack n of
            '(':_ -> assertFailure "parenthesis"
            _ -> return ()
        case operatorFixity n of
            MkFixity AssocLeft 10 -> assertFailure "unassigned fixity"
            _ -> return ()

testInfix :: TestTree
testInfix = let
    names = filter nameIsInfix $ fmap docName $ toList predefinedDoc
    in testGroup "infix" $ fmap testOp names

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
testNumbers = testGroup "numbers" [testNumbersArithemetic, testNumbersShowRead]

testQueryValues :: TestTree
testQueryValues = testGroup "query values" []

testQuery :: Text -> Maybe String -> TestTree
testQuery query expected =
    testCase (show $ unpack query) $
    case (expected, withNullPinaforeContext $ runPinaforeSourceScoped "<input>" $ parseValue query) of
        (Nothing, FailureResult _) -> return ()
        (Nothing, SuccessResult v) -> assertFailure $ "expected failure, found success: " ++ showPinaforeRef v
        (Just _, FailureResult e) -> assertFailure $ "expected success, found failure: " ++ show e
        (Just s, SuccessResult v) -> assertEqual "result" s (showPinaforeRef v)

testSubsumeSubtype :: Text -> Text -> [Text] -> [TestTree]
testSubsumeSubtype t1 t2 vs =
    [testQuery ("let x : " <> t1 <> "; x = x; y : " <> t2 <> "; y = x in ()") $ Just "unit"] <>
    fmap (\v -> testQuery ("let x : " <> t1 <> "; x = " <> v <> "; y : " <> t2 <> "; y = x in y") $ Just $ unpack v) vs

testFunctionSubtype :: Text -> Text -> [Text] -> [TestTree]
testFunctionSubtype t1 t2 vs =
    [testQuery ("let f : (" <> t1 <> ") -> (" <> t2 <> "); f x = x in f") $ Just "<?>"] <>
    fmap (\v -> testQuery ("let f : (" <> t1 <> ") -> (" <> t2 <> "); f x = x in f " <> v) $ Just $ unpack v) vs

testSubtype1 :: Bool -> Text -> Text -> [Text] -> [TestTree]
testSubtype1 b t1 t2 vs =
    testSubsumeSubtype t1 t2 vs <>
    if b
        then testFunctionSubtype t1 t2 vs
        else []

testSubtype :: Bool -> Text -> Text -> [Text] -> TestTree
testSubtype b t1 t2 vs = testGroup (unpack $ t1 <> " <= " <> t2) $ testSubtype1 b t1 t2 vs

testSameType :: Bool -> Text -> Text -> [Text] -> TestTree
testSameType b t1 t2 vs =
    testGroup (unpack $ t1 <> " = " <> t2) $ (testSubtype1 b t1 t2 vs) <> (testSubtype1 b t2 t1 vs)

testQueries :: TestTree
testQueries =
    testGroup
        "queries"
        [ testGroup "trivial" [testQuery "" $ Nothing, testQuery "x" $ Nothing]
        , testGroup
              "comments"
              [ testQuery "# comment\n1" $ Just "1"
              , testQuery "1# comment\n" $ Just "1"
              , testQuery "1 # comment\n" $ Just "1"
              , testQuery "{# comment #} 1" $ Just "1"
              , testQuery "{# comment #}\n1" $ Just "1"
              , testQuery "{# comment\ncomment #}\n1" $ Just "1"
              , testQuery "{# comment\ncomment\n#}\n1" $ Just "1"
              , testQuery "{# A {# B #} C #} 1" $ Just "1"
              , testQuery "{#\nA\n{#\nB\n#}\nC\n#}\n1" $ Just "1"
              ]
        , testGroup
              "constants"
              [ testGroup
                    "numeric"
                    [ testQuery "0.5" $ Just "1/2"
                    , testQuery "0._3" $ Just "1/3"
                    , testQuery "-0._3" $ Just "-1/3"
                    , testQuery "-0.0_3" $ Just "-1/30"
                    , testQuery "0.3_571428" $ Just "5/14"
                    , testQuery "0." $ Just "0"
                    , testQuery "0.0" $ Just "0"
                    , testQuery "0._" $ Just "0"
                    , testQuery "0._0" $ Just "0"
                    , testQuery "0.0_" $ Just "0"
                    , testQuery "0.0_0" $ Just "0"
                    , testQuery "3" $ Just "3"
                    , testQuery "3.2_4" $ Just "146/45"
                    , testQuery "~1" $ Just "~1.0"
                    , testQuery "~-2.4" $ Just "~-2.4"
                    , testQuery "NaN" $ Just "NaN"
                    , testQuery "~Infinity" $ Just "~Infinity"
                    , testQuery "~-Infinity" $ Just "~-Infinity"
                    ]
              , testQuery "\"\"" $ Just ""
              , testQuery "\"Hello \"" $ Just "Hello "
              , testQuery "True" $ Just "True"
              , testQuery "False" $ Just "False"
              , testQuery "\"1\"" $ Just "1"
              , testQuery "uiTable" $ Just "<?>"
              , testQuery "entity @Entity !\"example\"" $ Just "<?>"
              , testQuery "entityAnchor $ entity @Entity !\"example\"" $
                Just "!1AF8A5FD-24AAAF3E-3668C588-6C74D36A-70ED9618-CC874895-E4569C9F-FCD42CD3"
              ]
        , testGroup
              "list construction"
              [ testQuery "[]" $ Just $ show @[Text] []
              , testQuery "[1]" $ Just $ "[1]"
              , testQuery "[1,2,3]" $ Just "[1, 2, 3]"
              ]
        , testGroup
              "functions"
              [ testQuery "\\x -> x" $ Just "<?>"
              , testQuery "\\x -> 1" $ Just "<?>"
              , testQuery "\\x y -> y" $ Just "<?>"
              , testQuery "\\x y z -> [x,y,z]" $ Just "<?>"
              ]
        , testGroup
              "predefined"
              [ testQuery "abs" $ Just "<?>"
              , testQuery "fst" $ Just "<?>"
              , testQuery "(+)" $ Just "<?>"
              , testQuery "\\a b -> a + b" $ Just "<?>"
              , testQuery "(==)" $ Just "<?>"
              , testQuery "\\a b -> a == b" $ Just "<?>"
              ]
        , testGroup
              "let-binding"
              [ testQuery "let in 27" $ Just "27"
              , testQuery "let a=\"5\" in a" $ Just "5"
              , testQuery "let a=5 in a" $ Just "5"
              , testQuery "let a=1 in let a=2 in a" $ Just "2"
              , testQuery "let a=1;b=2 in a" $ Just "1"
              , testQuery "let a=1;b=2 in b" $ Just "2"
              , testQuery "let a=1;b=2 in b" $ Just "2"
              , testQuery "let a=1;b=\"2\" in b" $ Just "2"
              , testQuery "let a=1 ;b=\"2\" in b" $ Just "2"
              , testQuery "let a= 1 ;b=\"2\" in b" $ Just "2"
              , testQuery "let a=7;b=a in a" $ Just "7"
              , testQuery "let a=7;b=a in b" $ Just "7"
              , testQuery "let a=2 in let b=a in b" $ Just "2"
              ]
        , testGroup
              "partial keywords"
              [ testQuery "let i=1 in i" $ Just "1"
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
              ]
        , testGroup
              "recursive let-binding"
              [ testQuery "let a=1 in a" $ Just "1"
              , testQuery "let a=1 in let a=2 in a" $ Just "2"
              , testQuery "let a=1;a=2 in a" $ Nothing
              , testQuery "let a=1;b=a in b" $ Just "1"
              , testQuery "let b=a;a=1 in b" $ Just "1"
              , testQuery "let a x = x in a 1" $ Just "1"
              , testQuery "let a x = x; b = a in b" $ Just "<?>"
              , testQuery "let a = \\x -> x in let b = a 1 in b" $ Just "1"
              , testQuery "let a x = x; b = a 1 in b" $ Just "1"
              , testQuery "let a x = b; b = b in a" $ Just "<?>"
              , testQuery "let a x = 1; b = b in a b" $ Just "1"
              , testQuery "let a x = 1; b = a b in b" $ Just "1"
              , testQuery "let a x = 1 in let b = a b in b" $ Just "1"
              , testQuery "let b = (\\x -> 1) b in b" $ Just "1"
              , testQuery "let b = a b; a x = 1 in b" $ Just "1"
              , testQuery "let a x = 1; b = a c; c=b in b" $ Just "1"
              ]
        , testGroup
              "recursive let-binding polymorphism"
              [ testQuery "let i = \\x -> x in (1 + i 1, i False)" $ Just "(2, False)"
              , testQuery "let i = \\x -> x; r = (1 + i 1, i False) in r" $ Just "(2, False)"
              , testQuery "let r = (1 + i 1, i False); i = \\x -> x in r" $ Just "(2, False)"
              ]
        , testGroup
              "duplicate bindings"
              [ testQuery "let a=1;a=1 in a" $ Nothing
              , testQuery "let a=1;a=2 in a" $ Nothing
              , testQuery "let a=1;b=0;a=2 in a" $ Nothing
              ]
        , testGroup
              "lexical scoping"
              [ testQuery "let a=1 in let b=a in let a=3 in a" $ Just "3"
              , testQuery "let a=1;b=a;a=3 in a" $ Nothing
              , testQuery "let a=1 in let b=a in let a=3 in b" $ Just "1"
              , testQuery "let a=1;b=a;a=3 in b" $ Nothing
              ]
        , testGroup
              "operators"
              [ testQuery "0 == 1" $ Just "False"
              , testQuery "1 == 1" $ Just "True"
              , testQuery "0 /= 1" $ Just "True"
              , testQuery "1 /= 1" $ Just "False"
              , testQuery "0 <= 1" $ Just "True"
              , testQuery "1 <= 1" $ Just "True"
              , testQuery "2 <= 1" $ Just "False"
              , testQuery "0 < 1" $ Just "True"
              , testQuery "1 < 1" $ Just "False"
              , testQuery "2 < 1" $ Just "False"
              , testQuery "0 >= 1" $ Just "False"
              , testQuery "1 >= 1" $ Just "True"
              , testQuery "2 >= 1" $ Just "True"
              , testQuery "0 >= ~1" $ Just "False"
              , testQuery "1 >= ~1" $ Just "True"
              , testQuery "2 >= ~1" $ Just "True"
              , testQuery "0 > 1" $ Just "False"
              , testQuery "1 > 1" $ Just "False"
              , testQuery "2 > 1" $ Just "True"
              , testQuery "1 == ~1" $ Just "False"
              , testQuery "0 ~== 1" $ Just "False"
              , testQuery "1 ~== 1" $ Just "True"
              , testQuery "1 ~== ~1" $ Just "True"
              , testQuery "0 ~== ~1" $ Just "False"
              , testQuery "0 ~/= 1" $ Just "True"
              , testQuery "1 ~/= 1" $ Just "False"
              , testQuery "1 ~/= ~1" $ Just "False"
              , testQuery "0 ~/= ~1" $ Just "True"
              , testQuery "7+8" $ Just "15"
              , testQuery "7 +8" $ Just "15"
              , testQuery "7+ 8" $ Just "15"
              , testQuery "7 + 8" $ Just "15"
              , testQuery "\"abc\"<>\"def\"" $ Just "abcdef"
              , testQuery "\"abc\" <>\"def\"" $ Just "abcdef"
              , testQuery "\"abc\"<> \"def\"" $ Just "abcdef"
              , testQuery "\"abc\" <> \"def\"" $ Just "abcdef"
              , testQuery "let f x = x + 2 in f -1" $ Just "1"
              , testQuery "let f = 2 in f - 1" $ Just "1"
              ]
        , testGroup
              "boolean"
              [ testQuery "True && True" $ Just "True"
              , testQuery "True && False" $ Just "False"
              , testQuery "False && True" $ Just "False"
              , testQuery "False && False" $ Just "False"
              , testQuery "True || True" $ Just "True"
              , testQuery "True || False" $ Just "True"
              , testQuery "False || True" $ Just "True"
              , testQuery "False || False" $ Just "False"
              , testQuery "not True" $ Just "False"
              , testQuery "not False" $ Just "True"
              ]
        , testGroup
              "text"
              [ testQuery "\"pqrs\"" $ Just "pqrs"
              , testQuery "textLength \"abd\"" $ Just "3"
              , testQuery "textSection 4 3 \"ABCDEFGHIJKLMN\"" $ Just "EFG"
              ]
        , testGroup
              "operator precedence"
              [ testQuery "1 + 2 * 3" $ Just "7"
              , testQuery "3 * 2 + 1" $ Just "7"
              , testQuery "2 * 2 * 2" $ Just "8"
              , testQuery "12 / 2 / 2" $ Just "3"
              , testQuery "12 / 2 / 2" $ Just "3"
              , testQuery "0 == 0 == 0" $ Nothing
              ]
        , testGroup
              "if/then/else"
              [ testQuery "if True then 3 else 4" $ Just "3"
              , testQuery "if False then 3 else 4" $ Just "4"
              , testQuery "if False then if True then 1 else 2 else if True then 3 else 4" $ Just "3"
              ]
        , testGroup "pairs" [testQuery "fst (7,9)" $ Just "7", testQuery "snd (7,9)" $ Just "9"]
        , testGroup
              "either"
              [ testQuery "fromEither (\\a -> (\"Left\",a)) (\\a -> (\"Right\",a)) $ Left \"x\"" $ Just "(Left, x)"
              , testQuery "fromEither (\\a -> (\"Left\",a)) (\\a -> (\"Right\",a)) $ Right \"x\"" $ Just "(Right, x)"
              ]
        , testGroup
              "type signature"
              [ testQuery "let i : a -> a; i x = x in i 3" $ Just "3"
              , testQuery "let i : Number -> Number; i x = x in i 3" $ Just "3"
              , testQuery "let i : Text -> Text; i x = x in i 3" $ Nothing
              , testQuery "let i : a -> a; i x = x in i \"t\"" $ Just "t"
              , testQuery "let i : Number -> Number; i x = x in i \"t\"" $ Nothing
              , testQuery "let i : Text -> Text; i x = x in i \"t\"" $ Just "t"
              , testQuery "let i : a -> a; i x = x in 0" $ Just "0"
              , testQuery "let i : a -> Number; i x = x in 0" $ Nothing
              , testQuery "let i : Number -> a; i x = x in 0" $ Nothing
              , testQuery "let i : Number -> Number; i x = x in 0" $ Just "0"
              , testQuery "let i : Either Number Boolean; i = Left 5 in i" $ Just "Left 5"
              , testQuery "let i : Either Number Boolean; i = Right False in i" $ Just "Right False"
              , testQuery "let i : Maybe Number; i = Just 5 in i" $ Just "Just 5"
              , testQuery "let i : Maybe Number; i = Nothing in i" $ Just "Nothing"
              ]
        , testGroup
              "patterns"
              [ testQuery "(\\a -> 5) 2" $ Just "5"
              , testQuery "(\\a -> a) 2" $ Just "2"
              , testQuery "(\\_ -> 5) 2" $ Just "5"
              , testQuery "(\\a@b -> (a,b)) 2" $ Just "(2, 2)"
              , testQuery "(\\(a,b) -> a + b) (5,6)" $ Just "11"
              ]
        , testGroup
              "case"
              [ testGroup
                    "basic"
                    [ testQuery "case 2 of a -> 5 end" $ Just "5"
                    , testQuery "case 2 of a -> 5; a -> 3 end" $ Just "5"
                    , testQuery "case 2 of a -> 5; a -> 3; end" $ Just "5"
                    , testQuery "case 2 of a -> a end" $ Just "2"
                    , testQuery "case 2 of _ -> 5 end" $ Just "5"
                    , testQuery "case 2 of _ -> 5; _ -> 3 end" $ Just "5"
                    , testQuery "case 2 of a@b -> (a,b) end" $ Just "(2, 2)"
                    ]
              , testGroup
                    "Boolean"
                    [ testQuery "case True of True -> 5; False -> 7 end" $ Just "5"
                    , testQuery "case False of True -> 5; False -> 7 end" $ Just "7"
                    , testQuery "case True of False -> 7; True -> 5 end" $ Just "5"
                    , testQuery "case False of False -> 7; True -> 5 end" $ Just "7"
                    ]
              , testGroup
                    "Number"
                    [ testQuery "case 37 of 37 -> True; _ -> False end" $ Just "True"
                    , testQuery "case 38 of 37 -> True; _ -> False end" $ Just "False"
                    , testQuery "case -24.3 of 37 -> 1; -24.3 -> 2; _ -> 3 end" $ Just "2"
                    ]
              , testGroup
                    "String"
                    [ testQuery "case \"Hello\" of \"Hello\" -> True; _ -> False end" $ Just "True"
                    , testQuery "case \"thing\" of \"Hello\" -> True; _ -> False end" $ Just "False"
                    , testQuery "case \"thing\" of \"Hello\" -> 1; \"thing\" -> 2; _ -> 3 end" $ Just "2"
                    ]
              , testGroup
                    "Either"
                    [ testQuery "case Left 3 of Left a -> a; Right _ -> 1 end" $ Just "3"
                    , testQuery "case Right 4 of Left a -> a + 1; Right a -> a end" $ Just "4"
                    , testQuery "case Right 7 of Right 4 -> True; _ -> False end" $ Just "False"
                    , testQuery "case Right 7 of Right 4 -> 1; Right 7 -> 2; Left _ -> 3; _ -> 4 end" $ Just "2"
                    ]
              , testGroup "Unit" [testQuery "case () of () -> 4 end" $ Just "4"]
              , testGroup "Pair" [testQuery "case (2,True) of (2,a) -> a end" $ Just "True"]
              , testGroup
                    "Maybe"
                    [ testQuery "case Just 3 of Just a -> a + 1; Nothing -> 7 end" $ Just "4"
                    , testQuery "case Nothing of Just a -> a + 1; Nothing -> 7 end" $ Just "7"
                    ]
              , testGroup
                    "List"
                    [ testQuery "case [] of [] -> True; _ -> False end" $ Just "True"
                    , testQuery "case [] of _::_ -> True; _ -> False end" $ Just "False"
                    , testQuery "case [1,2] of [] -> True; _ -> False end" $ Just "False"
                    , testQuery "case [3,4] of _::_ -> True; _ -> False end" $ Just "True"
                    , testQuery "case [3] of a::b -> (a,b) end" $ Just "(3, [])"
                    , testQuery "case [3,4] of a::b -> (a,b) end" $ Just "(3, [4])"
                    , testQuery "case [3,4,5] of a::b -> (a,b) end" $ Just "(3, [4, 5])"
                    , testQuery "case [3] of [a,b] -> 1; _ -> 2 end" $ Just "2"
                    , testQuery "case [3,4] of [a,b] -> 1; _ -> 2 end" $ Just "1"
                    , testQuery "case [3,4,5] of [a,b] -> 1; _ -> 2 end" $ Just "2"
                    , testQuery "case [3,4] of [a,b] -> (a,b) end" $ Just "(3, 4)"
                    ]
              ]
        , testGroup
              "recursive"
              [ testQuery "let x : rec a. [a]; x = [] in x" $ Just "[]"
              , let
                    atree = ["[]", "[[]]", "[[[[]]]]", "[[], [[]]]"]
                    in testGroup
                           "equivalence"
                           [ testSameType True "Integer" "Integer" ["0"]
                           , testSameType True "Integer" "rec a. Integer" ["0"]
                           , testSameType True "[Integer]" "[rec a. Integer]" ["[0]"]
                           , testSameType True "rec a. [a]" "rec a. [a]" atree
                           , testSameType True "rec a. [a]" "rec a. [[a]]" atree
                           , testSameType
                                 False
                                 "rec a. Maybe a | [a]"
                                 "(rec a. Maybe a) | (rec b. [b])"
                                 ["[]", "Nothing", "Just []", "[[]]"]
                           , testSameType
                                 False
                                 "rec a. Maybe a | [a]"
                                 "(rec a. Maybe a) | (rec a. [a])"
                                 ["[]", "Nothing", "Just []", "[[]]"]
                           , testSubtype True "rec a. [a]" "Entity" []
                           , testSubtype True "[rec a. [a]]" "Entity" []
                           , testSubtype True "rec a. [a]" "[Entity]" ["[]"]
                           , testSubtype True "[rec a. [a]]" "[Entity]" ["[]"]
                           , testQuery "let x : None; x = x in ()" $ Just "unit"
                           , testSameType False "None" "None" []
                           , testSameType False "rec a. a" "None" []
                           , testSameType False "[rec a. a]" "[None]" ["[]"]
                           , testSameType True "rec a. Integer" "Integer" ["0"]
                           , testSameType True "[rec a. Integer]" "[Integer]" ["[0]"]
                           , testGroup
                                 "unroll"
                                 [ testSameType True "rec a. [a]" "[rec a. [a]]" atree
                                 , testSameType False "rec a. ([a]|Integer)" "[rec a. ([a]|Integer)]|Integer" ["[]"]
                                 , testSameType False "rec a. ([a]|Integer)" "[rec a. ([a]|Integer)]|Integer" ["2"]
                                 , testSameType False "rec a. [a|Integer]" "[rec a. [a|Integer]|Integer]" ["[]"]
                                 , testSameType False "rec a. [a|Integer]" "[rec a. [a|Integer]|Integer]" ["[3]"]
                                 ]
                           ]
              , testGroup
                    "case"
                    [ testQuery "let rcount x = case x of Nothing -> 0; Just y -> 1 + rcount y end in rcount Nothing" $
                      Just "0"
                    , testQuery
                          "let rcount x = case x of Nothing -> 0; Just y -> 1 + rcount y end in rcount $ Just Nothing" $
                      Just "1"
                    , testQuery
                          "let rcount x = case x of Nothing -> 0; Just y -> 1 + rcount y end in rcount $ Just $ Just Nothing" $
                      Just "2"
                    , testQuery
                          "let rcount x = case x of Nothing -> 0; Just y -> 1 + rcount y end in rcount $ Just $ Just $ Just Nothing" $
                      Just "3"
                    , testQuery
                          "let rcount x = case x of Nothing -> 0; Just y -> 1 + rcount y end in rcount $ Just $ Just $ Just $ Just Nothing" $
                      Just "4"
                    , testQuery
                          "let rcount x = case x of Nothing -> 0; Just y -> 1 + rcount y end in rcount $ Just $ Just $ Just $ Just $ Just Nothing" $
                      Just "5"
                    , testQuery
                          "let rcount x = case x of Nothing -> 0; Just y -> 1 + rcount y end; rval = Just $ Just Nothing in rcount rval" $
                      Just "2"
                    , testQuery
                          "let rcount x = case x of Nothing -> 0; Just y -> 1 + rcount y end; rval : Maybe (Maybe (Maybe None)) ; rval = Just $ Just Nothing in rcount rval" $
                      Just "2"
                    , testQuery
                          "let rcount x = case x of Nothing -> 0; Just y -> 1 + rcount y end; rval : Maybe (Maybe (Maybe (Maybe None))) ; rval = Just $ Just Nothing in rcount rval" $
                      Just "2"
                    , testQuery "Just $ Just $ Just Nothing" $ Just "Just Just Just Nothing"
                    , testQuery
                          "let rcount x = case x of Nothing -> 0; Just y -> 1 + rcount y end; rval = Just $ Just $ Just Nothing in rcount rval" $
                      Just "3"
                    , testQuery
                          "let rcount x = case x of Nothing -> 0; Just y -> 1 + rcount y end; rval : Maybe (Maybe (Maybe (Maybe None))) ; rval = Just $ Just $ Just Nothing in rcount rval" $
                      Just "3"
                    , testQuery
                          "let rcount x = case x of Nothing -> 0; Just y -> 1 + rcount y end; rval : Maybe (Maybe (Maybe (Maybe (Maybe None)))) ; rval = Just $ Just $ Just Nothing in rcount rval" $
                      Just "3"
                    , testQuery
                          "let rcount x = case x of Nothing -> 0; Just Nothing -> 1; Just (Just Nothing) -> 2; Just (Just (Just Nothing)) -> 3; _ -> 4 end in rcount $ Just $ Just $ Just Nothing" $
                      Just "3"
                    , testQuery
                          "let rcount x = case x of Nothing -> 0; Just y -> 1 + r1count y end; r1count x = case x of Nothing -> 0; Just y -> 1 + r1count y end in rcount $ Just $ Just $ Just Nothing" $
                      Just "3"
                    , testQuery
                          "case Just $ Just $ Just Nothing of Nothing -> 0; Just Nothing -> 1; Just (Just Nothing) -> 2; Just (Just (Just Nothing)) -> 3; _ -> 4 end" $
                      Just "3"
                    , testQuery
                          "let rcount x = case x of Nothing -> 0; Just Nothing -> 1; Just (Just Nothing) -> 2; Just (Just (Just Nothing)) -> 3; _ -> 4 end in rcount $ Just $ Just $ Just Nothing" $
                      Just "3"
                    , testQuery
                          "let rcount : (rec a . Maybe a) -> Integer; rcount x = case x of Nothing -> 0; Just Nothing -> 1; Just (Just Nothing) -> 2; Just (Just (Just Nothing)) -> 3; _ -> 4 end in rcount $ Just $ Just $ Just Nothing" $
                      Just "3"
                    , testQuery
                          "let rcount : (rec a . Maybe a) -> Integer; rcount x = case x of Nothing -> 0; Just Nothing -> 1; Just (Just Nothing) -> 2; Just (Just (Just Nothing)) -> 3; Just (Just (Just (Just y))) -> 4 + rcount y end in rcount $ Just $ Just $ Just Nothing" $
                      Just "3"
                    , ignoreTest $
                      testQuery
                          "let rcount : (rec a . Maybe a) -> Integer; rcount x = case x of Nothing -> 0; Just Nothing -> 1; Just (Just y) -> 2 + rcount y end in rcount $ Just $ Just $ Just Nothing" $
                      Just "3"
                    ]
              ]
        , testGroup
              "subtype"
              [ testQuery "let i : Integer -> Number; i x = x in i 3" $ Just "3"
              , testQuery "let a : Integer; a = 3; b : Number; b = a in b" $ Just "3"
              , testQuery "let i : FiniteSetRef -a -> SetRef a; i x = x in 3" $ Just "3"
              , testQuery "let i : FiniteSetRef {-a,+Integer} -> SetRef a; i x = x in 3" $ Just "3"
              ]
        , testGroup
              "subsume"
              [ testQuery "let a : [Integer|Text]; a = [] in a" $ Just "[]"
              , testQuery "let a : [Integer|Text]; a = []; b : [Integer]|[Text]; b = a in b" $ Just "[]"
              , testQuery "let a : Integer|Text; a = 3; b : [Integer]|[Text]; b = [a] in b" $ Just "[3]"
              , testQuery "let a : [Integer]|[Text]; a = [] in a" $ Just "[]"
              , testQuery "let a : [Integer]|[Text]; a = []; b : [Integer|Text]; b = a in b" $ Just "[]"
              ]
        ]

testShim :: Text -> String -> String -> TestTree
testShim query expectedType expectedShim =
    testCase (unpack query) $
    case withNullPinaforeContext $ runPinaforeSourceScoped "<input>" $ parseValue query of
        FailureResult e -> assertFailure $ "expected success, found failure: " ++ show e
        SuccessResult (MkAnyValue (MkPosShimWit t shim) _) -> do
            assertEqual "type" expectedType $ unpack $ exprShow t
            assertEqual "shim" expectedShim $ show shim

testShims :: TestTree
testShims =
    testGroup
        "shims"
        [ testShim "3" "Integer" "(join1 id)"
        , testShim "negate" "Integer -> Integer" "(join1 (co (contra id (meet1 id)) (join1 id)))"
        , testShim "negate 3" "Integer" "(join1 id)"
        , testShim "id" "a -> a" "(join1 (co (contra id (meet1 id)) (join1 id)))"
        , expectFail $ testShim "id 3" "Integer" "(join1 id)"
        , expectFail $ testShim "\\x -> x" "a -> a" "(join1 (co (contra id (meet1 id)) (join1 id)))"
        , expectFail $ testShim "(\\x -> x) 3" "Integer" "(join1 id)"
        , testShim "\\x -> 4" "Any -> Integer" "(join1 (co (contra id termf) (join1 id)))"
        , testShim "(\\x -> 4) 3" "Integer" "(join1 id)"
        ]

testLanguage :: TestTree
testLanguage =
    localOption (mkTimeout 2000000) $
    testGroup "language" [testInfix, testNumbers, testShims, testQueryValues, testQueries]
