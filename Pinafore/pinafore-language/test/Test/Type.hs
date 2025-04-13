module Test.Type
    ( testType
    )
where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan
import Language.Expression.TypeSystem
import Shapes
import Shapes.Test

import Pinafore.Test.Internal

type TS = QTypeSystem

exprTypeTest :: String -> Maybe Text -> QTypeM QExpression -> TestTree
exprTypeTest name expected mexpr =
    testTree name
        $ runTester defaultTester
        $ do
            result <- tryExc $ testerLiftInterpreter $ qRunTypeM mexpr
            liftIO
                $ assertEqual "" expected
                $ do
                    expr <- resultToMaybe result
                    return $ showExpressionType expr

apExpr :: QExpression -> QExpression -> QTypeM QExpression
apExpr = tsApply @TS

idExpr :: QExpression
idExpr = typeFConstExpression toJMPolyShimWit $ \(v :: X) -> v

nbFuncExpr :: QExpression
nbFuncExpr = typeFConstExpression toJMPolyShimWit $ \(_ :: Number) -> False

numExpr :: QExpression
numExpr = typeFConstExpression toJMPolyShimWit $ (3 :: Number)

boolExpr :: QExpression
boolExpr = typeFConstExpression toJMPolyShimWit False

varExpr :: QExpression
varExpr = tsVar @TS $ fst $ mkLambdaVarID szero $ Just "v"

ifelseExpr :: QExpression
ifelseExpr =
    typeFConstExpression toJMPolyShimWit $ \test (tb :: A) (eb :: A) ->
        if test
            then tb
            else eb

list1Expr :: QExpression
list1Expr = typeFConstExpression toJMPolyShimWit $ \(a :: A) -> [a]

sndExpr :: QExpression
sndExpr = typeFConstExpression toJMPolyShimWit $ \(MkTopType, a :: A) -> a

twiceExpr :: QExpression
twiceExpr = typeFConstExpression toJMPolyShimWit $ \(a :: A) -> (a, a)

thingExpr :: QExpression
thingExpr =
    typeFConstExpression toJMPolyShimWit $ \(a :: A, b :: B) ->
        ( a
        , if False
            then MkJoinType $ Left a
            else MkJoinType $ Right b
        )

dotExpr :: QExpression
dotExpr = typeFConstExpression toJMPolyShimWit $ \(f :: B -> C) (g :: A -> B) -> f . g

listNumBoolFuncExpr :: QExpression
listNumBoolFuncExpr = typeFConstExpression toJMPolyShimWit $ \(_ :: [Number]) -> [True]

listBoolNumFuncExpr :: QExpression
listBoolNumFuncExpr = typeFConstExpression toJMPolyShimWit $ \(_ :: [Bool]) -> [2 :: Number]

joinExpr :: QExpression -> QExpression -> QTypeM QExpression
joinExpr exp1 exp2 = do
    je <- apExpr ifelseExpr boolExpr
    e <- apExpr je exp1
    apExpr e exp2

textTypeTest :: Text -> Text -> TestTree
textTypeTest text expected =
    testTree (unpack text) $ parseExpressionToType text $ \found -> assertEqual "" expected found

rejectionTest :: Text -> TestTree
rejectionTest text =
    testTree ("REJECT: " <> unpack text)
        $ runTester defaultTester
        $ do
            result <- tryExc $ testerLiftInterpreter $ parseTopExpression text
            liftIO
                $ case result of
                    FailureResult _ -> return ()
                    SuccessResult _ -> assertFailure "no exception"

runSimplify ::
    forall a.
    TSMappable TS a =>
    a ->
    TSOuter TS a
runSimplify = unEndoM $ simplify @TS <> finalRenameMappable @TS

simplifyTypeTest :: Text -> String -> TestTree
simplifyTypeTest text e =
    testTree (unpack text)
        $ runTester defaultTester
        $ do
            simpexpr <-
                testerLiftInterpreter $ do
                    mt <- parseType @'Positive text
                    case mt of
                        MkSome t ->
                            qRunTypeM
                                $ runRenamer @TS [] []
                                $ do
                                    t' <- unEndoM (renameType @TS [] FreeName) t
                                    runSimplify @QExpression
                                        $ MkSealedExpression (mkPolarShimWit t')
                                        $ ClosedExpression undefined
            liftIO
                $ case simpexpr of
                    MkSealedExpression (MkShimWit t' _) _ -> assertEqual "" e $ exprShowShow t'

unrollTest :: Text -> Text -> TestTree
unrollTest rolledTypeText expectedUnrolledTypeText =
    testTree @Assertion (unpack rolledTypeText)
        $ runTester defaultTester
        $ do
            action <-
                testerLiftInterpreter $ do
                    mRolledType <- parseType @'Positive rolledTypeText
                    return
                        $ case mRolledType of
                            MkSome (ConsDolanType (RecursiveDolanSingularType var t) NilDolanType) ->
                                case unrollRecursiveType var t of
                                    MkShimWit unrolledType _ ->
                                        assertEqual "" expectedUnrolledTypeText $ toText $ exprShow unrolledType
                            _ -> fail "not a recursive type"
            liftIO $ action

testType :: TestTree
testType =
    testTree
        "type"
        [ testTree
            "pure"
            [ exprTypeTest "number" (return "{} -> Number.") $ return numExpr
            , exprTypeTest "boolean" (return "{} -> Boolean.") $ return boolExpr
            , exprTypeTest "id" (return "{} -> x -> x") $ return idExpr
            , exprTypeTest "nb" (return "{} -> Number. -> Boolean.") $ return nbFuncExpr
            , exprTypeTest "var" (return "{v. : a} -> a") $ return varExpr
            , exprTypeTest "apply-id-number" (return "{} -> Number.") $ apExpr idExpr numExpr
            , exprTypeTest "apply nb number" (return "{} -> Boolean.") $ apExpr nbFuncExpr numExpr
            , exprTypeTest "apply nb boolean" Nothing $ apExpr nbFuncExpr boolExpr
            , exprTypeTest "apply id var" (return "{v. : a} -> a") $ apExpr idExpr varExpr
            , exprTypeTest "apply nb var" (return "{v. : Number.} -> Boolean.") $ apExpr nbFuncExpr varExpr
            , exprTypeTest "ifelse" (return "{} -> Boolean. -> a -> a -> a") $ return ifelseExpr
            , exprTypeTest "list1" (return "{} -> a -> List. a") $ return list1Expr
            , exprTypeTest "listNumBool" (return "{} -> List. (Number. | Boolean.)") $ do
                lne <- apExpr list1Expr numExpr
                lbe <- apExpr list1Expr boolExpr
                joinExpr lne lbe
            , exprTypeTest "listlistNumBool" (return "{} -> List. (List. (Number. | Boolean.))") $ do
                lne <- apExpr list1Expr numExpr
                lbe <- apExpr list1Expr boolExpr
                llne <- apExpr list1Expr lne
                llbe <- apExpr list1Expr lbe
                joinExpr llne llbe
            , exprTypeTest "List. Number. -> List. Boolean." (return "{} -> List. Number. -> List. Boolean.")
                $ return listNumBoolFuncExpr
            , exprTypeTest "List. Boolean. -> List. Number." (return "{} -> List. Boolean. -> List. Number.")
                $ return listBoolNumFuncExpr
            , exprTypeTest "List. nn -> List. bb" (return "{} -> List. Number. -> List. Boolean.")
                $ joinExpr listNumBoolFuncExpr listNumBoolFuncExpr
            , exprTypeTest "List. bb -> List. nn" (return "{} -> List. Boolean. -> List. Number.")
                $ joinExpr listBoolNumFuncExpr listBoolNumFuncExpr
            , exprTypeTest
                "List. nb -> List. bn"
                (return "{} -> List. (Number. & Boolean.) -> List. (Boolean. | Number.)")
                $ joinExpr listNumBoolFuncExpr listBoolNumFuncExpr
            , exprTypeTest "snd" (return "{} -> Any *: a -> a") $ return sndExpr
            , exprTypeTest "thing" (return "{} -> a *: b -> a *: (a | b)") $ return thingExpr
            , exprTypeTest "snd . thing" (return "{} -> a *: a -> a") $ do
                e1 <- apExpr dotExpr sndExpr
                apExpr e1 thingExpr
            , exprTypeTest "twice" (return "{} -> a -> a *: a") $ return twiceExpr
            , exprTypeTest "thing . twice" (return "{} -> a -> a *: a") $ do
                e1 <- apExpr dotExpr thingExpr
                apExpr e1 twiceExpr
            , exprTypeTest "thing $ twice number" (return "{} -> Number. *: Number.") $ do
                e1 <- apExpr twiceExpr numExpr
                apExpr thingExpr e1
            , exprTypeTest "simplify $ thing $ twice number" (return "{} -> Number. *: Number.") $ do
                e1 <- apExpr twiceExpr numExpr
                r <- apExpr thingExpr e1
                runRenamer @TS [] [] $ runSimplify r
            , exprTypeTest "simplify duplicate" (return "{} -> Number.")
                $ runRenamer @TS [] []
                $ runSimplify
                $ typeFConstExpression toJMPolyShimWit (MkJoinType (Right 3) :: JoinType Number Number)
            , exprTypeTest "simplify duplicate list" (return "{} -> List. Number.")
                $ runRenamer @TS [] []
                $ runSimplify
                $ typeFConstExpression toJMPolyShimWit (MkJoinType (Right [3]) :: JoinType [Number] [Number])
            , exprTypeTest "simplify duplicate pair" (return "{} -> Number. *: Number.")
                $ runRenamer @TS [] []
                $ runSimplify
                $ typeFConstExpression
                    toJMPolyShimWit
                    (MkJoinType (Right (3, 3)) :: JoinType (Number, Number) (Number, Number))
            , exprTypeTest "simplify duplicate in pair" (return "{} -> Number. *: Number.")
                $ runRenamer @TS [] []
                $ runSimplify
                $ typeFConstExpression toJMPolyShimWit ((3, MkJoinType (Right 3)) :: (Number, JoinType Number Number))
            , exprTypeTest "simplify duplicate in pair" (return "{} -> Number. *: Number.")
                $ runRenamer @TS [] []
                $ runSimplify
                $ typeFConstExpression
                    toJMPolyShimWit
                    ((MkJoinType (Right 3), MkJoinType (Right 3)) :: (JoinType Number Number, JoinType Number Number))
            , exprTypeTest "simplify duplicate in list" (return "{} -> List. Number.")
                $ runRenamer @TS [] []
                $ runSimplify
                $ typeFConstExpression toJMPolyShimWit ([MkJoinType (Right 3)] :: [JoinType Number Number])
            ]
        , testTree
            "read"
            [ textTypeTest "v" "{v : a} -> a"
            , textTypeTest "if t then v1 else v2" "{t : Boolean., v1 : a, v2 : a} -> a"
            , textTypeTest "[]" "{} -> List. None"
            , textTypeTest "fn v => 1" "{} -> Any -> Natural."
            , textTypeTest "[v1,v2]" "{v1 : a, v2 : a} -> List1.List. a"
            , textTypeTest "[v,v,v]" "{v : a} -> List1.List. a"
            , textTypeTest "[x,y,x,y]" "{x : a, y : a} -> List1.List. a"
            , textTypeTest "(v 3,v \"text\")" "{v : (Text. | Natural.) -> a} -> a *: a"
            , textTypeTest "(v,v)" "{v : a} -> a *: a"
            , textTypeTest "(v 3,v 3)" "{v : Natural. -> a} -> a *: a"
            , textTypeTest "[v 3]" "{v : Natural. -> a} -> List1.List. a"
            , textTypeTest "(v 3,v False)" "{v : (Boolean. | Natural.) -> a} -> a *: a"
            , textTypeTest "((v 3,v False),v 3)" "{v : (Natural. | Boolean.) -> a} -> (a *: a) *: a"
            , testTree
                "function"
                [ textTypeTest "let {i: tvar -> tvar = id.Function} i" "{} -> a -> a"
                , textTypeTest "id.Function: tvar -> tvar" "{} -> a -> a"
                , textTypeTest "let {i = fn x => x} i" "{} -> a -> a"
                , textTypeTest "let {i : a -> a = fn x => x} i" "{} -> a -> a"
                , textTypeTest "let {i : tvar -> tvar = fn x => x} i" "{} -> a -> a"
                , textTypeTest "let {i : a -> a = fn x => x} i 3" "{} -> Natural."
                ]
            , textTypeTest "fn x => let {v = x} [v,v,v]" "{} -> a -> List1.List. a"
            , textTypeTest "fn v1, v2 => [v1,v2]" "{} -> a -> a -> List1.List. a"
            , textTypeTest
                "fn v1, v2, v3 => ([v1,v2],[v2,v3])"
                "{} -> a -> (a & b) -> b -> List1.List. a *: List1.List. b"
            , textTypeTest
                "fn v1, v2, v3 => (([v1,v2],[v2,v3]),[v3,v1])"
                "{} -> (a & b) -> (a & c) -> (c & b) -> (List1.List. a *: List1.List. c) *: List1.List. b"
            , testTree
                "inversion"
                [ textTypeTest "fn x => let {y : Integer = x} y" "{} -> Integer. -> Integer."
                , rejectionTest "fn x => let {y : Boolean | Number = x} y"
                , rejectionTest "fn x => let {y : (a -> a) *: (Boolean | Number) = x} y"
                , rejectionTest "fn x => let {y : (b -> b) *: (Boolean | Number) = x} y"
                , textTypeTest
                    "fn x => let {y: Boolean *: Number = (x,x)} y"
                    "{} -> (Boolean. & Number.) -> Boolean. *: Number."
                , textTypeTest
                    "fn x1 => fn x2 => let {y: Boolean *: Number = (x1,x2)} y"
                    "{} -> Boolean. -> Number. -> Boolean. *: Number."
                , textTypeTest "let {f : a -> a = fn x => x; g : a -> a = f} f 3" "{} -> Natural."
                , textTypeTest
                    "let rec {g : a -> a = f; f : a -> a = fn x => x} (f 3, g \"t\")"
                    "{} -> Natural. *: Text."
                , textTypeTest
                    "do {i1 <- pure.Action. $.Function fn x => x; pure.Action. i1;}"
                    "{} -> Action. (a -> a)"
                , textTypeTest
                    "do {i1 <- pure.Action. $.Function fn x => x; i2 <- pure.Action. i1; pure.Action. i2;}"
                    "{} -> Action. (a -> a)"
                , rejectionTest "do {i1 <- pure.Action. $.Function fn x => x; pure.Action. (i1 : a -> a);}"
                , textTypeTest
                    "do {i1 <- pure.Action. $.Function fn x => x; pure.Action. (i1 : Text -> Text);}"
                    "{} -> Action. (Text. -> Text.)"
                , textTypeTest "fn x => let {y = x} y" "{} -> a -> a"
                , rejectionTest "fn x => let {y : a -> a = x} y"
                , rejectionTest
                    "do {r <- newMem.WholeModel; (r: WholeModel a) := 3; t <- get (r: WholeModel a); pure.Action. $.Function textLength t}"
                , rejectionTest
                    "do {r <- newMem.WholeModel; let {r1: WholeModel a = r} r1 := 3; t <- let {r1: WholeModel a = r} get r1; pure.Action. $.Function textLength t}"
                , rejectionTest
                    "do {r <- newMem.WholeModel; (r1: WholeModel a) <- pure.Action. r; r1 := 3; t <- get r1; pure.Action. $.Function textLength t}"
                ]
            , textTypeTest "let {f : Entity = Nothing} f" "{} -> Entity."
            , textTypeTest "let {f : Entity -> Entity = Just} f" "{} -> Entity. -> Entity."
            , textTypeTest "let {f : Entity = []} f" "{} -> Entity."
            , textTypeTest "let {f : Entity -> Entity = fn x => [x]} f" "{} -> Entity. -> Entity."
            , textTypeTest
                "let {f : Entity -> Entity -> Entity = fn a, b => (a,b)} f"
                "{} -> Entity. -> Entity. -> Entity."
            , textTypeTest "let {f : Entity -> Entity = Left} f" "{} -> Entity. -> Entity."
            , textTypeTest "let {f : Entity -> Entity = Right} f" "{} -> Entity. -> Entity."
            , textTypeTest "fn x => if odd.Integer x then x else (x:Number)" "{} -> Integer. -> Number."
            , testTree
                "recursive"
                [ textTypeTest "let {x : rec a, Maybe a = Nothing} x" "{} -> rec a, Maybe. a"
                , textTypeTest "let rec {x : rec a, Maybe a = Just x} x" "{} -> rec a, Maybe. a"
                , textTypeTest "let rec {x = Just x} x" "{} -> rec a, Maybe. a"
                , textTypeTest "let rec {x : Entity = Just x} x" "{} -> Entity."
                , textTypeTest "let rec {x : Maybe Entity = Just x} x" "{} -> Maybe. Entity."
                , textTypeTest
                    "with Function let rec {rcount = fn {Nothing => 0; Just y => succ.Integer $ rcount y}} rcount"
                    "{} -> (rec a, Maybe. a) -> Integer."
                , textTypeTest
                    "Just $.Function Just $.Function Just Nothing"
                    "{} -> Maybe. (Maybe. (Maybe. (Maybe. None)))"
                , textTypeTest
                    "with Function let rec {rcount = fn {Nothing => 0; Just y => succ.Integer $ r1count y}; r1count = fn {Nothing => 0; Just y => succ.Integer $ r1count y}} rcount $ Just $ Just $ Just Nothing"
                    "{} -> Integer."
                , textTypeTest
                    "with Function let rec {rcount = fn {Nothing => 0; Just y => succ.Integer $ rcount y}; rval = Just rval} (rcount,(rval,rcount rval))"
                    "{} -> ((rec a, Maybe. a) -> Integer.) *: (rec b, Maybe. b) *: Integer."
                ]
            , testTree
                "tuple"
                [ textTypeTest "()" "{} -> Unit."
                , textTypeTest "(1,False)" "{} -> Natural. *: Boolean."
                , textTypeTest "(1,(False,(3,True)))" "{} -> Natural. *: Boolean. *: Natural. *: Boolean."
                , textTypeTest "(1,False,(3,True))" "{} -> Natural. *: Boolean. *: Natural. *: Boolean."
                , textTypeTest "(1,(False,3,True))" "{} -> Natural. *: Boolean. *: Natural. *: Boolean."
                , textTypeTest "(1,False,3,True)" "{} -> Natural. *: Boolean. *: Natural. *: Boolean."
                , textTypeTest "fn {(1,False) => ()}" "{} -> Literal. *: Literal. -> Unit."
                , textTypeTest
                    "fn {(1,False,3,True) => ()}"
                    "{} -> Literal. *: Literal. *: Literal. *: Literal. -> Unit."
                ]
            , testTree
                "let-binding"
                [ textTypeTest "fn x => let {} ()" "{} -> Any -> Unit."
                , textTypeTest "fn x => let {y = x} ()" "{} -> Any -> Unit."
                , textTypeTest "fn x => let {y = x} y" "{} -> a -> a"
                , textTypeTest "fn x => let {y = x +.Integer x} y" "{} -> Integer. -> Integer."
                , textTypeTest "fn x => let {y = x +.Integer x} ()" "{} -> Any -> Unit."
                , textTypeTest "fn x => with Integer let {y = x + x} y" "{} -> Integer. -> Integer."
                , textTypeTest "fn x => with Integer let {y = x + x} ()" "{} -> Any -> Unit."
                , textTypeTest
                    "fn x => let {subtype Unit <: Action Integer = fn () => pure.Action. x} ()"
                    "{} -> Any -> Unit."
                , textTypeTest
                    "fn x => let {subtype Unit <: Action Integer = fn () => pure.Action. x} ((): Action Integer)"
                    "{} -> Integer. -> Action. Integer."
                , textTypeTest
                    "let {subtype Unit <: Action Integer = fn () => pure.Action. x} ((): Action Integer)"
                    "{x : a & Integer.} -> Action. Integer."
                , testTree
                    "recursive"
                    [ textTypeTest "id: a -> (a | Integer.)" "{} -> a -> (a | Integer.)"
                    , textTypeTest "fn f => let rec {x = f x} x" "{} -> (a -> a) -> a"
                    , textTypeTest "let rec {f = seq (f 3)} f" "{} -> a -> (a | Natural.)"
                    , textTypeTest "let rec {f: a -> a = seq (f 3)} f" "{} -> a -> a"
                    , textTypeTest "let rec {f: a -> a = fn x => seq (f x) x} f" "{} -> a -> a"
                    , textTypeTest "let rec {f: a -> a = fn x => seq (f (Just x)) x} f" "{} -> a -> a"
                    , textTypeTest "let rec {f = seq (g 3); g = f} f" "{} -> a -> (a | Natural.)"
                    , textTypeTest "let rec {f = seq g3; g3 = f 3} f" "{} -> a -> (a | Natural.)"
                    , textTypeTest "let {rf = fn r => seq (r 3); r = fix rf} r" "{} -> a -> (a | Natural.)"
                    , textTypeTest "fn r => seq (r 3)" "{} -> (Natural. -> Any) -> a -> a"
                    , testTree "fixrec" $ let
                        fixTest :: Text -> Text -> Text -> TestTree
                        fixTest ta tr expected = let
                            typeText = "(" <> ta <> ") -> " <> tr
                            script = "fix (undefined: " <> typeText <> ")"
                            in textTypeTest script ("{} -> " <> expected)
                        recTest :: Text -> Text -> Text -> TestTree
                        recTest ta tr expected = let
                            typeText = "(" <> ta <> ") -> " <> tr
                            script = "let rec {f: " <> typeText <> " = f; x = f x} x"
                            in textTypeTest script ("{} -> " <> expected)
                        testPair :: Text -> Text -> Text -> TestTree
                        testPair ta tr expected =
                            testTree
                                (unpack $ ta <> " / " <> tr)
                                [fixTest ta tr expected, recTest ta tr expected]
                        in [ testPair "Text -> Any" "a -> a" "a -> (a | Text.)"
                           , testPair "None -> Text" "a -> a" "(a & Text.) -> a"
                           , testPair "Text -> Text" "a -> a" "Text. -> Text."
                           , testPair
                                "Maybe a -> Maybe a"
                                "a -> a"
                                "(rec a, b & Maybe. a) -> (rec c, b | Maybe. c)"
                           , testPair "Maybe b -> Maybe b" "a -> a" "Maybe. a -> Maybe. a"
                           , testPair "a" "Maybe a" "rec a, Maybe. a"
                           , testPair "Any" "Integer" "Integer."
                           , testPair "Maybe Integer" "Maybe None" "Maybe. None"
                           ]
                    ]
                ]
            , testTree "issue-229" [textTypeTest "(fn x => x x) id" "{} -> rec a, b | b -> a"]
            ]
        , testTree
            "simplify"
            [ simplifyTypeTest "a" "None"
            , simplifyTypeTest "a -> (a|a)" "a -> a"
            , simplifyTypeTest "a -> b -> (a|b)" "a -> a -> a"
            , simplifyTypeTest "a *: b -> (a|b)" "a *: a -> a"
            , simplifyTypeTest "a *: b -> a *: (a | b)" "a *: b -> a *: (a | b)"
            , simplifyTypeTest "a *: b -> b *: (a | b)" "a *: b -> b *: (a | b)"
            , simplifyTypeTest "(a&b) -> a *: b" "a -> a *: a"
            , simplifyTypeTest "(a & Integer) -> Boolean" "Integer. -> Boolean."
            , simplifyTypeTest "(b & Integer) -> Integer" "Integer. -> Integer."
            , simplifyTypeTest "(a & Integer) -> b" "Integer. -> None"
            , simplifyTypeTest "(a & Integer) -> a" "(a & Integer.) -> a"
            , simplifyTypeTest "(a & Integer) -> (a | Number)" "Integer. -> Number."
            , testTree
                "subtype"
                [ simplifyTypeTest "Boolean | Integer" "Boolean. | Integer."
                , simplifyTypeTest "Integer | Boolean" "Integer. | Boolean."
                , simplifyTypeTest "(Boolean & Integer) -> Unit" "(Boolean. & Integer.) -> Unit."
                , simplifyTypeTest "(Integer & Boolean) -> Unit" "(Integer. & Boolean.) -> Unit."
                , simplifyTypeTest "Literal | Integer" "Literal."
                , simplifyTypeTest "Integer | Literal" "Literal."
                , simplifyTypeTest "List Literal | List Integer" "List. Literal."
                , simplifyTypeTest "List Integer | List Literal" "List. Literal."
                , simplifyTypeTest "(Literal & Integer) -> Unit" "Integer. -> Unit."
                , simplifyTypeTest "(Integer & Literal) -> Unit" "Integer. -> Unit."
                , simplifyTypeTest "(List Literal & List Integer) -> Unit" "List. Integer. -> Unit."
                , simplifyTypeTest "(List Integer & List Literal) -> Unit" "List. Integer. -> Unit."
                ]
            , testTree
                "recursive"
                [ simplifyTypeTest "rec a, Maybe a" "rec a, Maybe. a"
                , simplifyTypeTest "rec a, Integer" "Integer."
                , simplifyTypeTest "Maybe (rec a, List a)" "Maybe. (rec a, List. a)"
                , simplifyTypeTest "Maybe (rec a, Integer)" "Maybe. Integer."
                , testTree "issue-62" [simplifyTypeTest "rec a, rec b, a *: b" "rec a, a *: a"]
                , testTree
                    "issue-234"
                    [ simplifyTypeTest "rec b, Maybe. (rec c, b | Maybe. c)" "rec a, Maybe. a"
                    , simplifyTypeTest "rec a, (rec b, Maybe. (rec c, b | Maybe. c)) | Maybe. a" "rec a, Maybe. a"
                    , simplifyTypeTest
                        "b -> (Maybe. (rec w, b | Maybe. w) | (Maybe. (rec an, b | Maybe. an) | Maybe. f))"
                        "a -> Maybe. (rec b, a | Maybe. b)"
                    , simplifyTypeTest "a -> (rec r, Maybe. r | a)" "a -> (rec b, Maybe. b | a)"
                    , simplifyTypeTest
                        "a -> ((rec r, Maybe. r | a) | (rec r, Maybe. r | a))"
                        "a -> (rec b, Maybe. b | a)"
                    , simplifyTypeTest
                        "a -> (Maybe. (rec r, Maybe. r | a) | Maybe. (rec r, Maybe. r | a))"
                        "a -> Maybe. (rec b, Maybe. b | a)"
                    ]
                , testTree
                    "roll"
                    [ simplifyTypeTest "Maybe (rec a, Maybe a)" "rec a, Maybe. a"
                    , simplifyTypeTest "Maybe (Maybe (rec a, Maybe a))" "rec a, Maybe. a"
                    , simplifyTypeTest "Any -> Maybe (rec a, Maybe a)" "Any -> (rec a, Maybe. a)"
                    , unrollTest "rec a, Maybe a" "Maybe. (rec a, Maybe. a)"
                    ]
                ]
            , testTree
                "fullconstraint"
                [ simplifyTypeTest "Integer | d" "Integer."
                , simplifyTypeTest "(rec a, Maybe a) | d" "rec a, Maybe. a"
                , simplifyTypeTest "rec a, (Maybe a | d)" "rec a, Maybe. a"
                , simplifyTypeTest "Maybe (rec a, Maybe a | d)" "rec a, Maybe. a"
                , simplifyTypeTest "d -> Maybe (rec a, Maybe a | d)" "a -> Maybe. (rec b, Maybe. b | a)"
                , simplifyTypeTest "(a -> Literal) | ((Text & b) -> a)" "Text. -> Literal."
                , simplifyTypeTest "(a & Text) -> (Literal | a)" "Text. -> Literal."
                ]
            ]
        , testTree
            "library"
            [ textTypeTest "()" "{} -> Unit."
            , textTypeTest "ap{3}" "{} -> WholeModel. +Natural."
            , textTypeTest "id.Property !$%.Attribute ap{3}" "{} -> WholeModel. +Natural."
            , textTypeTest "id.Property !$.Attribute ap{3}" "{} -> WholeModel. +Natural."
            , textTypeTest "(id.Property ..Property id.Property) !$%.Attribute ap{3}" "{} -> WholeModel. +Natural."
            , textTypeTest "(id.Property ..Property id.Property) !$.Attribute ap{3}" "{} -> WholeModel. +Natural."
            , textTypeTest
                "!{property.Store @Integer @Text !\"a\"} store **.Property !{property.Store @Number @Text !\"b\"} store"
                "{store : Store.} -> Property. (-Integer.,+Number.) (Text. *: Text.)"
            , textTypeTest
                "!{property.Store @Text @Integer !\"a\"} store ++.Property !{property.Store @Text @Number !\"b\"} store"
                "{store : Store.} -> Property. (Text. +: Text.) (-Integer.,+Number.)"
            , textTypeTest
                "(!{property.Store @Integer @Text !\"a\"} store **.Property !{property.Store @Number @Text !\"b\"} store) !$%.Attribute ap{3}"
                "{store : Store.} -> WholeModel. (Text. *: Text.)"
            , textTypeTest
                "(!{property.Store @Integer @Text !\"a\"} store **.Property !{property.Store @Number @Text !\"b\"} store) !$.Attribute ap{3}"
                "{store : Store.} -> WholeModel. (Text. *: Text.)"
            , textTypeTest
                "!{property.Store @Integer @Text !\"a\"} store !@%.Property ap{\"x\"}"
                "{store : Store.} -> FiniteSetModel. Integer."
            , textTypeTest
                "!{property.Store @Integer @Text !\"a\"} store !@.Property ap{\"x\"}"
                "{store : Store.} -> FiniteSetModel. Integer."
            , textTypeTest
                "(!{property.Store @Integer @Text !\"a\"} store !@%.Property ap{\"x\"}) <:*:>.FiniteSetModel (!{property.Store @Number @Text !\"b\"} store !@%.Property ap{\"y\"})"
                "{store : Store.} -> FiniteSetModel. (Integer. *: Number.)"
            , textTypeTest
                "product.WholeModel ap{3} ap{\"x\"}"
                "{} -> WholeModel. (-(Any *: Any),+(Natural. *: Text.))"
            , textTypeTest
                "immut.WholeModel $.Function product.WholeModel ap{3} ap{\"x\"}"
                "{} -> WholeModel. +(Natural. *: Text.)"
            ]
        ]
