module Test.Type
    ( testType
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan
import Pinafore
import Pinafore.Test
import Shapes
import Shapes.Test

type TS = QTypeSystem

type PExpression = TSSealedExpression TS

showVars :: NamedExpression VarID (QShimWit 'Negative) t -> [String]
showVars (ClosedExpression _) = []
showVars (OpenExpression (MkNameWitness name (MkShimWit t _)) expr) =
    (unpack $ toText $ exprShow name <> " : " <> exprShow t) : showVars expr

showTypes :: PExpression -> String
showTypes (MkSealedExpression (MkShimWit t _) expr) =
    "{" <> intercalate ", " (nub $ showVars expr) <> "} -> " <> unpack (toText $ exprShow t)

exprTypeTest :: String -> Maybe String -> QInterpreter PExpression -> TestTree
exprTypeTest name expected mexpr =
    testTree name $
    runTester defaultTester $ do
        result <- tryExc $ testerLiftInterpreter mexpr
        liftIO $
            assertEqual "" expected $ do
                expr <- resultToMaybe result
                return $ showTypes expr

apExpr :: PExpression -> PExpression -> QInterpreter PExpression
apExpr = tsApply @TS

idExpr :: PExpression
idExpr = typeFConstExpression toJMShimWit $ \(v :: X) -> v

nbFuncExpr :: PExpression
nbFuncExpr = typeFConstExpression toJMShimWit $ \(_ :: Number) -> False

numExpr :: PExpression
numExpr = typeFConstExpression toJMShimWit $ (3 :: Number)

boolExpr :: PExpression
boolExpr = typeFConstExpression toJMShimWit False

varExpr :: PExpression
varExpr = tsVar @TS $ mkVarID firstVarIDState "v"

ifelseExpr :: PExpression
ifelseExpr =
    typeFConstExpression toJMShimWit $ \test (tb :: A) (eb :: A) ->
        if test
            then tb
            else eb

list1Expr :: PExpression
list1Expr = typeFConstExpression toJMShimWit $ \(a :: A) -> [a]

sndExpr :: PExpression
sndExpr = typeFConstExpression toJMShimWit $ \(MkTopType, a :: A) -> a

twiceExpr :: PExpression
twiceExpr = typeFConstExpression toJMShimWit $ \(a :: A) -> (a, a)

thingExpr :: PExpression
thingExpr =
    typeFConstExpression toJMShimWit $ \(a :: A, b :: B) ->
        ( a
        , if False
              then MkJoinType $ Left a
              else MkJoinType $ Right b)

dotExpr :: PExpression
dotExpr = typeFConstExpression toJMShimWit $ \(f :: B -> C) (g :: A -> B) -> f . g

listNumBoolFuncExpr :: PExpression
listNumBoolFuncExpr = typeFConstExpression toJMShimWit $ \(_ :: [Number]) -> [True]

listBoolNumFuncExpr :: PExpression
listBoolNumFuncExpr = typeFConstExpression toJMShimWit $ \(_ :: [Bool]) -> [2 :: Number]

joinExpr :: PExpression -> PExpression -> QInterpreter PExpression
joinExpr exp1 exp2 = do
    je <- apExpr ifelseExpr boolExpr
    e <- apExpr je exp1
    apExpr e exp2

textTypeTest :: Text -> String -> TestTree
textTypeTest text r =
    testTree (unpack text) $
    runTester defaultTester $ do
        expr <- testerLiftInterpreter $ parseTopExpression text
        liftIO $ assertEqual "" r $ showTypes expr

rejectionTest :: Text -> TestTree
rejectionTest text =
    testTree ("REJECT: " <> unpack text) $
    runTester defaultTester $ do
        result <- tryExc $ testerLiftInterpreter $ parseTopExpression text
        liftIO $
            case result of
                FailureResult _ -> return ()
                SuccessResult _ -> assertFailure "no exception"

runSimplify ::
       forall a. TSMappable TS a
    => a
    -> TSOuter TS a
runSimplify = unEndoM $ simplify @TS

simplifyTypeTest :: Text -> String -> TestTree
simplifyTypeTest text e =
    testTree (unpack text) $
    runTester defaultTester $ do
        simpexpr <-
            testerLiftInterpreter $ do
                mt <- parseType @'Positive text
                case mt of
                    MkSome t ->
                        runRenamer @TS [] [] $
                        runSimplify @PExpression $ MkSealedExpression (mkPolarShimWit t) $ ClosedExpression undefined
        liftIO $
            case simpexpr of
                MkSealedExpression (MkShimWit t' _) _ -> assertEqual "" e $ unpack $ toText $ exprShow t'

unrollTest :: Text -> Text -> TestTree
unrollTest rolledTypeText expectedUnrolledTypeText =
    testTree @Assertion (unpack rolledTypeText) $
    runTester defaultTester $ do
        action <-
            testerLiftInterpreter $ do
                mRolledType <- parseType @'Positive rolledTypeText
                return $
                    case mRolledType of
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
              , exprTypeTest "listNumBool" (return "{} -> List. (Boolean. | Number.)") $ do
                    lne <- apExpr list1Expr numExpr
                    lbe <- apExpr list1Expr boolExpr
                    joinExpr lne lbe
              , exprTypeTest "listlistNumBool" (return "{} -> List. (List. (Boolean. | Number.))") $ do
                    lne <- apExpr list1Expr numExpr
                    lbe <- apExpr list1Expr boolExpr
                    llne <- apExpr list1Expr lne
                    llbe <- apExpr list1Expr lbe
                    joinExpr llne llbe
              , exprTypeTest "List. Number. -> List. Boolean." (return "{} -> List. Number. -> List. Boolean.") $
                return listNumBoolFuncExpr
              , exprTypeTest "List. Boolean. -> List. Number." (return "{} -> List. Boolean. -> List. Number.") $
                return listBoolNumFuncExpr
              , exprTypeTest "List. nn -> List. bb" (return "{} -> List. Number. -> List. Boolean.") $
                joinExpr listNumBoolFuncExpr listNumBoolFuncExpr
              , exprTypeTest "List. bb -> List. nn" (return "{} -> List. Boolean. -> List. Number.") $
                joinExpr listBoolNumFuncExpr listBoolNumFuncExpr
              , exprTypeTest
                    "List. nb -> List. bn"
                    (return "{} -> List. (Boolean. & Number.) -> List. (Number. | Boolean.)") $
                joinExpr listNumBoolFuncExpr listBoolNumFuncExpr
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
              , exprTypeTest "simplify duplicate" (return "{} -> Number.") $
                runRenamer @TS [] [] $
                runSimplify $ typeFConstExpression toJMShimWit (MkJoinType (Right 3) :: JoinType Number Number)
              , exprTypeTest "simplify duplicate list" (return "{} -> List. Number.") $
                runRenamer @TS [] [] $
                runSimplify $ typeFConstExpression toJMShimWit (MkJoinType (Right [3]) :: JoinType [Number] [Number])
              , exprTypeTest "simplify duplicate pair" (return "{} -> Number. *: Number.") $
                runRenamer @TS [] [] $
                runSimplify $
                typeFConstExpression
                    toJMShimWit
                    (MkJoinType (Right (3, 3)) :: JoinType (Number, Number) (Number, Number))
              , exprTypeTest "simplify duplicate in pair" (return "{} -> Number. *: Number.") $
                runRenamer @TS [] [] $
                runSimplify $
                typeFConstExpression toJMShimWit ((3, MkJoinType (Right 3)) :: (Number, JoinType Number Number))
              , exprTypeTest "simplify duplicate in pair" (return "{} -> Number. *: Number.") $
                runRenamer @TS [] [] $
                runSimplify $
                typeFConstExpression
                    toJMShimWit
                    ((MkJoinType (Right 3), MkJoinType (Right 3)) :: (JoinType Number Number, JoinType Number Number))
              , exprTypeTest "simplify duplicate in list" (return "{} -> List. Number.") $
                runRenamer @TS [] [] $
                runSimplify $ typeFConstExpression toJMShimWit ([MkJoinType (Right 3)] :: [JoinType Number Number])
              ]
        , testTree
              "read"
              [ textTypeTest "v" "{v : a} -> a"
              , textTypeTest "if t then v1 else v2" "{t : Boolean., v1 : a, v2 : a} -> a"
              , textTypeTest "[]" "{} -> List. None"
              , textTypeTest "fn v => 1" "{} -> Any -> Integer."
              , textTypeTest "[v1,v2]" "{v1 : a, v2 : a} -> List1. a"
              , textTypeTest "[v,v,v]" "{v : a} -> List1. a"
              , textTypeTest "[x,y,x,y]" "{x : a, y : a} -> List1. a"
              , textTypeTest "(v 3,v \"text\")" "{v : Integer. -> a, v : Text. -> b} -> a *: b"
              , textTypeTest "(v,v)" "{v : a, v : b} -> a *: b"
              , textTypeTest "(v 3,v 3)" "{v : Integer. -> a, v : Integer. -> b} -> a *: b"
              , textTypeTest "[v 3]" "{v : Integer. -> a} -> List1. a"
              , textTypeTest "(v 3,v False)" "{v : Integer. -> a, v : Boolean. -> b} -> a *: b"
              , textTypeTest
                    "((v 3,v False),v 3)"
                    "{v : Integer. -> a, v : Boolean. -> b, v : Integer. -> c} -> (a *: b) *: c"
              , testTree
                    "function"
                    [ textTypeTest "let i: tvar -> tvar = id.Function in i" "{} -> a -> a"
                    , textTypeTest "id.Function: tvar -> tvar" "{} -> tvar -> tvar"
                    , textTypeTest "let i = fn x => x in i" "{} -> a -> a"
                    , textTypeTest "let i : a -> a = fn x => x in i" "{} -> a -> a"
                    , textTypeTest "let i : tvar -> tvar = fn x => x in i" "{} -> a -> a"
                    , textTypeTest "let i : a -> a = fn x => x in i 3" "{} -> Integer."
                    ]
              , textTypeTest "fn x => let v = x in [v,v,v]" "{} -> a -> List1. a"
              , textTypeTest "fn v1, v2 => [v1,v2]" "{} -> a -> a -> List1. a"
              , textTypeTest "fn v1, v2, v3 => ([v1,v2],[v2,v3])" "{} -> a -> (a & b) -> b -> List1. a *: List1. b"
              , textTypeTest
                    "fn v1, v2, v3 => (([v1,v2],[v2,v3]),[v3,v1])"
                    "{} -> (a & b) -> (a & c) -> (c & b) -> (List1. a *: List1. c) *: List1. b"
              , testTree
                    "inversion"
                    [ textTypeTest "fn x => let y : Integer = x in y" "{} -> Integer. -> Integer."
                    , rejectionTest "fn x => let y : Boolean | Number = x in y"
                    , rejectionTest "fn x => let y : (a -> a) *: (Boolean | Number) = x in y"
                    , rejectionTest "fn x => let y : (b -> b) *: (Boolean | Number) = x in y"
                    , textTypeTest
                          "fn x => let y: Boolean *: Number = (x,x) in y"
                          "{} -> (Number. & Boolean.) -> Boolean. *: Number."
                    , textTypeTest
                          "fn x1 => fn x2 => let y: Boolean *: Number = (x1,x2) in y"
                          "{} -> Boolean. -> Number. -> Boolean. *: Number."
                    , textTypeTest "let f : a -> a = fn x => x; g : a -> a = f in f 3" "{} -> Integer."
                    , textTypeTest
                          "let rec g : a -> a = f; f : a -> a = fn x => x end in (f 3, g \"t\")"
                          "{} -> Integer. *: Text."
                    , textTypeTest
                          "do i1 <- pure.Action. $.Function fn x => x; pure.Action. i1; end"
                          "{} -> Action. (a -> a)"
                    , textTypeTest
                          "do i1 <- pure.Action. $.Function fn x => x; i2 <- pure.Action. i1; pure.Action. i2; end"
                          "{} -> Action. (a -> a)"
                    , rejectionTest "do i1 <- pure.Action. $.Function fn x => x; pure.Action. (i1 : a -> a); end"
                    , textTypeTest
                          "do i1 <- pure.Action. $.Function fn x => x; pure.Action. (i1 : Text -> Text); end"
                          "{} -> Action. (Text. -> Text.)"
                    , textTypeTest "fn x => let y = x in y" "{} -> a -> a"
                    , rejectionTest "fn x => let y : a -> a = x in y"
                    , rejectionTest
                          "do r <- newMem.WholeModel; (r: WholeModel a) := 3; t <- get (r: WholeModel a); pure.Action. $.Function textLength t end"
                    , rejectionTest
                          "do r <- newMem.WholeModel; let r1: WholeModel a = r in r1 := 3; t <- let r1: WholeModel a = r in get r1; pure.Action. $.Function textLength t end"
                    , rejectionTest
                          "do r <- newMem.WholeModel; (r1: WholeModel a) <- pure.Action. r; r1 := 3; t <- get r1; pure.Action. $.Function textLength t end"
                    ]
              , textTypeTest "let f : Entity = Nothing in f" "{} -> Entity."
              , textTypeTest "let f : Entity -> Entity = Just in f" "{} -> Entity. -> Entity."
              , textTypeTest "let f : Entity = [] in f" "{} -> Entity."
              , textTypeTest "let f : Entity -> Entity = fn x => [x] in f" "{} -> Entity. -> Entity."
              , textTypeTest
                    "let f : Entity -> Entity -> Entity = fn a, b => (a,b) in f"
                    "{} -> Entity. -> Entity. -> Entity."
              , textTypeTest "let f : Entity -> Entity = Left in f" "{} -> Entity. -> Entity."
              , textTypeTest "let f : Entity -> Entity = Right in f" "{} -> Entity. -> Entity."
              , textTypeTest "fn x => if odd.Integer x then x else (x:Number)" "{} -> Integer. -> Number."
              , testTree
                    "recursive"
                    [ textTypeTest "let x : rec a, Maybe a = Nothing in x" "{} -> rec a, Maybe. a"
                    , textTypeTest "let rec x : rec a, Maybe a = Just x end in x" "{} -> rec a, Maybe. a"
                    , textTypeTest "let rec x = Just x end in x" "{} -> rec a, Maybe. a"
                    , textTypeTest "let rec x : Entity = Just x end in x" "{} -> Entity."
                    , textTypeTest "let rec x : Maybe Entity = Just x end in x" "{} -> Maybe. Entity."
                    , textTypeTest
                          "let using Function; rec rcount = match Nothing => 0; Just y => succ.Integer $ rcount y end end in rcount"
                          "{} -> (rec a, Maybe. a) -> Integer."
                    , textTypeTest
                          "Just $.Function Just $.Function Just Nothing"
                          "{} -> Maybe. (Maybe. (Maybe. (Maybe. None)))"
                    , textTypeTest
                          "let using Function; rec rcount = match Nothing => 0; Just y => succ.Integer $ r1count y end; r1count = match Nothing => 0; Just y => succ.Integer $ r1count y end end in rcount $ Just $ Just $ Just Nothing"
                          "{} -> Integer."
                    , textTypeTest
                          "let using Function; rec rcount = match Nothing => 0; Just y => succ.Integer $ rcount y end end; rec rval = Just rval end in (rcount,(rval,rcount rval))"
                          "{} -> ((rec a, Maybe. a) -> Integer.) *: (rec b, Maybe. b) *: Integer."
                    ]
              , testTree
                    "tuple"
                    [ textTypeTest "()" "{} -> Unit."
                    , textTypeTest "(1,False)" "{} -> Integer. *: Boolean."
                    , textTypeTest "(1,(False,(3,True)))" "{} -> Integer. *: Boolean. *: Integer. *: Boolean."
                    , textTypeTest "(1,False,(3,True))" "{} -> Integer. *: Boolean. *: Integer. *: Boolean."
                    , textTypeTest "(1,(False,3,True))" "{} -> Integer. *: Boolean. *: Integer. *: Boolean."
                    , textTypeTest "(1,False,3,True)" "{} -> Integer. *: Boolean. *: Integer. *: Boolean."
                    , textTypeTest "match (1,False) => () end" "{} -> Literal. *: Literal. -> Unit."
                    , textTypeTest
                          "match (1,False,3,True) => () end"
                          "{} -> Literal. *: Literal. *: Literal. *: Literal. -> Unit."
                    ]
              , testTree
                    "let-binding"
                    [ textTypeTest "fn x => let in ()" "{} -> Any -> Unit."
                    , textTypeTest "fn x => let y = x in ()" "{} -> Any -> Unit."
                    , textTypeTest "fn x => let y = x in y" "{} -> a -> a"
                    , textTypeTest "fn x => let y = x +.Integer x in y" "{} -> Integer. -> Integer."
                    , textTypeTest "fn x => let y = x +.Integer x in ()" "{} -> Any -> Unit."
                    , textTypeTest "fn x => let using Integer; y = x + x in y" "{} -> Integer. -> Integer."
                    , textTypeTest "fn x => let using Integer; y = x + x in ()" "{} -> Any -> Unit."
                    , textTypeTest
                          "fn x => let subtype Unit <: Action Integer = fn () => pure.Action. x in ()"
                          "{} -> Any -> Unit."
                    , textTypeTest
                          "fn x => let subtype Unit <: Action Integer = fn () => pure.Action. x in ((): Action Integer)"
                          "{} -> Integer. -> Action. Integer."
                    , textTypeTest
                          "let subtype Unit <: Action Integer = fn () => pure.Action. x in ((): Action Integer)"
                          "{x : b & Integer.} -> Action. Integer."
                    , testTree "recursive" $ let
                          fixTest :: Text -> Text -> String -> TestTree
                          fixTest ta tr expected = let
                              typeText = "(" <> ta <> ") -> " <> tr
                              script = "let rec f: (t -> t) -> t = f end; rec u: " <> typeText <> " = u end in f u"
                              in textTypeTest script ("{} -> " <> expected)
                          recTest :: Text -> Text -> String -> TestTree
                          recTest ta tr expected = let
                              typeText = "(" <> ta <> ") -> " <> tr
                              script = "let rec f: " <> typeText <> " = f end; rec x = f x end in x"
                              in textTypeTest script ("{} -> " <> expected)
                          in [ textTypeTest "id: a -> (a | Integer.)" "{} -> a -> (a | Integer.)"
                             , textTypeTest "let rec f = seq (f 3) end in f" "{} -> a -> a"
                             , textTypeTest "let rec f: a -> a = seq (f 3) end in f" "{} -> a -> a"
                             , textTypeTest "let rec f: a -> a = fn x => seq (f x) x end in f" "{} -> a -> a"
                             , textTypeTest "let rec f: a -> a = fn x => seq (f (Just x)) x end in f" "{} -> a -> a"
                             , textTypeTest "let rec f = seq (g 3); g = f end in f" "{} -> a -> a"
                             , textTypeTest "let rec f = seq g3; g3 = f 3 end in f" "{} -> a -> a"
                             , textTypeTest
                                   "let fixf = fn f => let rec x = f x end in x; rf = fn r => seq (r 3); r = fixf rf in r"
                                   "{} -> a -> (a | Integer.)"
                             , textTypeTest "fn r => seq (r 3)" "{} -> (Integer. -> Any) -> a -> a"
                             , textTypeTest "(fn r => seq (r 3)): (a -> a) -> a -> a" "{} -> (a -> a) -> a -> a"
                             , textTypeTest "(fn r => seq (r 3)): (a -> a) -> b -> b" "{} -> (a -> a) -> b -> b"
                             , fixTest "Integer -> Any" "a -> a" "a -> (a | Integer.)"
                             , recTest "Integer -> Any" "a -> a" "a -> a"
                             , fixTest "Maybe a -> Maybe a" "a -> a" "a -> a"
                             , recTest "Maybe a -> Maybe a" "a -> a" "a -> a"
                             , fixTest "a" "Maybe a" "rec a, Maybe. a"
                             , recTest "a" "Maybe a" "rec a, Maybe. a"
                             , fixTest "Any" "Integer" "Integer."
                             , recTest "Any" "Integer" "Integer."
                             , fixTest "Maybe Integer" "Maybe None" "Maybe. None"
                             , recTest "Maybe Integer" "Maybe None" "Maybe. None"
                             ]
                    ]
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
                    [ simplifyTypeTest "rec a, a" "None"
                    , simplifyTypeTest "rec a, (a | Maybe a)" "rec a, Maybe. a"
                    , simplifyTypeTest "rec a, (a | Integer)" "Integer."
                    , simplifyTypeTest "rec a, Maybe a" "rec a, Maybe. a"
                    , simplifyTypeTest "rec a, Integer" "Integer."
                    , simplifyTypeTest "Maybe (rec a, a)" "Maybe. None"
                    , simplifyTypeTest "Maybe (rec a, List a)" "Maybe. (rec a, List. a)"
                    , simplifyTypeTest "Maybe (rec a, Integer)" "Maybe. Integer."
                    , expectFailBecause "ISSUE #61" $ simplifyTypeTest "rec a, rec b, a *: b" "rec b, b *: b"
                    , expectFailBecause "ISSUE #61" $
                      simplifyTypeTest "(rec a, Maybe a) | (rec b, List b)" "rec a, Maybe. a | List. a"
                    , expectFailBecause "ISSUE #61" $
                      simplifyTypeTest "(rec a, Maybe a) | (rec a, List a)" "rec a, Maybe. a | List. a"
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
                    , simplifyTypeTest "d -> Maybe (rec a, Maybe a | d)" "d -> Maybe. (rec a, Maybe. a | d)"
                    , simplifyTypeTest "(a -> Literal) | ((Text & b) -> a)" "Text. -> Literal."
                    , simplifyTypeTest "(a & Text) -> (Literal | a)" "Text. -> Literal."
                    ]
              ]
        , testTree
              "library"
              [ textTypeTest "()" "{} -> Unit."
              , textTypeTest "{3}" "{} -> WholeModel. +Integer."
              , textTypeTest "id.Property !$%.Attribute {3}" "{} -> WholeModel. +Integer."
              , textTypeTest "id.Property !$.Attribute {3}" "{} -> WholeModel. +Integer."
              , textTypeTest "(id.Property ..Property id.Property) !$%.Attribute {3}" "{} -> WholeModel. +Integer."
              , textTypeTest "(id.Property ..Property id.Property) !$.Attribute {3}" "{} -> WholeModel. +Integer."
              , textTypeTest
                    "property.Store @Integer @Text !\"a\" store **.Property property.Store @Number @Text !\"b\" store"
                    "{store : Store.} -> Property. {-Integer.,+Number.} (Text. *: Text.)"
              , textTypeTest
                    "property.Store @Text @Integer !\"a\" store ++.Property property.Store @Text @Number !\"b\" store"
                    "{store : Store.} -> Property. (Text. +: Text.) {-Integer.,+Number.}"
              , textTypeTest
                    "(property.Store @Integer @Text !\"a\" store **.Property property.Store @Number @Text !\"b\" store) !$%.Attribute {3}"
                    "{store : Store.} -> WholeModel. (Text. *: Text.)"
              , textTypeTest
                    "(property.Store @Integer @Text !\"a\" store **.Property property.Store @Number @Text !\"b\" store) !$.Attribute {3}"
                    "{store : Store.} -> WholeModel. (Text. *: Text.)"
              , textTypeTest
                    "property.Store @Integer @Text !\"a\" store !@%.Property {\"x\"}"
                    "{store : Store.} -> FiniteSetModel. Integer."
              , textTypeTest
                    "property.Store @Integer @Text !\"a\" store !@.Property {\"x\"}"
                    "{store : Store.} -> FiniteSetModel. Integer."
              , textTypeTest
                    "(property.Store @Integer @Text !\"a\" store !@%.Property {\"x\"}) <:*:>.FiniteSetModel (property.Store @Number @Text !\"b\" store !@%.Property {\"y\"})"
                    "{store : Store.} -> FiniteSetModel. (Integer. *: Number.)"
              , textTypeTest "product.WholeModel {3} {\"x\"}" "{} -> WholeModel. {-(Any *: Any),+(Integer. *: Text.)}"
              , textTypeTest
                    "immut.WholeModel $.Function product.WholeModel {3} {\"x\"}"
                    "{} -> WholeModel. +(Integer. *: Text.)"
              ]
        ]
