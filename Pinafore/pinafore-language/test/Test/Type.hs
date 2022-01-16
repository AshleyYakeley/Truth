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

type TS = PinaforeTypeSystem

type PExpression = TSSealedExpression TS

showVars :: NamedExpression VarID (PinaforeShimWit 'Negative) t -> [String]
showVars (ClosedExpression _) = []
showVars (OpenExpression (MkNameWitness name (MkShimWit t _)) expr) =
    (show name <> " : " <> unpack (exprShow t)) : showVars expr

showTypes :: PExpression -> String
showTypes (MkSealedExpression (MkShimWit t _) expr) =
    "{" <> intercalate ", " (showVars expr) <> "} -> " <> unpack (exprShow t)

exprTypeTest :: String -> Maybe String -> PinaforeInterpreter PExpression -> TestTree
exprTypeTest name expected mexpr =
    testTree name $ do
        result <- runInterpretResult $ runTestPinaforeSourceScoped mexpr
        assertEqual "" expected $ do
            expr <- resultToMaybe result
            return $ showTypes expr

apExpr :: PExpression -> PExpression -> PinaforeInterpreter PExpression
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

joinExpr :: PExpression -> PExpression -> PinaforeInterpreter PExpression
joinExpr exp1 exp2 = do
    je <- apExpr ifelseExpr boolExpr
    e <- apExpr je exp1
    apExpr e exp2

textTypeTest :: Text -> String -> TestTree
textTypeTest text r =
    testTree (unpack text) $ do
        expr <- throwInterpretResult $ runTestPinaforeSourceScoped $ parseTopExpression text
        assertEqual "" r $ showTypes expr

badInterpretTest :: Text -> TestTree
badInterpretTest text =
    testTree (unpack text) $ do
        result <- runInterpretResult $ runTestPinaforeSourceScoped $ parseTopExpression text
        case result of
            FailureResult _ -> return ()
            SuccessResult _ -> assertFailure "no exception"

simplifyTypeTest :: Text -> String -> TestTree
simplifyTypeTest text e =
    testTree (unpack text) $ do
        simpexpr <-
            throwInterpretResult $
            runTestPinaforeSourceScoped $ do
                mt <- parseType @'Positive text
                case mt of
                    MkAnyW t ->
                        runRenamer @PinaforeTypeSystem $
                        simplify @PinaforeTypeSystem @PExpression $
                        MkSealedExpression (mkPolarShimWit t) $ ClosedExpression undefined
        case simpexpr of
            MkSealedExpression (MkShimWit t' _) _ -> assertEqual "" e $ unpack $ exprShow t'

unrollTest :: Text -> Text -> TestTree
unrollTest rolledTypeText expectedUnrolledTypeText =
    testTree @Assertion (unpack rolledTypeText) $ do
        action <-
            throwInterpretResult $
            runTestPinaforeSourceScoped $ do
                mRolledType <- parseType @'Positive rolledTypeText
                return $
                    case mRolledType of
                        MkAnyW (ConsDolanType (RecursiveDolanSingularType var t) NilDolanType) ->
                            case unrollRecursiveType var t of
                                MkShimWit unrolledType _ ->
                                    assertEqual "" expectedUnrolledTypeText $ exprShow unrolledType
                        _ -> fail "not a recursive type"
        action

testType :: TestTree
testType =
    testTree
        "type"
        [ testTree
              "pure"
              [ exprTypeTest "number" (return "{} -> Number") $ return numExpr
              , exprTypeTest "boolean" (return "{} -> Boolean") $ return boolExpr
              , exprTypeTest "id" (return "{} -> x -> x") $ return idExpr
              , exprTypeTest "nb" (return "{} -> Number -> Boolean") $ return nbFuncExpr
              , exprTypeTest "var" (return "{v : a} -> a") $ return varExpr
              , exprTypeTest "apply id number" (return "{} -> Number") $ apExpr idExpr numExpr
              , exprTypeTest "apply nb number" (return "{} -> Boolean") $ apExpr nbFuncExpr numExpr
              , exprTypeTest "apply nb boolean" Nothing $ apExpr nbFuncExpr boolExpr
              , exprTypeTest "apply id var" (return "{v : b} -> b") $ apExpr idExpr varExpr
              , exprTypeTest "apply nb var" (return "{v : Number} -> Boolean") $ apExpr nbFuncExpr varExpr
              , exprTypeTest "ifelse" (return "{} -> Boolean -> a -> a -> a") $ return ifelseExpr
              , exprTypeTest "list1" (return "{} -> a -> List a") $ return list1Expr
              , exprTypeTest "listNumBool" (return "{} -> List (Boolean | Number)") $ do
                    lne <- apExpr list1Expr numExpr
                    lbe <- apExpr list1Expr boolExpr
                    joinExpr lne lbe
              , exprTypeTest "listlistNumBool" (return "{} -> List (List (Boolean | Number))") $ do
                    lne <- apExpr list1Expr numExpr
                    lbe <- apExpr list1Expr boolExpr
                    llne <- apExpr list1Expr lne
                    llbe <- apExpr list1Expr lbe
                    joinExpr llne llbe
              , exprTypeTest "List Number -> List Boolean" (return "{} -> List Number -> List Boolean") $
                return listNumBoolFuncExpr
              , exprTypeTest "List Boolean -> List Number" (return "{} -> List Boolean -> List Number") $
                return listBoolNumFuncExpr
              , exprTypeTest "List nn -> List bb" (return "{} -> List Number -> List Boolean") $
                joinExpr listNumBoolFuncExpr listNumBoolFuncExpr
              , exprTypeTest "List bb -> List nn" (return "{} -> List Boolean -> List Number") $
                joinExpr listBoolNumFuncExpr listBoolNumFuncExpr
              , exprTypeTest "List nb -> List bn" (return "{} -> List (Boolean & Number) -> List (Number | Boolean)") $
                joinExpr listNumBoolFuncExpr listBoolNumFuncExpr
              , exprTypeTest "snd" (return "{} -> Any :*: a -> a") $ return sndExpr
              , exprTypeTest "thing" (return "{} -> a :*: b -> a :*: (a | b)") $ return thingExpr
              , exprTypeTest "snd . thing" (return "{} -> c :*: c -> c") $ do
                    e1 <- apExpr dotExpr sndExpr
                    apExpr e1 thingExpr
              , exprTypeTest "twice" (return "{} -> a -> a :*: a") $ return twiceExpr
              , exprTypeTest "thing . twice" (return "{} -> e -> e :*: e") $ do
                    e1 <- apExpr dotExpr thingExpr
                    apExpr e1 twiceExpr
              , exprTypeTest "thing $ twice number" (return "{} -> Number :*: Number") $ do
                    e1 <- apExpr twiceExpr numExpr
                    apExpr thingExpr e1
              , exprTypeTest "simplify $ thing $ twice number" (return "{} -> Number :*: Number") $ do
                    e1 <- apExpr twiceExpr numExpr
                    r <- apExpr thingExpr e1
                    runRenamer @TS $ simplify @TS r
              , exprTypeTest "simplify duplicate" (return "{} -> Number") $
                runRenamer @TS $
                simplify @TS $ typeFConstExpression toJMShimWit (MkJoinType (Right 3) :: JoinType Number Number)
              , exprTypeTest "simplify duplicate list" (return "{} -> List Number") $
                runRenamer @TS $
                simplify @TS $ typeFConstExpression toJMShimWit (MkJoinType (Right [3]) :: JoinType [Number] [Number])
              , exprTypeTest "simplify duplicate pair" (return "{} -> Number :*: Number") $
                runRenamer @TS $
                simplify @TS $
                typeFConstExpression
                    toJMShimWit
                    (MkJoinType (Right (3, 3)) :: JoinType (Number, Number) (Number, Number))
              , exprTypeTest "simplify duplicate in pair" (return "{} -> Number :*: Number") $
                runRenamer @TS $
                simplify @TS $
                typeFConstExpression toJMShimWit ((3, MkJoinType (Right 3)) :: (Number, JoinType Number Number))
              , exprTypeTest "simplify duplicate in pair" (return "{} -> Number :*: Number") $
                runRenamer @TS $
                simplify @TS $
                typeFConstExpression
                    toJMShimWit
                    ((MkJoinType (Right 3), MkJoinType (Right 3)) :: (JoinType Number Number, JoinType Number Number))
              , exprTypeTest "simplify duplicate in list" (return "{} -> List Number") $
                runRenamer @TS $
                simplify @TS $ typeFConstExpression toJMShimWit ([MkJoinType (Right 3)] :: [JoinType Number Number])
              ]
        , testTree
              "read"
              [ textTypeTest "v" "{v : a} -> a"
              , textTypeTest "if t then v1 else v2" "{t : Boolean, v1 : c, v2 : c} -> c"
              , textTypeTest "[]" "{} -> List None"
              , textTypeTest "\\v -> 1" "{} -> Any -> Integer"
              , textTypeTest "[v1,v2]" "{v1 : a, v2 : a} -> List1 a"
              , textTypeTest "[v,v,v]" "{v : a, v : a, v : a} -> List1 a"
              , textTypeTest "[x,y,x,y]" "{x : a, y : a, x : a, y : a} -> List1 a"
              , textTypeTest "(v 3,v \"text\")" "{v : Integer -> a, v : Text -> b} -> a :*: b"
              , textTypeTest "(v,v)" "{v : a, v : b} -> a :*: b"
              , textTypeTest "(v 3,v 3)" "{v : Integer -> a, v : Integer -> b} -> a :*: b"
              , textTypeTest "[v 3]" "{v : Integer -> a} -> List1 a"
              , textTypeTest "(v 3,v False)" "{v : Integer -> a, v : Boolean -> b} -> a :*: b"
              , textTypeTest
                    "((v 3,v False),v 3)"
                    "{v : Integer -> c, v : Boolean -> d, v : Integer -> b} -> (c :*: d) :*: b"
              , testTree
                    "function"
                    [ textTypeTest "let i: tvar -> tvar; i = id in i" "{} -> tvar -> tvar"
                    , textTypeTest "id: tvar -> tvar" "{} -> tvar -> tvar"
                    , textTypeTest "let i x = x in i" "{} -> a -> a"
                    , textTypeTest "let i : a -> a; i x = x in i" "{} -> a -> a"
                    , textTypeTest "let i : tvar -> tvar; i x = x in i" "{} -> tvar -> tvar"
                    , textTypeTest "let i : a -> a; i x = x in i 3" "{} -> Integer"
                    ]
              , textTypeTest "\\x -> let v = x in [v,v,v]" "{} -> a -> List1 a"
              , textTypeTest "\\v1 v2 -> [v1,v2]" "{} -> a -> a -> List1 a"
              , textTypeTest "\\v1 v2 v3 -> ([v1,v2],[v2,v3])" "{} -> c -> (c & a) -> a -> List1 c :*: List1 a"
              , textTypeTest
                    "\\v1 v2 v3 -> (([v1,v2],[v2,v3]),[v3,v1])"
                    "{} -> (c & a) -> (c & d) -> (d & a) -> (List1 c :*: List1 d) :*: List1 a"
              , testTree
                    "inversion"
                    [ textTypeTest "\\x -> let y : Integer; y = x in y" "{} -> Integer -> Integer"
                    , badInterpretTest "\\x -> let y : Boolean | Number; y = x in y"
                    , badInterpretTest "\\x -> let y : (a -> a) :*: (Boolean | Number); y = x in y"
                    , badInterpretTest "\\x -> let y : (b -> b) :*: (Boolean | Number); y = x in y"
                    , textTypeTest
                          "\\x -> let y: Boolean :*: Number; y = (x,x) in y"
                          "{} -> (Number & Boolean) -> Boolean :*: Number"
                    , textTypeTest
                          "\\x1 -> \\x2 -> let y: Boolean :*: Number; y = (x1,x2) in y"
                          "{} -> Boolean -> Number -> Boolean :*: Number"
                    , textTypeTest
                          "\\x1 -> \\x2 -> let y: (a -> a) :*: (a -> a :*: a); y = (x1,x2) in y"
                          "{} -> (a -> a) -> (a -> a :*: a) -> (a -> a) :*: (a -> a :*: a)"
                    , textTypeTest
                          "\\x1 -> \\x2 -> let y: (a -> a) :*: (b -> b :*: b); y = (x1,x2) in y"
                          "{} -> (a -> a) -> (b -> b :*: b) -> (a -> a) :*: (b -> b :*: b)"
                    , textTypeTest
                          "\\x1 -> \\x2 -> let y: (b -> b) :*: (a -> a :*: a); y = (x1,x2) in y"
                          "{} -> (b -> b) -> (a -> a :*: a) -> (b -> b) :*: (a -> a :*: a)"
                    , textTypeTest
                          "\\x1 -> \\x2 -> let y: (a -> b) :*: (b -> a); y = (x1,x2) in y"
                          "{} -> (a -> b) -> (b -> a) -> (a -> b) :*: (b -> a)"
                    , textTypeTest
                          "\\x1 -> \\x2 -> let y: (c -> d) :*: (d -> c); y = (x1,x2) in y"
                          "{} -> (c -> d) -> (d -> c) -> (c -> d) :*: (d -> c)"
                    ]
              , textTypeTest "let f : Entity; f = Nothing in f" "{} -> Entity"
              , textTypeTest "let f : Entity -> Entity; f = Just in f" "{} -> Entity -> Entity"
              , textTypeTest "let f : Entity; f = [] in f" "{} -> Entity"
              , textTypeTest "let f : Entity -> Entity; f x = [x] in f" "{} -> Entity -> Entity"
              , textTypeTest "let f : Entity -> Entity -> Entity; f a b = (a,b) in f" "{} -> Entity -> Entity -> Entity"
              , textTypeTest "let f : Entity -> Entity; f = Left in f" "{} -> Entity -> Entity"
              , textTypeTest "let f : Entity -> Entity; f = Right in f" "{} -> Entity -> Entity"
              , textTypeTest "\\x -> if odd x then x else (x:Number)" "{} -> Integer -> Number"
              , testTree
                    "recursive"
                    [ textTypeTest "let x : rec a. Maybe a; x = Nothing in x" "{} -> rec a. Maybe a"
                    , textTypeTest "let rec x : rec a. Maybe a; x = Just x end in x" "{} -> rec a. Maybe a"
                    , textTypeTest "let rec x = Just x end in x" "{} -> rec e. Maybe e"
                    , textTypeTest "let rec x : Entity; x = Just x end in x" "{} -> Entity"
                    , textTypeTest "let rec x : Maybe Entity; x = Just x end in x" "{} -> Maybe Entity"
                    , textTypeTest
                          "let rec rcount x = case x of Nothing -> 0; Just y -> 1 + rcount y end end in rcount"
                          "{} -> (rec e. Maybe e) -> Integer"
                    , textTypeTest "Just $ Just $ Just Nothing" "{} -> Maybe (Maybe (Maybe (Maybe None)))"
                    , textTypeTest
                          "let rec rcount x = case x of Nothing -> 0; Just y -> 1 + r1count y end; r1count x = case x of Nothing -> 0; Just y -> 1 + r1count y end end in rcount $ Just $ Just $ Just Nothing"
                          "{} -> Integer"
                    , textTypeTest
                          "let rec rcount x = case x of Nothing -> 0; Just y -> 1 + rcount y end end; rec rval = Just rval end in (rcount,(rval,rcount rval))"
                          "{} -> ((rec e. Maybe e) -> Integer) :*: (rec e. Maybe e) :*: Integer"
                    ]
              ]
        , testTree
              "simplify"
              [ simplifyTypeTest "a" "None"
              , simplifyTypeTest "a -> (a|a)" "a -> a"
              , simplifyTypeTest "a -> b -> (a|b)" "a -> a -> a"
              , simplifyTypeTest "a :*: b -> (a|b)" "a :*: a -> a"
              , simplifyTypeTest "a :*: b -> a :*: (a | b)" "a :*: b -> a :*: (a | b)"
              , simplifyTypeTest "a :*: b -> b :*: (a | b)" "a :*: b -> b :*: (a | b)"
              , simplifyTypeTest "(a&b) -> a :*: b" "a -> a :*: a"
              , simplifyTypeTest "(a & Integer) -> Boolean" "Integer -> Boolean"
              , simplifyTypeTest "(b & Integer) -> Integer" "Integer -> Integer"
              , simplifyTypeTest "(a & Integer) -> b" "Integer -> None"
              , simplifyTypeTest "(a & Integer) -> a" "(a & Integer) -> a"
              , simplifyTypeTest "(a & Integer) -> (a | Number)" "Integer -> Number"
              , testTree
                    "subtype"
                    [ simplifyTypeTest "Boolean | Integer" "Boolean | Integer"
                    , simplifyTypeTest "Integer | Boolean" "Integer | Boolean"
                    , simplifyTypeTest "(Boolean & Integer) -> Unit" "(Boolean & Integer) -> Unit"
                    , simplifyTypeTest "(Integer & Boolean) -> Unit" "(Integer & Boolean) -> Unit"
                    , simplifyTypeTest "Literal | Integer" "Literal"
                    , simplifyTypeTest "Integer | Literal" "Literal"
                    , simplifyTypeTest "List Literal | List Integer" "List Literal"
                    , simplifyTypeTest "List Integer | List Literal" "List Literal"
                    , simplifyTypeTest "(Literal & Integer) -> Unit" "Integer -> Unit"
                    , simplifyTypeTest "(Integer & Literal) -> Unit" "Integer -> Unit"
                    , simplifyTypeTest "(List Literal & List Integer) -> Unit" "List Integer -> Unit"
                    , simplifyTypeTest "(List Integer & List Literal) -> Unit" "List Integer -> Unit"
                    ]
              , testTree
                    "recursive"
                    [ simplifyTypeTest "rec a. a" "None"
                    , simplifyTypeTest "rec a. (a | Maybe a)" "rec a. Maybe a"
                    , simplifyTypeTest "rec a. (a | Integer)" "Integer"
                    , simplifyTypeTest "rec a. Maybe a" "rec a. Maybe a"
                    , simplifyTypeTest "rec a. Integer" "Integer"
                    , simplifyTypeTest "Maybe (rec a. a)" "Maybe None"
                    , simplifyTypeTest "Maybe (rec a. List a)" "Maybe (rec a. List a)"
                    , simplifyTypeTest "Maybe (rec a. Integer)" "Maybe Integer"
                    , expectFailBecause "ISSUE #61" $ simplifyTypeTest "rec a. rec b. a :*: b" "rec b. b :*: b"
                    , expectFailBecause "ISSUE #61" $
                      simplifyTypeTest "(rec a. Maybe a) | (rec b. List b)" "rec a. Maybe a | List a"
                    , expectFailBecause "ISSUE #61" $
                      simplifyTypeTest "(rec a. Maybe a) | (rec a. List a)" "rec a. Maybe a | List a"
                    , testTree
                          "roll"
                          [ simplifyTypeTest "Maybe (rec a. Maybe a)" "rec a. Maybe a"
                          , simplifyTypeTest "Maybe (Maybe (rec a. Maybe a))" "rec a. Maybe a"
                          , simplifyTypeTest "Any -> Maybe (rec a. Maybe a)" "Any -> (rec a. Maybe a)"
                          , unrollTest "rec a. Maybe a" "Maybe (rec a. Maybe a)"
                          ]
                    ]
              , testTree
                    "fullconstraint"
                    [ simplifyTypeTest "Integer | d" "Integer"
                    , simplifyTypeTest "(rec a. Maybe a) | d" "rec a. Maybe a"
                    , simplifyTypeTest "rec a. (Maybe a | d)" "rec a. Maybe a"
                    , simplifyTypeTest "Maybe (rec a. Maybe a | d)" "rec a. Maybe a"
                    , simplifyTypeTest "d -> Maybe (rec a. Maybe a | d)" "d -> Maybe (rec a. Maybe a | d)"
                    , simplifyTypeTest "(a -> Literal) | ((Text & b) -> a)" "Text -> Literal"
                    , simplifyTypeTest "(a & Text) -> (Literal | a)" "Text -> Literal"
                    ]
              ]
        , testTree
              "library"
              [ textTypeTest "()" "{} -> Unit"
              , textTypeTest "{3}" "{} -> WholeRef +Integer"
              , textTypeTest "identity !$% {3}" "{} -> WholeRef +Integer"
              , textTypeTest "identity !$ {3}" "{} -> WholeRef +Integer"
              , textTypeTest "(identity !. identity) !$% {3}" "{} -> WholeRef +Integer"
              , textTypeTest "(identity !. identity) !$ {3}" "{} -> WholeRef +Integer"
              , textTypeTest
                    "property @Integer @Text !\"a\" !** property @Number @Text !\"b\""
                    "{} -> {-Integer,+Number} ~> (Text :*: Text)"
              , textTypeTest
                    "property @Text @Integer !\"a\" !++ property @Text @Number !\"b\""
                    "{} -> (Text :+: Text) ~> {-Integer,+Number}"
              , textTypeTest
                    "(property @Integer @Text !\"a\" !** property @Number @Text !\"b\") !$% {3}"
                    "{} -> WholeRef (Text :*: Text)"
              , textTypeTest
                    "(property @Integer @Text !\"a\" !** property @Number @Text !\"b\") !$ {3}"
                    "{} -> WholeRef (Text :*: Text)"
              , textTypeTest "property @Integer @Text !\"a\" !@% {\"x\"}" "{} -> FiniteSetRef Integer"
              , textTypeTest "property @Integer @Text !\"a\" !@ {\"x\"}" "{} -> FiniteSetRef Integer"
              , textTypeTest
                    "(property @Integer @Text !\"a\" !@% {\"x\"}) <:*:> (property @Number @Text !\"b\" !@% {\"y\"})"
                    "{} -> FiniteSetRef (Integer :*: Number)"
              , textTypeTest "pairWhole {3} {\"x\"}" "{} -> WholeRef {-(Any :*: Any),+(Integer :*: Text)}"
              , textTypeTest "immutWhole $ pairWhole {3} {\"x\"}" "{} -> WholeRef +(Integer :*: Text)"
              ]
        ]
