module Test.Type
    ( testType
    ) where

import Data.Shim
import Language.Expression.Common
import Pinafore
import Pinafore.Test
import Shapes
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit

type TS = PinaforeTypeSystem

type PExpression = TSSealedExpression TS

showVars :: NamedExpression Name (PinaforeShimWit 'Negative) t -> [String]
showVars (ClosedExpression _) = []
showVars (OpenExpression (MkNameWitness name (MkShimWit t _)) expr) =
    (show name <> " : " <> unpack (exprShow t)) : showVars expr

showTypes :: PExpression -> String
showTypes (MkSealedExpression (MkShimWit t _) expr) =
    "{" <> intercalate ", " (showVars expr) <> "} -> " <> unpack (exprShow t)

exprTypeTest :: String -> Maybe String -> PinaforeSourceScoped PExpression -> TestTree
exprTypeTest name expected mexpr =
    testCase name $
    assertEqual "" expected $ do
        expr <- resultToMaybe $ runTestPinaforeSourceScoped mexpr
        return $ showTypes expr

apExpr :: PExpression -> PExpression -> PinaforeSourceScoped PExpression
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
varExpr = tsVar @TS "v"

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

joinExpr :: PExpression -> PExpression -> PinaforeSourceScoped PExpression
joinExpr exp1 exp2 = do
    je <- apExpr ifelseExpr boolExpr
    e <- apExpr je exp1
    apExpr e exp2

textTypeTest :: Text -> String -> TestTree
textTypeTest text r =
    testCase (unpack text) $ do
        expr <- throwResult $ runTestPinaforeSourceScoped $ parseTopExpression text
        assertEqual "" r $ showTypes expr

badInterpretTest :: Text -> TestTree
badInterpretTest text =
    testCase (unpack text) $
    case runTestPinaforeSourceScoped $ parseTopExpression text of
        FailureResult _ -> return ()
        SuccessResult _ -> assertFailure "no exception"

simplifyTypeTest :: Text -> String -> TestTree
simplifyTypeTest text e =
    testCase (unpack text) $ do
        simpexpr <-
            throwResult $
            runTestPinaforeSourceScoped $ do
                mt <- parseType @'Positive text
                case mt of
                    MkAnyW t ->
                        runRenamer @PinaforeTypeSystem $
                        simplify @PinaforeTypeSystem @PExpression $
                        MkSealedExpression (mkShimWit t) $ ClosedExpression undefined
        case simpexpr of
            MkSealedExpression (MkShimWit t' _) _ -> assertEqual "" e $ unpack $ exprShow t'

testType :: TestTree
testType =
    testGroup
        "type"
        [ testGroup
              "pure"
              [ exprTypeTest "number" (return "{} -> Number") $ return numExpr
              , exprTypeTest "boolean" (return "{} -> Boolean") $ return boolExpr
              , exprTypeTest "id" (return "{} -> x -> x") $ return idExpr
              , exprTypeTest "nb" (return "{} -> Number -> Boolean") $ return nbFuncExpr
              , exprTypeTest "var" (return "{v : a} -> a") $ return varExpr
              , exprTypeTest "apply id number" (return "{} -> Number") $ apExpr idExpr numExpr
              , exprTypeTest "apply nb number" (return "{} -> Boolean") $ apExpr nbFuncExpr numExpr
              , exprTypeTest "apply nb boolean" Nothing $ apExpr nbFuncExpr boolExpr
              , exprTypeTest "apply id var" (return "{v : c} -> c") $ apExpr idExpr varExpr
              , exprTypeTest "apply nb var" (return "{v : Number} -> Boolean") $ apExpr nbFuncExpr varExpr
              , exprTypeTest "ifelse" (return "{} -> Boolean -> a -> a -> a") $ return ifelseExpr
              , exprTypeTest "list1" (return "{} -> a -> [a]") $ return list1Expr
              , exprTypeTest "listNumBool" (return "{} -> [Boolean | Number]") $ do
                    lne <- apExpr list1Expr numExpr
                    lbe <- apExpr list1Expr boolExpr
                    joinExpr lne lbe
              , exprTypeTest "listlistNumBool" (return "{} -> [[Boolean | Number]]") $ do
                    lne <- apExpr list1Expr numExpr
                    lbe <- apExpr list1Expr boolExpr
                    llne <- apExpr list1Expr lne
                    llbe <- apExpr list1Expr lbe
                    joinExpr llne llbe
              , exprTypeTest "[number] -> [boolean]" (return "{} -> [Number] -> [Boolean]") $ return listNumBoolFuncExpr
              , exprTypeTest "[boolean] -> [number]" (return "{} -> [Boolean] -> [Number]") $ return listBoolNumFuncExpr
              , exprTypeTest "[nn] -> [bb]" (return "{} -> [Number] -> [Boolean]") $
                joinExpr listNumBoolFuncExpr listNumBoolFuncExpr
              , exprTypeTest "[bb] -> [nn]" (return "{} -> [Boolean] -> [Number]") $
                joinExpr listBoolNumFuncExpr listBoolNumFuncExpr
              , exprTypeTest "[nb] -> [bn]" (return "{} -> [Boolean & Number] -> [Number | Boolean]") $
                joinExpr listNumBoolFuncExpr listBoolNumFuncExpr
              , exprTypeTest "snd" (return "{} -> (Any, a) -> a") $ return sndExpr
              , exprTypeTest "thing" (return "{} -> (a, b) -> (a, a | b)") $ return thingExpr
              , exprTypeTest "snd . thing" (return "{} -> (c, c) -> c") $ do
                    e1 <- apExpr dotExpr sndExpr
                    apExpr e1 thingExpr
              , exprTypeTest "twice" (return "{} -> a -> (a, a)") $ return twiceExpr
              , expectFail $
                exprTypeTest "thing . twice" (return "{} -> a -> (a, a)") $ do
                    e1 <- apExpr dotExpr thingExpr
                    apExpr e1 twiceExpr
              , exprTypeTest "thing $ twice number" (return "{} -> (Number, Number)") $ do
                    e1 <- apExpr twiceExpr numExpr
                    apExpr thingExpr e1
              , exprTypeTest "simplify $ thing $ twice number" (return "{} -> (Number, Number)") $ do
                    e1 <- apExpr twiceExpr numExpr
                    r <- apExpr thingExpr e1
                    runRenamer @TS $ simplify @TS r
              , exprTypeTest "simplify duplicate" (return "{} -> Number") $
                runRenamer @TS $
                simplify @TS $ typeFConstExpression toJMShimWit (MkJoinType (Right 3) :: JoinType Number Number)
              , exprTypeTest "simplify duplicate list" (return "{} -> [Number]") $
                runRenamer @TS $
                simplify @TS $ typeFConstExpression toJMShimWit (MkJoinType (Right [3]) :: JoinType [Number] [Number])
              , exprTypeTest "simplify duplicate pair" (return "{} -> (Number, Number)") $
                runRenamer @TS $
                simplify @TS $
                typeFConstExpression
                    toJMShimWit
                    (MkJoinType (Right (3, 3)) :: JoinType (Number, Number) (Number, Number))
              , exprTypeTest "simplify duplicate in pair" (return "{} -> (Number, Number)") $
                runRenamer @TS $
                simplify @TS $
                typeFConstExpression toJMShimWit ((3, MkJoinType (Right 3)) :: (Number, JoinType Number Number))
              , exprTypeTest "simplify duplicate in pair" (return "{} -> (Number, Number)") $
                runRenamer @TS $
                simplify @TS $
                typeFConstExpression
                    toJMShimWit
                    ((MkJoinType (Right 3), MkJoinType (Right 3)) :: (JoinType Number Number, JoinType Number Number))
              , exprTypeTest "simplify duplicate in list" (return "{} -> [Number]") $
                runRenamer @TS $
                simplify @TS $ typeFConstExpression toJMShimWit ([MkJoinType (Right 3)] :: [JoinType Number Number])
              ]
        , testGroup
              "read"
              [ textTypeTest "v" "{v : a} -> a"
              , textTypeTest "if t then v1 else v2" "{t : Boolean, v1 : c, v2 : c} -> c"
              , textTypeTest "[]" "{} -> [None]"
              , textTypeTest "\\v -> 1" "{} -> Any -> Integer"
              , textTypeTest "[v1,v2]" "{v1 : a, v2 : a} -> [a]"
              , textTypeTest "[v,v,v]" "{v : a, v : a, v : a} -> [a]"
              , textTypeTest "[x,y,x,y]" "{x : a, y : a, x : a, y : a} -> [a]"
              , textTypeTest "(v 3,v \"text\")" "{v : Integer -> a, v : Text -> b} -> (a, b)"
              , textTypeTest "(v,v)" "{v : a, v : b} -> (a, b)"
              , textTypeTest "(v 3,v 3)" "{v : Integer -> a, v : Integer -> b} -> (a, b)"
              , textTypeTest "[v 3]" "{v : Integer -> a} -> [a]"
              , textTypeTest "(v 3,v False)" "{v : Integer -> a, v : Boolean -> b} -> (a, b)"
              , textTypeTest
                    "((v 3,v False),v 3)"
                    "{v : Integer -> c, v : Boolean -> d, v : Integer -> b} -> ((c, d), b)"
              , textTypeTest "let v = x in [v,v,v]" "{x : a, x : a, x : a} -> [a]"
              , textTypeTest "\\x -> let v = x in [v,v,v]" "{} -> a -> [a]"
              , textTypeTest "\\v1 v2 -> [v1,v2]" "{} -> a -> a -> [a]"
              , textTypeTest "\\v1 v2 v3 -> ([v1,v2],[v2,v3])" "{} -> c -> (a & c) -> a -> ([c], [a])"
              , textTypeTest
                    "\\v1 v2 v3 -> (([v1,v2],[v2,v3]),[v3,v1])"
                    "{} -> (a & c) -> (d & c) -> (a & d) -> (([c], [d]), [a])"
              , testGroup
                    "inversion"
                    [ textTypeTest "\\x -> let y : Integer; y = x in y" "{} -> Integer -> Integer"
                    , badInterpretTest "\\x -> let y : Boolean | Number; y = x in y"
                    , badInterpretTest "\\x -> let y : (a -> a, Boolean | Number); y = x in y"
                    , badInterpretTest "\\x -> let y : (b -> b, Boolean | Number); y = x in y"
                    , textTypeTest
                          "\\x -> let y : (Boolean, Number); y = (x,x) in y"
                          "{} -> (Number & Boolean) -> (Boolean, Number)"
                    , textTypeTest
                          "\\x1 -> \\x2 -> let y : (Boolean, Number); y = (x1,x2) in y"
                          "{} -> Boolean -> Number -> (Boolean, Number)"
                    , textTypeTest
                          "\\x1 -> \\x2 -> let y : (a -> a, a -> (a,a)); y = (x1,x2) in y"
                          "{} -> (a -> a) -> (a -> (a, a)) -> (a -> a, a -> (a, a))"
                    , textTypeTest
                          "\\x1 -> \\x2 -> let y : (a -> a, b -> (b,b)); y = (x1,x2) in y"
                          "{} -> (a -> a) -> (b -> (b, b)) -> (a -> a, b -> (b, b))"
                    , textTypeTest
                          "\\x1 -> \\x2 -> let y : (b -> b, a -> (a,a)); y = (x1,x2) in y"
                          "{} -> (b -> b) -> (a -> (a, a)) -> (b -> b, a -> (a, a))"
                    , textTypeTest
                          "\\x1 -> \\x2 -> let y : (a -> b, b -> a); y = (x1,x2) in y"
                          "{} -> (a -> b) -> (b -> a) -> (a -> b, b -> a)"
                    , textTypeTest
                          "\\x1 -> \\x2 -> let y : (c -> d, d -> c); y = (x1,x2) in y"
                          "{} -> (c -> d) -> (d -> c) -> (c -> d, d -> c)"
                    ]
              , textTypeTest "let f : Entity; f = Nothing in f" "{} -> Entity"
              , textTypeTest "let f : Entity -> Entity; f = Just in f" "{} -> Entity -> Entity"
              , textTypeTest "let f : Entity; f = [] in f" "{} -> Entity"
              , textTypeTest "let f : Entity -> Entity; f x = [x] in f" "{} -> Entity -> Entity"
              , textTypeTest "let f : Entity -> Entity -> Entity; f a b = (a,b) in f" "{} -> Entity -> Entity -> Entity"
              , textTypeTest "let f : Entity -> Entity; f = Left in f" "{} -> Entity -> Entity"
              , textTypeTest "let f : Entity -> Entity; f = Right in f" "{} -> Entity -> Entity"
              , testGroup
                    "recursive"
                    [ textTypeTest "let x : rec a. Maybe a; x = Nothing in x" "{} -> rec a. Maybe a"
                    , textTypeTest "let x : rec a. Maybe a; x = Just x in x" "{} -> rec a. Maybe a"
                    , textTypeTest "let x = Just x in x" "{} -> rec d. Maybe d"
                    , textTypeTest "let x : Entity; x = Just x in x" "{} -> Entity"
                    , textTypeTest "let x : Maybe Entity; x = Just x in x" "{} -> Maybe Entity"
                    , textTypeTest
                          "let rcount x = case x of Nothing -> 0; Just y -> 1 + rcount y end in rcount"
                          "{} -> (rec e. Maybe e) -> Integer"
                    , textTypeTest "Just $ Just $ Just Nothing" "{} -> Maybe (Maybe (Maybe (Maybe None)))"
                    , textTypeTest
                          "let rcount x = case x of Nothing -> 0; Just y -> 1 + r1count y end; r1count x = case x of Nothing -> 0; Just y -> 1 + r1count y end in rcount $ Just $ Just $ Just Nothing"
                          "{} -> Integer"
                    ]
              ]
        , testGroup
              "simplify"
              [ simplifyTypeTest "a" "None"
              , simplifyTypeTest "a -> (a|a)" "a -> a"
              , simplifyTypeTest "a -> b -> (a|b)" "a -> a -> a"
              , simplifyTypeTest "(a,b) -> (a|b)" "(a, a) -> a"
              , simplifyTypeTest "(a,b) -> (a,a|b)" "(a, b) -> (a, a | b)"
              , simplifyTypeTest "(a,b) -> (b,a|b)" "(a, b) -> (b, a | b)"
              , simplifyTypeTest "(a&b) -> (a,b)" "a -> (a, a)"
              , simplifyTypeTest "(a & Integer) -> Boolean" "Integer -> Boolean"
              , simplifyTypeTest "(b & Integer) -> Integer" "Integer -> Integer"
              , simplifyTypeTest "(a & Integer) -> b" "Integer -> None"
              , simplifyTypeTest "(a & Integer) -> a" "(a & Integer) -> a"
              , testGroup
                    "subtype"
                    [ simplifyTypeTest "Boolean | Integer" "Boolean | Integer"
                    , simplifyTypeTest "Integer | Boolean" "Integer | Boolean"
                    , simplifyTypeTest "(Boolean & Integer) -> ()" "(Boolean & Integer) -> ()"
                    , simplifyTypeTest "(Integer & Boolean) -> ()" "(Integer & Boolean) -> ()"
                    , simplifyTypeTest "Literal | Integer" "Literal"
                    , simplifyTypeTest "Integer | Literal" "Literal"
                    , simplifyTypeTest "[Literal] | [Integer]" "[Literal]"
                    , simplifyTypeTest "[Integer] | [Literal]" "[Literal]"
                    , simplifyTypeTest "(Literal & Integer) -> ()" "Integer -> ()"
                    , simplifyTypeTest "(Integer & Literal) -> ()" "Integer -> ()"
                    , simplifyTypeTest "([Literal] & [Integer]) -> ()" "[Integer] -> ()"
                    , simplifyTypeTest "([Integer] & [Literal]) -> ()" "[Integer] -> ()"
                    ]
              , testGroup
                    "recursive"
                    [ simplifyTypeTest "rec a. a" "None"
                    , simplifyTypeTest "rec a. Maybe a" "rec a. Maybe a"
                    , simplifyTypeTest "rec a. Integer" "Integer"
                    , simplifyTypeTest "Maybe (rec a. a)" "Maybe None"
                    , simplifyTypeTest "Maybe (rec a. [a])" "Maybe (rec a. [a])"
                    , simplifyTypeTest "Maybe (rec a. Integer)" "Maybe Integer"
                    , simplifyTypeTest "rec a. rec b. (a, b)" "rec b. (b, b)"
                    , simplifyTypeTest "(rec a. Maybe a) | (rec b. [b])" "rec a. Maybe a | [a]"
                    , simplifyTypeTest "(rec a. Maybe a) | (rec a. [a])" "rec a. Maybe a | [a]"
                    ]
              ]
        ]
