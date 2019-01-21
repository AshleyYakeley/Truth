module Test.Type
    ( testType
    ) where

import Language.Expression.Dolan
import Language.Expression.Expression
import Language.Expression.Named
import Language.Expression.Polarity
import Language.Expression.Renamer
import Language.Expression.Sealed
import Language.Expression.TypeSystem
import Language.Expression.Unifier
import Pinafore
import Pinafore.Test
import Shapes
import Test.Tasty
import Test.Tasty.HUnit

type TS = PinaforeTypeSystem PinaforeEdit

type PExpression = TSSealedExpression TS

showVars :: NamedExpression Name (PinaforeType PinaforeEdit 'Negative) t -> [String]
showVars (ClosedExpression _) = []
showVars (OpenExpression (MkNameWitness name t) expr) = (show name <> " :: " <> show t) : showVars expr

showTypes :: PExpression -> String
showTypes (MkSealedExpression t expr) = "{" <> intercalate ", " (showVars expr) <> "} -> " <> show t

exprTypeTest :: String -> Result Text String -> PinaforeSourceScoped PinaforeEdit PExpression -> TestTree
exprTypeTest name expected mexpr =
    testCase name $
    assertEqual "" expected $ do
        expr <- runSourceScoped (initialPos "<input>") mexpr
        return $ showTypes expr

apExpr :: PExpression -> PExpression -> PinaforeSourceScoped PinaforeEdit PExpression
apExpr = tsApply @TS

idExpr :: PExpression
idExpr = typeFConstExpression toTypeF $ \(v :: UVar "x") -> v

nbFuncExpr :: PExpression
nbFuncExpr = typeFConstExpression toTypeF $ \(_ :: Number) -> False

numExpr :: PExpression
numExpr = typeFConstExpression toTypeF $ (3 :: Number)

boolExpr :: PExpression
boolExpr = typeFConstExpression toTypeF False

varExpr :: PExpression
varExpr = tsVar @TS "v"

ifelseExpr :: PExpression
ifelseExpr =
    typeFConstExpression toTypeF $ \test (tb :: UVar "a") (eb :: UVar "a") ->
        if test
            then tb
            else eb

list1Expr :: PExpression
list1Expr = typeFConstExpression toTypeF $ \(a :: UVar "a") -> [a]

sndExpr :: PExpression
sndExpr = typeFConstExpression toTypeF $ \(MkTopType, a :: UVar "a") -> a

twiceExpr :: PExpression
twiceExpr = typeFConstExpression toTypeF $ \(a :: UVar "a") -> (a, a)

thingExpr :: PExpression
thingExpr =
    typeFConstExpression toTypeF $ \(a :: UVar "a", b :: UVar "b") ->
        ( a
        , if False
              then MkJoinType $ Left a
              else MkJoinType $ Right b)

dotExpr :: PExpression
dotExpr = typeFConstExpression toTypeF $ \(f :: UVar "b" -> UVar "c") (g :: UVar "a" -> UVar "b") -> f . g

listNumBoolFuncExpr :: PExpression
listNumBoolFuncExpr = typeFConstExpression toTypeF $ \(_ :: [Number]) -> [True]

listBoolNumFuncExpr :: PExpression
listBoolNumFuncExpr = typeFConstExpression toTypeF $ \(_ :: [Bool]) -> [2 :: Number]

joinExpr :: PExpression -> PExpression -> PinaforeSourceScoped PinaforeEdit PExpression
joinExpr exp1 exp2 = do
    je <- apExpr ifelseExpr boolExpr
    e <- apExpr je exp1
    apExpr e exp2

textTypeTest :: Text -> String -> TestTree
textTypeTest text r =
    testCase (unpack text) $ do
        expr <- resultTextToM $ withNullPinaforeContext $ parseExpression @PinaforeEdit (initialPos "<input>") text
        assertEqual "" r $ showTypes expr

simplifyTypeTest :: Text -> String -> TestTree
simplifyTypeTest text e =
    testCase (unpack text) $ do
        simpexpr <-
            resultTextToM $ do
                MkAnyW t <- runSourceScoped (initialPos "<input>") $ parseType @PinaforeEdit @'Positive text
                return $
                    pinaforeSimplifyTypes @PinaforeEdit @PExpression $ MkSealedExpression t $ ClosedExpression undefined
        case simpexpr of
            MkSealedExpression t' _ -> assertEqual "" e $ show t'

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
              , exprTypeTest "var" (return "{v :: a} -> a") $ return varExpr
              , exprTypeTest "apply id number" (return "{} -> Number") $ apExpr idExpr numExpr
              , exprTypeTest "apply nb number" (return "{} -> Boolean") $ apExpr nbFuncExpr numExpr
              , exprTypeTest
                    "apply nb boolean"
                    (fail "\"<input>\" (line 1, column 1): cannot convert Boolean to Number") $
                apExpr nbFuncExpr boolExpr
              , exprTypeTest "apply id var" (return "{v :: c} -> c") $ apExpr idExpr varExpr
              , exprTypeTest "apply nb var" (return "{v :: Number} -> Boolean") $ apExpr nbFuncExpr varExpr
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
              {-
              , exprTypeTest "thing . twice" (return "{} -> a -> (a, a)") $ do
                    e1 <- apExpr dotExpr thingExpr
                    apExpr e1 twiceExpr
              -}
              , exprTypeTest "thing $ twice number" (return "{} -> (Number, Number)") $ do
                    e1 <- apExpr twiceExpr numExpr
                    apExpr thingExpr e1
              , exprTypeTest "simplify $ thing $ twice number" (return "{} -> (Number, Number)") $ do
                    e1 <- apExpr twiceExpr numExpr
                    r <- apExpr thingExpr e1
                    runRenamer @(TSRenamer TS) $ simplify @(TSUnifier TS) r
              , exprTypeTest "simplify duplicate" (return "{} -> Number") $
                runRenamer @(TSRenamer TS) $
                simplify @(TSUnifier TS) $ typeFConstExpression toTypeF (MkJoinType (Right 3) :: JoinType Number Number)
              , exprTypeTest "simplify duplicate list" (return "{} -> [Number]") $
                runRenamer @(TSRenamer TS) $
                simplify @(TSUnifier TS) $
                typeFConstExpression toTypeF (MkJoinType (Right [3]) :: JoinType [Number] [Number])
              , exprTypeTest "simplify duplicate pair" (return "{} -> (Number, Number)") $
                runRenamer @(TSRenamer TS) $
                simplify @(TSUnifier TS) $
                typeFConstExpression toTypeF (MkJoinType (Right (3, 3)) :: JoinType (Number, Number) (Number, Number))
              , exprTypeTest "simplify duplicate in pair" (return "{} -> (Number, Number)") $
                runRenamer @(TSRenamer TS) $
                simplify @(TSUnifier TS) $
                typeFConstExpression toTypeF ((3, MkJoinType (Right 3)) :: (Number, JoinType Number Number))
              , exprTypeTest "simplify duplicate in pair" (return "{} -> (Number, Number)") $
                runRenamer @(TSRenamer TS) $
                simplify @(TSUnifier TS) $
                typeFConstExpression
                    toTypeF
                    ((MkJoinType (Right 3), MkJoinType (Right 3)) :: (JoinType Number Number, JoinType Number Number))
              , exprTypeTest "simplify duplicate in list" (return "{} -> [Number]") $
                runRenamer @(TSRenamer TS) $
                simplify @(TSUnifier TS) $
                typeFConstExpression toTypeF ([MkJoinType (Right 3)] :: [JoinType Number Number])
              ]
        , testGroup
              "read"
              [ textTypeTest "v" "{v :: a} -> a"
              , textTypeTest "if t then v1 else v2" "{t :: Boolean, v1 :: c, v2 :: c} -> c"
              , textTypeTest "[]" "{} -> [None]"
              , textTypeTest "\\v -> 1" "{} -> Any -> Number"
              , textTypeTest "[v1,v2]" "{v1 :: a, v2 :: a} -> [a]"
              , textTypeTest "[v,v,v]" "{v :: a, v :: a, v :: a} -> [a]"
              , textTypeTest "[x,y,x,y]" "{x :: a, y :: a, x :: a, y :: a} -> [a]"
              , textTypeTest "(v 3,v \"text\")" "{v :: Number -> a, v :: Text -> b} -> (a, b)"
              , textTypeTest "(v,v)" "{v :: a, v :: b} -> (a, b)"
              , textTypeTest "(v 3,v 3)" "{v :: Number -> a, v :: Number -> b} -> (a, b)"
              , textTypeTest "[v 3]" "{v :: Number -> a} -> [a]"
              , textTypeTest "(v 3,v False)" "{v :: Number -> a, v :: Boolean -> b} -> (a, b)"
              , textTypeTest
                    "((v 3,v False),v 3)"
                    "{v :: Number -> a', v :: Boolean -> b', v :: Number -> b} -> ((a', b'), b)"
              , textTypeTest "let v = x in [v,v,v]" "{x :: a, x :: a, x :: a} -> [a]"
              , textTypeTest "\\x -> let v = x in [v,v,v]" "{} -> a -> [a]"
              , textTypeTest "\\v1 v2 -> [v1,v2]" "{} -> a -> a -> [a]"
              , textTypeTest "\\v1 v2 v3 -> ([v1,v2],[v2,v3])" "{} -> a' -> (a & a') -> a -> ([a'], [a])"
              , textTypeTest
                    "\\v1 v2 v3 -> (([v1,v2],[v2,v3]),[v3,v1])"
                    "{} -> (a & a') -> (a'' & a') -> (a & a'') -> (([a'], [a'']), [a])"
              , textTypeTest "\\x -> let y :: Boolean | Number; y = x in y" "{} -> Boolean -> Boolean | Number"
              , textTypeTest
                    "\\x -> let y :: (a -> a, Boolean | Number); y = x in y"
                    "{} -> (a -> a, Boolean) -> (a -> a, Boolean | Number)"
              , textTypeTest
                    "\\x -> let y :: (b -> b, Boolean | Number); y = x in y"
                    "{} -> (b -> b, Boolean) -> (b -> b, Boolean | Number)"
              , textTypeTest
                    "\\x -> let y :: (Boolean, Number); y = (x,x) in y"
                    "{} -> (Number & Boolean) -> (Boolean, Number)"
              , textTypeTest
                    "\\x1 -> \\x2 -> let y :: (Boolean, Number); y = (x1,x2) in y"
                    "{} -> Boolean -> Number -> (Boolean, Number)"
              , textTypeTest
                    "\\x1 -> \\x2 -> let y :: (a -> a, a -> (a,a)); y = (x1,x2) in y"
                    "{} -> (a -> a) -> (a -> (a, a)) -> (a -> a, a -> (a, a))"
              , textTypeTest
                    "\\x1 -> \\x2 -> let y :: (a -> a, b -> (b,b)); y = (x1,x2) in y"
                    "{} -> (a -> a) -> (b -> (b, b)) -> (a -> a, b -> (b, b))"
              , textTypeTest
                    "\\x1 -> \\x2 -> let y :: (b -> b, a -> (a,a)); y = (x1,x2) in y"
                    "{} -> (b -> b) -> (a -> (a, a)) -> (b -> b, a -> (a, a))"
              , textTypeTest
                    "\\x1 -> \\x2 -> let y :: (a -> b, b -> a); y = (x1,x2) in y"
                    "{} -> (a -> b) -> (b -> a) -> (a -> b, b -> a)"
              , textTypeTest
                    "\\x1 -> \\x2 -> let y :: (c -> d, d -> c); y = (x1,x2) in y"
                    "{} -> (c -> d) -> (d -> c) -> (c -> d, d -> c)"
              ]
        , testGroup
              "simplify"
              [ simplifyTypeTest "a" "None"
              , simplifyTypeTest "a -> (a|a)" "a -> a"
              , simplifyTypeTest "a -> b -> (a|b)" "a -> a -> a"
              , simplifyTypeTest "(a,b)-> (a|b)" "(a, a) -> a"
              , simplifyTypeTest "(a,b)-> (a,a|b)" "(a, b) -> (a, a | b)"
              , simplifyTypeTest "(a,b)-> (b,a|b)" "(a, b) -> (b, a | b)"
              , simplifyTypeTest "(a&b)-> (a,b)" "a -> (a, a)"
              ]
        ]
