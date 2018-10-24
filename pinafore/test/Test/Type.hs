module Test.Type
    ( testType
    ) where

import Language.Expression.Dolan
import Language.Expression.Expression
import Language.Expression.Named
import Language.Expression.Sealed
import Language.Expression.Typed
import Language.Expression.Unifier
import Pinafore
import Pinafore.Language.Convert
import Pinafore.Language.Name
import Pinafore.Language.Type
import Shapes
import Test.Tasty
import Test.Tasty.HUnit

type TS = PinaforeTypeSystem PinaforeEdit

type PExpression = TypedExpression Name TS

showVars :: NamedExpression Name (PinaforeType PinaforeEdit 'NegativePolarity) t -> [String]
showVars (ClosedExpression _) = []
showVars (OpenExpression (MkNameWitness name t) expr) = (show name <> " :: " <> show t) : showVars expr

showTypes :: PExpression -> String
showTypes (MkSealedExpression t expr) = "{" <> intercalate ", " (showVars expr) <> "} -> " <> show t

exprTypeTest :: String -> Result Text String -> PinaforeTypeCheck PExpression -> TestTree
exprTypeTest name expected mexpr =
    testCase name $
    assertEqual "" expected $ do
        expr <- runPinaforeTypeCheck mexpr
        return $ showTypes expr

apExpr :: PExpression -> PExpression -> PinaforeTypeCheck PExpression
apExpr = applyTypedExpression @TS

idExpr :: PExpression
idExpr = typeFConstExpression toTypeF $ \(v :: UVar "x") -> v

nbFuncExpr :: PExpression
nbFuncExpr = typeFConstExpression toTypeF $ \(_ :: Number) -> False

numExpr :: PExpression
numExpr = typeFConstExpression toTypeF $ (3 :: Number)

boolExpr :: PExpression
boolExpr = typeFConstExpression toTypeF False

varExpr :: PExpression
varExpr = varTypedExpression @TS "v"

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

joinExpr :: PExpression -> PExpression -> PinaforeTypeCheck PExpression
joinExpr exp1 exp2 = do
    je <- apExpr ifelseExpr boolExpr
    e <- apExpr je exp1
    apExpr e exp2

testType :: TestTree
testType =
    testGroup
        "type"
        [ exprTypeTest "number" (return "{} -> Number") $ return numExpr
        , exprTypeTest "boolean" (return "{} -> Boolean") $ return boolExpr
        , exprTypeTest "id" (return "{} -> x -> x") $ return idExpr
        , exprTypeTest "nb" (return "{} -> Number -> Boolean") $ return nbFuncExpr
        , exprTypeTest "var" (return "{v :: a} -> a") $ return varExpr
        , exprTypeTest "apply id number" (return "{} -> Number") $ apExpr idExpr numExpr
        , exprTypeTest "apply nb number" (return "{} -> Boolean") $ apExpr nbFuncExpr numExpr
        , exprTypeTest "apply nb boolean" (fail "cannot convert Boolean to Number") $ apExpr nbFuncExpr boolExpr
        , exprTypeTest "apply id var" (return "{v :: c} -> c") $ apExpr idExpr varExpr
        , exprTypeTest "apply nb var" (return "{v :: Number} -> Boolean") $ apExpr nbFuncExpr varExpr
        , exprTypeTest "ifelse" (return "{} -> Boolean -> a -> a -> a") $ return ifelseExpr
        , exprTypeTest "list1" (return "{} -> a -> [a]") $ return list1Expr
        , exprTypeTest "listNumBool" (return "{} -> [Number | Boolean]") $ do
              lne <- apExpr list1Expr numExpr
              lbe <- apExpr list1Expr boolExpr
              joinExpr lne lbe
        , exprTypeTest "listlistNumBool" (return "{} -> [[Number | Boolean]]") $ do
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
        , exprTypeTest "[nb] -> [bn]" (return "{} -> [Number & Boolean] -> [Boolean | Number]") $
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
              return $ simplifyExpressionType @(TypeUnifier TS) r
        , exprTypeTest "simplify duplicate" (return "{} -> Number") $
          return $
          simplifyExpressionType @(TypeUnifier TS) $
          typeFConstExpression toTypeF (MkJoinType (Right 3) :: JoinType Number Number)
        , exprTypeTest "simplify duplicate list" (return "{} -> [Number]") $
          return $
          simplifyExpressionType @(TypeUnifier TS) $
          typeFConstExpression toTypeF (MkJoinType (Right [3]) :: JoinType [Number] [Number])
        , exprTypeTest "simplify duplicate pair" (return "{} -> (Number, Number)") $
          return $
          simplifyExpressionType @(TypeUnifier TS) $
          typeFConstExpression toTypeF (MkJoinType (Right (3, 3)) :: JoinType (Number, Number) (Number, Number))
        , exprTypeTest "simplify duplicate in pair" (return "{} -> (Number, Number)") $
          return $
          simplifyExpressionType @(TypeUnifier TS) $
          typeFConstExpression toTypeF ((3, MkJoinType (Right 3)) :: (Number, JoinType Number Number))
        , exprTypeTest "simplify duplicate in pair" (return "{} -> (Number, Number)") $
          return $
          simplifyExpressionType @(TypeUnifier TS) $
          typeFConstExpression
              toTypeF
              ((MkJoinType (Right 3), MkJoinType (Right 3)) :: (JoinType Number Number, JoinType Number Number))
        , exprTypeTest "simplify duplicate in list" (return "{} -> [Number]") $
          return $
          simplifyExpressionType @(TypeUnifier TS) $
          typeFConstExpression toTypeF ([MkJoinType (Right 3)] :: [JoinType Number Number])
        ]
