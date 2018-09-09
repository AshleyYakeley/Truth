module Test.Type
    ( testType
    ) where

import Language.Expression.Dolan
import Language.Expression.Expression
import Language.Expression.Named
import Language.Expression.Sealed
import Language.Expression.Typed
import Pinafore
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

exprTypeTest :: String -> Result Text String -> Result Text PExpression -> TestTree
exprTypeTest name expected mexpr =
    testCase name $
    assertEqual "" expected $ do
        expr <- mexpr
        return $ showTypes expr

idExpr :: PExpression
idExpr =
    toSymbolWitness "x" $ \v ->
        MkSealedExpression
            (GroundPinaforeType FuncPinaforeGroundType $
             ConsDolanArguments (VarPinaforeType v) $ ConsDolanArguments (VarPinaforeType v) NilDolanArguments) $
        ClosedExpression id

nbFuncExpr :: PExpression
nbFuncExpr =
    MkSealedExpression
        (GroundPinaforeType FuncPinaforeGroundType $
         ConsDolanArguments (GroundPinaforeType (LiteralPinaforeGroundType NumberLiteralType) NilDolanArguments) $
         ConsDolanArguments
             (GroundPinaforeType (LiteralPinaforeGroundType BooleanLiteralType) NilDolanArguments)
             NilDolanArguments) $
    ClosedExpression $ \_ -> Just False

numExpr :: PExpression
numExpr =
    constTypedExpression @TS (GroundPinaforeType (LiteralPinaforeGroundType NumberLiteralType) NilDolanArguments) $
    Just 3

boolExpr :: PExpression
boolExpr =
    constTypedExpression @TS (GroundPinaforeType (LiteralPinaforeGroundType BooleanLiteralType) NilDolanArguments) $
    Just False

varExpr :: PExpression
varExpr = varTypedExpression @TS "v"

testType :: TestTree
testType =
    testGroup
        "type"
        [ exprTypeTest "number" (return "{} -> Number") $ return numExpr
        , exprTypeTest "boolean" (return "{} -> Boolean") $ return boolExpr
        , exprTypeTest "id" (return "{} -> x -> x") $ return idExpr
        , exprTypeTest "nb" (return "{} -> Number -> Boolean") $ return nbFuncExpr
        , exprTypeTest "var" (return "{v :: a} -> a") $ return varExpr
        , exprTypeTest "apply id number" (return "{} -> Number | b") $ applyTypedExpression @TS idExpr numExpr
        , exprTypeTest "apply nb number" (return "{} -> Boolean | a") $ applyTypedExpression @TS nbFuncExpr numExpr
        , exprTypeTest "apply nb boolean" (fail "can't cast Boolean to Number") $
          applyTypedExpression @TS nbFuncExpr boolExpr
        , exprTypeTest "apply id var" (return "{v :: (c & x) & a} -> c") $ applyTypedExpression @TS idExpr varExpr
        , exprTypeTest "apply nb var" (return "{v :: Number & a} -> Boolean | b") $
          applyTypedExpression @TS nbFuncExpr varExpr
        ]
