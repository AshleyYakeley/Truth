module Test.Type
    ( testType
    ) where

import GHC.TypeLits
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

class ToPinaforeType baseedit t where
    toPinaforeType :: PinaforeTypeF baseedit 'PositivePolarity t

class FromPinaforeType baseedit t where
    fromPinaforeType :: PinaforeTypeF baseedit 'NegativePolarity t

instance KnownSymbol name => ToPinaforeType baseedit (UVar name) where
    toPinaforeType = singlePositivePinaforeTypeF $ mkTypeF $ VarPinaforeSingularType $ MkSymbolWitness

instance KnownSymbol name => FromPinaforeType baseedit (UVar name) where
    fromPinaforeType = singleNegativePinaforeTypeF $ mkTypeF $ VarPinaforeSingularType $ MkSymbolWitness

instance (FromPinaforeType baseedit a, ToPinaforeType baseedit b) => ToPinaforeType baseedit (a -> b) where
    toPinaforeType =
        unTypeF fromPinaforeType $ \ta conva ->
            unTypeF toPinaforeType $ \tb convb ->
                contramap (\ab -> convb . ab . conva) $
                singlePositivePinaforeTypeF $
                mkTypeF $
                GroundPinaforeSingularType FuncPinaforeGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance ToPinaforeType baseedit (Maybe Bool) where
    toPinaforeType =
        singlePositivePinaforeTypeF $
        mkTypeF $ GroundPinaforeSingularType (LiteralPinaforeGroundType BooleanLiteralType) NilDolanArguments

instance ToPinaforeType baseedit (Maybe Number) where
    toPinaforeType =
        singlePositivePinaforeTypeF $
        mkTypeF $ GroundPinaforeSingularType (LiteralPinaforeGroundType NumberLiteralType) NilDolanArguments

instance FromPinaforeType baseedit (Maybe Number) where
    fromPinaforeType =
        singleNegativePinaforeTypeF $
        mkTypeF $ GroundPinaforeSingularType (LiteralPinaforeGroundType NumberLiteralType) NilDolanArguments

idExpr :: PExpression
idExpr = typeFExpression toPinaforeType $ \(v :: UVar "x") -> v

nbFuncExpr :: PExpression
nbFuncExpr = typeFExpression toPinaforeType $ \(_ :: Maybe Number) -> Just False

numExpr :: PExpression
numExpr = typeFExpression toPinaforeType $ Just (3 :: Number)

boolExpr :: PExpression
boolExpr = typeFExpression toPinaforeType $ Just False

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
        , exprTypeTest "apply id number" (return "{} -> b | Number") $ applyTypedExpression @TS idExpr numExpr
        , exprTypeTest "apply nb number" (return "{} -> a | Boolean") $ applyTypedExpression @TS nbFuncExpr numExpr
        , exprTypeTest "apply nb boolean" (fail "can't cast Boolean to Number") $
          applyTypedExpression @TS nbFuncExpr boolExpr
        , exprTypeTest "apply id var" (return "{v :: a & (x & c)} -> c") $ applyTypedExpression @TS idExpr varExpr
        , exprTypeTest "apply nb var" (return "{v :: a & Number} -> b | Boolean") $
          applyTypedExpression @TS nbFuncExpr varExpr
        ]
