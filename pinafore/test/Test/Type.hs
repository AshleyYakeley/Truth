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

class ToTypeF wit t where
    toTypeF :: TypeF wit 'PositivePolarity t

class FromTypeF wit t where
    fromTypeF :: TypeF wit 'NegativePolarity t

instance ToTypeF (PinaforeType baseedit) BottomType where
    toTypeF = mkTypeF NilPinaforeType

instance FromTypeF (PinaforeType baseedit) TopType where
    fromTypeF = mkTypeF NilPinaforeType

instance (ToTypeF (PinaforeType baseedit) a, ToTypeF (PinaforeType baseedit) b) =>
             ToTypeF (PinaforeType baseedit) (JoinType a b) where
    toTypeF = joinPinaforeTypeF toTypeF toTypeF

instance (FromTypeF (PinaforeType baseedit) a, FromTypeF (PinaforeType baseedit) b) =>
             FromTypeF (PinaforeType baseedit) (MeetType a b) where
    fromTypeF = meetPinaforeTypeF fromTypeF fromTypeF

instance KnownSymbol name => ToTypeF (PinaforeSingularType baseedit) (UVar name) where
    toTypeF = mkTypeF $ VarPinaforeSingularType MkSymbolWitness

instance KnownSymbol name => ToTypeF (PinaforeType baseedit) (UVar name) where
    toTypeF = singlePositivePinaforeTypeF toTypeF

instance KnownSymbol name => FromTypeF (PinaforeSingularType baseedit) (UVar name) where
    fromTypeF = mkTypeF $ VarPinaforeSingularType MkSymbolWitness

instance KnownSymbol name => FromTypeF (PinaforeType baseedit) (UVar name) where
    fromTypeF = singleNegativePinaforeTypeF fromTypeF

instance (ToTypeF (PinaforeType baseedit) a, ToTypeF (PinaforeType baseedit) b) =>
             ToTypeF (PinaforeSingularType baseedit) (a, b) where
    toTypeF =
        unTypeF toTypeF $ \ta conva ->
            unTypeF toTypeF $ \tb convb ->
                contramap (\(a, b) -> (conva a, convb b)) $
                mkTypeF $
                GroundPinaforeSingularType PairPinaforeGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance (ToTypeF (PinaforeType baseedit) a, ToTypeF (PinaforeType baseedit) b) =>
             ToTypeF (PinaforeType baseedit) (a, b) where
    toTypeF = singlePositivePinaforeTypeF toTypeF

instance (FromTypeF (PinaforeType baseedit) a, FromTypeF (PinaforeType baseedit) b) =>
             FromTypeF (PinaforeSingularType baseedit) (a, b) where
    fromTypeF =
        unTypeF fromTypeF $ \ta conva ->
            unTypeF fromTypeF $ \tb convb ->
                fmap (\(a, b) -> (conva a, convb b)) $
                mkTypeF $
                GroundPinaforeSingularType PairPinaforeGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance (FromTypeF (PinaforeType baseedit) a, FromTypeF (PinaforeType baseedit) b) =>
             FromTypeF (PinaforeType baseedit) (a, b) where
    fromTypeF = singleNegativePinaforeTypeF fromTypeF

instance (FromTypeF (PinaforeType baseedit) a, ToTypeF (PinaforeType baseedit) b) =>
             ToTypeF (PinaforeSingularType baseedit) (a -> b) where
    toTypeF =
        unTypeF fromTypeF $ \ta conva ->
            unTypeF toTypeF $ \tb convb ->
                contramap (\ab -> convb . ab . conva) $
                mkTypeF $
                GroundPinaforeSingularType FuncPinaforeGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance (FromTypeF (PinaforeType baseedit) a, ToTypeF (PinaforeType baseedit) b) =>
             ToTypeF (PinaforeType baseedit) (a -> b) where
    toTypeF = singlePositivePinaforeTypeF toTypeF

instance (ToTypeF (PinaforeType baseedit) a, FromTypeF (PinaforeType baseedit) b) =>
             FromTypeF (PinaforeSingularType baseedit) (a -> b) where
    fromTypeF =
        unTypeF toTypeF $ \ta conva ->
            unTypeF fromTypeF $ \tb convb ->
                fmap (\ab -> convb . ab . conva) $
                mkTypeF $
                GroundPinaforeSingularType FuncPinaforeGroundType $
                ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments

instance (ToTypeF (PinaforeType baseedit) a, FromTypeF (PinaforeType baseedit) b) =>
             FromTypeF (PinaforeType baseedit) (a -> b) where
    fromTypeF = singleNegativePinaforeTypeF fromTypeF

instance (ToTypeF (PinaforeType baseedit) a) => ToTypeF (PinaforeSingularType baseedit) [a] where
    toTypeF =
        unTypeF toTypeF $ \ta conva ->
            contramap (fmap conva) $
            mkTypeF $ GroundPinaforeSingularType ListPinaforeGroundType $ ConsDolanArguments ta NilDolanArguments

instance (ToTypeF (PinaforeType baseedit) a) => ToTypeF (PinaforeType baseedit) [a] where
    toTypeF = singlePositivePinaforeTypeF toTypeF

instance ToTypeF (PinaforeSingularType baseedit) Bool where
    toTypeF = mkTypeF $ GroundPinaforeSingularType (LiteralPinaforeGroundType BooleanLiteralType) NilDolanArguments

instance ToTypeF (PinaforeType baseedit) Bool where
    toTypeF = singlePositivePinaforeTypeF toTypeF

instance FromTypeF (PinaforeSingularType baseedit) Bool where
    fromTypeF = mkTypeF $ GroundPinaforeSingularType (LiteralPinaforeGroundType BooleanLiteralType) NilDolanArguments

instance FromTypeF (PinaforeType baseedit) Bool where
    fromTypeF = singleNegativePinaforeTypeF fromTypeF

instance ToTypeF (PinaforeSingularType baseedit) Number where
    toTypeF = mkTypeF $ GroundPinaforeSingularType (LiteralPinaforeGroundType NumberLiteralType) NilDolanArguments

instance ToTypeF (PinaforeType baseedit) Number where
    toTypeF = singlePositivePinaforeTypeF toTypeF

instance FromTypeF (PinaforeSingularType baseedit) Number where
    fromTypeF = mkTypeF $ GroundPinaforeSingularType (LiteralPinaforeGroundType NumberLiteralType) NilDolanArguments

instance FromTypeF (PinaforeType baseedit) Number where
    fromTypeF = singleNegativePinaforeTypeF fromTypeF

apExpr :: PExpression -> PExpression -> Result Text PExpression
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
        , exprTypeTest "apply nb boolean" (fail "can't cast Boolean to Number") $ apExpr nbFuncExpr boolExpr
        , exprTypeTest "apply id var" (return "{v :: c} -> c") $ apExpr idExpr varExpr
        , exprTypeTest "apply nb var" (return "{v :: Number} -> Boolean") $ apExpr nbFuncExpr varExpr
        , exprTypeTest "ifelse" (return "{} -> Boolean -> a -> a -> a") $ return ifelseExpr
        , exprTypeTest "list1" (return "{} -> a -> [a]") $ return list1Expr
        , exprTypeTest "listNumBool" (return "{} -> [Number | Boolean]") $ do
              lne <- apExpr list1Expr numExpr
              lbe <- apExpr list1Expr boolExpr
              e1 <- apExpr ifelseExpr boolExpr
              e2 <- apExpr e1 lne
              apExpr e2 lbe
        , exprTypeTest "snd" (return "{} -> (Top, a) -> a") $ return sndExpr
        , exprTypeTest "thing" (return "{} -> (a, b) -> (a, a | b)") $ return thingExpr
        , exprTypeTest "snd . thing" (return "{} -> (c, c) -> c") $ do
              e1 <- apExpr dotExpr sndExpr
              apExpr e1 thingExpr
        , exprTypeTest "twice" (return "{} -> a -> (a, a)") $ return twiceExpr
        , exprTypeTest "thing . twice" (return "{} -> a -> (a, a)") $ do
              e1 <- apExpr dotExpr thingExpr
              apExpr e1 twiceExpr
        , exprTypeTest "thing $ twice number" (return "{} -> (Number, Number)") $ do
              e1 <- apExpr twiceExpr numExpr
              apExpr thingExpr e1
        ]
