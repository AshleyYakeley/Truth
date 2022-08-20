module Test.Subtype
    ( testSubtype
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan
import Pinafore
import Pinafore.Language.API
import Pinafore.Test
import Shapes
import Test.RunScript

newtype T =
    MkT Integer

tGroundType :: PinaforeGroundType '[] T
tGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily T)|]) "T"

instance HasPinaforeGroundType '[] T where
    pinaforeGroundType = tGroundType

tShimWit ::
       forall polarity. Is PolarityType polarity
    => PinaforeShimWit polarity T
tShimWit = pinaforeType

subtypeEntry ::
       forall a b. (HasPinaforeType 'Negative a, HasPinaforeType 'Positive b)
    => PinaforeOpenExpression (PinaforePolyShim Type a b)
    -> SubtypeConversionEntry PinaforeGroundType
subtypeEntry convexpr =
    fromJust $
    toGroundedDolanShimWit (pinaforeType :: _ a) $ \gta argsa ->
        fromJust $
        toGroundedDolanShimWit (pinaforeType :: _ b) $ \gtb argsb -> subtypeConversionEntry gta argsa gtb argsb convexpr

simpleConversionExpression :: PinaforeOpenExpression (PinaforePolyShim Type () T)
simpleConversionExpression = pure $ functionToShim "conv" $ \() -> MkT 12

openConversionExpression :: VarID -> PinaforeOpenExpression (PinaforePolyShim Type () T)
openConversionExpression var =
    OpenExpression (MkNameWitness var pinaforeType) $ pure $ \i -> functionToShim "conv" $ \() -> MkT i

unitExpression :: PinaforeExpression
unitExpression = MkSealedExpression (pinaforeType :: _ ()) $ pure ()

runPinafore :: PinaforeInterpreter a -> IO a
runPinafore ia =
    withTestPinaforeContext mempty stdout $ \_getTableState -> throwInterpretResult $ runTestPinaforeSourceScoped ia

testSimple :: TestTree
testSimple =
    testTree "simple" $ do
        MkT i <-
            runPinafore $ do
                tExpression <-
                    unmapTransformT (registerSubtypeConversion (subtypeEntry simpleConversionExpression)) $ do
                        qSubsumeExpr (shimWitToSome tShimWit) unitExpression
                resultOpenExpression <- typedExpressionToOpen tShimWit tExpression
                evalExpression resultOpenExpression
        assertEqual "" 12 i

constIntegerExpression :: Integer -> PinaforeExpression
constIntegerExpression i = MkSealedExpression pinaforeType $ pure i

testDependentLet :: TestTree
testDependentLet =
    testTree "dependent-let" $ do
        MkT i <-
            runPinafore $
            allocateVar "x" $ \varid -> do
                tExpression <-
                    unmapTransformT (registerSubtypeConversion (subtypeEntry $ openConversionExpression varid)) $ do
                        qSubsumeExpr (shimWitToSome tShimWit) unitExpression
                resultExpression <- qLetExpr varid (constIntegerExpression 17) tExpression
                resultOpenExpression <- typedExpressionToOpen tShimWit resultExpression
                evalExpression resultOpenExpression
        assertEqual "" 17 i

testDependentFunction :: TestTree
testDependentFunction =
    testTree "dependent-function" $ do
        MkT i <-
            runPinafore $
            allocateVar "x" $ \varid -> do
                tExpression <-
                    unmapTransformT (registerSubtypeConversion (subtypeEntry $ openConversionExpression varid)) $ do
                        qSubsumeExpr (shimWitToSome tShimWit) unitExpression
                funcExpression <- qAbstractExpr varid tExpression
                resultExpression <- qApplyExpr funcExpression (constIntegerExpression 91)
                resultOpenExpression <- typedExpressionToOpen tShimWit resultExpression
                evalExpression resultOpenExpression
        assertEqual "" 91 i

newtype T1 a =
    MkT1 a

instance Functor T1 where
    fmap ab (MkT1 a) = MkT1 $ ab a

instance RepresentationalRole T1 where
    representationalCoercion MkCoercion = MkCoercion

instance MaybeRepresentational T1 where
    maybeRepresentational = Just Dict

instance HasVariance T1 where
    type VarianceOf T1 = 'Covariance

t1GroundType :: PinaforeGroundType '[ CoCCRVariance] T1
t1GroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily T1)|]) "T1"

instance HasPinaforeGroundType '[ CoCCRVariance] T1 where
    pinaforeGroundType = t1GroundType

t1ShimWit ::
       forall polarity. Is PolarityType polarity
    => PinaforeShimWit polarity (T1 Integer)
t1ShimWit = pinaforeType

openConversionExpression1 :: VarID -> PinaforeOpenExpression (PinaforePolyShim Type () (T1 Integer))
openConversionExpression1 var =
    OpenExpression (MkNameWitness var pinaforeType) $ pure $ \i -> functionToShim "conv" $ \() -> MkT1 i

testPolyDependentFunction :: TestTree
testPolyDependentFunction =
    testTree "poly-dependent-function" $ do
        MkT1 i <-
            runPinafore $
            allocateVar "x" $ \varid -> do
                tExpression <-
                    unmapTransformT (registerSubtypeConversion (subtypeEntry $ openConversionExpression1 varid)) $ do
                        qSubsumeExpr (shimWitToSome t1ShimWit) unitExpression
                funcExpression <- qAbstractExpr varid tExpression
                resultExpression <- qApplyExpr funcExpression (constIntegerExpression 91)
                resultOpenExpression <- typedExpressionToOpen t1ShimWit resultExpression
                evalExpression resultOpenExpression
        assertEqual "" 91 i

testScript :: TestTree
testScript =
    runScriptTestTree $
    tGroup
        "script"
        [ tDecls ["datatype T of MkT Integer end", "unT = \\(MkT x) => x"] $
          testExpectSuccess $
          "let f = \\x => let subtype Unit <: T = \\() => MkT x in unT () in if f 17 == 17 then return () else fail \"FAILED\""
        , tModify (failTestBecause "crashes") $
          tDecls ["datatype T1 +a of MkT1 a end", "unT1 = \\(MkT1 x) => x"] $
          testExpectSuccess $
          "let f = \\x => let subtype Unit <: T1 Integer = \\() => MkT1 x in unT1 () in if f 17 == 17 then return () else fail \"FAILED\""
        ]

testSubtype :: TestTree
testSubtype =
    testTree "subtype" [testSimple, testDependentLet, testDependentFunction, testPolyDependentFunction, testScript]
