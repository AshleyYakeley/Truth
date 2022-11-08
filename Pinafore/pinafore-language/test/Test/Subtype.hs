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

tGroundType :: QGroundType '[] T
tGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily T)|]) "T"

instance HasQGroundType '[] T where
    qGroundType = tGroundType

tShimWit ::
       forall polarity. Is PolarityType polarity
    => QShimWit polarity T
tShimWit = qType

subtypeEntry ::
       forall a b. (HasQType 'Negative a, HasQType 'Positive b)
    => QOpenExpression (QPolyShim Type a b)
    -> SubtypeConversionEntry QGroundType
subtypeEntry convexpr = let
    ta = fromJust $ dolanToMaybeShimWit (qType :: _ a)
    tb = fromJust $ dolanToMaybeShimWit (qType :: _ b)
    in subtypeConversionEntry Verify ta tb convexpr

simpleConversionExpression :: QOpenExpression (QPolyShim Type () T)
simpleConversionExpression = pure $ functionToShim "conv" $ \() -> MkT 12

openConversionExpression :: VarID -> QOpenExpression (QPolyShim Type () T)
openConversionExpression var =
    OpenExpression (MkNameWitness var qType) $ pure $ \i -> functionToShim "conv" $ \() -> MkT i

unitExpression :: QExpression
unitExpression = MkSealedExpression (qType :: _ ()) $ pure ()

testSimple :: TestTree
testSimple =
    testTree "simple" $
    runTester defaultTester $ do
        MkT i <-
            testerLiftInterpreter $ do
                tExpression <-
                    unTransformT (registerSubtypeConversion (subtypeEntry simpleConversionExpression)) $ \() -> do
                        qSubsumeExpr (shimWitToSome tShimWit) unitExpression
                resultOpenExpression <- typedUnifyExpressionToOpen tShimWit tExpression
                evalExpression resultOpenExpression
        liftIO $ assertEqual "" 12 i

constIntegerExpression :: Integer -> QExpression
constIntegerExpression i = MkSealedExpression qType $ pure i

testDependentLet :: TestTree
testDependentLet =
    testTree "dependent-let" $
    runTester defaultTester $ do
        MkT i <-
            testerLiftInterpreter $
            unTransformT (allocateVar $ UnqualifiedFullNameRef "x") $ \(_, varid) -> do
                tExpression <-
                    unTransformT (registerSubtypeConversion (subtypeEntry $ openConversionExpression varid)) $ \() -> do
                        qSubsumeExpr (shimWitToSome tShimWit) unitExpression
                resultExpression <- qLetExpr varid (constIntegerExpression 17) tExpression
                resultOpenExpression <- typedUnifyExpressionToOpen tShimWit resultExpression
                evalExpression resultOpenExpression
        liftIO $ assertEqual "" 17 i

testDependentFunction :: TestTree
testDependentFunction =
    testTree "dependent-function" $
    runTester defaultTester $ do
        MkT i <-
            testerLiftInterpreter $
            unTransformT (allocateVar $ UnqualifiedFullNameRef "x") $ \(_, varid) -> do
                tExpression <-
                    unTransformT (registerSubtypeConversion (subtypeEntry $ openConversionExpression varid)) $ \() -> do
                        qSubsumeExpr (shimWitToSome tShimWit) unitExpression
                funcExpression <- qAbstractExpr varid tExpression
                resultExpression <- qApplyExpr funcExpression (constIntegerExpression 91)
                resultOpenExpression <- typedUnifyExpressionToOpen tShimWit resultExpression
                evalExpression resultOpenExpression
        liftIO $ assertEqual "" 91 i

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

t1GroundType :: QGroundType '[ CoCCRVariance] T1
t1GroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily T1)|]) "T1"

instance HasQGroundType '[ CoCCRVariance] T1 where
    qGroundType = t1GroundType

t1ShimWit ::
       forall polarity. Is PolarityType polarity
    => QShimWit polarity (T1 Integer)
t1ShimWit = qType

testPolyDependentFunction :: TestTree
testPolyDependentFunction =
    testTree "poly-dependent-function" $
    runTester defaultTester $ do
        let
            openConversionExpression1 :: VarID -> QOpenExpression (QPolyShim Type () (T1 Integer))
            openConversionExpression1 var =
                OpenExpression (MkNameWitness var qType) $ pure $ \i -> functionToShim "conv" $ \() -> MkT1 i
        MkT1 i <-
            testerLiftInterpreter $
            unTransformT (allocateVar $ UnqualifiedFullNameRef "x") $ \(_, varid) -> do
                tExpression <-
                    unTransformT (registerSubtypeConversion (subtypeEntry $ openConversionExpression1 varid)) $ \() -> do
                        qSubsumeExpr (shimWitToSome t1ShimWit) unitExpression
                funcExpression <- qAbstractExpr varid tExpression
                resultExpression <- qApplyExpr funcExpression (constIntegerExpression 91)
                resultOpenExpression <- typedUnifyExpressionToOpen t1ShimWit resultExpression
                evalExpression resultOpenExpression
        liftIO $ assertEqual "" 91 i

registerT1Stuff :: QScopeInterpreter ()
registerT1Stuff = do
    registerType (UnqualifiedFullNameRef "T1") "" t1GroundType
    registerPatternConstructor
        (UnqualifiedFullNameRef "MkT1")
        ""
        (MkSealedExpression (qType :: _ (AP -> T1 AP)) $ pure MkT1) $
        qToPatternConstructor $ PureFunction $ \(MkT1 (a :: AQ)) -> (a, ())

testFunctionType :: TestTree
testFunctionType =
    testTree "function-type" $
    runTester defaultTester $ do
        funcExpression <-
            testerLiftInterpreter $
            unTransformT registerT1Stuff $ \() -> do
                parseTopExpression "fn x => let subtype Unit <: T1 Integer = fn () => x in ((): T1 Integer)"
        liftIO $ assertEqual "function type" "T1 Integer -> T1 Integer" $ show (sealedExpressionType funcExpression)

testSemiScript1 :: TestTree
testSemiScript1 =
    testTree "semiscript-1" $
    runTester defaultTester $ do
        let
            openConversionExpression1 :: VarID -> QOpenExpression (QPolyShim Type () (T1 Integer))
            openConversionExpression1 var =
                OpenExpression (MkNameWitness var qType) $ pure $ \i -> functionToShim "conv" $ \() -> i
        MkT1 i <-
            testerLiftInterpreter $
            unTransformT
                (do
                     registerT1Stuff
                     allocateVar $ UnqualifiedFullNameRef "x") $ \(_, varid) -> do
                tExpression <-
                    unTransformT (registerSubtypeConversion (subtypeEntry $ openConversionExpression1 varid)) $ \() -> do
                        qSubsumeExpr (shimWitToSome t1ShimWit) unitExpression
                funcExpression <- qAbstractExpr varid tExpression
                unTransformT (registerLetBinding (UnqualifiedFullNameRef "f") "" funcExpression) $ \() -> do
                    parseValueUnify @(T1 Integer) "f (MkT1 62)"
        liftIO $ assertEqual "" 62 i

testSemiScript2 :: TestTree
testSemiScript2 =
    testTree "semiscript-2" $
    runTester defaultTester $ do
        MkT1 i <-
            testerLiftInterpreter $
            unTransformT registerT1Stuff $ \() -> do
                parseValueUnify
                    @(T1 Integer)
                    "let f = fn x => let subtype Unit <: T1 Integer = fn () => x in ((): T1 Integer) in f (MkT1 17)"
        liftIO $ assertEqual "" 17 i

testSemiScript3 :: TestTree
testSemiScript3 =
    testTree "semiscript-3" $
    runTester defaultTester $ do
        MkT1 i <-
            testerLiftInterpreter $
            unTransformT registerT1Stuff $ \() -> do
                parseValueUnify
                    @(T1 Integer)
                    "let f = fn x => let subtype Unit <: T1 Integer = fn () => MkT1 x in ((): T1 Integer) in f 17"
        liftIO $ assertEqual "" 17 i

testSemiScript4 :: TestTree
testSemiScript4 =
    testTree "semiscript-4" $
    runTester defaultTester $ do
        i <-
            testerLiftInterpreter $
            unTransformT registerT1Stuff $ \() -> do
                parseValueUnify
                    @Integer
                    "let f = fn x => let subtype Unit <: T1 Integer = fn () => MkT1 x in (fn MkT1 y => y) () in f 17"
        liftIO $ assertEqual "" 17 i

testScript :: TestTree
testScript =
    runScriptTestTree $
    tGroup
        "script"
        [ tDecls ["datatype T of MkT Integer end", "unT = fn (MkT x) => x"] $
          testExpectSuccess $
          "let f = fn x => let subtype Unit <: T = fn () => MkT x in unT () in if f 17 == 17 then return () else fail \"FAILED\""
        , tDecls ["datatype T1 +a of MkT1 a end", "unT1 = fn MkT1 x => x"] $
          testExpectSuccess $
          "let f = fn x => let subtype Unit <: T1 Integer = fn () => MkT1 x in unT1 () in if f 17 == 17 then return () else fail \"FAILED\""
        ]

testSubtype :: TestTree
testSubtype =
    testTree
        "subtype"
        [ testSimple
        , testDependentLet
        , testDependentFunction
        , testPolyDependentFunction
        , testFunctionType
        , testSemiScript1
        , testSemiScript2
        , testSemiScript3
        , testSemiScript4
        , testScript
        ]
