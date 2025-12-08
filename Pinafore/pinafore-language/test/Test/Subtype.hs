module Test.Subtype
    ( testSubtype
    )
where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan
import Shapes

import Pinafore.Test.Internal
import Test.RunScript

testLists :: TestTree
testLists =
    testTree "lists"
        $ runTester defaultTester
        $ testerLiftInterpreter
        $ do
            stl1 <- parseType @'Positive "List1 Integer"
            stl <- parseType @'Positive "List Integer"
            case (stl1, stl) of
                (MkSome tl1, MkSome tl) -> do
                    _ <- qSubsume (mkShimWit tl1) tl
                    pure ()

emptyDefDoc :: DefDoc
emptyDefDoc = MkDefDoc (HeadingDocItem mempty) mempty

newtype T
    = MkT Integer

tGroundType :: QGroundType '[] T
tGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily T)|]) "T"

instance HasQGroundType '[] T where
    qGroundType = tGroundType

tShimWit ::
    forall polarity.
    Is PolarityType polarity =>
    QShimWit polarity T
tShimWit = qType

subtypeEntry ::
    forall a b.
    (HasQType QPolyShim 'Negative a, HasQType QPolyShim 'Positive b) =>
    QOpenExpression (QShim a b) ->
    QSubtypeConversionEntry
subtypeEntry convexpr = let
    ta = fromJust $ dolanToMaybeShimWit (qType :: _ a)
    tb = fromJust $ dolanToMaybeShimWit (qType :: _ b)
    in subtypeConversionEntry Verify Nothing ta tb convexpr

simpleConversionExpression :: QOpenExpression (QShim () T)
simpleConversionExpression = pure $ functionToShim "conv" $ \() -> MkT 12

openConversionExpression :: VarID -> QOpenExpression (QShim () T)
openConversionExpression var =
    OpenExpression (MkNameWitness var qType) $ pure $ \i -> functionToShim "conv" $ \() -> MkT i

unitExpression :: QExpression
unitExpression = MkSealedExpression (qType :: _ ()) $ pure ()

testSimple :: TestTree
testSimple =
    testTree "simple"
        $ runTester defaultTester
        $ do
            MkT i <-
                testerLiftInterpreter $ do
                    tExpression <-
                        withScopeBuilder (registerSubtypeConversion (subtypeEntry simpleConversionExpression)) $ \() -> do
                            qSubsumeExpr (shimWitToSome tShimWit) unitExpression
                    resultOpenExpression <- qUnifyExpressionToOpen tShimWit tExpression
                    qRunTypeResult $ evalExpression resultOpenExpression
            liftIO $ assertEqual "" 12 i

constIntegerExpression :: Integer -> QExpression
constIntegerExpression i = MkSealedExpression qType $ pure i

testDependentLet :: TestTree
testDependentLet =
    testTree "dependent-let"
        $ runTester defaultTester
        $ do
            MkT i <-
                testerLiftInterpreter
                    $ withScopeBuilder (allocateLambdaVar $ Just "x.")
                    $ \(_, varid) -> do
                        tExpression <-
                            withScopeBuilder (registerSubtypeConversion (subtypeEntry $ openConversionExpression varid)) $ \() -> do
                                qSubsumeExpr (shimWitToSome tShimWit) unitExpression
                        resultExpression <- qLetExpr varid (constIntegerExpression 17) tExpression
                        resultOpenExpression <- qUnifyExpressionToOpen tShimWit resultExpression
                        qRunTypeResult $ evalExpression resultOpenExpression
            liftIO $ assertEqual "" 17 i

testDependentFunction :: TestTree
testDependentFunction =
    testTree "dependent-function"
        $ runTester defaultTester
        $ do
            MkT i <-
                testerLiftInterpreter
                    $ withScopeBuilder (allocateLambdaVar $ Just "x.")
                    $ \(_, varid) -> do
                        tExpression <-
                            withScopeBuilder (registerSubtypeConversion (subtypeEntry $ openConversionExpression varid)) $ \() -> do
                                qSubsumeExpr (shimWitToSome tShimWit) unitExpression
                        funcExpression <- qAbstractExpr varid tExpression
                        resultExpression <- qApplyExpr funcExpression (constIntegerExpression 91)
                        resultOpenExpression <- qUnifyExpressionToOpen tShimWit resultExpression
                        qRunTypeResult $ evalExpression resultOpenExpression
            liftIO $ assertEqual "" 91 i

newtype T1 a
    = MkT1 a

instance Functor T1 where
    fmap ab (MkT1 a) = MkT1 $ ab a

instance RepresentationalRole T1 where
    representationalCoercion MkCoercion = MkCoercion

instance MaybeRepresentational T1 where
    maybeRepresentational = Just Dict

instance HasVariance T1 where
    type VarianceOf T1 = 'Covariance

t1GroundType :: QGroundType '[CoCCRVariance] T1
t1GroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily T1)|]) "T1"

instance HasQGroundType '[CoCCRVariance] T1 where
    qGroundType = t1GroundType

t1ShimWit ::
    forall polarity.
    Is PolarityType polarity =>
    QShimWit polarity (T1 Integer)
t1ShimWit = qType

testPolyDependentFunction :: TestTree
testPolyDependentFunction =
    testTree "poly-dependent-function"
        $ runTester defaultTester
        $ do
            let
                openConversionExpression1 :: VarID -> QOpenExpression (QShim () (T1 Integer))
                openConversionExpression1 var =
                    OpenExpression (MkNameWitness var qType) $ pure $ \i -> functionToShim "conv" $ \() -> MkT1 i
            MkT1 i <-
                testerLiftInterpreter
                    $ withScopeBuilder (allocateLambdaVar $ Just "x.")
                    $ \(_, varid) -> do
                        tExpression <-
                            withScopeBuilder (registerSubtypeConversion (subtypeEntry $ openConversionExpression1 varid)) $ \() -> do
                                qSubsumeExpr (shimWitToSome t1ShimWit) unitExpression
                        funcExpression <- qAbstractExpr varid tExpression
                        resultExpression <- qApplyExpr funcExpression (constIntegerExpression 91)
                        resultOpenExpression <- qUnifyExpressionToOpen t1ShimWit resultExpression
                        qRunTypeResult $ evalExpression resultOpenExpression
            liftIO $ assertEqual "" 91 i

registerT1Stuff :: QScopeBuilder ()
registerT1Stuff = do
    registerGroundType "T1." emptyDefDoc t1GroundType
    registerPatternConstructor "MkT1." emptyDefDoc (MkSealedExpression (qType :: _ (AP -> T1 AP)) $ pure MkT1)
        $ qToPatternConstructor
        $ PureFunction
        $ pure
        $ \(MkT1 (a :: AQ)) -> (a, ())

testFunctionType :: TestTree
testFunctionType =
    testTree "function-type"
        $ runTester defaultTester
        $ do
            funcExpression <-
                testerLiftInterpreter
                    $ withScopeBuilder registerT1Stuff
                    $ \() -> do
                        parseTopExpression "fn x => let {subtype Unit <: T1 Integer = fn () => x} ((): T1 Integer)"
            liftIO $ assertEqual "function type" "T1. Integer. -> T1. Integer." $ show (sealedExpressionType funcExpression)

testSemiScript1 :: TestTree
testSemiScript1 =
    testTree "semiscript-1"
        $ runTester defaultTester
        $ do
            let
                openConversionExpression1 :: VarID -> QOpenExpression (QShim () (T1 Integer))
                openConversionExpression1 var =
                    OpenExpression (MkNameWitness var qType) $ pure $ \i -> functionToShim "conv" $ \() -> i
            MkT1 i <-
                testerLiftInterpreter
                    $ withScopeBuilder
                        ( do
                            registerT1Stuff
                            allocateLambdaVar $ Just "x."
                        )
                    $ \(_, varid) -> do
                        tExpression <-
                            withScopeBuilder (registerSubtypeConversion (subtypeEntry $ openConversionExpression1 varid)) $ \() -> do
                                qSubsumeExpr (shimWitToSome t1ShimWit) unitExpression
                        funcExpression <- qAbstractExpr varid tExpression
                        withScopeBuilder (registerLetBinding "f." emptyDefDoc funcExpression) $ \() -> do
                            parseToValueUnify @(T1 Integer) "f (MkT1 62)" []
            liftIO $ assertEqual "" 62 i

testSemiScript2 :: TestTree
testSemiScript2 =
    testTree "semiscript-2"
        $ runTester defaultTester
        $ do
            MkT1 i <-
                testerLiftInterpreter
                    $ withScopeBuilder registerT1Stuff
                    $ \() -> do
                        parseToValueUnify
                            @(T1 Integer)
                            "let {f = fn x => let {subtype Unit <: T1 Integer = fn () => x} ((): T1 Integer)} f (MkT1 17)"
                            []
            liftIO $ assertEqual "" 17 i

testSemiScript3 :: TestTree
testSemiScript3 =
    testTree "semiscript-3"
        $ runTester defaultTester
        $ do
            MkT1 i <-
                testerLiftInterpreter
                    $ withScopeBuilder registerT1Stuff
                    $ \() -> do
                        parseToValueUnify
                            @(T1 Integer)
                            "let {f = fn x => let {subtype Unit <: T1 Integer = fn () => MkT1 x} ((): T1 Integer)} f 17"
                            []
            liftIO $ assertEqual "" 17 i

testSemiScript4 :: TestTree
testSemiScript4 =
    testTree "semiscript-4"
        $ runTester defaultTester
        $ do
            i <-
                testerLiftInterpreter
                    $ withScopeBuilder registerT1Stuff
                    $ \() -> do
                        parseToValueUnify
                            @Integer
                            "let {f = fn x => let {subtype Unit <: T1 Integer = fn () => MkT1 x} (fn MkT1 y => y) ()} f 17"
                            []
            liftIO $ assertEqual "" 17 i

testScript :: TestTree
testScript =
    runScriptTestTree
        $ tGroup
            "script"
            [ tDecls ["datatype T {Mk Integer}", "unT = fn (Mk.T x) => x"]
                $ testExpectSuccess
                $ "let {f = fn x => let {subtype Unit <: T = fn () => Mk.T x} unT ()} if f 17 ==.Entity 17 then pure.Action () else fail.Action \"FAILED\""
            , tDecls ["datatype T1 +a {Mk a}", "unT1 = fn Mk.T1 x => x"]
                $ testExpectSuccess
                $ "let {f = fn x => let {subtype Unit <: T1 Integer = fn () => Mk.T1 x} unT1 ()} if f 17 ==.Entity 17 then pure.Action () else fail.Action \"FAILED\""
            ]

testSubtype :: TestTree
testSubtype =
    testTree
        "subtype"
        [ testLists
        , testSimple
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
