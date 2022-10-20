module Test.Unifier
    ( testUnifier
    ) where

import Data.Shim
import Debug.Trace.Null
import Language.Expression.Common
import Language.Expression.Dolan.Test
import Pinafore
import Pinafore.Language.API
import Pinafore.Test
import Shapes
import Test.RunScript
import Test.Tester

type PinaforeBisubstitution = Bisubstitution QGroundType (QPolyShim Type) (UnifierM QGroundType)

pinaforeBisubstitutes :: [PinaforeBisubstitution] -> QValue -> QInterpreter QValue
pinaforeBisubstitutes bisubs val = do
    liftIO $ traceIO $ "bisubstitute: before: " <> showValType val
    val' <- runUnifierM @QGroundType $ bisubstitutes @QGroundType bisubs val
    liftIO $ traceIO $ "bisubstitute: after: " <> showValType val'
    return val'

testValue :: Text -> ((?qcontext :: QContext, ?library :: LibraryContext) => IO ()) -> TestTree
testValue name call = testTree (unpack name) $ withTestQContext mempty stdout $ \_ -> liftIO call

testSourceScoped :: Text -> QInterpreter () -> TestTree
testSourceScoped name action = testValue name $ fromInterpretResult $ runPinaforeScoped "<test>" $ action

showValType :: QValue -> String
showValType (MkSomeOf (MkShimWit t _) _) = show t

testUnifyToType ::
       forall t. HasQType 'Negative t
    => QInterpreter QValue
    -> [PinaforeBisubstitution]
    -> (t -> IO ())
    -> TestTree
testUnifyToType mval bisubs checkVal =
    testSourceScoped ("unify to " <> qNegativeTypeDescription @t) $ do
        val <- mval
        liftIO $ traceIO $ "original type: " <> showValType val
        val' <- pinaforeBisubstitutes bisubs val
        liftIO $ traceIO $ "bisub type: " <> showValType val'
        found <- qUnifyValue @t val'
        liftIO $ checkVal found

testInterpret ::
       forall t. HasQType 'Negative t
    => Text
    -> (t -> IO ())
    -> ScriptTestTree
testInterpret expr checkVal =
    testExpression @t expr expr $ \interpret ->
        liftIO $ do
            found <- interpret
            checkVal found

op1 :: X -> (X -> X) -> X
op1 v r = r $ r v

op2 :: X -> (X -> Text) -> (X -> X) -> Text
op2 v withVal r = withVal $ r $ r v

op2Text :: Text -> (Text -> Text) -> (Text -> Text) -> Text
op2Text v withVal r = withVal $ r $ r v

op3 :: X -> (X -> Action ()) -> (X -> X) -> Action ()
op3 v withVal r = withVal $ r $ r v

op4 :: (X -> Text) -> (X -> X) -> X -> Text
op4 withVal r v = withVal $ r $ r v

idText :: Text -> Text
idText = id

testLib :: LibraryModule
testLib = let
    testSameT :: Text -> Text -> Action ()
    testSameT expected found =
        if expected == found
            then return ()
            else fail "different"
    testSameI :: Integer -> Integer -> Action ()
    testSameI expected found =
        if expected == found
            then return ()
            else fail "different"
    in MkDocTree "TEST" "" $
       [ mkValEntry "idText" "TEST" idText
       , mkValEntry "testSameT" "TEST" testSameT
       , mkValEntry "testSameI" "TEST" testSameI
       , mkValEntry "op1" "TEST" op1
       , mkValEntry "op2" "TEST" op2
       , mkValEntry "op3" "TEST" op3
       , mkValEntry "op4" "TEST" op4
       ]

testUnifier :: TestTree
testUnifier =
    testTree
        "unifier"
        [ testTree
              "unifier"
              [ testTree
                    "op1"
                    [ testTree
                          "unify"
                          [ testUnifyToType @(Text -> (Text -> Text) -> Text) (return $ jmToValue op1) [] $ \found ->
                                assertEqual "" "PQPQPQ" $ found "PQPQPQ" id
                          , testUnifyToType @(X -> (X -> X) -> X) (return $ jmToValue op1) [] $ \found ->
                                assignUVarT @Text (MkSymbolType @"x") $
                                assertEqual "" "PQPQPQ" $ unVar $ found (MkVar "PQPQPQ") id
                          , testUnifyToType @(A -> (A -> A) -> A) (return $ jmToValue op1) [] $ \found ->
                                assignUVarT @Text (MkSymbolType @"a") $
                                assertEqual "" "PQPQPQ" $ unVar $ found (MkVar "PQPQPQ") id
                          ]
                    , runScriptTestTree $
                      tLibrary testLib $
                      tDecls ["import TEST"] $
                      tGroup
                          "interpret"
                          [ testInterpret @(A -> (A -> A) -> A) "op1" $ \found ->
                                assignUVarT @Text (MkSymbolType @"a") $
                                assertEqual "" "PQPQPQ" $ unVar $ found (MkVar "PQPQPQ") id
                          , testInterpret @(Text -> (Text -> Text) -> Text) "op1" $ \found ->
                                assertEqual "" "PQPQPQ" $ found "PQPQPQ" id
                          , testInterpret @((Text -> Text) -> Text) "op1 \"PQPQPQ\"" $ \found ->
                                assertEqual "" "PQPQPQ" $ found id
                          , testInterpret @Text "op1 \"PQPQPQ\" id" $ \found -> assertEqual "" "PQPQPQ" found
                          , testExpectSuccess "testSameT \"PQPQPQ\" $ op1 \"PQPQPQ\" idText"
                          , testExpectSuccess "testSameT \"PQPQPQ\" $ op1 \"PQPQPQ\" id"
                          ]
                    ]
              , testTree
                    "op2"
                    [ testTree
                          "unify"
                          [ testUnifyToType
                                @(Text -> (Text -> Text) -> (Text -> Text) -> Text)
                                (return $ jmToValue op2)
                                [] $ \found -> assertEqual "" "PQPQPQ" $ found "PQPQPQ" id id
                          , testUnifyToType @(A -> (A -> Text) -> (A -> A) -> Text) (return $ jmToValue op2) [] $ \found ->
                                assignUVarT @Text (MkSymbolType @"a") $
                                assertEqual "" "PQPQPQ" $ found (MkVar "PQPQPQ") unVar id
                          , let
                                makeVal :: QInterpreter QValue
                                makeVal = do
                                    expr1 <- qApplyExpr (qConstExpr op2) (qConstExpr @Text "PQPQPQ")
                                    qEvalExpr expr1
                                checkVal found = assertEqual "" "PQPQPQ" $ found idText id
                                in testUnifyToType @((Text -> Text) -> (Text -> Text) -> Text) makeVal [] checkVal
                          , let
                                makeVal :: QInterpreter QValue
                                makeVal = do
                                    expr1 <- qApplyExpr (qConstExpr op2Text) (qConstExpr @Text "PQPQPQ")
                                    expr2 <- qApplyExpr expr1 (qConstExpr idText)
                                    qEvalExpr expr2
                                checkVal found = assertEqual "" "PQPQPQ" $ found id
                                in testUnifyToType @((Text -> Text) -> Text) makeVal [] checkVal
                          , testSourceScoped "value1" $ do
                                expr1 <- qApplyExpr (qConstExpr op2) (qConstExpr @Text "PQPQPQ")
                                val1 <- qEvalExpr expr1
                                found1 <- qUnifyValue @((Text -> Text) -> (Text -> Text) -> Text) val1
                                liftIO $ assertEqual "found1" "PQPQPQ" $ found1 idText id
                          , let
                                makeVal :: QInterpreter QValue
                                makeVal = do
                                    expr1 <- qApplyExpr (qConstExpr op2) (qConstExpr @Text "PQPQPQ")
                                    val1 <- qEvalExpr expr1
                                    expr2 <- qApplyExpr (qConstExprAny val1) (qConstExpr idText)
                                    qEvalExpr expr2
                                checkVal found = assertEqual "" "PQPQPQ" $ found id
                                in testUnifyToType @((Text -> Text) -> Text) makeVal [] checkVal
                          , let
                                makeVal :: QInterpreter QValue
                                makeVal = do
                                    expr1 <- qApplyExpr (qConstExpr op2) (qConstExpr @Text "PQPQPQ")
                                    expr2 <- qApplyExpr expr1 (qConstExpr idText)
                                    qEvalExpr expr2
                                checkVal found = assertEqual "" "PQPQPQ" $ found id
                                in testUnifyToType @((Text -> Text) -> Text) makeVal [] checkVal
                          ]
                    , runScriptTestTree $
                      tLibrary testLib $
                      tDecls ["import TEST"] $
                      tGroup
                          "interpret"
                          [ testInterpret @(Text -> (Text -> Text) -> (Text -> Text) -> Text) "op2" $ \found ->
                                assertEqual "" "PQPQPQ" $ found "PQPQPQ" id id
                          , testInterpret @((Text -> Text) -> (Text -> Text) -> Text) "op2 \"PQPQPQ\"" $ \found ->
                                assertEqual "" "PQPQPQ" $ found id id
                          , testInterpret @((Text -> Text) -> Text) "op2 \"PQPQPQ\" id" $ \found ->
                                assertEqual "" "PQPQPQ" $ found id
                          , testInterpret @((Text -> Text) -> Text) "op2 \"PQPQPQ\" idText" $ \found ->
                                assertEqual "" "PQPQPQ" $ found id
                          , testInterpret @Text "op2 \"PQPQPQ\" id idText" $ \found -> assertEqual "" "PQPQPQ" found
                          , testExpectSuccess "testSameT \"PQPQPQ\" $ op2 \"PQPQPQ\" id idText"
                          , testExpectSuccess "testSameT \"PQPQPQ\" $ op2 \"PQPQPQ\" id id"
                          ]
                    ]
              , testTree
                    "op3"
                    [ testTree @[TestTree] "unify" []
                    , runScriptTestTree $
                      tLibrary testLib $
                      tDecls ["import TEST"] $
                      tGroup
                          "interpret"
                          [ testExpectSuccess "testSameT \"PQPQPQ\" \"PQPQPQ\""
                          , testExpectSuccess "op3 \"PQPQPQ\" (testSameT \"PQPQPQ\") idText"
                          , testExpectSuccess "op3 \"PQPQPQ\" (testSameT \"PQPQPQ\") id"
                          , testExpectSuccess "op3 10 (testSameI 10) id"
                          ]
                    ]
              , testTree
                    "op4"
                    [ testTree
                          "unify"
                          [ let
                                makeVal :: QInterpreter QValue
                                makeVal = do
                                    expr1 <- qApplyExpr (qConstExpr op4) (qConstExpr idText)
                                    qEvalExpr expr1
                                checkVal found = assertEqual "" "PQPQPQ" $ found idText "PQPQPQ"
                                in testUnifyToType @((Text -> Text) -> Text -> Text) makeVal [] checkVal
                          , let
                                makeVal :: QInterpreter QValue
                                makeVal = do
                                    expr1 <- qApplyExpr (qConstExpr op4) (qConstExpr idText)
                                    expr2 <- qApplyExpr expr1 (qConstExpr idText)
                                    qEvalExpr expr2
                                checkVal found = assertEqual "" "PQPQPQ" $ found "PQPQPQ"
                                in testUnifyToType @(Text -> Text) makeVal [] checkVal
                          , let
                                makeVal :: QInterpreter QValue
                                makeVal = do
                                    expr1 <- qApplyExpr (qConstExpr op4) (qConstExpr idText)
                                    expr2 <- qApplyExpr expr1 (qConstExpr idText)
                                    expr3 <- qApplyExpr expr2 (qConstExpr @Text "PQPQPQ")
                                    qEvalExpr expr3
                                checkVal found = assertEqual "" "PQPQPQ" found
                                in testUnifyToType @Text makeVal [] checkVal
                          ]
                    ]
              ]
        , testTree
              "subtype"
              [ testTree "t8" $
                runTester mempty $ do
                    action <-
                        testerLiftInterpreter $ do
                            a1Expr <-
                                return $
                                qConstExpr ((>>=) newMemListModel :: (LangListModel '( A, A) -> Action B) -> Action B)
                            a2Expr <-
                                return $
                                qConstExpr (return :: LangListModel '( A, A) -> Action (LangListModel '( A, A)))
                            actionExpr <- qApplyExpr a1Expr a2Expr
                            actionVal <- qEvalExpr actionExpr
                            qUnifyValue @(Action (LangWholeModel '( [Integer], [Integer]))) actionVal
                    testerLiftAction $ do
                        wr <- action
                        langWholeModelSet wr $ Known [10, 20]
                        l <- langWholeModelGet wr
                        if l == [10, 20]
                            then return ()
                            else fail $ "different: " <> show l
              , testTree "t7" $
                runTester mempty $ do
                    action <-
                        testerLiftInterpreter $ do
                            actionExpr <- parseTopExpression "do r <- newMemListModel; return (r: ListModel a) end"
                            actionVal <- qEvalExpr actionExpr
                            qUnifyValue @(Action (LangWholeModel '( [Integer], [Integer]))) actionVal
                    testerLiftAction $ do
                        wr <- action
                        langWholeModelSet wr $ Known [10, 20]
                        l <- langWholeModelGet wr
                        if l == [10, 20]
                            then return ()
                            else fail $ "different: " <> show l
              , testTree "t6" $
                runTester mempty $ do
                    action <-
                        testerLiftInterpreter $ do
                            a1Expr <-
                                return $
                                qConstExpr ((>>=) newMemListModel :: (LangListModel '( A, A) -> Action B) -> Action B)
                            a2Expr <-
                                parseTopExpression "fn r => (return (r,r): Action (ListModel a *: WholeModel (List a)))"
                            actionExpr <- qApplyExpr a1Expr a2Expr
                            actionVal <- qEvalExpr actionExpr
                            qUnifyValue
                                @(Action (LangListModel '( Integer, Integer), LangWholeModel '( [Integer], [Integer])))
                                actionVal
                    testerLiftAction $ do
                        (_r', wr') <- action
                        langWholeModelSet wr' $ Known [10, 20]
                        l <- langWholeModelGet wr'
                        if l == [10, 20]
                            then return ()
                            else fail $ "different: " <> show l
              , testTree "t5" $
                runTester mempty $ do
                    r <- testerLiftAction $ newMemListModel @A
                    (_r' :: LangListModel '( Integer, Integer), wr' :: LangWholeModel '( [Integer], [Integer])) <-
                        testerLiftInterpreter $ do
                            convertExpr <- parseTopExpression "fn r => (r,r)"
                            convertVal <- qEvalExpr convertExpr
                            convert <- qUnifyValueToFree convertVal
                            return $ convert r
                    testerLiftAction $ langWholeModelSet wr' $ Known [10, 20]
                    l <- testerLiftAction $ langWholeModelGet wr'
                    if l == [10, 20]
                        then return ()
                        else fail "different"
              , testTree "t4" $
                runTester mempty $ do
                    r <- testerLiftAction $ newMemListModel @A
                    action <-
                        testerLiftInterpreter $ do
                            bodyExpr <- parseTopExpression "fn r => do r := [10,20]; listModelSet 0 25 r; get r end"
                            bodyVal <- qEvalExpr bodyExpr
                            body <- qUnifyValueToFree bodyVal
                            return $ body r
                    l :: [Integer] <- testerLiftAction action
                    if l == [25, 20]
                        then return ()
                        else fail "different"
              , testTree "t3" $
                runTester mempty $ do
                    r <- testerLiftAction $ newMemListModel @A
                    action <-
                        testerLiftInterpreter $ do
                            bodyExpr <- parseTopExpression "fn r => do r := [10,20]; listModelSet 0 25 r; get r end"
                            actionExpr <- qApplyExpr bodyExpr (qConstExpr r)
                            actionVal <- qEvalExpr actionExpr
                            qUnifyValue actionVal
                    l :: [Integer] <- testerLiftAction action
                    if l == [25, 20]
                        then return ()
                        else fail "different"
              , testTree "t2" $
                runTester mempty $ do
                    action <-
                        testerLiftInterpreter $ do
                            expr <-
                                parseTopExpression
                                    "do r <- newMemListModel: Action (ListModel a); r := [10,20]; ir <- listModelItem True 0 r; ir := 25; get r end"
                            val <- qEvalExpr expr
                            qUnifyValue val
                    l :: [Integer] <- testerLiftAction action
                    if l == [25, 20]
                        then return ()
                        else fail "different"
              , testTree "t1" $
                runTester mempty $ do
                    action <-
                        testerLiftInterpreter $ do
                            expr <-
                                parseTopExpression
                                    "do r <- newMemListModel: Action (ListModel a); r := [10,20]; ir <- listModelItem True 0 r; ir := 25; l <- get r; if l == [25,20] then return () else fail \"different\"; end"
                            val <- qEvalExpr expr
                            qUnifyValue val
                    testerLiftAction action
              , runScriptTestTree $
                testExpectSuccess
                    "do r <- newMemListModel; r := [10,20]; ir <- listModelItem True 0 r; ir := 25; l <- get r; if l == [25,20] then return () else fail \"different\"; end"
              ]
        ]
