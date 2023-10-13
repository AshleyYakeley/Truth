module Test.Unifier
    ( testUnifier
    ) where

import Data.Shim
import Debug.Trace.Null
import Language.Expression.Common
import Language.Expression.Dolan
import Language.Expression.Dolan.Test
import Pinafore
import Pinafore.Language.API
import Pinafore.Test
import Shapes
import Test.RunScript

type PinaforeBisubstitution = Bisubstitution QGroundType (QPolyShim Type) (TypeResult QGroundType)

pinaforeBisubstitutes :: [PinaforeBisubstitution] -> QValue -> QInterpreter QValue
pinaforeBisubstitutes bisubs val = do
    liftIO $ traceIO $ "bisubstitute: before: " <> showValType val
    val' <- runTypeResult @QGroundType $ unEndoM (bisubstitutes @QGroundType bisubs) val
    liftIO $ traceIO $ "bisubstitute: after: " <> showValType val'
    return val'

testSourceScoped :: Text -> QInterpreter () -> TestTree
testSourceScoped name action = testTree (unpack name) $ runTester defaultTester $ testerLiftInterpreter action

showValType :: QValue -> String
showValType (MkSomeOf (MkShimWit t _) _) = show t

testUnifyToType ::
       forall t. HasQType 'Negative t
    => QInterpreter QValue
    -> [PinaforeBisubstitution]
    -> (t -> IO ())
    -> TestTree
testUnifyToType mval bisubs checkVal =
    testSourceScoped (toText $ "unify to " <> qNegativeTypeDescription @t) $ do
        val <- mval
        liftIO $ traceIO $ "original type: " <> showValType val
        val' <- pinaforeBisubstitutes bisubs val
        liftIO $ traceIO $ "bisub type: " <> showValType val'
        found <- qUnifyValue @t val'
        liftIO $ checkVal found

testInterpret ::
       forall t. HasQType 'Negative t
    => Text
    -> (t -> Tester ())
    -> ScriptTestTree
testInterpret expr checkVal =
    testExpression @t expr expr $ \interpret -> do
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

testLib :: LibraryModule context
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
    in MkLibraryModule "TEST" $
       headingBDT "TEST" "" $
       [ valBDS "idText" "TEST" idText
       , valBDS "testSameT" "TEST" testSameT
       , valBDS "testSameI" "TEST" testSameI
       , valBDS "op1" "TEST" op1
       , valBDS "op2" "TEST" op2
       , valBDS "op3" "TEST" op3
       , valBDS "op4" "TEST" op4
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
                      tImport ["TEST"] $
                      tGroup
                          "interpret"
                          [ testInterpret @(A -> (A -> A) -> A) "op1" $ \found ->
                                assignUVarT @Text (MkSymbolType @"a") $
                                liftIO $ assertEqual "" "PQPQPQ" $ unVar $ found (MkVar "PQPQPQ") id
                          , testInterpret @(Text -> (Text -> Text) -> Text) "op1" $ \found ->
                                liftIO $ assertEqual "" "PQPQPQ" $ found "PQPQPQ" id
                          , testInterpret @((Text -> Text) -> Text) "op1 \"PQPQPQ\"" $ \found ->
                                liftIO $ assertEqual "" "PQPQPQ" $ found id
                          , testInterpret @Text "op1 \"PQPQPQ\" id.Function" $ \found ->
                                liftIO $ assertEqual "" "PQPQPQ" found
                          , testExpectSuccess "testSameT \"PQPQPQ\" $.Function op1 \"PQPQPQ\" idText"
                          , testExpectSuccess "testSameT \"PQPQPQ\" $.Function op1 \"PQPQPQ\" id.Function"
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
                      tImport ["TEST"] $
                      tGroup
                          "interpret"
                          [ testInterpret @(Text -> (Text -> Text) -> (Text -> Text) -> Text) "op2" $ \found ->
                                liftIO $ assertEqual "" "PQPQPQ" $ found "PQPQPQ" id id
                          , testInterpret @((Text -> Text) -> (Text -> Text) -> Text) "op2 \"PQPQPQ\"" $ \found ->
                                liftIO $ assertEqual "" "PQPQPQ" $ found id id
                          , testInterpret @((Text -> Text) -> Text) "op2 \"PQPQPQ\" id.Function" $ \found ->
                                liftIO $ assertEqual "" "PQPQPQ" $ found id
                          , testInterpret @((Text -> Text) -> Text) "op2 \"PQPQPQ\" idText" $ \found ->
                                liftIO $ assertEqual "" "PQPQPQ" $ found id
                          , testInterpret @Text "op2 \"PQPQPQ\" id.Function idText" $ \found ->
                                liftIO $ assertEqual "" "PQPQPQ" found
                          , testExpectSuccess "testSameT \"PQPQPQ\" $.Function op2 \"PQPQPQ\" id.Function idText"
                          , testExpectSuccess "testSameT \"PQPQPQ\" $.Function op2 \"PQPQPQ\" id.Function id.Function"
                          ]
                    ]
              , testTree
                    "op3"
                    [ testTree @[TestTree] "unify" []
                    , runScriptTestTree $
                      tLibrary testLib $
                      tImport ["TEST"] $
                      tGroup
                          "interpret"
                          [ testExpectSuccess "testSameT \"PQPQPQ\" \"PQPQPQ\""
                          , testExpectSuccess "op3 \"PQPQPQ\" (testSameT \"PQPQPQ\") idText"
                          , testExpectSuccess "op3 \"PQPQPQ\" (testSameT \"PQPQPQ\") id.Function"
                          , testExpectSuccess "op3 10 (testSameI 10) id.Function"
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
                runTester defaultTester $ do
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
              , testTree "t5" $
                runTester defaultTester $ do
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
                runTester defaultTester $ do
                    r <- testerLiftAction $ newMemListModel @A
                    action <-
                        testerLiftInterpreter $ do
                            bodyExpr <-
                                parseTopExpression
                                    "fn r => do r :=.WholeModel [10,20]; set.ListModel 0 25 r; get.WholeModel r end"
                            bodyVal <- qEvalExpr bodyExpr
                            body <- qUnifyValueToFree bodyVal
                            return $ body r
                    l :: [Integer] <- testerLiftAction action
                    if l == [25, 20]
                        then return ()
                        else fail "different"
              , testTree "t3" $
                runTester defaultTester $ do
                    r <- testerLiftAction $ newMemListModel @A
                    action <-
                        testerLiftInterpreter $ do
                            bodyExpr <-
                                parseTopExpression
                                    "fn r => do r :=.WholeModel [10,20]; set.ListModel 0 25 r; get.WholeModel r end"
                            actionExpr <- qApplyExpr bodyExpr (qConstExpr r)
                            actionVal <- qEvalExpr actionExpr
                            qUnifyValue actionVal
                    l :: [Integer] <- testerLiftAction action
                    if l == [25, 20]
                        then return ()
                        else fail "different"
              , testTree "t2" $
                runTester defaultTester $ do
                    action <-
                        testerLiftInterpreter $ do
                            expr <-
                                parseTopExpression
                                    "do r <- newMem.ListModel: Action (ListModel a); r :=.WholeModel [10,20]; ir <- item.ListModel True 0 r; ir :=.WholeModel 25; get.WholeModel r end"
                            val <- qEvalExpr expr
                            qUnifyValue val
                    l :: [Integer] <- testerLiftAction action
                    if l == [25, 20]
                        then return ()
                        else fail "different"
              , testTree "t1" $
                runTester defaultTester $ do
                    action <-
                        testerLiftInterpreter $ do
                            expr <-
                                parseTopExpression
                                    "do r <- newMem.ListModel: Action (ListModel a); r :=.WholeModel [10,20]; ir <- item.ListModel True 0 r; ir :=.WholeModel 25; l <- get.WholeModel r; if l ==.Entity [25,20] then pure.Action () else fail.Action \"different\"; end"
                            val <- qEvalExpr expr
                            qUnifyValue val
                    testerLiftAction action
              , runScriptTestTree $
                testExpectSuccess
                    "do r <- newMem.ListModel; r :=.WholeModel [10,20]; ir <- item.ListModel True 0 r; ir :=.WholeModel 25; l <- get.WholeModel r; if l ==.Entity [25,20] then pure.Action () else fail.Action \"different\"; end"
              ]
        , testTree
              "retraction"
              [ testTree "list-1" $
                runTester defaultTester $
                testerLiftInterpreter $ do
                    expr <- parseTopExpression "[1,2]"
                    val <- qEvalExpr expr
                    tval :: Showable <- qUnifyValue val
                    liftIO $ assertEqual "" "[1, 2]" $ textShow tval
              , testTree "list-2" $
                runTester defaultTester $
                testerLiftInterpreter $ do
                    expr <- parseTopExpression "[1,2]"
                    val <- qEvalExpr expr
                    tval :: NonEmpty Integer <- qUnifyValue val
                    liftIO $ assertEqual "" (1 :| [2]) tval
              , testTree "list-3" $
                runTester defaultTester $
                testerLiftInterpreter $ do
                    expr <- parseTopExpression "1 :: (2 :: [])"
                    val <- qEvalExpr expr
                    tval :: NonEmpty Integer <- qUnifyValue val
                    liftIO $ assertEqual "" (1 :| [2]) tval
              ]
        , testTree
              "action"
              [ testTree "1" $
                runTester defaultTester $ do
                    smodel <- testerGetDefaultStore
                    action :: QStore -> Action () <-
                        testerLiftInterpreter $ do
                            expr <-
                                parseTopExpression $
                                "fn store => let opentype E in\n" <>
                                "property @E @Integer !\"r\" store !$ {point.OpenEntity @E !\"p\"} := 456"
                            val <- qEvalExpr expr
                            qUnifyValue val
                    testerRunAction $ action smodel
              , testTree "2" $
                runTester defaultTester $ do
                    smodel <- testerGetDefaultStore
                    rval :: QStore -> LangWholeModel '( Integer, Integer) <-
                        testerLiftInterpreter $ do
                            expr <-
                                parseTopExpression $
                                "fn store => let opentype E in property @E @Integer !\"r\" store !$ {point.OpenEntity @E !\"p\"}"
                            val <- qEvalExpr expr
                            qUnifyValue val
                    testerRunAction $ langWholeModelSet (rval smodel) $ Known 345
              ]
        , failTestBecause "HANGS" $
          testTree
              "recursive-shims"
              [ testTree "pass-1" $
                runTester defaultTester $ do
                    tval :: [Integer] <-
                        testerLiftInterpreter $ do
                            expr <-
                                parseTopExpression $
                                "let rec\n" <>
                                "fromRec = match Nothing => []; Just (t,tt) => t :: fromRec tt end;\n" <>
                                "in fromRec $ Just (5,Just (3,Nothing))"
                            val <- qEvalExpr expr
                            qUnifyValue val
                    if tval == [5, 3]
                        then return ()
                        else fail "different"
              , testMark $
                testTree "fails-2" $
                runTester defaultTester $ do
                    tval :: [Integer] <-
                        testerLiftInterpreter $ do
                            expr <-
                                parseTopExpression $
                                "let rec\n" <>
                                "datatype Q +t of Mk (rec a, Maybe (t *: a)) end;\n" <>
                                "fromQ = fn Mk.Q x => x >- match Nothing => []; (Just (t,a)) => t :: fromQ (Mk.Q a) end;\n" <>
                                "in fromQ $ Mk.Q $ Just (5,Just (3,Just (1,Nothing)))"
                            val <- qEvalExpr expr
                            qUnifyValue val
                    if tval == [5, 3, 1]
                        then return ()
                        else fail "different"
              , testTree "fails-1" $
                runTester defaultTester $ do
                    tval :: [Integer] <-
                        testerLiftInterpreter $ do
                            expr <-
                                parseTopExpression $
                                "let rec\n" <>
                                "datatype Q +t of Mk (rec a, Maybe (t *: a)) end;\n" <>
                                "fromQ = match Mk.Q Nothing => []; Mk.Q (Just (t,a)) => t :: fromQ (Mk.Q a) end;\n" <>
                                "in fromQ $ Mk.Q $ Just (5,Just (3,Just (1,Nothing)))"
                            val <- qEvalExpr expr
                            qUnifyValue val
                    if tval == [5, 3, 1]
                        then return ()
                        else fail "different"
              , testTree "rs-2" $ let
                    f :: (A -> [B]) -> Maybe (B, A) -> JoinType [BottomType] (NonEmpty B)
                    f r =
                        \case
                            Nothing -> LeftJoinType []
                            Just (t, tt) -> RightJoinType $ t :| r tt
                    v :: Maybe (Integer, Maybe (Integer, Maybe BottomType))
                    v = Just (5, Just (3, Nothing))
                    lib = bindsLibrary "test" [("f", MkSomeValue f), ("v", MkSomeValue v)]
                    in runTester (addTesterLibrary lib defaultTester) $ do
                           tval :: [Integer] <-
                               testerLiftInterpreter $ do
                                   expr <- parseTopExpression $ "import \"test\" in fix f v"
                                   val <- qEvalExpr expr
                                   qUnifyValue val
                           if tval == [5, 3]
                               then return ()
                               else fail "different"
              ]
        ]
