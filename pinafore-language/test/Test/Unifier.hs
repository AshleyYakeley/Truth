module Test.Unifier
    ( testUnifier
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Test
import Pinafore
import Pinafore.Language.API
import Pinafore.Test
import Shapes
import Test.RunScript

type TS = PinaforeTypeSystem

type PinaforeBisubstitution = Bisubstitution PinaforeGroundType (PinaforePolyShim Type) (UnifierM PinaforeGroundType)

testValue :: Text -> ((?pinafore :: PinaforeContext, ?library :: LibraryContext) => IO ()) -> TestTree
testValue name call = testTree (unpack name) $ withTestPinaforeContext mempty stdout $ \_ _ _ -> call

testSourceScoped :: Text -> PinaforeSourceInterpreter () -> TestTree
testSourceScoped name action = testValue name $ throwInterpretResult $ runPinaforeSourceScoped "<test>" $ action

testUnifyToType ::
       forall t. FromPinaforeType t
    => PinaforeSourceInterpreter QValue
    -> (t -> IO ())
    -> TestTree
testUnifyToType mval checkval =
    testSourceScoped ("unify to " <> qNegativeTypeDescription @t) $ do
        val <- mval
        found <- typedAnyToPinaforeVal @t val
        liftIO $ checkval found

testBisubstitute ::
       forall t. FromPinaforeType t
    => QValue
    -> [PinaforeBisubstitution]
    -> (t -> IO ())
    -> TestTree
testBisubstitute val bisubs checkVal = let
    wExpected :: PinaforeShimWit 'Negative t
    wExpected = fromJMShimWit
    in case wExpected of
           MkShimWit tExpected _ ->
               testValue (pack $ show tExpected) $ do
                   MkAnyValue wFound vFound <-
                       throwInterpretResult $
                       runPinaforeSourceScoped "<test>" $
                       runUnifierM @PinaforeGroundType $ bisubstitutes @PinaforeGroundType bisubs val
                   conv <-
                       throwInterpretResult $
                       runPinaforeSourceScoped "<test>" $
                       runRenamer @TS $ do
                           wExpected' <- rename @TS RigidName wExpected
                           solveUnifyPosNegShimWit @TS wFound wExpected'
                   let
                       found :: t
                       found = shimToFunction conv vFound
                   checkVal found

testInterpret ::
       forall t. FromPinaforeType t
    => Text
    -> (t -> IO ())
    -> ScriptTestTree
testInterpret expr checkVal =
    testExpression @t expr expr $ \_cc interpret ->
        liftIO $ do
            found <- interpret
            checkVal found

op1 :: X -> (X -> X) -> X
op1 v r = r $ r v

op2 :: X -> (X -> Text) -> (X -> X) -> Text
op2 v withVal r = withVal $ r $ r v

op2Partial :: Text -> (JoinType X Text -> Text) -> (JoinType X Text -> X) -> Text
op2Partial v withVal r = withVal $ LeftJoinType $ r $ LeftJoinType $ r $ RightJoinType v

op2Text :: Text -> (Text -> Text) -> (Text -> Text) -> Text
op2Text v withVal r = withVal $ r $ r v

op3 :: X -> (X -> PinaforeAction ()) -> (X -> X) -> PinaforeAction ()
op3 v withVal r = withVal $ r $ r v

op4 :: (X -> Text) -> (X -> X) -> X -> Text
op4 withVal r v = withVal $ r $ r v

idText :: Text -> Text
idText = id

testLib :: LibraryModule
testLib = let
    testSameT :: Text -> Text -> PinaforeAction ()
    testSameT expected found =
        if expected == found
            then return ()
            else fail "different"
    testSameI :: Integer -> Integer -> PinaforeAction ()
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
              "op1"
              [ testTree
                    "unify"
                    [ testUnifyToType @(Text -> (Text -> Text) -> Text) (return $ jmToValue op1) $ \found ->
                          assertEqual "" "PQPQPQ" $ found "PQPQPQ" id
                    , testUnifyToType @(A -> (A -> A) -> A) (return $ jmToValue op1) $ \found ->
                          assignUVarT @Text (MkSymbolType @"a") $
                          assertEqual "" "PQPQPQ" $ unVar $ found (MkVar "PQPQPQ") id
                    ]
              , testTree
                    "bisubstitute"
                    [ testBisubstitute @(X -> (X -> X) -> X) (jmToValue op1) [] $ \found ->
                          assignUVarT @Text (MkSymbolType @"x") $
                          assertEqual "" "PQPQPQ" $ unVar $ found (MkVar "PQPQPQ") id
                    , testBisubstitute @(A -> (A -> A) -> A) (jmToValue op1) [] $ \found ->
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
                    [ testUnifyToType @(Text -> (Text -> Text) -> (Text -> Text) -> Text) (return $ jmToValue op2) $ \found ->
                          assertEqual "" "PQPQPQ" $ found "PQPQPQ" id id
                    , testUnifyToType @(A -> (A -> Text) -> (A -> A) -> Text) (return $ jmToValue op2) $ \found ->
                          assignUVarT @Text (MkSymbolType @"a") $
                          assertEqual "" "PQPQPQ" $ found (MkVar "PQPQPQ") unVar id
                    , let
                          makeVal :: PinaforeSourceInterpreter QValue
                          makeVal = do
                              expr1 <- qApplyExpr (qConstExpr op2) (qConstExpr @Text "PQPQPQ")
                              qEvalExpr expr1
                          checkVal found = assertEqual "" "PQPQPQ" $ found idText id
                          in testUnifyToType @((Text -> Text) -> (Text -> Text) -> Text) makeVal checkVal
                    , let
                          makeVal :: PinaforeSourceInterpreter QValue
                          makeVal = do
                              expr1 <- qApplyExpr (qConstExpr op2Text) (qConstExpr @Text "PQPQPQ")
                              expr2 <- qApplyExpr expr1 (qConstExpr idText)
                              qEvalExpr expr2
                          checkVal found = assertEqual "" "PQPQPQ" $ found id
                          in testUnifyToType @((Text -> Text) -> Text) makeVal checkVal
                    , testSourceScoped "value1" $ do
                          expr1 <- qApplyExpr (qConstExpr op2) (qConstExpr @Text "PQPQPQ")
                          val1 <- qEvalExpr expr1
                          found1 <- typedAnyToPinaforeVal @((Text -> Text) -> (Text -> Text) -> Text) val1
                          liftIO $ assertEqual "found1" "PQPQPQ" $ found1 idText id
                    , failTestBecause "ISSUE #108" $
                      testMark $
                      testSourceScoped "value2" $ do
                          expr2 <- qApplyExpr (qConstExpr $ op2Partial "PQPQPQ") (qConstExpr idText)
                          val2 <- qEvalExpr expr2
                          case val2 of
                              MkAnyValue t _ -> liftIO $ hPutStrLn stderr $ show t
                          found2 <- typedAnyToPinaforeVal @((Text -> Text) -> Text) val2
                          liftIO $ assertEqual "found2" "PQPQPQ" $ found2 id
                    , failTestBecause "ISSUE #108" $ let
                          makeVal :: PinaforeSourceInterpreter QValue
                          makeVal = do
                              expr1 <- qApplyExpr (qConstExpr op2) (qConstExpr @Text "PQPQPQ")
                              val1 <- qEvalExpr expr1
                              expr2 <- qApplyExpr (qConstExprAny val1) (qConstExpr idText)
                              qEvalExpr expr2
                          checkVal found = assertEqual "" "PQPQPQ" $ found id
                          in testUnifyToType @((Text -> Text) -> Text) makeVal checkVal
                    , failTestBecause "ISSUE #108" $ let
                          makeVal :: PinaforeSourceInterpreter QValue
                          makeVal = do
                              expr1 <- qApplyExpr (qConstExpr op2) (qConstExpr @Text "PQPQPQ")
                              expr2 <- qApplyExpr expr1 (qConstExpr idText)
                              qEvalExpr expr2
                          checkVal found = assertEqual "" "PQPQPQ" $ found id
                          in testUnifyToType @((Text -> Text) -> Text) makeVal checkVal
                    ]
              , testTree
                    "bisubstitute"
                    [ testBisubstitute @(A -> (A -> Text) -> (A -> A) -> Text) (jmToValue op2) [] $ \found ->
                          assignUVarT @Text (MkSymbolType @"a") $
                          assertEqual "" "PQPQPQ" $ found (MkVar "PQPQPQ") unVar id
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
                    , tModify (failTestBecause "ISSUE #108") $
                      testInterpret @((Text -> Text) -> Text) "op2 \"PQPQPQ\" id" $ \found ->
                          assertEqual "" "PQPQPQ" $ found id
                    , tModify (failTestBecause "ISSUE #108") $
                      testInterpret @((Text -> Text) -> Text) "op2 \"PQPQPQ\" idText" $ \found ->
                          assertEqual "" "PQPQPQ" $ found id
                    , tModify (failTestBecause "ISSUE #108") $
                      testInterpret @Text "op2 \"PQPQPQ\" id idText" $ \found -> assertEqual "" "PQPQPQ" found
                    , tModify (failTestBecause "ISSUE #108") $
                      testExpectSuccess "testSameT \"PQPQPQ\" $ op2 \"PQPQPQ\" id idText"
                    , tModify (failTestBecause "ISSUE #108") $
                      testExpectSuccess "testSameT \"PQPQPQ\" $ op2 \"PQPQPQ\" id id"
                    ]
              ]
        , testTree
              "op3"
              [ testTree @[TestTree] "unify" []
              , testTree @[TestTree] "bisubstitute" []
              , runScriptTestTree $
                tLibrary testLib $
                tDecls ["import TEST"] $
                tGroup
                    "interpret"
                    [ testExpectSuccess "testSameT \"PQPQPQ\" \"PQPQPQ\""
                    , tModify (failTestBecause "ISSUE #108") $
                      testExpectSuccess "op3 \"PQPQPQ\" (testSameT \"PQPQPQ\") idText"
                    , tModify (failTestBecause "ISSUE #108") $
                      testExpectSuccess "op3 \"PQPQPQ\" (testSameT \"PQPQPQ\") id"
                    , tModify (failTestBecause "ISSUE #108") $ testExpectSuccess "op3 10 (testSameI 10) id"
                    ]
              ]
        , testTree
              "op4"
              [ testTree
                    @[TestTree]
                    "unify"
                    [ let
                          makeVal :: PinaforeSourceInterpreter QValue
                          makeVal = do
                              expr1 <- qApplyExpr (qConstExpr op4) (qConstExpr idText)
                        -- expr2 <- qApplyExpr expr1 (qConstExpr @Text "PQPQPQ")
                              qEvalExpr expr1
                          checkVal found = assertEqual "" "PQPQPQ" $ found idText "PQPQPQ"
                          in testUnifyToType @((Text -> Text) -> Text -> Text) makeVal checkVal
                    , let
                          makeVal :: PinaforeSourceInterpreter QValue
                          makeVal = do
                              expr1 <- qApplyExpr (qConstExpr op4) (qConstExpr idText)
                              expr2 <- qApplyExpr expr1 (qConstExpr idText)
                              qEvalExpr expr2
                          checkVal found = assertEqual "" "PQPQPQ" $ found "PQPQPQ"
                          in testUnifyToType @(Text -> Text) makeVal checkVal
                    , let
                          makeVal :: PinaforeSourceInterpreter QValue
                          makeVal = do
                              expr1 <- qApplyExpr (qConstExpr op4) (qConstExpr idText)
                              expr2 <- qApplyExpr expr1 (qConstExpr idText)
                              expr3 <- qApplyExpr expr2 (qConstExpr @Text "PQPQPQ")
                              qEvalExpr expr3
                          checkVal found = assertEqual "" "PQPQPQ" found
                          in testUnifyToType @Text makeVal checkVal
                    ]
              ]
        ]
