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

testUnifyToType ::
       forall t. FromPinaforeType t
    => PinaforeSourceInterpreter QValue
    -> (t -> IO ())
    -> TestTree
testUnifyToType mval checkval =
    testValue ("unify to " <> qNegativeTypeDescription @t) $ do
        found <-
            throwInterpretResult $
            runPinaforeSourceScoped "<test>" $ do
                val <- mval
                typedAnyToPinaforeVal @t val
        checkval found

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

op3 :: X -> (X -> PinaforeAction ()) -> (X -> X) -> PinaforeAction ()
op3 v withVal r = withVal $ r $ r v

idText :: Text -> Text
idText = id

testLib :: LibraryModule
testLib = let
    msgT :: Text -> PinaforeAction ()
    msgT x = liftIO $ hPutStrLn stderr $ unpack x
    msgI :: Integer -> PinaforeAction ()
    msgI x = liftIO $ hPutStrLn stderr $ show x
    in MkDocTree "TEST" "" $
       [ mkValEntry "idText" "TEST" idText
       , mkValEntry "msgT" "TEST" msgT
       , mkValEntry "msgI" "TEST" msgI
       , mkValEntry "op1" "TEST" op1
       , mkValEntry "op2" "TEST" op2
       , mkValEntry "op3" "TEST" op3
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
                    , testExpectSuccess "msgT $ op1 \"PQPQPQ\" idText"
                    , testExpectSuccess "msgT $ op1 \"PQPQPQ\" id"
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
                    , tModify (failTestBecause "ISSUE #108") $ testExpectSuccess "msgT $ op2 \"PQPQPQ\" id idText"
                    , tModify (failTestBecause "ISSUE #108") $ testExpectSuccess "msgT $ op2 \"PQPQPQ\" id id"
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
                    [ tModify (failTestBecause "ISSUE #108") $ testExpectSuccess "op3 \"PQPQPQ\" msgT idText"
                    , tModify (failTestBecause "ISSUE #108") $ testExpectSuccess "op3 \"PQPQPQ\" msgT id"
                    , tModify (failTestBecause "ISSUE #108") $ testExpectSuccess "op3 10 msgI id"
                    ]
              ]
        ]
