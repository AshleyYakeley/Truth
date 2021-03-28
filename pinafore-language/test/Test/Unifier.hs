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

type PinaforeBisubstitution = Bisubstitution PinaforeGroundType (PinaforePolyShim Type) (UnifierM PinaforeGroundType)

-- type PinaforeShimWitMappable = PShimWitMappable (PinaforePolyShim Type) (DolanType PinaforeGroundType)
pinaforeBisubstitutes :: [PinaforeBisubstitution] -> QValue -> PinaforeSourceInterpreter QValue
pinaforeBisubstitutes bisubs val = do
    liftIO $ traceIO $ "bisubstitute: before: " <> showValType val
    val' <- runUnifierM @PinaforeGroundType $ bisubstitutes @PinaforeGroundType bisubs val
    liftIO $ traceIO $ "bisubstitute: after: " <> showValType val'
    return val'

testValue :: Text -> ((?pinafore :: PinaforeContext, ?library :: LibraryContext) => IO ()) -> TestTree
testValue name call = testTree (unpack name) $ withTestPinaforeContext mempty stdout $ \_ _ _ -> call

testSourceScoped :: Text -> PinaforeSourceInterpreter () -> TestTree
testSourceScoped name action = testValue name $ throwInterpretResult $ runPinaforeSourceScoped "<test>" $ action

showValType :: QValue -> String
showValType (MkAnyValue (MkShimWit t _) _) = show t

testUnifyToType ::
       forall t. FromPinaforeType t
    => PinaforeSourceInterpreter QValue
    -> [PinaforeBisubstitution]
    -> (t -> IO ())
    -> TestTree
testUnifyToType mval bisubs checkVal =
    testSourceScoped ("unify to " <> qNegativeTypeDescription @t) $ do
        val <- mval
        liftIO $ traceIO $ "original type: " <> showValType val
        val' <- pinaforeBisubstitutes bisubs val
        liftIO $ traceIO $ "bisub type: " <> showValType val'
        found <- typedAnyToPinaforeVal @t val'
        liftIO $ checkVal found

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
                    , tModify (failTestBecause "ISSUE #108") $
                      testInterpret @Text "op1 \"PQPQPQ\" id" $ \found -> assertEqual "" "PQPQPQ" found
                    , testExpectSuccess "testSameT \"PQPQPQ\" $ op1 \"PQPQPQ\" idText"
                    , tModify (failTestBecause "ISSUE #108") $
                      testExpectSuccess "testSameT \"PQPQPQ\" $ op1 \"PQPQPQ\" id"
                    ]
              ]
        , testTree
              "op2"
              [ testTree
                    "unify"
                    [ testUnifyToType @(Text -> (Text -> Text) -> (Text -> Text) -> Text) (return $ jmToValue op2) [] $ \found ->
                          assertEqual "" "PQPQPQ" $ found "PQPQPQ" id id
                    , testUnifyToType @(A -> (A -> Text) -> (A -> A) -> Text) (return $ jmToValue op2) [] $ \found ->
                          assignUVarT @Text (MkSymbolType @"a") $
                          assertEqual "" "PQPQPQ" $ found (MkVar "PQPQPQ") unVar id
                    , let
                          makeVal :: PinaforeSourceInterpreter QValue
                          makeVal = do
                              expr1 <- qApplyExpr (qConstExpr op2) (qConstExpr @Text "PQPQPQ")
                              qEvalExpr expr1
                          checkVal found = assertEqual "" "PQPQPQ" $ found idText id
                          in testUnifyToType @((Text -> Text) -> (Text -> Text) -> Text) makeVal [] checkVal
                    , let
                          makeVal :: PinaforeSourceInterpreter QValue
                          makeVal = do
                              expr1 <- qApplyExpr (qConstExpr op2Text) (qConstExpr @Text "PQPQPQ")
                              expr2 <- qApplyExpr expr1 (qConstExpr idText)
                              qEvalExpr expr2
                          checkVal found = assertEqual "" "PQPQPQ" $ found id
                          in testUnifyToType @((Text -> Text) -> Text) makeVal [] checkVal
                    , testSourceScoped "value1" $ do
                          expr1 <- qApplyExpr (qConstExpr op2) (qConstExpr @Text "PQPQPQ")
                          val1 <- qEvalExpr expr1
                          found1 <- typedAnyToPinaforeVal @((Text -> Text) -> (Text -> Text) -> Text) val1
                          liftIO $ assertEqual "found1" "PQPQPQ" $ found1 idText id
                    , ignoreTestBecause "ISSUE #108" $
                      testMark $
                      testSourceScoped "value2" $ do
                          liftIO $ traceIO "\nCASE"
                          let
                              op :: Text -> (JoinType X Text -> Text) -> (JoinType X Text -> X) -> Text
                              op v withVal r = withVal $ join1 $ r $ join1 $ r $ join2 v
                              -- opA :: Text -> (JoinType B Text -> Text) -> (JoinType B Text -> MeetType B Text) -> Text
                              -- opA v withVal r = withVal $ join1 $ meet1 $ r $ foo
                              bisub1 :: PinaforeBisubstitution
                              bisub1 =
                                  newUVar "x" $ \oldvar ->
                                      newUVar "b" $ \(newvar :: SymbolType newname) -> let
                                          ptw :: DolanShimWit PinaforeGroundType 'Negative Text
                                          ptw = fromJMShimWit
                                          in assignUVarT @(MeetType (UVarT newname) Text) oldvar $
                                             mkPolarBisubstitution
                                                 False
                                                 oldvar
                                                 (return $ joinMeetShimWit (varDolanShimWit newvar) ptw)
                                                 (return $
                                                  singleDolanShimWit $
                                                  MkShimWit (VarDolanSingularType newvar) $ MkPolarMap meet1)
                              posbisub :: AnyW SymbolType -> PinaforeBisubstitution
                              posbisub (MkAnyW var) =
                                  assignUVarT @BottomType var $
                                  MkBisubstitution
                                      False
                                      var
                                      (return $ mkShimWit NilDolanType)
                                      (return $ varDolanShimWit var)
                              negbisub :: AnyW SymbolType -> PinaforeBisubstitution
                              negbisub (MkAnyW var) =
                                  assignUVarT @TopType var $
                                  MkBisubstitution
                                      False
                                      var
                                      (return $ varDolanShimWit var)
                                      (return $ mkShimWit NilDolanType)
                              elimBisubs =
                                  [posbisub $ MkAnyW $ MkSymbolType @"b", negbisub $ MkAnyW $ MkSymbolType @"b"]
                              qval1 :: QValue
                              qval1 = jmToValue $ op "PQPQPQ"
                          liftIO $ hPutStrLn stderr $ "bisubstituting " <> showValType qval1
                          qval2 <- pinaforeBisubstitutes [bisub1] qval1
                          let
                              funcType ::
                                     PinaforeType (InvertPolarity polarity) a
                                  -> PinaforeType polarity b
                                  -> PinaforeType polarity (JoinMeetType polarity (a -> b) (LimitType polarity))
                              funcType ta tb =
                                  singleDolanType $
                                  GroundDolanSingularType funcGroundType $
                                  ConsDolanArguments ta $ ConsDolanArguments tb NilDolanArguments
                              singleTextType :: PinaforeSingularType polarity Text
                              singleTextType =
                                  GroundDolanSingularType
                                      (EntityPinaforeGroundType NilListType $ LiteralEntityGroundType TextLiteralType)
                                      NilDolanArguments
                              textType :: PinaforeType polarity (JoinMeetType polarity Text (LimitType polarity))
                              textType = singleDolanType singleTextType
                              joinMeetBTextType ::
                                     PinaforeType polarity (JoinMeetType polarity (UVarT "b") (JoinMeetType polarity Text (LimitType polarity)))
                              joinMeetBTextType =
                                  ConsDolanType (VarDolanSingularType $ MkSymbolType @"b") $
                                  ConsDolanType singleTextType NilDolanType
                              expected2Type :: PinaforeType 'Positive _
                              expected2Type =
                                  funcType (funcType joinMeetBTextType textType) $
                                  funcType (funcType joinMeetBTextType joinMeetBTextType) textType
                          liftIO $
                              assignUVarT @Text @_ @(IO ()) (MkSymbolType @"b") $
                              case qval2 of
                                  MkAnyValue (MkShimWit t2 (MkPolarMap conv2)) v2 ->
                                      case testEquality t2 expected2Type of
                                          Just Refl -> do
                                              let val2 = shimToFunction conv2 v2
                                              assertEqual @Text "" "PQPQPQ" $
                                                  iJoinL1 $
                                                  (iJoinL1 $
                                                   iJoinL1 val2 $ iMeetR1 $ iMeetR1 . joinf id id . iJoinL1 . iJoinSwapR)
                                                      (iMeetR1 $
                                                       iMeetSwapL .
                                                       iMeetR1 . meetf id id . joinf id id . iJoinL1 . iJoinSwapR)
                                          Nothing -> fail "wrong type"
                          liftIO $ hPutStrLn stderr $ "simplifying " <> showValType qval2
                          qval3 <- pinaforeBisubstitutes elimBisubs qval2
                          let
                              expected3Type :: PinaforeType 'Positive _
                              expected3Type =
                                  funcType (funcType textType textType) $ funcType (funcType textType textType) textType
                          liftIO @_ @() $
                              case qval3 of
                                  MkAnyValue (MkShimWit t3 (MkPolarMap conv3)) v3 ->
                                      case testEquality t3 expected3Type of
                                          Just Refl -> do
                                              let val3 = shimToFunction conv3 v3
                                              assertEqual @Text "" "PQPQPQ" $
                                                  iJoinL1 $
                                                  (iJoinL1 $ iJoinL1 val3 $ iMeetR1 $ iMeetR1 . iJoinL1) $
                                                  iMeetR1 $ iMeetR1 . iJoinL1
                                          Nothing -> fail "wrong type"
                    , let
                          makeVal :: PinaforeSourceInterpreter QValue
                          makeVal = do
                              expr1 <- qApplyExpr (qConstExpr op2) (qConstExpr @Text "PQPQPQ")
                              val1 <- qEvalExpr expr1
                              expr2 <- qApplyExpr (qConstExprAny val1) (qConstExpr idText)
                              qEvalExpr expr2
                          checkVal found = assertEqual "" "PQPQPQ" $ found id
                          in testUnifyToType @((Text -> Text) -> Text) makeVal [] checkVal
                    , let
                          makeVal :: PinaforeSourceInterpreter QValue
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
                    , tModify (failTestBecause "ISSUE #108") $
                      testExpectSuccess "testSameT \"PQPQPQ\" $ op2 \"PQPQPQ\" id id"
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
                    , tModify (failTestBecause "ISSUE #108") $
                      testExpectSuccess "op3 \"PQPQPQ\" (testSameT \"PQPQPQ\") id"
                    , tModify (failTestBecause "ISSUE #108") $ testExpectSuccess "op3 10 (testSameI 10) id"
                    ]
              ]
        , testTree
              "op4"
              [ testTree
                    "unify"
                    [ let
                          makeVal :: PinaforeSourceInterpreter QValue
                          makeVal = do
                              expr1 <- qApplyExpr (qConstExpr op4) (qConstExpr idText)
                              qEvalExpr expr1
                          checkVal found = assertEqual "" "PQPQPQ" $ found idText "PQPQPQ"
                          in testUnifyToType @((Text -> Text) -> Text -> Text) makeVal [] checkVal
                    , let
                          makeVal :: PinaforeSourceInterpreter QValue
                          makeVal = do
                              expr1 <- qApplyExpr (qConstExpr op4) (qConstExpr idText)
                              expr2 <- qApplyExpr expr1 (qConstExpr idText)
                              qEvalExpr expr2
                          checkVal found = assertEqual "" "PQPQPQ" $ found "PQPQPQ"
                          in testUnifyToType @(Text -> Text) makeVal [] checkVal
                    , let
                          makeVal :: PinaforeSourceInterpreter QValue
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
