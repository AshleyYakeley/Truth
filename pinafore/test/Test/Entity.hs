module Test.Entity
    ( testEntity
    , testUpdates
    ) where

import Control.Exception
import Pinafore
import Pinafore.Test
import Shapes
import Test.Context
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit
import Truth.Core

nullViewIO :: View a -> IO a
nullViewIO = uitRunView nullUIToolkit emptyResourceContext

scriptTest ::
       FromPinaforeType PinaforeUpdate a
    => Text
    -> Text
    -> ((?pinafore :: PinaforeContext PinaforeUpdate) => a -> IO ())
    -> ContextTestTree
scriptTest name text checker =
    contextTestCase name text $ \t ->
        withTestPinaforeContext nullUIToolkit $ \_getTableState -> do
            action <-
                ioRunInterpretResult $ pinaforeInterpretFileAtType "<test>" $ "onStop (" <> t <> ") (fail \"stopped\")"
            checker action

pointTest :: Text -> ContextTestTree
pointTest text = scriptTest text text $ nullViewIO . runPinaforeAction

assertThrows :: IO a -> IO ()
assertThrows ma = do
    t <- catch (ma >> return True) $ \(_ :: SomeException) -> return False
    if t
        then assertFailure "no exception"
        else return ()

badPointTest :: Text -> ContextTestTree
badPointTest text = scriptTest text text $ assertThrows . nullViewIO . runPinaforeAction

badInterpretTest :: Text -> ContextTestTree
badInterpretTest text c =
    testCase (unpack text) $
    withTestPinaforeContext nullUIToolkit $ \_getTableState -> do
        assertThrows $ ioRunInterpretResult $ pinaforeInterpretFile "<test>" $ prefix c <> text

exceptionTest :: Text -> ContextTestTree
exceptionTest text c =
    testCase (unpack text) $
    withTestPinaforeContext nullUIToolkit $ \_getTableState -> do
        action <- ioRunInterpretResult $ pinaforeInterpretFile "<test>" $ prefix c <> text
        assertThrows $ nullViewIO action

updateTest :: Text -> ContextTestTree
updateTest text =
    scriptTest text text $ \action -> do
        (sendUpdate, ref) <- nullViewIO $ unliftPinaforeActionOrFail action
        runLifeCycle $
            subscribeEditor emptyResourceContext (unPinaforeRef $ immutableRefToRejectingRef ref) $
            checkUpdateEditor (Known (1 :: Integer)) $ nullViewIO $ unliftPinaforeActionOrFail sendUpdate

testUpdates :: TestTree
testUpdates = runContext $ tgroup "update" [updateTest "do ref <- newMemRef; return (ref := 1, ref) end"]

testEntity :: TestTree
testEntity =
    runContext $
    context
        [ "pass = return ()"
        , "runreforfail r = runRef (r ?? {fail \"unknown ref\"})"
        , "testeq expected found = runreforfail {if %expected == %found then pass else fail \"not equal\"}"
        , "testneq expected found = runreforfail {if %expected /= %found then pass else fail \"equal\"}"
        , "testisknown t = runRef {if %(known t) then pass else fail \"known\"}"
        , "testisunknown t = runRef {if %(known t) then fail \"known\" else pass}"
        , "testeqval e f = testeq {e} {f}"
        , "expectStop p = onStop (p >> fail \"no stop\") pass"
        ] $
    tgroup
        "entity"
        [ context [] $
          tgroup
              "current" -- stack test pinafore --test-arguments "--pattern entity.current"
              []
        , tgroup
              "pass"
              [ pointTest "pass"
              , pointTest "pass >> pass"
              , pointTest "if True then pass else fail \"failed\""
              , pointTest "pass >> if True then pass else fail \"failed\""
              ]
        , tgroup
              "bad parse"
              [ badInterpretTest ""
              , badInterpretTest "x"
              , badInterpretTest "("
              , badInterpretTest ")"
              , badInterpretTest "pass x"
              , badInterpretTest "pass pass"
              , badInterpretTest "pass in"
              , badInterpretTest "pass ("
              , badInterpretTest "pass )"
              , badInterpretTest "pass let"
              ]
        , tgroup
              "fail"
              [ exceptionTest "fail \"text\""
              , exceptionTest "let in fail \"text\""
              , exceptionTest "let t = 1 in fail \"text\""
              , exceptionTest "let opentype T in fail \"text\""
              ]
        , tgroup
              "do"
              [ pointTest "do return () end"
              , pointTest "do return (); end"
              , pointTest "do testeqval 3 3 end"
              , pointTest "do a <- return 3; testeqval 3 a end"
              , pointTest "do a <- return 3; b <- return $ a + a; testeqval 6 b end"
              ]
        , context ["flagRef = do r <- newMemRef; r := False; return r; end"] $
          tgroup
              "stop"
              [ pointTest "return ()"
              , badPointTest "fail \"failure\""
              , pointTest "expectStop stop"
              , pointTest "expectStop $ do stop; fail \"unstopped\"; end"
              , pointTest "do a <- onStop (return 1) (return 2); testeqval 1 a; end"
              , pointTest "do a <- onStop (return 1) stop; testeqval 1 a; end"
              , badPointTest "do a <- onStop (return 1) stop; fail \"unstopped\"; end"
              , pointTest "do a <- onStop stop (return 2); testeqval 2 a; end"
              , badPointTest "do a <- onStop stop (return 2); fail \"unstopped\"; end"
              , pointTest
                    "do r1 <- flagRef; r2 <- flagRef; onStop (r1 := True) (r2 := True); testeq {True} r1; testeq {False} r2; end"
              , pointTest
                    "do r1 <- flagRef; r2 <- flagRef; onStop (do r1 := True; stop; end) (r2 := True); testeq {True} r1; testeq {True} r2; end"
              , pointTest
                    "do r1 <- flagRef; r2 <- flagRef; onStop (do stop; r1 := True; end) (r2 := True); testeq {False} r1; testeq {True} r2; end"
              ]
        , tgroup
              "equality"
              [ pointTest "testeqval 1 1"
              , pointTest "testeqval 1 \"1\""
              , pointTest "testeqval False $ 0 == 1"
              , pointTest "testeqval True $ 1 == 1"
              , pointTest "testeqval False $ 1 == ~1"
              ]
        , tgroup
              "reference notation"
              [ pointTest "runreforfail {pass}"
              , pointTest "let p = pass in runreforfail {p}"
              , pointTest "runreforfail {let p = pass in p}"
              , pointTest "runreforfail {%{pass}}"
              , pointTest "let rp = {pass} in runreforfail {%rp}"
              , pointTest "runreforfail {let rp = {pass} in %rp}"
              , pointTest "let rp = {pass} in runreforfail {let p= %rp in p}"
              ]
        , tgroup
              "reference stop"
              [ pointTest "expectStop $ stop"
              , pointTest "expectStop $ get unknown"
              , pointTest "expectStop $ {1} := 1"
              , pointTest "expectStop $ delete {1}"
              ]
        , tgroup
              "memory references"
              [ pointTest "expectStop $ do r <- newMemRef; get r; end"
              , pointTest "do r <- newMemRef; r := 45; a <- get r; testeqval 45 a; end"
              , pointTest "do r <- newMemRef; r := 3; r := 4; a <- get r; testeqval 4 a; end"
              , pointTest "do s <- newMemFiniteSet; n <- get $ count s; testeqval 0 n; end"
              , pointTest "do s <- newMemFiniteSet; s += 57; n <- get $ count s; testeqval 1 n; end"
              , pointTest "do s <- newMemFiniteSet; s -= 57; n <- get $ count s; testeqval 0 n; end"
              , pointTest "do s <- newMemFiniteSet; s += 57; s -= 57; n <- get $ count s; testeqval 0 n; end"
              , pointTest
                    "do s <- newMemFiniteSet; s += 57; m54 <- get $ member s {54}; m57 <- get $ member s {57}; testeqval False m54; testeqval True m57; end"
              , pointTest "do s <- newMemFiniteSet; s -= 57; m57 <- get $ member s {57}; testeqval False m57; end"
              , pointTest
                    "do s <- newMemFiniteSet; s += 57; s -= 57; m57 <- get $ member s {57}; testeqval False m57; end"
              , pointTest
                    "do s <- newMemFiniteSet; member s {57} := True; m54 <- get $ member s {54}; m57 <- get $ member s {57}; testeqval False m54; testeqval True m57; end"
              , pointTest
                    "do s <- newMemFiniteSet; member s {57} := False; m57 <- get $ member s {57}; testeqval False m57; end"
              , pointTest
                    "do s <- newMemFiniteSet; member s {57} := True; member s {57} := False; m57 <- get $ member s {57}; testeqval False m57; end"
              , pointTest "expectStop $ do r <- newMemRef; immutRef r := 5; end"
              ]
        , context
              [ "convr :: Rational -> Rational;convr = id"
              , "convn :: Number -> Number;convn = id"
              , "convl :: Literal -> Literal;convl = id"
              , "testconvr :: Rational -> Action ();testconvr r = testeq {convl r} {convl $ convn r}"
              ] $
          tgroup
              "literal conversion"
              [ tgroup
                    "Rational to Number"
                    [ pointTest "testconvr 1"
                    , pointTest "testconvr 2.5"
                    , pointTest "testeq {convl 31.5} {convl $ convn 31.5}"
                    , pointTest "testeq {\"63/2\"} {toText 31.5}"
                    , pointTest "testeq {\"63/2\"} {toText $ convn 31.5}"
                    ]
              ]
        , context
              [ "opentype E"
              , "eea = property @E @E !\"eea\""
              , "eeb = property @E @E !\"eeb\""
              , "eec = property @E @E !\"eec\""
              , "eed = property @E @E !\"eed\""
              , "eta = property @E @Text !\"eta\""
              , "eia = property @E @Integer !\"eia\""
              , "eib = property @E @Integer !\"eib\""
              , "eic = property @E @Integer !\"eic\""
              , "tea = property @Text @E !\"tea\""
              , "nea = property @Integer @E !\"nea\""
              , "e1 = entity @E !\"e1\""
              , "e2 = entity @E !\"e2\""
              , "e3 = entity @E !\"e3\""
              , "e4 = entity @E !\"e4\""
              , "eba = property @E @Boolean !\"eba\""
              , "era = property @E @Rational !\"era\""
              , "ena = property @E @Number !\"ena\""
              ] $
          tgroup
              "Storage"
              [ tgroup
                    "unknown & known"
                    [ pointTest "testisunknown {% (eta !$ {e1}) == % (eta !$ {e1})}"
                    , pointTest "runreforfail {if %(known unknown) then fail \"failed\" else pass}"
                    , pointTest "runreforfail {if %(known $ eta !$ {e1}) then fail \"failed\" else pass}"
                    , pointTest "pass >> runreforfail {if %(known $ eta !$ {e1}) then fail \"failed\" else pass}"
                    , pointTest "runreforfail {pass >> if %(known $ eta !$ {e1}) then fail \"failed\" else pass}"
                    , pointTest "runreforfail {if %(known $ eta !$ {e1}) then fail \"failed\" else pass} >> pass"
                    , pointTest "testisunknown unknown"
                    , pointTest "testisunknown (eta !$ {e1})"
                    , pointTest "testisunknown $ unknown ?? unknown"
                    , pointTest "testeq {0} $ unknown ?? {0}"
                    , pointTest "testeq {1} $ {1} ?? {0}"
                    , pointTest "testeq {1} $ {1} ?? unknown"
                    ]
              , tgroup
                    ":="
                    [ pointTest "eta !$ {e1} := \"hello\""
                    , pointTest "eea !$ {e1} := e2"
                    , pointTest "eea !$ {e1} := e2 >> testeq {e2} (eea !$ {e1})"
                    , pointTest "eta !$ {e1} := \"hello\" >> testeq {\"hello\"} (eta !$ {e1})"
                    , pointTest "tea !$ {\"hello\"} := e1 >> testeq {e1} (tea !$ {\"hello\"})"
                    , pointTest "tea !$ {\"hello\"} := e1 >> runRef {outputLn (toText $ %(count (tea !@ {e1})))}"
                    , pointTest "tea !$ {\"hello\"} := e1 >> testeq {1} (count (tea !@ {e1}))"
                    , pointTest "(eea !. eea) !$ {e1} := e2"
                    , pointTest
                          "do (eea !. eea) !$ {e1} := e2; testeq {e2} ((eea !. eea) !$ {e1}); testeq {e2} (eea !$ (eea !$ {e1})); end"
                    , pointTest
                          "do eea !$ (eea !$ {e1}) := e2; testeq {e2} ((eea !. eea) !$ {e1}); testeq {e2} (eea !$ (eea !$ {e1})); end"
                    , pointTest "expectStop $ do r <- newMemRef; eia !$ r := 4; end"
                    ]
              , tgroup
                    "+="
                    [ pointTest "eta !@ {\"hello\"} += e1"
                    , pointTest "eta !@ {\"hello\"} += e1 >> pass"
                    , pointTest "eta !@ {\"hello\"} += e1 >> testeq {\"hello\"} (eta !$ {e1})"
                    ]
              , tgroup
                    "-="
                    [pointTest "eta !@ {\"hello\"} += e1 >> eta !@ {\"hello\"} -= e1 >> testisunknown (eta !$ {e1})"]
              , tgroup
                    "removeAll"
                    [ pointTest
                          "eta !@ {\"hello\"} += e1 >> removeAll (eta !@ {\"hello\"}) >> testisunknown (eta !$ {e1})"
                    ]
              , tgroup
                    "literal storage"
                    [ tgroup
                          "Boolean"
                          [ pointTest "eba !$ {e1} := True >> testeq {True} (eba !$ {e1})"
                          , pointTest "eba !$ {e1} := False >> testeq {False} (eba !$ {e1})"
                          ]
                    , tgroup
                          "Text"
                          [ pointTest "eta !$ {e1} := \"\" >> testeq {\"\"} (eta !$ {e1})"
                          , pointTest "eta !$ {e1} := \"hello\" >> testeq {\"hello\"} (eta !$ {e1})"
                          ]
                    , tgroup
                          "Integer"
                          [ pointTest "eia !$ {e1} := 0 >> testeq {0} (eia !$ {e1})"
                          , pointTest "eia !$ {e1} := 47 >> testeq {47} (eia !$ {e1})"
                          , pointTest "eia !$ {e1} := -12 >> testeq {-12} (eia !$ {e1})"
                          ]
                    , tgroup
                          "Rational"
                          [ pointTest "era !$ {e1} := 0 >> testeq {0} (era !$ {e1})"
                          , pointTest "era !$ {e1} := 47 >> testeq {47} (era !$ {e1})"
                          , pointTest "era !$ {e1} := -12 >> testeq {-12} (era !$ {e1})"
                          , pointTest "era !$ {e1} := 31.5 >> testeq {31.5} (era !$ {e1})"
                          , pointTest "era !$ {e1} := -22.8_70 >> testeq {-22.8_70} (era !$ {e1})"
                          ]
                    , tgroup
                          "Number"
                          [ pointTest "ena !$ {e1} := 0 >> testeq {0} (ena !$ {e1})"
                          , pointTest "ena !$ {e1} := 47 >> testeq {47} (ena !$ {e1})"
                          , pointTest "ena !$ {e1} := -12 >> testeq {-12} (ena !$ {e1})"
                          , pointTest "ena !$ {e1} := 31.5 >> testeq {31.5} (ena !$ {e1})"
                          , pointTest "ena !$ {e1} := -22.8_70 >> testeq {-22.8_70} (ena !$ {e1})"
                          , pointTest "ena !$ {e1} := ~36.4 >> testeq {~36.4} (ena !$ {e1})"
                          , pointTest "ena !$ {e1} := ~-22.1 >> testeq {~-22.1} (ena !$ {e1})"
                          , pointTest "ena !$ {e1} := ~-0 >> testeq {~-0} (ena !$ {e1})"
                          , pointTest "ena !$ {e1} := (0 ~/ 0) >> testeq {0 ~/ 0} (ena !$ {e1})"
                          , pointTest "ena !$ {e1} := (1 ~/ 0) >> testeq {1 ~/ 0} (ena !$ {e1})"
                          , pointTest "ena !$ {e1} := (-1 ~/ 0) >> testeq {-1 ~/ 0} (ena !$ {e1})"
                          ]
                    ]
              , tgroup
                    "matching literals"
                    [ pointTest
                          "eta !$ {e1} := \"hello\" >> eta !$ {e2} := \"hello\" >> testeq (eta !$ {e1}) (eta !$ {e2})"
                    ]
              , tgroup
                    "identity morphism"
                    [ pointTest "(identity !$ eea !$ {e1}) := e2 >> testeq {e2} (eea !$ {e1})"
                    , pointTest "(eea !$ identity !$ {e1}) := e2 >> testeq {e2} (eea !$ {e1})"
                    , pointTest "((identity !. eea) !$ {e1}) := e2 >> testeq {e2} (eea !$ {e1})"
                    , pointTest "((eea !. identity) !$ {e1}) := e2 >> testeq {e2} (eea !$ {e1})"
                    , pointTest "eea !$ {e1} := e2 >> testeq {e2} (identity !$ eea !$ {e1})"
                    , pointTest "eea !$ {e1} := e2 >> testeq {e2} (eea !$ identity !$ {e1})"
                    , pointTest "eea !$ {e1} := e2 >> testeq {e2} ((identity !. eea) !$ {e1})"
                    , pointTest "eea !$ {e1} := e2 >> testeq {e2} ((eea !. identity) !$ {e1})"
                    , pointTest "(identity !$ eea !$ {e1}) := e2 >> testeq {e2} (identity !$ eea !$ {e1})"
                    ]
              , tgroup
                    "identity inverse morphism"
                    [ pointTest "(identity !@@ eta !@ {\"hello\"}) += e1 >> testisunknown (eta !$ {e1})"
                    , pointTest "(eea !@@ identity !@ {e2}) += e1 >> testneq {e2} (eea !$ {e1})"
                    , pointTest "(eta !@ {\"hello\"}) += e1 >> testeq {\"hello\"} (eta !$ {e1})"
                    , pointTest "((identity !. eta) !@ {\"hello\"}) += e1 >> testeq {\"hello\"} (eta !$ {e1})"
                    , pointTest "((eta !. identity) !@ {\"hello\"}) += e1 >> testeq {\"hello\"} (eta !$ {e1})"
                    , pointTest "eta !@ {\"hello\"} += e1 >> eta !@ {\"hello\"} -= e1 >> testisunknown (eta !$ {e1})"
                    , pointTest
                          "eta !@ {\"hello\"} += e1 >> (identity !@@ eta !@ {\"hello\"}) -= e1 >> testeq {\"hello\"} (eta !$ {e1})"
                    , pointTest "eea !@ {e2} += e1 >> testeq {e2} (eea !$ {e1})"
                    , pointTest "eea !@ {e2} += e1 >> (eea !@@ identity !@ {e2}) -= e1 >> testneq {e2} (eea !$ {e1})"
                    , pointTest
                          "eta !@ {\"hello\"} += e1 >> ((identity !. eta) !@ {\"hello\"}) -= e1 >> testisunknown (eta !$ {e1})"
                    , pointTest
                          "eta !@ {\"hello\"} += e1 >> ((eta !. identity) !@ {\"hello\"}) -= e1 >> testisunknown (eta !$ {e1})"
                    ]
              , tgroup
                    "composed morphisms"
                    [ pointTest "(eea !$ eeb !$ {e1}) := e2 >> testeq {e2} (eea !$ eeb !$ {e1})"
                    , pointTest "(eta !$ eeb !$ {e1}) := \"hello\" >> testeq {\"hello\"} (eta !$ eeb !$ {e1})"
                    , pointTest "(eea !. eeb !$ {e1}) := e2 >> testeq {e2} (eea !$ eeb !$ {e1})"
                    , pointTest "(eta !. eeb !$ {e1}) := \"hello\" >> testeq {\"hello\"} (eta !$ eeb !$ {e1})"
                    , pointTest "(eea !$ eeb !$ {e1}) := e2 >> testeq {e2} (eea !. eeb !$ {e1})"
                    , pointTest "(eta !$ eeb !$ {e1}) := \"hello\" >> testeq {\"hello\"} (eta !. eeb !$ {e1})"
                    , pointTest "(eeb !. eea) !$ {e2} := e1 >> testeq {e1} (eeb !$ eea !$ {e2})"
                    ]
              , tgroup
                    "composed inverse morphisms"
                    [ pointTest "(eeb !@@ eta !@ {\"hello\"}) += e1 >> testeq {\"hello\"} (eta !$ eeb !$ {e1})"
                    , pointTest "((eta !. eeb) !@ {\"hello\"}) += e1 >> testeq {\"hello\"} (eta !$ eeb !$ {e1})"
                    , pointTest "((eta !. eeb) !@ {\"hello\"}) += e1 >> testisunknown (eta !$ {e1})"
                    , pointTest "eeb !$ {e1} := e2 >> ((eta !. eeb) !@ {\"hello\"}) += e1 >> testeq {e2} (eeb !$ {e1})"
                    , pointTest
                          "eeb !$ {e1} := e2 >> ((eta !. eeb) !@ {\"hello\"}) += e1 >> testeq {\"hello\"} (eta !$ {e2})"
                    , pointTest "eeb !$ {e1} := e2 >> (eeb !@@ eta !@  {\"hello\"}) += e1 >> testneq {e2} (eeb !$ {e1})"
                    , pointTest
                          "eeb !$ {e1} := e2 >> (eeb !@@ eta !@  {\"hello\"}) += e1 >> testisunknown (eta !$ {e2})"
                    , pointTest
                          "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> ((eta !. eeb) !@ {\"hello\"}) -= e1 >> testeq {e2} (eeb !$ {e1})"
                    , pointTest
                          "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> ((eta !. eeb) !@ {\"hello\"}) -= e1 >> testisunknown (eta !$ {e2})"
                    , pointTest
                          "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> (eeb !@@ eta !@ {\"hello\"}) -= e1 >> testneq {e2} (eeb !$ {e1})"
                    , pointTest
                          "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> (eeb !@@ eta !@ {\"hello\"}) -= e1 >> testeq {\"hello\"} (eta !$ {e2})"
                    , pointTest
                          "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> removeAll ((eta !. eeb) !@ {\"hello\"}) >> testeq {e2} (eeb !$ {e1})"
                    , pointTest
                          "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> removeAll ((eta !. eeb) !@ {\"hello\"}) >> testisunknown (eta !$ {e2})"
                    , pointTest
                          "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> removeAll (eeb !@@ eta !@ {\"hello\"}) >> testneq {e2} (eeb !$ {e1})"
                    , pointTest
                          "eeb !$ {e1} := e2 >> eta !$ {e2} := \"hello\" >> removeAll (eeb !@@ eta !@ {\"hello\"}) >> testeq {\"hello\"} (eta !$ {e2})"
                    ]
              , tgroup
                    "single"
                    [ pointTest "testisunknown (single $ eib !$$ eia !@ {0})"
                    , pointTest "eib !$ {e1} := 1 >> eia !$ {e1} := 0 >> testeq {1} (single $ eib !$$ eia !@ {0})"
                    , pointTest
                          "eib !$ {e1} := 1 >> eia !$ {e1} := 0 >> eic !$ {e1} := 0 >> testeq {1} (single $ eib !$$ eia !@ {0})"
                    , pointTest
                          "eib !$ {e1} := 1 >> eia !$ {e1} := 0 >> eia !$ {e1} := 0 >> testeq {1} (single $ eib !$$ eia !@ {0})"
                    , pointTest
                          "eib !$ {e1} := 1 >> eib !$ {e2} := 2 >> eia !$ {e1} := 0 >> eia !$ {e2} := 0 >> testisunknown (single $ eib !$$ eia !@ {0})"
                    , pointTest
                          "eib !$ {e1} := 1 >> eib !$ {e2} := 1 >> eia !$ {e1} := 0 >> eia !$ {e2} := 0 >> testeq {1} (single $ eib !$$ eia !@ {0})"
                    ]
              , tgroup
                    "multiple set member"
                    [ pointTest "testeq {0} (count (tea !@ {e1}))"
                    , pointTest "tea !@ {e1} += \"hello\" >> testeq {e1} (tea !$ {\"hello\"})"
                    , pointTest "tea !@ {e1} += \"hello\" >> testeq {1} (count (tea !@ {e1}))"
                    , pointTest
                          "tea !@ {e1} += \"hello\" >> tea !@ {e1} += \"hello\" >> testeq {1} (count (tea !@ {e1}))"
                    , pointTest "tea !@ {e1} += \"h\" >> tea !@ {e1} += \"hello\" >> testeq {2} (count (tea !@ {e1}))"
                    , pointTest "eea !$ {e2} := e1 >> testeq {1} (count (eea !@ {e1}))"
                    , pointTest $
                      "let counter = eia !$ {e1};someset = nea !@ {e1} in " <>
                      "counter := 0 >> someset += 1 >> someset += 1 >> (get (members noOrder someset) >>= \\pp -> for pp $ \\p -> runRef {counter := %counter + 1}) >> testeq {1} counter"
                    ]
              , tgroup
                    "types"
                    [ pointTest "let opentype T1; p = property @T1 @T1 !\"p\" in pass"
                    , pointTest "let opentype T1 in let p = property @T1 @T1 !\"p\" in pass"
                    , pointTest "let opentype T1; opentype T2; p = property @T1 @T2 !\"p\" in pass"
                    , pointTest "let opentype T1; opentype T2 in let p = property @T1 @T2 !\"p\" in pass"
                    , pointTest "let opentype T1 in let opentype T2; p = property @T1 @T2 !\"p\" in pass"
                    , pointTest "let opentype T1 in let opentype T2 in let p = property @T1 @T2 !\"p\" in pass"
                    , badInterpretTest "let opentype T1 in let opentype T1 in pass"
                    , badInterpretTest "let opentype T1; opentype T1 in pass"
                    ]
              , tgroup
                    "Maybe"
                    [ pointTest
                          "let enta = property @E @(Maybe Text) !\"enta\" in enta !$ {e1} := Just \"abc\" >> (testeq {Just \"abc\"} $ enta !$ {e1})"
                    , pointTest
                          "let enta = property @E @(Maybe Text) !\"enta\" in enta !$ {e1} := Nothing >> (testeq {Nothing} $ enta !$ {e1})"
                    ]
              , tgroup
                    "List"
                    [ pointTest
                          "let enta = property @E @[Text] !\"enta\" in enta !$ {e1} := [\"abc\", \"def\"] >> (testeq {[\"abc\", \"def\"]} $ enta !$ {e1})"
                    , pointTest
                          "let enta = property @E @[Text] !\"enta\" in enta !$ {e1} := [] >> (testeq {[]} $ enta !$ {e1})"
                    ]
              , tgroup
                    "Pair/Either"
                    [ pointTest
                          "let enta = property @E @(Number, Text) !\"enta\" in enta !$ {e1} := (74,\"hmm\") >> (testneq {(71,\"hmm\")} $ enta !$ {e1})"
                    , pointTest
                          "let enta = property @E @(Number, Text) !\"enta\" in enta !$ {e1} := (74,\"hmm\") >> (testeq {(74,\"hmm\")} $ enta !$ {e1})"
                    , pointTest
                          "let enta = property @E @(Either Number Text) !\"enta\" in enta !$ {e1} := Left 74 >> (testneq {Left 73} $ enta !$ {e1})"
                    , pointTest
                          "let enta = property @E @(Either Number Text) !\"enta\" in enta !$ {e1} := Left 74 >> (testeq {Left 74} $ enta !$ {e1})"
                    , pointTest
                          "let enta = property @E @(Either Number Text) !\"enta\" in enta !$ {e1} := Right \"abc\" >> (testneq {Right \"adbc\"} $ enta !$ {e1})"
                    , pointTest
                          "let enta = property @E @(Either Number Text) !\"enta\" in enta !$ {e1} := Right \"abc\" >> (testeq {Right \"abc\"} $ enta !$ {e1})"
                    ]
              ]
        , tgroup
              "subtype"
              [ context ["opentype P", "opentype Q", "subtype P <= Q"] $
                tgroup
                    "1"
                    [ pointTest "pass"
                    , pointTest "let f :: P -> Q; f x = x in pass"
                    , pointTest "let f :: [P] -> [Q]; f x = x in pass"
                    , badInterpretTest "let f :: Q -> P; f x = x in pass"
                    ]
              , context ["opentype P", "subtype P <= Q", "opentype Q"] $
                tgroup
                    "2"
                    [ pointTest "pass"
                    , pointTest "let f :: P -> Q; f x = x in pass"
                    , pointTest "let f :: [P] -> [Q]; f x = x in pass"
                    , badInterpretTest "let f :: Q -> P; f x = x in pass"
                    ]
              , context ["subtype P <= Q", "opentype P", "opentype Q"] $
                tgroup
                    "3"
                    [ pointTest "pass"
                    , pointTest "let f :: P -> Q; f x = x in pass"
                    , pointTest "let f :: [P] -> [Q]; f x = x in pass"
                    , badInterpretTest "let f :: Q -> P; f x = x in pass"
                    ]
              , context ["opentype P", "opentype Q"] $
                tgroup
                    "local 1"
                    [ pointTest "let subtype P <= Q in pass"
                    , pointTest "let subtype P <= Q; f :: P -> Q; f x = x in pass"
                    , badInterpretTest "let subtype P <= Q; f :: Q -> P; f x = x in pass"
                    ]
              , context ["opentype P"] $
                tgroup
                    "local 2"
                    [ pointTest "let opentype Q; subtype P <= Q in pass"
                    , pointTest "let opentype Q; subtype P <= Q; f :: P -> Q; f x = x in pass"
                    , badInterpretTest "let opentype Q; subtype P <= Q; f :: Q -> P; f x = x in pass"
                    ]
              , context ["opentype Q"] $
                tgroup
                    "local 3"
                    [ pointTest "let opentype P; subtype P <= Q in pass"
                    , pointTest "let opentype P; subtype P <= Q; f :: P -> Q; f x = x in pass"
                    , badInterpretTest "let opentype P; subtype P <= Q; f :: Q -> P; f x = x in pass"
                    ]
              , context ["opentype Q", "subtype Maybe Number <= Q"] $
                tgroup
                    "closed-open" -- not allowed, per issue #28
                    [badInterpretTest "pass"]
              , tgroup
                    "Entity"
                    [ pointTest "let f :: Number -> Entity; f x = x in pass"
                    , pointTest "let f :: (a & Number) -> (Entity,a); f x = (x,x) in pass"
                    , pointTest "let f :: Maybe Number -> Entity; f x = x in pass"
                    , pointTest "let f :: Maybe (a & Number) -> (Entity,Maybe a); f x = (x,x) in pass"
                    ]
              ]
        , context
              [ "datatype T = T1 Text Number | T2 | T3 Boolean | T4 (Ref {-Boolean,+Integer} -> Integer) | T5 Text (Boolean -> Integer)"
              ] $
          tgroup
              "datatype"
              [ pointTest "pass"
              , pointTest "let t1 = T1 \"hello\" 3 in pass"
              , pointTest "let f (T1 x _) = x in pass"
              , pointTest "case T1 \"hello\" 3 of T1 \"hello\" 3 -> pass end"
              , pointTest
                    "case T1 \"hello\" 3 of T2 -> fail \"T2\"; T1 \"hello\" 2 -> fail \"T1 2\"; T1 \"hell\" 3 -> fail \"T1 hell\"; T1 \"hello\" 3 -> pass end"
              , pointTest
                    "let f :: Boolean -> Integer; f b = if b then 1 else 0 in case T5 \"abcd\" f of T5 _ ff -> if ff True == 1 then pass else fail \"ff\" end"
              , badInterpretTest "let datatype B = MkB a in pass"
              , tmodify ignoreTest $ pointTest "let datatype B a = MkB a in pass" {- ISSUE #41 -}
              ]
        , context ["closedtype T = T1 Text Number !\"T.T1\" | T2 !\"T.T2\" | T3 Boolean !\"T.T3\""] $
          tgroup
              "closedtype"
              [ pointTest "pass"
              , pointTest "let t1 = T1 \"hello\" 3 in pass"
              , pointTest "let f (T1 x _) = x in pass"
              , pointTest "case T1 \"hello\" 3 of T1 \"hello\" 3 -> pass end"
              , pointTest
                    "case T1 \"hello\" 3 of T2 -> fail \"T2\"; T1 \"hello\" 2 -> fail \"T1 2\"; T1 \"hell\" 3 -> fail \"T1 hell\"; T1 \"hello\" 3 -> pass end"
              ]
        , tgroup
              "type escape"
              [ pointTest
                    "let opentype T; t = let in entity @T !\"t\"; f = let f :: T -> Action (); f _ = pass in f; in f t"
              , badInterpretTest
                    "let opentype T1; opentype T2; t = let in entity @T1 !\"t\"; f = let f :: T2 -> Action (); f _ = pass in f; in f t"
              , badInterpretTest
                    "let t = let opentype T in entity @T !\"t\"; f = let opentype T; f :: T -> Action (); f _ = pass in f; in f t"
              , badInterpretTest
                    "let t = let opentype T1 in entity @T1 !\"t\"; f = let opentype T2; f :: T2 -> Action (); f _ = pass in f; in f t"
              ]
        , context ["opentype E", "eta = property @E @Text !\"eta\"", "e1 = entity @E !\"e1\"", "rt1 = eta !$ {e1}"] $
          tgroup
              "undo"
              [ pointTest "do rt1 := \"A\"; testeq {\"A\"} rt1; rt1 := \"B\"; testeq {\"B\"} rt1; end"
              , pointTest
                    "do rt1 := \"A\"; testeq {\"A\"} rt1; rt1 := \"B\"; testeq {\"B\"} rt1; queueUndo; testeq {\"A\"} rt1; end"
              , pointTest
                    "do rt1 := \"A\"; testeq {\"A\"} rt1; rt1 := \"B\"; testeq {\"B\"} rt1; queueUndo; testeq {\"A\"} rt1; queueRedo; testeq {\"B\"} rt1; end"
              ]
        , tgroup
              "interpret"
              [ pointTest "do r <- newMemRef; interpretIntegerAsText r := \"37\"; testeq {37} r; end"
              , pointTest "do r <- newMemRef; interpretDateAsText r := \"2015-08-12\"; testeq {Date 2015 08 12} r; end"
              ]
        ]
