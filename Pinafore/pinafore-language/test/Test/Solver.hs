module Test.Solver
    ( testSolver
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan
import Pinafore
import Pinafore.Language.API
import Pinafore.Test
import Shapes
import Test.RunScript

data SomeType polarity =
    forall a. MkSomeType (QType polarity a)

instance Is PolarityType polarity => Eq (SomeType polarity) where
    MkSomeType a == MkSomeType b = isJust $ testEquality a b

instance Is PolarityType polarity => Show (SomeType polarity) where
    show (MkSomeType t) = show t

instance Is PolarityType polarity =>
             WitnessMappable (PShimWit (JMShim Type) QType 'Positive) (PShimWit (JMShim Type) QType 'Negative) (SomeType polarity) where
    mapWitnessesM mapPos mapNeg =
        case polarityType @polarity of
            PositiveType ->
                MkEndoM $ \(MkSomeType t) -> fmap (\(MkShimWit t' _) -> MkSomeType t') (unEndoM mapPos $ mkShimWit t)
            NegativeType ->
                MkEndoM $ \(MkSomeType t) -> fmap (\(MkShimWit t' _) -> MkSomeType t') (unEndoM mapNeg $ mkShimWit t)

instance Is PolarityType polarity => VarRenameable (SomeType polarity) where
    varRename ev = MkEndoM $ \(MkSomeType t) -> fmap MkSomeType $ unEndoM (varRename ev) t

parseSomeType ::
       forall polarity. Is PolarityType polarity
    => Text
    -> QInterpreter (SomeType polarity)
parseSomeType text = do
    st <- parseType @polarity text
    case st of
        MkSome t -> return $ MkSomeType t

type SolverTester us = WriterT (TSOpenSolverExpression QTypeSystem us ()) (TSOuter QTypeSystem)

type UnifierTester = SolverTester (Unifier QTypeSystem)

type SubsumerTester = SolverTester (Subsumer QTypeSystem)

stParseType ::
       forall us polarity. (Applicative us, Is PolarityType polarity)
    => Text
    -> SolverTester us (SomeType polarity)
stParseType text = lift $ lift $ parseSomeType @polarity text

stParseTypeBoth :: Applicative us => Text -> SolverTester us (SomeType 'Negative, SomeType 'Positive)
stParseTypeBoth text = do
    tn <- stParseType text
    tp <- stParseType text
    return (tn, tp)

stRename ::
       forall us a. Applicative us
    => TSMappable QTypeSystem a => [String] -> NameRigidity -> a -> SolverTester us a
stRename fixedNames rgd a = lift $ renameMappable @QTypeSystem fixedNames rgd a

stUnify :: SomeType 'Positive -> SomeType 'Negative -> UnifierTester ()
stUnify (MkSomeType posw) (MkSomeType negw) = do
    w <-
        lift $ do
            MkComposeShim sshim <- unifyPosNegWitnesses @QTypeSystem posw negw
            return $ sshim *> pure ()
    tell w

stSubsume :: SomeType 'Positive -> SomeType 'Positive -> SubsumerTester ()
stSubsume (MkSomeType tinf) (MkSomeType tdecl) = do
    w <-
        lift $ do
            sshim <- subsumePosWitnesses @QTypeSystem tinf tdecl
            return $ sshim *> pure ()
    tell w

runUnifierTester :: TSMappable QTypeSystem a => UnifierTester a -> QInterpreter a
runUnifierTester ma =
    runRenamer @QTypeSystem [] [] $ do
        (a, MkSolverExpression ut _) <- runWriterT ma
        (_, usubs) <- solveUnifier @QTypeSystem ut
        unEndoM (unifierSubstituteSimplifyFinalRename @QTypeSystem usubs) a

runSubsumerTester :: TSMappable QTypeSystem a => [String] -> SubsumerTester a -> QInterpreter a
runSubsumerTester names sta =
    runRenamer @QTypeSystem names [] $ do
        (a, se) <- runWriterT sta
        (_expr, ssubs) <- solveSubsumerExpression @QTypeSystem se
        unEndoM (mconcat [subsumerSubstitute @QTypeSystem ssubs, simplify @QTypeSystem]) a {-, finalRenameMappable @QTypeSystem -}

testSolver :: TestTree
testSolver =
    testTree
        "solver"
        [ testTree "renamer" $ let
              renameTest :: Text -> Text -> TestTree
              renameTest origtext expectedtext =
                  testTree (unpack origtext) $
                  runTester defaultTester $
                  testerLiftInterpreter $
                  runUnifierTester $ do
                      expectedtype <- stParseType @_ @'Positive expectedtext
                      origtype <- stParseType @_ @'Positive origtext
                      foundtype <- stRename [] FreeName origtype
                      liftIO $ assertEqual "" expectedtype foundtype
              in [renameTest "x -> x" "a -> a", renameTest "rec y, Maybe. y" "rec a, Maybe. a"]
        , testTree "simplifier" $ let
              simplifyTest :: Text -> Text -> TestTree
              simplifyTest origtext expectedtext =
                  testTree (unpack origtext) $
                  runTester defaultTester $
                  testerLiftInterpreter $ do
                      expectedtype <- parseSomeType @'Positive expectedtext
                      simplifiedType <- runUnifierTester $ stParseType @_ @'Positive origtext
                      liftIO $ assertEqual "" expectedtype simplifiedType
              in [ simplifyTest "x -> x" "a -> a"
                 , simplifyTest "rec y, Maybe. y" "rec a, Maybe. a"
                 , simplifyTest "xa -> xa | xa -> xa" "a -> a"
                 , simplifyTest "rec xa, Unit" "Unit"
                 , simplifyTest "rec xa, Integer | rec xa, Maybe xa" "Integer | rec a, Maybe a"
                 , simplifyTest "Maybe Unit | (rec ra, Maybe ra)" "Maybe Unit | (rec a, Maybe a)"
                 ]
        , testTree "unifier" $ let
              unifierTest :: String -> Text -> UnifierTester (SomeType 'Positive) -> TestTree
              unifierTest name expectedtext uta =
                  testTree name $
                  runTester defaultTester $
                  testerLiftInterpreter $ do
                      expectedtype <- parseSomeType @'Positive expectedtext
                      found <- runUnifierTester uta
                      liftIO $ assertEqual "" expectedtype found
              scriptTest :: String -> Text -> Text -> TestTree
              scriptTest name text r =
                  testTree name $
                  runTester defaultTester $ do
                      expr <- testerLiftInterpreter $ parseTopExpression text
                      liftIO $ assertEqual "" r $ showText expr
              applyTest :: String -> Text -> Text -> Text -> TestTree
              applyTest name ftext xtext extext =
                  testTree
                      name
                      [ unifierTest "unifier" extext $ do
                            ftype <- stParseType ftext
                            gtype <- stParseType $ xtext <> " -> r"
                            rtype <- stParseType "r"
                            ftype' <- stRename [] FreeName ftype
                            (gtype', rtype') <- stRename [] FreeName (gtype, rtype)
                            stUnify ftype' gtype'
                            return rtype'
                      , scriptTest
                            "script"
                            ("let f: " <> ftext <> " = error \"f\"; x: " <> xtext <> " = error \"x\" in f x")
                            ("{} => " <> extext)
                      ]
              in [ testTree
                       "simple"
                       [ unifierTest "a" "Unit" $ do
                             tu <- stParseType "Unit"
                             ta <- stParseTypeBoth "a"
                             (tan, tap) <- stRename [] FreeName ta
                             stUnify tu tan
                             return tap
                       , unifierTest "a -> a" "Unit" $ do
                             ta <- stParseType "a"
                             tua <- stParseType "a -> Unit"
                             tbb <- stParseType "b -> b"
                             (ta', (tua', tbb')) <- stRename [] FreeName (ta, (tua, tbb))
                             stUnify tua' tbb'
                             return ta'
                       ]
                 , testTree
                       "apply"
                       [ applyTest "function-0" "Number -> Text" "Integer" "Text."
                       , applyTest "id-0" "t -> t" "Unit" "Unit."
                       , applyTest "id-1" "t -> t" "(a -> a)" "a -> a"
                       , applyTest "fix-0" "(t -> t) -> t" "(Unit -> Unit)" "Unit."
                       , applyTest "fix-1" "(t -> t) -> t" "(a -> a)" "None"
                       , applyTest "fix-2" "(t -> t) -> t" "((a -> a) -> (a -> a))" "a -> a"
                       ]
                 , failTestBecause "#206" $
                   testTree
                       "issue-206"
                       [ unifierTest "rec-0" "rec a, Maybe. a" $ do
                             ta <- stParseTypeBoth "a"
                             tma <- stParseTypeBoth "Maybe a"
                             ((tan, tap), (_tman, tmap)) <- stRename [] FreeName (ta, tma)
                             stUnify tmap tan
                             return tap
                       , unifierTest "rec-1" "None" $ do
                             ta <- stParseTypeBoth "a"
                             tma <- stParseTypeBoth "Maybe a"
                             ((_tan, tap), (tman, _tmap)) <- stRename [] FreeName (ta, tma)
                             stUnify tap tman
                             return tap
                       , unifierTest "rec-2" "rec a, Maybe. a" $ do
                             ta <- stParseTypeBoth "a"
                             tma <- stParseTypeBoth "Maybe a"
                             ((tan, tap), (tman, tmap)) <- stRename [] FreeName (ta, tma)
                             stUnify tmap tan
                             stUnify tap tman
                             return tap
                       , applyTest
                             "issue-206-1"
                             "(t -> t) -> t"
                             "((Maybe a -> Maybe a) -> (a -> a))"
                             "Any -> (rec a, Maybe. a)"
                       , applyTest
                             "issue-206-2"
                             "(t -> t) -> t"
                             "((a -> a) -> (Maybe a -> Maybe a))"
                             "(rec a, Maybe. a) -> Maybe. None"
                       ]
                 ]
        , testTree "recursive" $ let
              recursiveTest :: Text -> Text -> Text -> TestTree
              recursiveTest textp textq expected =
                  testTree (unpack $ "(" <> textp <> ")" <> " -> " <> textq) $
                  runTester defaultTester $
                  testerLiftInterpreter $ do
                      expectedtype <- parseSomeType @'Positive expected
                      found <-
                          runUnifierTester $ do
                              tp <- stParseType @_ @'Negative textp
                              tq <- stParseType @_ @'Positive textq
                              (tp', tq') <- stRename [] FreeName (tp, tq)
                              stUnify tq' tp'
                              return tq'
                      liftIO $ assertEqual "" expectedtype found
              in [ recursiveTest "Integer -> Any" "a -> a" "a -> (a | Integer.)"
                 , recursiveTest "Maybe b -> Maybe b" "a -> a" "Maybe a -> Maybe a"
                 , recursiveTest "a" "Maybe a" "rec a, Maybe a"
                 , recursiveTest "Any" "Integer" "Integer"
                 , recursiveTest "(Text | Integer) -> Any" "a -> a" "a -> (a | (Integer. | Text.))"
                 , testTree
                       "issue-229"
                       [ recursiveTest "a & (a -> b)" "c -> c" "a -> (rec b, a | a -> b)"
                       , recursiveTest "c" "c -> c" "a -> (rec b, a | a -> b)"
                       ]
                 ]
        , testTree "subsumer" $ let
              subsumerTest :: String -> [String] -> SubsumerTester () -> TestTree
              subsumerTest name names stu =
                  testTree name $ runTester defaultTester $ testerLiftInterpreter $ runSubsumerTester names stu
              in [ subsumerTest "simple-1" [] $ do
                       tinf <- stParseType "a -> a"
                       tinf' <- stRename [] FreeName tinf
                       tdecl <- stParseType "Integer -> Integer"
                       tdecl' <- stRename [] RigidName tdecl
                       stSubsume tinf' tdecl'
                 , subsumerTest "simple-2" ["b"] $ do
                       tinf <- stParseType "a -> a"
                       tinf' <- stRename [] FreeName tinf
                       tdecl <- stParseType "b -> b"
                       tdecl' <- stRename ["b"] RigidName tdecl
                       stSubsume tinf' tdecl'
                 , subsumerTest "simple-3" ["a"] $ do
                       tinf <- stParseType "a -> a"
                       tinf' <- stRename [] FreeName tinf
                       tdecl <- stParseType "Maybe a -> Maybe a"
                       tdecl' <- stRename ["a"] RigidName tdecl
                       stSubsume tinf' tdecl'
                 ]
        ]
