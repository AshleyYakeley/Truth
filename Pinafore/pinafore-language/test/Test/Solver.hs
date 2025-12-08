module Test.Solver
    ( testSolver
    )
where

import Data.Shim
import Language.Expression.TypeSystem
import Shapes
import Shapes.Test

import Pinafore.Test.Internal

data SomeType polarity
    = forall a. MkSomeType (QType polarity a)

instance Is PolarityType polarity => Eq (SomeType polarity) where
    MkSomeType a == MkSomeType b = isJust $ testEquality a b

instance Is PolarityType polarity => Show (SomeType polarity) where
    show (MkSomeType t) = show t

instance
    Is PolarityType polarity =>
    WitnessMappable (PShimWit (JMShim Type) QType 'Positive) (PShimWit (JMShim Type) QType 'Negative) (SomeType polarity)
    where
    mapWitnessesM mapPos mapNeg =
        case polarityType @polarity of
            PositiveType ->
                MkEndoM $ \(MkSomeType t) -> fmap (\(MkShimWit t' _) -> MkSomeType t') (unEndoM mapPos $ mkShimWit t)
            NegativeType ->
                MkEndoM $ \(MkSomeType t) -> fmap (\(MkShimWit t' _) -> MkSomeType t') (unEndoM mapNeg $ mkShimWit t)

instance Is PolarityType polarity => VarRenameable (SomeType polarity) where
    varRename ev = MkEndoM $ \(MkSomeType t) -> fmap MkSomeType $ unEndoM (varRename ev) t

parseSomeType ::
    forall polarity.
    Is PolarityType polarity =>
    Text ->
    QInterpreter (SomeType polarity)
parseSomeType text = do
    st <- parseType @polarity text
    case st of
        MkSome t -> return $ MkSomeType t

parseTypeBoth :: Text -> QInterpreter (SomeType 'Negative, SomeType 'Positive)
parseTypeBoth text = do
    tn <- parseSomeType text
    tp <- parseSomeType text
    return (tn, tp)

type SolverTester us = WriterT (TSOpenSolverExpression QTypeSystem us ()) (TSOuter QTypeSystem)

type UnifierTester = SolverTester (Unifier QTypeSystem)

type SubsumerTester = SolverTester (Subsumer QTypeSystem)

stRename ::
    forall us a.
    Applicative us =>
    TSMappable QTypeSystem a =>
    [String] -> NameRigidity -> a -> SolverTester us a
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
    qRunTypeM
        $ runRenamer @QTypeSystem [] []
        $ do
            (a, MkSolverExpression ut _) <- runWriterT ma
            (_, usubs) <- solveUnifier @QTypeSystem ut
            unEndoM (unifierSubstituteSimplifyFinalRename @QTypeSystem usubs) a

runSubsumerTester :: TSMappable QTypeSystem a => [String] -> SubsumerTester a -> QInterpreter a
runSubsumerTester names sta =
    qRunTypeM
        $ runRenamer @QTypeSystem names []
        $ do
            (a, se) <- runWriterT sta
            (_expr, ssubs) <- solveSubsumerExpression @QTypeSystem se
            unEndoM
                (mconcat [subsumerSubstitute @QTypeSystem ssubs, simplify @QTypeSystem, finalRenameMappable @QTypeSystem])
                a

typeTest :: String -> Text -> QInterpreter (SomeType 'Positive) -> TestTree
typeTest name expectedtext ma =
    testTree name
        $ runTester defaultTester
        $ testerLiftInterpreter
        $ do
            expectedtype <- parseSomeType @'Positive expectedtext
            found <- ma
            liftIO $ assertEqual "" expectedtype found

testSolver :: TestTree
testSolver =
    testTree
        "solver"
        [ testTree "equality" $ let
            equalityTest :: Bool -> Text -> Text -> TestTree
            equalityTest expectEqual sta stb =
                testTree
                    ( unpack
                        $ sta
                        <> ( if expectEqual
                                then " = "
                                else " /= "
                           )
                        <> stb
                    )
                    $ runTester defaultTester
                    $ testerLiftInterpreter
                    $ do
                        MkSomeType ta <- parseSomeType @'Positive sta
                        MkSomeType tb <- parseSomeType @'Positive stb
                        liftIO
                            $ case testEquality ta tb of
                                Just Refl ->
                                    if expectEqual
                                        then return ()
                                        else assertFailure "not equal"
                                Nothing ->
                                    if expectEqual
                                        then assertFailure "equal"
                                        else return ()
            in [ equalityTest True "Unit" "Unit"
               , equalityTest False "Unit" "Integer"
               , equalityTest False "a" "b"
               , equalityTest True "a" "a"
               , equalityTest True "rec a, Maybe a" "rec a, Maybe a"
               , equalityTest True "rec a, Maybe a" "rec b, Maybe b"
               , equalityTest True "rec a, List (rec b, Maybe b) | Maybe a" "rec b, List (rec a, Maybe a) | Maybe b"
               , equalityTest True "(rec a, Maybe a) *: (rec a, List a)" "(rec a, Maybe a) *: (rec b, List b)"
               , equalityTest True "Maybe (rec r, Maybe r | a) | a" "Maybe (rec s, Maybe s | a) | a"
               ]
        , testTree "renamer" $ let
            renameTest :: Text -> Text -> TestTree
            renameTest origtext expectedtext =
                testTree (unpack origtext)
                    $ runTester defaultTester
                    $ testerLiftInterpreter
                    $ do
                        expectedtype <- parseSomeType @'Positive expectedtext
                        origtype <- parseSomeType @'Positive origtext
                        foundtype <- runUnifierTester $ stRename [] FreeName origtype
                        liftIO $ assertEqual "" expectedtype foundtype
            in [renameTest "x -> x" "a -> a", renameTest "rec y, Maybe y" "rec a, Maybe a"]
        , testTree "simplifier" $ let
            simplifyTest :: Text -> Text -> TestTree
            simplifyTest origtext expectedtext =
                testTree (unpack origtext)
                    $ runTester defaultTester
                    $ testerLiftInterpreter
                    $ do
                        expectedtype <- parseSomeType @'Positive expectedtext
                        origtype <- parseSomeType @'Positive origtext
                        simplifiedType <- runUnifierTester $ pure origtype
                        liftIO $ assertEqual "" expectedtype simplifiedType
            in [ simplifyTest "x -> x" "a -> a"
               , simplifyTest "rec y, Maybe y" "rec a, Maybe a"
               , simplifyTest "xa -> xa | xa -> xa" "a -> a"
               , simplifyTest "rec xa, Unit" "Unit"
               , simplifyTest "rec xa, Integer | rec xa, Maybe xa" "Integer | rec a, Maybe a"
               , simplifyTest "Maybe Number | (rec ra, Maybe ra)" "Maybe. (Number. | (rec a, Maybe. a))"
               ]
        , testTree "unifier" $ let
            subtypeTest :: String -> Text -> Text -> TestTree
            subtypeTest name stp stn =
                testTree name
                    $ runTester defaultTester
                    $ testerLiftInterpreter
                    $ do
                        tpos <- parseSomeType stp
                        tneg <- parseSomeType stn
                        runUnifierTester $ do
                            (tpos', tneg') <- stRename [] FreeName (tpos, tneg)
                            stUnify tpos' tneg'
            scriptTest :: String -> Text -> Text -> TestTree
            scriptTest name text r =
                testTree name
                    $ runTester defaultTester
                    $ do
                        expr <- testerLiftInterpreter $ parseTopExpression text
                        liftIO $ assertEqual "" r $ showText expr
            applyTest :: String -> Text -> Text -> Text -> TestTree
            applyTest name ftext xtext extext =
                testTree
                    name
                    [ typeTest "unifier" extext $ do
                        ftype <- parseSomeType ftext
                        gtype <- parseSomeType $ xtext <> " -> r"
                        rtype <- parseSomeType "r"
                        runUnifierTester $ do
                            ftype' <- stRename [] FreeName ftype
                            (gtype', rtype') <- stRename [] FreeName (gtype, rtype)
                            stUnify ftype' gtype'
                            return rtype'
                    , scriptTest
                        "script"
                        ("let {f: " <> ftext <> " = error \"f\"; x: " <> xtext <> " = error \"x\"} f x")
                        ("{} => " <> extext)
                    ]
            in [ testTree
                    "simple"
                    [ typeTest "a" "Unit" $ do
                        tu <- parseSomeType "Unit"
                        ta <- parseTypeBoth "a"
                        runUnifierTester $ do
                            (tan, tap) <- stRename [] FreeName ta
                            stUnify tu tan
                            return tap
                    , typeTest "a -> a" "Unit" $ do
                        ta <- parseSomeType "a"
                        tua <- parseSomeType "a -> Unit"
                        tbb <- parseSomeType "b -> b"
                        runUnifierTester $ do
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
               , testTree
                    "issue-206"
                    [ typeTest "rec-0" "rec a, Maybe a" $ do
                        ta <- parseTypeBoth "a"
                        tma <- parseTypeBoth "Maybe a"
                        runUnifierTester $ do
                            ((tan, tap), (_tman, tmap)) <- stRename [] FreeName (ta, tma)
                            stUnify tmap tan
                            return tap
                    , typeTest "rec-1" "None" $ do
                        ta <- parseTypeBoth "a"
                        tma <- parseTypeBoth "Maybe a"
                        runUnifierTester $ do
                            ((_tan, tap), (tman, _tmap)) <- stRename [] FreeName (ta, tma)
                            stUnify tap tman
                            return tap
                    , typeTest "rec-2" "rec a, Maybe a" $ do
                        ta <- parseTypeBoth "a"
                        tma <- parseTypeBoth "Maybe a"
                        runUnifierTester $ do
                            ((tan, tap), (tman, tmap)) <- stRename [] FreeName (ta, tma)
                            stUnify tmap tan
                            stUnify tap tman
                            return tap
                    , applyTest
                        "issue-206-1"
                        "(t -> t) -> t"
                        "((Maybe a -> Maybe a) -> (a -> a))"
                        "(rec a, b & Maybe. a) -> (rec c, b | Maybe. c)"
                    , applyTest
                        "issue-206-2"
                        "(t -> t) -> t"
                        "((a -> a) -> (Maybe a -> Maybe a))"
                        "Maybe. (rec a, b & Maybe. a) -> Maybe. (rec c, b | Maybe. c)"
                    ]
               , testTree
                    "issue-234"
                    [ subtypeTest "0" "Integer" "Integer"
                    , subtypeTest "1" "rec b, Maybe (rec c, b | Maybe c)" "rec a, Maybe a"
                    , subtypeTest "2" "rec a, (rec b, Maybe (rec c, b | Maybe c)) | Maybe a" "rec a, Maybe a"
                    ]
               , testTree
                    "issue-237"
                    [ subtypeTest
                        "concrete"
                        "rec r, Maybe r"
                        "rec d, (rec f, (rec h, (rec i, Maybe (rec d, (rec f, (rec h, i & Maybe (rec d, (rec f, h & Maybe (rec d, f & Maybe d)) & Maybe d)) & Maybe (rec d, f & Maybe d)) & Maybe d)) & Maybe (rec d, (rec f, h & Maybe (rec d, f & Maybe d)) & Maybe d)) & Maybe (rec d, f & Maybe d)) & Maybe d"
                    , typeTest "free" "rec a, Maybe. a" $ do
                        tb <- parseSomeType "b"
                        tpos <- parseSomeType "rec r, b | Maybe r"
                        tneg <-
                            parseSomeType
                                "rec d, (rec f, (rec h, (rec i, b & Maybe (rec d, (rec f, (rec h, i & Maybe (rec d, (rec f, h & Maybe (rec d, f & Maybe d)) & Maybe d)) & Maybe (rec d, f & Maybe d)) & Maybe d)) & Maybe (rec d, (rec f, h & Maybe (rec d, f & Maybe d)) & Maybe d)) & Maybe (rec d, f & Maybe d)) & Maybe d"
                        runUnifierTester $ do
                            (tb', (tpos', tneg')) <- stRename [] FreeName (tb, (tpos, tneg))
                            stUnify tpos' tneg'
                            return tb'
                    ]
               ]
        , testTree "recursive" $ let
            recursiveTest :: Text -> Text -> Text -> TestTree
            recursiveTest textp textq expected =
                testTree (unpack $ "(" <> textp <> ")" <> " -> " <> textq)
                    $ runTester defaultTester
                    $ testerLiftInterpreter
                    $ do
                        expectedtype <- parseSomeType @'Positive expected
                        tp <- parseSomeType @'Negative textp
                        tq <- parseSomeType @'Positive textq
                        found <-
                            runUnifierTester $ do
                                (tp', tq') <- stRename [] FreeName (tp, tq)
                                stUnify tq' tp'
                                return tq'
                        liftIO $ assertEqual "" expectedtype found
            in [ recursiveTest "Integer -> Any" "a -> a" "a -> (a | Integer.)"
               , recursiveTest "Maybe b -> Maybe b" "a -> a" "Maybe a -> Maybe a"
               , recursiveTest "a" "Maybe a" "rec a, Maybe a"
               , recursiveTest "Any" "Integer" "Integer"
               , recursiveTest "(Text | Integer) -> Any" "a -> a" "a -> (a | (Text. | Integer.))"
               , testTree
                    "issue-229"
                    [ recursiveTest "a & (a -> b)" "c -> c" "a -> (rec b, a | a -> b)"
                    , recursiveTest "c" "Integer -> c" "rec a, Integer -> a"
                    , recursiveTest "c" "c -> Integer" "Any -> Integer"
                    , recursiveTest "c" "c -> c" "a -> (rec b, a | a -> b)"
                    ]
               ]
        , testTree "subsumer" $ let
            subsumeTest :: String -> Text -> Text -> [String] -> TestTree
            subsumeTest name sinf sdecl sdeclfreevars =
                testTree name $ runTester defaultTester $ testerLiftInterpreter $ do
                    tinf <- parseSomeType sinf
                    tdecl <- parseSomeType sdecl
                    runSubsumerTester sdeclfreevars $ do
                        tinf' <- stRename [] FreeName tinf
                        tdecl' <- stRename sdeclfreevars RigidName tdecl
                        stSubsume tinf' tdecl'
            in [ subsumeTest "novars" "a -> a" "Integer -> Integer" []
               , subsumeTest "var-none" "None" "b" ["b"]
               , subsumeTest "var-free" "a" "b" ["b"]
               , subsumeTest "vars" "a -> a" "b -> b" ["b"]
               , subsumeTest "simple-3" "a -> a" "Maybe a -> Maybe a" ["a"]
               , testTree
                    "union"
                    [ subsumeTest "plain" "Integer | Text" "Integer | Text" []
                    , subsumeTest "list" "List (Integer | Text)" "List (Integer | Text)" []
                    ]
               , testTree
                    "split"
                    [ testTree
                        "plain"
                        [ subsumeTest "fst" "Integer" "Integer | Text" []
                        , subsumeTest "snd" "Text" "Integer | Text" []
                        ]
                    , testTree
                        "recursive"
                        [ subsumeTest "fst" "Maybe None" "(rec a, Maybe a) | (rec b, List b)" []
                        , subsumeTest "snd" "List None" "(rec a, Maybe a) | (rec b, List b)" []
                        ]
                    ]
               ]
        ]
