module Test.Solver
    ( testSolver
    ) where

import Data.Shim
import Language.Expression.Common
import Pinafore
import Pinafore.Language.API
import Pinafore.Test
import Shapes
import Test.RunScript

type SomeType polarity = Some (QShimWit polarity)

toShimWit :: Is PolarityType polarity => Some (QType polarity) -> SomeType polarity
toShimWit (MkSome t) = MkSome $ mkShimWit t

fromShimWit :: SomeType polarity -> Some (QType polarity)
fromShimWit (MkSome (MkShimWit t _)) = MkSome t

type SolverTester us = WriterT (TSOpenSolverExpression QTypeSystem us ()) (TSOuter QTypeSystem)

type UnifierTester = SolverTester (Unifier QTypeSystem)

utParseType ::
       forall polarity. Is PolarityType polarity
    => Text
    -> UnifierTester (SomeType polarity)
utParseType text =
    lift $
    lift $ do
        t <- parseType @polarity text
        return $ toShimWit t

utParseTypeBoth :: Text -> UnifierTester (SomeType 'Negative, SomeType 'Positive)
utParseTypeBoth text = do
    tn <- utParseType text
    tp <- utParseType text
    return (tn, tp)

utRename :: TSMappable QTypeSystem a => a -> UnifierTester a
utRename a = lift $ renameMappableSimple @QTypeSystem a

utUnify :: SomeType 'Positive -> SomeType 'Negative -> UnifierTester ()
utUnify (MkSome (MkShimWit posw _)) (MkSome (MkShimWit negw _)) = do
    w <-
        lift $ do
            MkComposeShim uushim <- unifyPosNegWitnesses @QTypeSystem posw negw
            return $ uushim *> pure ()
    tell w

runUnifierTester :: TSMappable QTypeSystem b => UnifierTester a -> (a -> b) -> QInterpreter b
runUnifierTester sta ab =
    runRenamer @QTypeSystem [] [] $ do
        (a, se) <- runWriterT sta
        unifierSolve @QTypeSystem se $ \_ -> return $ ab a

unifierTest :: String -> Text -> UnifierTester (SomeType 'Positive) -> TestTree
unifierTest name expectedtext uta =
    testTree name $
    runTester defaultTester $
    testerLiftInterpreter $ do
        expectedtype <- parseType @'Positive expectedtext
        found <- runUnifierTester uta id
        liftIO $ assertEqual "" expectedtype (fromShimWit found)

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
              ftype <- utParseType ftext
              gtype <- utParseType $ xtext <> " -> r"
              rtype <- utParseType "r"
              ftype' <- utRename ftype
              (gtype', rtype') <- utRename (gtype, rtype)
              utUnify ftype' gtype'
              return rtype'
        , scriptTest
              "script"
              ("let f: " <> ftext <> " = error \"f\"; x: " <> xtext <> " = error \"x\" in f x")
              ("{} => " <> extext)
        ]

testSolver :: TestTree
testSolver =
    testTree
        "solver"
        [ applyTest "function-0" "Number -> Text" "Integer" "Text."
        , applyTest "id-0" "t -> t" "Unit" "Unit."
        , applyTest "id-1" "t -> t" "(a -> a)" "a -> a"
        , applyTest "fix-0" "(t -> t) -> t" "(Unit -> Unit)" "Unit."
        , applyTest "fix-1" "(t -> t) -> t" "(a -> a)" "None"
        , applyTest "fix-2" "(t -> t) -> t" "((a -> a) -> (a -> a))" "a -> a"
        , applyTest "issue-206-1" "(t -> t) -> t" "((Maybe a -> Maybe a) -> (a -> a))" "Any -> (rec a, Maybe. a)"
        , applyTest
              "issue-206-2"
              "(t -> t) -> t"
              "((a -> a) -> (Maybe a -> Maybe a))"
              "(rec a, Maybe. a) -> Maybe. None"
        , unifierTest "rec-0" "rec a, Maybe. a" $ do
              ta <- utParseTypeBoth "a"
              tma <- utParseTypeBoth "Maybe a"
              ((tan, tap), (_tman, tmap)) <- utRename (ta, tma)
              utUnify tmap tan
              return tap
        , unifierTest "rec-1" "None" $ do
              ta <- utParseTypeBoth "a"
              tma <- utParseTypeBoth "Maybe a"
              ((_tan, tap), (tman, _tmap)) <- utRename (ta, tma)
              utUnify tap tman
              return tap
        , unifierTest "rec-2" "rec a, Maybe. a" $ do
              ta <- utParseTypeBoth "a"
              tma <- utParseTypeBoth "Maybe a"
              ((tan, tap), (tman, tmap)) <- utRename (ta, tma)
              utUnify tmap tan
              utUnify tap tman
              return tap
        ]
