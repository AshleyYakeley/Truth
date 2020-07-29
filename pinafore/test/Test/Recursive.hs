module Test.Recursive
    ( testRecursive
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan
import Language.Expression.Dolan.Test
import Pinafore
import Pinafore.Test
import Shapes
import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec.Pos (initialPos)

substituteVar :: forall name f t x. SymbolType name -> (f (UVarT name) -> t) -> f x -> Apply (USub name t) x
substituteVar var conv fx = assignUVar @Type @x var $ withRefl (usubIdentity @t var) $ conv fx

unsubsituteVar :: forall name f t x. SymbolType name -> (t -> f (UVarT name)) -> Apply (USub name t) x -> f x
unsubsituteVar var conv x = assignUVar @Type @x var $ withRefl (usubIdentity @t var) $ conv x

getUnifyTo ::
       forall t tp. FromPinaforeType t
    => PinaforeShimWit 'Positive tp
    -> IO (tp -> t)
getUnifyTo (MkShimWit twp (MkPolarMap convp)) = do
    MkShimWit twn (MkPolarMap convn) <- return (fromShimWit :: PinaforeShimWit 'Negative t)
    (convu, _) <-
        resultToM $
        mapResultFailure show $
        runSourceScoped (initialPos "test") $
        runVarRenamerT $ do
            uuconvu <- unifyPosNegWitnesses @PinaforeTypeSystem twp twn
            solveUnifier @PinaforeTypeSystem $ uuGetShim uuconvu
    return $ shimToFunction $ convn . convu . convp

getSubsumeFrom ::
       forall t tdecl. ToPinaforeType t
    => PinaforeType 'Positive tdecl
    -> IO (t -> tdecl)
getSubsumeFrom twdecl = do
    MkShimWit twinf (MkPolarMap convp) <- return (toShimWit :: PinaforeShimWit 'Positive t)
    (convu, _) <-
        resultToM $
        mapResultFailure show $
        runSourceScoped (initialPos "test") $
        runVarRenamerT $ do
            econv <- subsumePosWitnesses @PinaforeTypeSystem twinf twdecl
            solveSubsumer @PinaforeTypeSystem econv
    return $ shimToFunction $ convu . convp

testSubstituteFunctorList :: TestTree
testSubstituteFunctorList =
    testCase "list" $ do
        MkShimWit (twp :: _ tp) (MkPolarMap convp) <- return (toShimWit :: PinaforeShimWit 'Positive [A])
        convTo <- getUnifyTo @[A] $ mkShimWit twp
        let
            var :: SymbolType "a"
            var = representative
            af :: ApplyFunctor (USub "a" tp)
            af = substituteApplyFunctor var twp
            f :: String -> Int
            f = length
            xx :: [String]
            xx = ["hello", "goodbye"]
            xxa :: Apply (USub "a" tp) String
            xxa = substituteVar var (shimToFunction convp . fmap MkVar) xx
            afmaplength :: Apply (USub "a" tp) String -> Apply (USub "a" tp) Int
            afmaplength = unApplyFunctor af f
            yya :: Apply (USub "a" tp) Int
            yya = afmaplength xxa
            yy :: [Int]
            yy = unsubsituteVar var (fmap unVar . convTo) yya
        assertEqual "" (fmap f xx) yy

testSubstituteFunctorMaybe :: TestTree
testSubstituteFunctorMaybe =
    testCase "maybe" $ do
        MkShimWit (twp :: _ tp) (MkPolarMap convp) <- return (toShimWit :: PinaforeShimWit 'Positive (Maybe A))
        convTo <- getUnifyTo @(Maybe A) $ mkShimWit twp
        let
            var :: SymbolType "a"
            var = representative
            af :: ApplyFunctor (USub "a" tp)
            af = substituteApplyFunctor var twp
            f :: String -> Int
            f = length
            xx :: Maybe String
            xx = Just "hello"
            xxa :: Apply (USub "a" tp) String
            xxa = substituteVar var (shimToFunction convp . fmap MkVar) xx
            afmaplength :: Apply (USub "a" tp) String -> Apply (USub "a" tp) Int
            afmaplength = unApplyFunctor af f
            yya :: Apply (USub "a" tp) Int
            yya = afmaplength xxa
            yy :: Maybe Int
            yy = unsubsituteVar var (fmap unVar . convTo) yya
        assertEqual "" (fmap f xx) yy

testSubstituteFunctor :: TestTree
testSubstituteFunctor = testGroup "substitute-functor" [testSubstituteFunctorList, testSubstituteFunctorMaybe]

type TestType1 = Maybe (Maybe (Maybe (Maybe BottomType)))

type TestType2 = Maybe (Maybe (Maybe (Maybe TopType)))

mapTestType :: TestType1 -> TestType2
mapTestType = fmap $ fmap $ fmap $ fmap $ never

testConvert :: TestTree
testConvert =
    testCase "convert" $ do
        let
            var :: SymbolType "a"
            var = representative
        -- create plain type
        MkShimWit (twp :: _ tp) _ <- return $ (toShimWit :: PinaforeSingularShimWit 'Positive (Maybe A))
        assertEqual "plain type" "Maybe a" $ exprShow twp
        -- create recursive type
        MkShimWit (twr :: _ tr) _ <- return $ bisubstituteWitnessForTest var twp
        assertEqual "recursive type" "rec a. Maybe a" $ exprShow twr
        -- find unify to TestType
        convTo <- getUnifyTo @TestType2 $ mkShimWit twr
        -- find subsume from TestType
        confFrom <- getSubsumeFrom @TestType1 twr
        let
        -- test values
            a1 :: TestType1
            a1 = Just $ Just $ Just Nothing
            a2 :: tr
            a2 = confFrom a1
        assertEqual "a2" "Just Just Just Nothing" $ typedShowValue twr a2
        let
            a3 :: TestType2
            a3 = convTo a2
        assertEqual "" (mapTestType a1) a3

testBisubstituteWitness :: TestTree
testBisubstituteWitness =
    testCase "bisubstitute-witness" $ do
        let
            var :: SymbolType "a"
            var = representative
        -- create plain type
        MkShimWit (twp :: _ tp) (MkPolarMap convp) <-
            return $ (toShimWit :: PinaforeSingularShimWit 'Positive (Maybe A))
        assertEqual "plain type" "Maybe a" $ exprShow twp
        -- create recursive type
        MkShimWit (twr :: _ tr) (MkPolarMap convr) <- return $ bisubstituteWitnessForTest var twp
        assertEqual "recursive type" "rec a. Maybe a" $ exprShow twr
        -- find unify to TestType
        convTo <- getUnifyTo @TestType2 $ mkShimWit twr
        let
        -- test values
            a1 :: Maybe A
            a1 = error "x"
            a2 :: tr
            a2 = shimToFunction (convr . convp) a1
        assertEqual "a2" "Just Just Just Nothing" $ typedShowValue twr a2
        let
            a3 :: TestType2
            a3 = convTo a2
        assertEqual "" (Just Nothing) a3

testBisubstituteInternal :: TestTree
testBisubstituteInternal =
    testCase "bisubstitute-internal" $ do
        MkShimWit (twp :: _ tp) (MkPolarMap convp) <-
            return $ (toShimWit :: PinaforeSingularShimWit 'Positive (Maybe (Var "a")))
        convTo <- getUnifyTo @(Maybe (Var "a")) $ singleDolanShimWit $ mkShimWit twp
        let
            var :: SymbolType "a"
            var = representative
            af :: ApplyFunctor (USub "a" tp)
            af = substituteApplyFunctor var twp
            f :: String -> Int
            f = length
            xx :: Maybe String
            xx = Just "hello"
            xxa :: Apply (USub "a" tp) String
            xxa = substituteVar var (shimToFunction convp . fmap MkVar) xx
            afmaplength :: Apply (USub "a" tp) String -> Apply (USub "a" tp) Int
            afmaplength = unApplyFunctor af f
            yya :: Apply (USub "a" tp) Int
            yya = afmaplength xxa
            yy :: Maybe Int
            yy = unsubsituteVar var (fmap unVar . convTo) yya
        assertEqual "" (fmap f xx) yy

testRecursive :: TestTree
testRecursive =
    testGroup "recursive" [testSubstituteFunctor, testConvert, testBisubstituteWitness, testBisubstituteInternal]
