module Test.Recursive
    ( testRecursive
    ) where

import Data.Shim
import Language.Expression.Common
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
    => PinaforeType 'Positive tp
    -> IO (tp -> t)
getUnifyTo twp = do
    MkShimWit twn (MkPolarMap convn) <- return (fromShimWit :: PinaforeShimWit 'Negative t)
    (convu, _) <-
        resultToM $
        mapResultFailure show $
        runSourceScoped (initialPos "test") $
        runVarRenamerT $ do
            uuconvu <- unifyPosNegWitnesses @PinaforeTypeSystem twp twn
            solveUnifier @PinaforeTypeSystem $ uuGetShim uuconvu
    return $ shimToFunction $ convn . convu

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

testSubstituteFunctor :: TestTree
testSubstituteFunctor =
    testCase "substitute-functor" $ do
        MkShimWit (twp :: _ tp) (MkPolarMap convp) <- return (toShimWit :: PinaforeShimWit 'Positive [A])
        convTo <- getUnifyTo @[A] twp
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
            xxa = substituteVar var (\aa -> shimToFunction convp $ fmap MkVar aa) xx
            afmaplength :: Apply (USub "a" tp) String -> Apply (USub "a" tp) Int
            afmaplength = unApplyFunctor af f
            yya :: Apply (USub "a" tp) Int
            yya = afmaplength xxa
            yy :: [Int]
            yy = unsubsituteVar var (\aa -> fmap unVar $ convTo aa) yya
        assertEqual "" (fmap f xx) yy

type TestType1 = Maybe (Maybe (Maybe (Maybe BottomType)))

type TestType2 = Maybe (Maybe (Maybe (Maybe TopType)))

mapTestType :: TestType1 -> TestType2
mapTestType = fmap $ fmap $ fmap $ fmap $ never

testRecursiveConvert :: TestTree
testRecursiveConvert =
    testCase "recursive-convert" $ do
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
        convTo <- getUnifyTo @TestType2 twr
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

testRecursive :: TestTree
testRecursive = testGroup "recursive" [testSubstituteFunctor, testRecursiveConvert]
