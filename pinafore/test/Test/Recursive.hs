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

getConvertTo ::
       forall t tp. FromPinaforeType t
    => PinaforeType 'Positive tp
    -> IO (tp -> t)
getConvertTo twp = do
    MkShimWit (twn :: _ tn) (MkPolarMap convn) <- return (fromShimWit :: PinaforeShimWit 'Negative t)
    (convu, _) <-
        resultToM $
        mapResultFailure show $
        runSourceScoped (initialPos "test") $
        runVarRenamerT $ do
            uuconvu <- unifyPosNegWitnesses @PinaforeTypeSystem twp twn
            solveUnifier @PinaforeTypeSystem $ uuGetShim uuconvu
    return $ shimToFunction $ convn . convu

testSubstituteFunctor :: TestTree
testSubstituteFunctor =
    testCase "substitute-functor" $ do
        MkShimWit (twp :: _ tp) (MkPolarMap convp) <- return (toShimWit :: PinaforeShimWit 'Positive [A])
        convTo <- getConvertTo @[A] twp
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

testBisubstituteWitness :: TestTree
testBisubstituteWitness =
    testCase "bisubstitute-witness" $ do
        MkShimWit (twp :: _ tp) (MkPolarMap convp) <-
            return $ (toShimWit :: PinaforeSingularShimWit 'Positive (Maybe A))
        let
            var :: SymbolType "a"
            var = representative
        MkShimWit (twp' :: _ tp') (MkPolarMap convp') <- return $ bisubstituteWitnessForTest var twp
        convTo <- getConvertTo @(Maybe TopType) twp'
        let
            a1 :: Maybe A
            a1 = Nothing
            a2 :: Maybe TopType
            a2 = convTo $ shimToFunction (convp' . convp) a1
        assertEqual "" Nothing a2

testRecursive :: TestTree
testRecursive = testGroup "recursive" [testSubstituteFunctor, testBisubstituteWitness]
