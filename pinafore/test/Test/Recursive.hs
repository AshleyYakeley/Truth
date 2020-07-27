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

testSubstituteFunctor :: TestTree
testSubstituteFunctor =
    testCase "substitute-functor" $
    case toShimWit :: PinaforeShimWit 'Positive [A] of
        MkShimWit (twp :: _ t) (MkPolarMap convp) ->
            case fromShimWit :: PinaforeShimWit 'Negative [A] of
                MkShimWit (twn :: _ tn) (MkPolarMap convn) -> do
                    (convu, _) <-
                        resultToM $
                        mapResultFailure show $
                        runSourceScoped (initialPos "test") $
                        runVarRenamerT $ do
                            uuconvu <- unifyPosNegWitnesses @PinaforeTypeSystem twp twn
                            solveUnifier @PinaforeTypeSystem $ uuGetShim uuconvu
                    let
                        var :: SymbolType "a"
                        var = representative
                        af :: ApplyFunctor (USub "a" t)
                        af = substituteApplyFunctor var twp
                        f :: String -> Int
                        f = length
                        xx :: [String]
                        xx = ["hello", "goodbye"]
                        xxa :: Apply (USub "a" t) String
                        xxa = substituteVar var (\aa -> shimToFunction convp $ fmap MkVar aa) xx
                        afmaplength :: Apply (USub "a" t) String -> Apply (USub "a" t) Int
                        afmaplength = unApplyFunctor af f
                        yya :: Apply (USub "a" t) Int
                        yya = afmaplength xxa
                        yy :: [Int]
                        yy = unsubsituteVar var (\aa -> fmap unVar $ shimToFunction (convn . convu) aa) yya
                    assertEqual "" (fmap f xx) yy

testRecursive :: TestTree
testRecursive = testGroup "recursive" [testSubstituteFunctor]
