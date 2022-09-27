{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Dolan.SubtypeEntry
    ( TrustOrVerify(..)
    , SubtypeConversionEntry(..)
    , subtypeConversionEntry
    , IsDolanSubtypeEntriesGroundType(..)
    , entries_subtypeGroundedTypes
    , SubtypeGroupTest(..)
    , testEqualitySubtypeGroupTest
    , SubtypeGroup(..)
    , singletonSubtypeGroup
    , checkSubtypeConsistency
    ) where

import Control.Applicative.Wrapped
import qualified Data.List as List
import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Variance
import Shapes

data TrustOrVerify
    = TrustMe
    | Verify
    deriving (Eq)

type SubtypeConversionEntry :: GroundTypeKind -> Type
data SubtypeConversionEntry ground =
    forall dva gta dvb gtb. MkSubtypeConversionEntry TrustOrVerify
                                                     (ground dva gta)
                                                     (ground dvb gtb)
                                                     (SubtypeConversion ground dva gta dvb gtb)

instance forall (ground :: GroundTypeKind). DebugIsDolanGroundType ground => Show (SubtypeConversionEntry ground) where
    show (MkSubtypeConversionEntry _ ta tb _) = show ta <> " <: " <> show tb

subtypeConversionEntry ::
       forall (ground :: GroundTypeKind) a b. IsDolanSubtypeGroundType ground
    => TrustOrVerify
    -> DolanGroundedShimWit ground 'Negative a
    -> DolanGroundedShimWit ground 'Positive b
    -> TSOpenExpression (DolanTypeSystem ground) (DolanShim ground a b)
    -> SubtypeConversionEntry ground
subtypeConversionEntry trustme (MkShimWit (MkDolanGroundedType gta argsa) conva) (MkShimWit (MkDolanGroundedType gtb argsb) convb) conv =
    MkSubtypeConversionEntry trustme gta gtb $
    subtypeConversion gta (MkShimWit argsa conva) gtb (MkShimWit argsb convb) conv

type SubtypeGroupTest :: GroundTypeKind -> Type
newtype SubtypeGroupTest ground = MkSubtypeGroupTest
    { runSubtypeGroupTest :: forall (dva :: DolanVariance) (gta :: DolanVarianceKind dva) (dvb :: DolanVariance) (gtb :: DolanVarianceKind dvb).
                                     ground dva gta -> ground dvb gtb -> Maybe (SubtypeConversion ground dva gta dvb gtb)
    }

instance forall (ground :: GroundTypeKind). Semigroup (SubtypeGroupTest ground) where
    MkSubtypeGroupTest fp <> MkSubtypeGroupTest fq = MkSubtypeGroupTest $ \ta tb -> fp ta tb <|> fq ta tb

instance forall (ground :: GroundTypeKind). Monoid (SubtypeGroupTest ground) where
    mempty = MkSubtypeGroupTest $ \_ _ -> Nothing

testEqualitySubtypeGroupTest ::
       forall (ground :: GroundTypeKind). IsDolanGroundType ground
    => SubtypeGroupTest ground
testEqualitySubtypeGroupTest =
    MkSubtypeGroupTest $ \ta tb -> do
        (Refl, HRefl) <- groundTypeTestEquality ta tb
        return IdentitySubtypeConversion

type SubtypeGroup :: GroundTypeKind -> Type
data SubtypeGroup ground = MkSubtypeGroup
    { sgWitness :: SomeGroundType ground
    , sgIsSubtype :: SubtypeGroupTest ground
    }

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => Eq (SubtypeGroup ground) where
    MkSubtypeGroup ta _ == MkSubtypeGroup tb _ = ta == tb

instance forall (ground :: GroundTypeKind). DebugIsDolanGroundType ground => Show (SubtypeGroup ground) where
    show (MkSubtypeGroup t _) = show t

singletonSubtypeGroup ::
       forall (ground :: GroundTypeKind) dv gt. IsDolanGroundType ground
    => ground dv gt
    -> SubtypeGroup ground
singletonSubtypeGroup gt = MkSubtypeGroup (MkSomeGroundType gt) testEqualitySubtypeGroupTest

matchBySubtypeGroup ::
       forall (ground :: GroundTypeKind) (dva :: DolanVariance) (gta :: DolanVarianceKind dva) (dvb :: DolanVariance) (gtb :: DolanVarianceKind dvb).
       IsDolanSubtypeEntriesGroundType ground
    => ground dva gta
    -> ground dvb gtb
    -> Maybe (SubtypeConversion ground dva gta dvb gtb)
matchBySubtypeGroup ta tb = let
    ga = getSubtypeGroup ta
    gb = getSubtypeGroup tb
    in if ga == gb
           then runSubtypeGroupTest (sgIsSubtype ga) ta tb
           else Nothing

type GreaterConversionWit :: GroundTypeKind -> GroundTypeKind
data GreaterConversionWit ground dva gta =
    forall dvb gtb. MkGreaterConversionWit (ground dvb gtb)
                                           (SubtypeConversion ground dva gta dvb gtb)

instance forall (ground :: GroundTypeKind) (dv :: DolanVariance) (gt :: DolanVarianceKind dv). DebugIsDolanGroundType ground =>
             Show (GreaterConversionWit ground dv gt) where
    show (MkGreaterConversionWit gt _) = show gt

mkGreaterConversionWit :: forall (ground :: GroundTypeKind) dv gt. ground dv gt -> GreaterConversionWit ground dv gt
mkGreaterConversionWit t = MkGreaterConversionWit t IdentitySubtypeConversion

greaterToSome :: forall (ground :: GroundTypeKind) dv gt. GreaterConversionWit ground dv gt -> SomeGroundType ground
greaterToSome (MkGreaterConversionWit t _) = MkSomeGroundType t

type LesserConversionWit :: GroundTypeKind -> GroundTypeKind
data LesserConversionWit ground dvb gtb =
    forall dva gta. MkLesserConversionWit (ground dva gta)
                                          (SubtypeConversion ground dva gta dvb gtb)

instance forall (ground :: GroundTypeKind) (dv :: DolanVariance) (gt :: DolanVarianceKind dv). DebugIsDolanGroundType ground =>
             Show (LesserConversionWit ground dv gt) where
    show (MkLesserConversionWit t _) = show t

mkLesserConversionWit :: forall (ground :: GroundTypeKind) dv gt. ground dv gt -> LesserConversionWit ground dv gt
mkLesserConversionWit t = MkLesserConversionWit t IdentitySubtypeConversion

lesserToSome :: forall (ground :: GroundTypeKind) dv gt. LesserConversionWit ground dv gt -> SomeGroundType ground
lesserToSome (MkLesserConversionWit t _) = MkSomeGroundType t

someCWGroup ::
       forall (ground :: GroundTypeKind). IsDolanSubtypeEntriesGroundType ground
    => SomeGroundType ground
    -> SubtypeGroup ground
someCWGroup (MkSomeGroundType t) = getSubtypeGroup t

greaterCWGroup ::
       forall (ground :: GroundTypeKind) (dv :: DolanVariance) (gt :: DolanVarianceKind dv).
       IsDolanSubtypeEntriesGroundType ground
    => GreaterConversionWit ground dv gt
    -> SubtypeGroup ground
greaterCWGroup (MkGreaterConversionWit t _) = getSubtypeGroup t

lesserCWGroup ::
       forall (ground :: GroundTypeKind) (dv :: DolanVariance) (gt :: DolanVarianceKind dv).
       IsDolanSubtypeEntriesGroundType ground
    => LesserConversionWit ground dv gt
    -> SubtypeGroup ground
lesserCWGroup (MkLesserConversionWit t _) = getSubtypeGroup t

instance forall (ground :: GroundTypeKind) dva gta. IsDolanSubtypeEntriesGroundType ground =>
             Eq (GreaterConversionWit ground dva gta) where
    p == q = greaterCWGroup p == greaterCWGroup q

instance forall (ground :: GroundTypeKind) dva gta. IsDolanSubtypeEntriesGroundType ground =>
             Eq (LesserConversionWit ground dva gta) where
    p == q = lesserCWGroup p == lesserCWGroup q

matchGreater ::
       forall (ground :: GroundTypeKind) dva gta. IsDolanSubtypeEntriesGroundType ground
    => Bool
    -> SubtypeConversionEntry ground
    -> GreaterConversionWit ground dva gta
    -> Maybe (GreaterConversionWit ground dva gta)
matchGreater True (MkSubtypeConversionEntry TrustMe _ _ _) _ = Nothing
matchGreater _ (MkSubtypeConversionEntry _ sta stb convE) (MkGreaterConversionWit ta conv) = do
    sconv <- matchBySubtypeGroup ta sta
    return $ MkGreaterConversionWit stb $ composeSubtypeConversion convE $ composeSubtypeConversion sconv conv

matchLesser ::
       forall (ground :: GroundTypeKind) dva gta. IsDolanSubtypeEntriesGroundType ground
    => Bool
    -> SubtypeConversionEntry ground
    -> LesserConversionWit ground dva gta
    -> Maybe (LesserConversionWit ground dva gta)
matchLesser True (MkSubtypeConversionEntry TrustMe _ _ _) _ = Nothing
matchLesser _ (MkSubtypeConversionEntry _ sta stb convE) (MkLesserConversionWit tb conv) = do
    sconv <- matchBySubtypeGroup stb tb
    return $ MkLesserConversionWit sta $ composeSubtypeConversion conv $ composeSubtypeConversion sconv convE

getImmediateGreaters ::
       forall (ground :: GroundTypeKind) dva gta. IsDolanSubtypeEntriesGroundType ground
    => Bool
    -> [SubtypeConversionEntry ground]
    -> GreaterConversionWit ground dva gta
    -> [GreaterConversionWit ground dva gta]
getImmediateGreaters rejectTrustMe entries t = mapMaybe (\entry -> matchGreater rejectTrustMe entry t) entries

getImmediateLessers ::
       forall (ground :: GroundTypeKind) dva gta. IsDolanSubtypeEntriesGroundType ground
    => Bool
    -> [SubtypeConversionEntry ground]
    -> LesserConversionWit ground dva gta
    -> [LesserConversionWit ground dva gta]
getImmediateLessers rejectTrustMe entries t = mapMaybe (\entry -> matchLesser rejectTrustMe entry t) entries

greaterContains ::
       forall (ground :: GroundTypeKind) dva gta dvb gtb. IsDolanSubtypeEntriesGroundType ground
    => [GreaterConversionWit ground dva gta]
    -> ground dvb gtb
    -> Maybe (SubtypeConversion ground dva gta dvb gtb)
greaterContains [] _ = Nothing
greaterContains (MkGreaterConversionWit ta conv:_) tb
    | Just convm <- matchBySubtypeGroup ta tb = Just $ composeSubtypeConversion convm conv
greaterContains (_:aa) tb = greaterContains aa tb

findGreater ::
       forall (ground :: GroundTypeKind) dva gta dvb gtb. IsDolanSubtypeEntriesGroundType ground
    => [SubtypeConversionEntry ground]
    -> [GreaterConversionWit ground dva gta]
    -> [GreaterConversionWit ground dva gta]
    -> ground dvb gtb
    -> Maybe (SubtypeConversion ground dva gta dvb gtb)
findGreater _entries _old [] _target = Nothing
findGreater _entries _old current target
    | Just conv <- greaterContains current target = Just conv
findGreater entries old current target = let
    allnew = nub $ mconcat $ fmap (getImmediateGreaters False entries) current
    new = allnew List.\\ old
    in findGreater entries (old <> current) new target

getAllGreaters ::
       forall (ground :: GroundTypeKind) dv gt. IsDolanSubtypeEntriesGroundType ground
    => [SubtypeConversionEntry ground]
    -> [GreaterConversionWit ground dv gt]
    -> [GreaterConversionWit ground dv gt]
getAllGreaters entries current = let
    allnew = nub $ mconcat $ fmap (getImmediateGreaters True entries) current
    in case allnew List.\\ current of
           [] -> current
           new -> getAllGreaters entries $ current <> new

getAllLessers ::
       forall (ground :: GroundTypeKind) dv gt. IsDolanSubtypeEntriesGroundType ground
    => [SubtypeConversionEntry ground]
    -> [LesserConversionWit ground dv gt]
    -> [LesserConversionWit ground dv gt]
getAllLessers entries current = let
    allnew = nub $ mconcat $ fmap (getImmediateLessers True entries) current
    in case allnew List.\\ current of
           [] -> current
           new -> getAllLessers entries $ current <> new

getSubtypeShim ::
       forall (ground :: GroundTypeKind) dva gta dvb gtb. IsDolanSubtypeEntriesGroundType ground
    => [SubtypeConversionEntry ground]
    -> ground dva gta
    -> ground dvb gtb
    -> Maybe (SubtypeConversion ground dva gta dvb gtb)
getSubtypeShim entries gta gtb = findGreater entries [] [mkGreaterConversionWit gta] gtb

type IsDolanSubtypeEntriesGroundType :: GroundTypeKind -> Constraint
class IsDolanSubtypeGroundType ground => IsDolanSubtypeEntriesGroundType ground where
    getSubtypeGroup :: forall (dv :: DolanVariance) (gt :: DolanVarianceKind dv). ground dv gt -> SubtypeGroup ground
    getSubtypeGroup = singletonSubtypeGroup
    subtypeConversionEntries :: DolanM ground [SubtypeConversionEntry ground]
    throwGroundTypeConvertError :: ground dva gta -> ground dvb gtb -> DolanM ground a

entries_subtypeGroundedTypes ::
       forall (ground :: GroundTypeKind) solver pola polb a b.
       ( IsDolanSubtypeEntriesGroundType ground
       , WrappedApplicative solver
       , WAInnerM solver ~ DolanTypeCheckM ground
       , Is PolarityType pola
       , Is PolarityType polb
       )
    => DolanSubtypeContext ground solver
    -> DolanGroundedType ground pola a
    -> DolanGroundedType ground polb b
    -> solver (DolanShim ground a b)
entries_subtypeGroundedTypes sc (MkDolanGroundedType (ta :: ground dva gta) argsa) (MkDolanGroundedType (tb :: ground dvb gtb) argsb) = let
    margswit :: DolanTypeCheckM ground (SubtypeArguments ground solver dvb gtb a)
    margswit = do
        entries <- lift subtypeConversionEntries
        case getSubtypeShim entries ta tb of
            Just sconv -> getSubtypeConversion sconv sc argsa
            Nothing -> lift $ throwGroundTypeConvertError ta tb
    in wbind margswit $ \(MkSubtypeArguments argsb' sargsconv) ->
           (\p q -> p . q) <$> subtypeDolanArguments sc tb argsb' argsb <*> sargsconv

checkSubtypeConsistency ::
       forall (ground :: GroundTypeKind). IsDolanSubtypeEntriesGroundType ground
    => [SubtypeConversionEntry ground]
    -> SomeGroundType ground
    -> SomeGroundType ground
    -> Maybe (SubtypeConversionEntry ground)
checkSubtypeConsistency entries (MkSomeGroundType ta) (MkSomeGroundType tb) = let
    bgreaters :: [SomeGroundType ground]
    bgreaters = fmap greaterToSome $ getAllGreaters entries [mkGreaterConversionWit tb]
    alessers :: [SomeGroundType ground]
    alessers = fmap lesserToSome $ getAllLessers entries [mkLesserConversionWit ta]
    tryLessers :: [SubtypeGroup ground] -> [SomeGroundType ground] -> Maybe (SubtypeConversionEntry ground)
    tryLessers _ [] = Nothing
    tryLessers missed (MkSomeGroundType (ea :: ground dva gta):eaa) = let
        allagreaters :: [GreaterConversionWit ground dva gta]
        allagreaters = getAllGreaters entries [mkGreaterConversionWit ea]
        agreaters :: [GreaterConversionWit ground dva gta]
        agreaters = filter (\ag -> not $ elem (greaterCWGroup ag) missed) allagreaters
        in case [ag | ag <- agreaters, bg <- bgreaters, greaterCWGroup ag == someCWGroup bg] of
               (MkGreaterConversionWit eb conv):_ -> Just $ MkSubtypeConversionEntry Verify ea eb conv
               [] -> tryLessers (missed <> fmap greaterCWGroup agreaters) eaa
    in tryLessers [] alessers
