{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Dolan.SubtypeEntry
    ( TrustOrVerify(..)
    , SubtypeConversionEntry(..)
    , subtypeConversionEntry
    , IsDolanSubtypeEntriesGroundType(..)
    , entries_subtypeGroundedTypes
    , testEqualitySubtypeGroupTest
    , SubtypeGroup(..)
    , singletonSubtypeGroup
    , checkSubtypeConsistency
    ) where

import Control.Applicative.Wrapped
import Data.Shim
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Unifier
import Language.Expression.Dolan.Variance
import Shapes

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = return ([], [])
partitionM t (a:aa) = do
    r <- t a
    let
        pair =
            if r
                then ([a], [])
                else ([], [a])
    pairs <- partitionM t aa
    return $ pair <> pairs

bestM :: Monad m => (a -> a -> m Bool) -> [a] -> m [a]
bestM _ [] = return []
bestM _ [a] = return [a]
bestM asGoodAs (a:aa) = do
    bb <- bestM asGoodAs aa
    rr <- partitionM (asGoodAs a) bb
    case rr of
        ([], bb') -> do
            has <- shortOr (\b -> asGoodAs b a) bb'
            return $
                if has
                    then bb'
                    else a : bb'
        (_:_, bb') -> return $ a : bb'

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
    -> Maybe (DolanSubtypeHint ground)
    -> DolanGroundedShimWit ground 'Negative a
    -> DolanGroundedShimWit ground 'Positive b
    -> DolanOpenExpression ground (DolanShim ground a b)
    -> SubtypeConversionEntry ground
subtypeConversionEntry trustme hint (MkShimWit (MkDolanGroundedType gta argsa) conva) (MkShimWit (MkDolanGroundedType gtb argsb) convb) conv =
    MkSubtypeConversionEntry trustme gta gtb $
    subtypeConversion hint gta (MkShimWit argsa conva) gtb (MkShimWit argsb convb) conv

testEqualitySubtypeGroupTest ::
       forall (ground :: GroundTypeKind) dv gt. IsDolanGroundType ground
    => ground dv gt
    -> ground dv gt
    -> Bool
testEqualitySubtypeGroupTest ta tb = isJust $ groundTypeTestEquality ta tb

type SubtypeGroup :: GroundTypeKind -> GroundTypeKind
data SubtypeGroup ground dv gt = MkSubtypeGroup
    { sgWitness :: ground dv gt
    , sgIsSubtype :: ground dv gt -> ground dv gt -> Bool
    }

subtypeGroupTestEquality ::
       forall (ground :: GroundTypeKind) dva gta dvb gtb. IsDolanSubtypeEntriesGroundType ground
    => SubtypeGroup ground dva gta
    -> SubtypeGroup ground dvb gtb
    -> Maybe (dva :~: dvb, gta :~~: gtb)
subtypeGroupTestEquality ta tb = groundTypeTestEquality (sgWitness ta) (sgWitness tb)

matchSubtypeGroup ::
       forall (ground :: GroundTypeKind) dva gta dvb gtb. IsDolanSubtypeEntriesGroundType ground
    => ground dva gta
    -> ground dvb gtb
    -> Maybe (dva :~: dvb, gta :~~: gtb)
matchSubtypeGroup ta tb = do
    let
        ga = getSubtypeGroup ta
        gb = getSubtypeGroup tb
    (Refl, HRefl) <- subtypeGroupTestEquality ga gb
    if sgIsSubtype ga ta tb
        then return (Refl, HRefl)
        else Nothing

type SomeSubtypeGroup :: GroundTypeKind -> Type
data SomeSubtypeGroup ground where
    MkSomeSubtypeGroup :: forall (ground :: GroundTypeKind) dv gt. SubtypeGroup ground dv gt -> SomeSubtypeGroup ground

instance forall (ground :: GroundTypeKind). IsDolanSubtypeEntriesGroundType ground => Eq (SomeSubtypeGroup ground) where
    MkSomeSubtypeGroup a == MkSomeSubtypeGroup b = isJust $ subtypeGroupTestEquality a b

instance forall (ground :: GroundTypeKind) dv gt. DebugIsDolanGroundType ground => Show (SubtypeGroup ground dv gt) where
    show (MkSubtypeGroup t _) = show t

singletonSubtypeGroup ::
       forall (ground :: GroundTypeKind) dv gt. IsDolanGroundType ground
    => ground dv gt
    -> SubtypeGroup ground dv gt
singletonSubtypeGroup gt = MkSubtypeGroup gt testEqualitySubtypeGroupTest

type GreaterConversionWit :: GroundTypeKind -> GroundTypeKind
data GreaterConversionWit ground dva gta =
    forall dvb gtb. MkGreaterConversionWit (ground dvb gtb)
                                           (SubtypeConversion ground dva gta dvb gtb)

instance forall (ground :: GroundTypeKind) (dv :: DolanVariance) (gt :: DolanVarianceKind dv). DebugIsDolanGroundType ground =>
             Show (GreaterConversionWit ground dv gt) where
    show (MkGreaterConversionWit gt _) = show gt

mkGreaterConversionWit :: forall (ground :: GroundTypeKind) dv gt. ground dv gt -> GreaterConversionWit ground dv gt
mkGreaterConversionWit t = MkGreaterConversionWit t identitySubtypeConversion

type LesserConversionWit :: GroundTypeKind -> GroundTypeKind
data LesserConversionWit ground dvb gtb =
    forall dva gta. MkLesserConversionWit (ground dva gta)
                                          (SubtypeConversion ground dva gta dvb gtb)

instance forall (ground :: GroundTypeKind) (dv :: DolanVariance) (gt :: DolanVarianceKind dv). DebugIsDolanGroundType ground =>
             Show (LesserConversionWit ground dv gt) where
    show (MkLesserConversionWit t _) = show t

mkLesserConversionWit :: forall (ground :: GroundTypeKind) dv gt. ground dv gt -> LesserConversionWit ground dv gt
mkLesserConversionWit t = MkLesserConversionWit t identitySubtypeConversion

greaterCWGroup ::
       forall (ground :: GroundTypeKind) (dv :: DolanVariance) (gt :: DolanVarianceKind dv).
       IsDolanSubtypeEntriesGroundType ground
    => GreaterConversionWit ground dv gt
    -> SomeSubtypeGroup ground
greaterCWGroup (MkGreaterConversionWit t _) = MkSomeSubtypeGroup $ getSubtypeGroup t

lesserCWGroup ::
       forall (ground :: GroundTypeKind) (dv :: DolanVariance) (gt :: DolanVarianceKind dv).
       IsDolanSubtypeEntriesGroundType ground
    => LesserConversionWit ground dv gt
    -> SomeSubtypeGroup ground
lesserCWGroup (MkLesserConversionWit t _) = MkSomeSubtypeGroup $ getSubtypeGroup t

matchGreater ::
       forall (ground :: GroundTypeKind) dva gta. IsDolanSubtypeEntriesGroundType ground
    => Bool
    -> SubtypeConversionEntry ground
    -> GreaterConversionWit ground dva gta
    -> Maybe (GreaterConversionWit ground dva gta)
matchGreater True (MkSubtypeConversionEntry TrustMe _ _ _) _ = Nothing
matchGreater _ (MkSubtypeConversionEntry _ sta stb convE) (MkGreaterConversionWit ta conv) = do
    (Refl, HRefl) <- matchSubtypeGroup ta sta
    return $ MkGreaterConversionWit stb $ composeSubtypeConversion convE conv

matchLesser ::
       forall (ground :: GroundTypeKind) dva gta. IsDolanSubtypeEntriesGroundType ground
    => Bool
    -> SubtypeConversionEntry ground
    -> LesserConversionWit ground dva gta
    -> Maybe (LesserConversionWit ground dva gta)
matchLesser True (MkSubtypeConversionEntry TrustMe _ _ _) _ = Nothing
matchLesser _ (MkSubtypeConversionEntry _ sta stb convE) (MkLesserConversionWit tb conv) = do
    (Refl, HRefl) <- matchSubtypeGroup stb tb
    return $ MkLesserConversionWit sta $ composeSubtypeConversion conv convE

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
    => ground dvb gtb
    -> GreaterConversionWit ground dva gta
    -> Maybe (SubtypeConversion ground dva gta dvb gtb)
greaterContains tb (MkGreaterConversionWit ta conv) = do
    (Refl, HRefl) <- matchSubtypeGroup ta tb
    return conv

findGreater ::
       forall (ground :: GroundTypeKind) dva gta dvb gtb. IsDolanSubtypeEntriesGroundType ground
    => [SubtypeConversionEntry ground]
    -> [SomeSubtypeGroup ground]
    -> [GreaterConversionWit ground dva gta]
    -> ground dvb gtb
    -> [SubtypeConversion ground dva gta dvb gtb]
findGreater _entries _old [] _target = []
findGreater entries old current target = let
    convs = mapMaybe (greaterContains target) current
    allnew = mconcat $ fmap (getImmediateGreaters False entries) current
    new = filter (\w -> not $ elem (greaterCWGroup w) old) allnew
    in convs <> findGreater entries (fmap greaterCWGroup current <> old) new target

getAllGreaters ::
       forall (ground :: GroundTypeKind) dv gt. IsDolanSubtypeEntriesGroundType ground
    => [SubtypeConversionEntry ground]
    -> [GreaterConversionWit ground dv gt]
    -> [GreaterConversionWit ground dv gt]
getAllGreaters entries current = let
    allnew = mconcat $ fmap (getImmediateGreaters True entries) current
    old = fmap greaterCWGroup current
    in case filter (\w -> not $ elem (greaterCWGroup w) old) allnew of
           [] -> current
           new -> getAllGreaters entries $ current <> new

getAllLessers ::
       forall (ground :: GroundTypeKind) dv gt. IsDolanSubtypeEntriesGroundType ground
    => [SubtypeConversionEntry ground]
    -> [LesserConversionWit ground dv gt]
    -> [LesserConversionWit ground dv gt]
getAllLessers entries current = let
    allnew = mconcat $ fmap (getImmediateLessers True entries) current
    old = fmap lesserCWGroup current
    in case filter (\w -> not $ elem (lesserCWGroup w) old) allnew of
           [] -> current
           new -> getAllLessers entries $ current <> new

getSubtypeShim ::
       forall (ground :: GroundTypeKind) dva gta dvb gtb. IsDolanSubtypeEntriesGroundType ground
    => [SubtypeConversionEntry ground]
    -> ground dva gta
    -> ground dvb gtb
    -> [SubtypeConversion ground dva gta dvb gtb]
getSubtypeShim entries gta gtb = findGreater entries [] [mkGreaterConversionWit gta] gtb

type IsDolanSubtypeEntriesGroundType :: GroundTypeKind -> Constraint
class IsDolanSubtypeGroundType ground => IsDolanSubtypeEntriesGroundType ground where
    getSubtypeGroup ::
           forall (dv :: DolanVariance) (gt :: DolanVarianceKind dv). ground dv gt -> SubtypeGroup ground dv gt
    getSubtypeGroup = singletonSubtypeGroup
    subtypeConversionEntries :: DolanM ground [SubtypeConversionEntry ground]
    throwNoGroundTypeConversionError :: ground dva gta -> ground dvb gtb -> DolanM ground a
    throwIncoherentGroundTypeConversionError :: ground dva gta -> ground dvb gtb -> DolanM ground a

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
entries_subtypeGroundedTypes sc (MkDolanGroundedType (ta :: ground dva gta) argsa) (MkDolanGroundedType (tb :: ground dvb gtb) argsb) =
    wexec $ do
        entries <- lift subtypeConversionEntries
        let conversions = getSubtypeShim entries ta tb
        bestConversions <- lift $ bestM unifierSubtypeConversionAsGeneralAs conversions
        case bestConversions of
            [sconv] -> runSubtypeConversion sc sconv (groundTypeVarianceMap ta) argsa argsb
            [] -> lift $ throwNoGroundTypeConversionError ta tb
            _ -> lift $ throwIncoherentGroundTypeConversionError ta tb

type SubtypeConversionPair :: GroundTypeKind -> Type
data SubtypeConversionPair ground =
    forall dva gta dvb gtb. MkSubtypeConversionPair (ground dva gta)
                                                    (ground dvb gtb)
                                                    (SubtypeConversion ground dva gta dvb gtb)
                                                    (SubtypeConversion ground dva gta dvb gtb)

findConsistencyPair ::
       forall (ground :: GroundTypeKind). IsDolanSubtypeEntriesGroundType ground
    => [SubtypeConversionEntry ground]
    -> SubtypeConversionEntry ground
    -> Maybe (SubtypeConversionPair ground)
findConsistencyPair entries (MkSubtypeConversionEntry _ (ta :: ground dva gta) (tb :: ground dvb gtb) tconv) = let
    bgreaters :: [GreaterConversionWit ground dvb gtb]
    bgreaters = getAllGreaters entries [mkGreaterConversionWit tb]
    alessers :: [LesserConversionWit ground dva gta]
    alessers = getAllLessers entries [mkLesserConversionWit ta]
    tryLessers ::
           [SomeSubtypeGroup ground] -> [LesserConversionWit ground dva gta] -> Maybe (SubtypeConversionPair ground)
    tryLessers _ [] = Nothing
    tryLessers missed (MkLesserConversionWit (ea :: ground dva' gta') aconv:eaa) = let
        allagreaters :: [GreaterConversionWit ground dva' gta']
        allagreaters = getAllGreaters entries [mkGreaterConversionWit ea]
        agreaters :: [GreaterConversionWit ground dva' gta']
        agreaters = filter (\ag -> not $ elem (greaterCWGroup ag) missed) allagreaters
        matches = do
            MkGreaterConversionWit eb conv <- agreaters
            MkGreaterConversionWit maxb bconv <- bgreaters
            (Refl, HRefl) <- mpure $ matchSubtypeGroup eb maxb
            return $
                MkSubtypeConversionPair ea eb conv $
                composeSubtypeConversion bconv $ composeSubtypeConversion tconv aconv
        in case matches of
               m:_ -> Just m
               [] -> tryLessers (missed <> fmap greaterCWGroup agreaters) eaa
    in tryLessers [] alessers

checkSubtypeConsistency ::
       forall (ground :: GroundTypeKind). IsDolanSubtypeEntriesGroundType ground
    => [SubtypeConversionEntry ground]
    -> SubtypeConversionEntry ground
    -> Maybe (SomeGroundType ground, SomeGroundType ground)
checkSubtypeConsistency _ (MkSubtypeConversionEntry TrustMe _ _ _) = Nothing
checkSubtypeConsistency entries sce@(MkSubtypeConversionEntry Verify _ _ _) = do
    MkSubtypeConversionPair ta tb conv1 conv2 <- findConsistencyPair entries sce
    if conv1 == conv2
        then Nothing
        else return (MkSomeGroundType ta, MkSomeGroundType tb)
