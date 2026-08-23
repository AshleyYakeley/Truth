{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Dolan.SubtypeEntry
    ( TrustOrVerify (..)
    , DolanSubtypeHint
    , SubtypeConversion
    , identitySubtypeConversion
    , coerceSubtypeConversion
    , singleSubtypeConversion
    , matchIdentitySubtypeConversion
    , SubtypeConversionEntry (..)
    , subtypeConversionEntry
    , neutralSubtypeConversionEntry
    , IsDolanSubtypeEntriesGroundType (..)
    , runDolanTypeMEntries
    , testEqualitySubtypeGroupTest
    , SubtypeGroup (..)
    , singletonSubtypeGroup
    , checkSubtypeConsistency
    )
where

import Data.Shim
import Shapes

import Language.Expression.Dolan.Nonpolar
import Language.Expression.Dolan.Solver.Crumble.Type
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.SubtypeChain
import Language.Expression.Dolan.SubtypeEntry.Conversion
import Language.Expression.Dolan.SubtypeEntry.Group
import Language.Expression.Dolan.SubtypeEntry.Knowledge
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeResult
import Language.Expression.Dolan.TypeSystem
import Language.Expression.TypeSystem

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = return ([], [])
partitionM t (a : aa) = do
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
bestM asGoodAs (a : aa) = do
    bb <- bestM asGoodAs aa
    rr <- partitionM (asGoodAs a) bb
    case rr of
        ([], bb') -> do
            has <- shortOr $ fmap (\b -> asGoodAs b a) bb'
            return
                $ if has
                    then bb'
                    else a : bb'
        (_ : _, bb') -> return $ a : bb'

data TrustOrVerify
    = TrustMe
    | Verify
    deriving stock (Eq, Show)

type SubtypeConversionEntry :: GroundTypeKind -> Type
data SubtypeConversionEntry ground
    = forall dva gta dvb gtb. MkSubtypeConversionEntry
        TrustOrVerify
        (ground dva gta)
        (ground dvb gtb)
        (SubtypeConversion ground dva gta dvb gtb)

instance forall (ground :: GroundTypeKind). (IsDolanGroundType ground, ShowGroundType ground, Show (DolanSubtypeHint ground)) => Show (SubtypeConversionEntry ground) where
    show (MkSubtypeConversionEntry trustme ta tb sc) = show ta <> " <: " <> show tb <> " [" <> show trustme <> "] = " <> show sc

subtypeConversionEntry_ ::
    forall (ground :: GroundTypeKind) a b.
    IsDolanSubtypeGroundType ground =>
    TrustOrVerify ->
    SubtypeKnowledge ground ->
    DolanGroundedShimWit ground 'Negative a ->
    DolanGroundedShimWit ground 'Positive b ->
    DolanOpenExpression ground (DolanShim ground a b) ->
    SubtypeConversionEntry ground
subtypeConversionEntry_ trustme sk (MkShimWit (MkDolanGroundedType gta argsa) conva) (MkShimWit (MkDolanGroundedType gtb argsb) convb) conv =
    MkSubtypeConversionEntry trustme gta gtb
        $ subtypeConversion sk gta (MkShimWit argsa conva) (MkShimWit argsb convb) conv

subtypeConversionEntry ::
    forall (ground :: GroundTypeKind) a b.
    IsDolanSubtypeGroundType ground =>
    TrustOrVerify ->
    Maybe (DolanSubtypeHint ground) ->
    DolanGroundedShimWit ground 'Negative a ->
    DolanGroundedShimWit ground 'Positive b ->
    DolanOpenExpression ground (DolanShim ground a b) ->
    SubtypeConversionEntry ground
subtypeConversionEntry trustme hint = subtypeConversionEntry_ trustme $ maybe UnknownSK HintSK hint

neutralSubtypeConversionEntry ::
    forall (ground :: GroundTypeKind) (a :: Type) (b :: Type).
    (IsDolanSubtypeGroundType ground, Coercible a b) =>
    NonpolarGroundedType ground a ->
    NonpolarGroundedType ground b ->
    SubtypeConversionEntry ground
neutralSubtypeConversionEntry ta tb =
    subtypeConversionEntry_ Verify NeutralSK (groundedNonpolarToDolanType ta) (groundedNonpolarToDolanType tb) (pure $ coerceShim "subtype")

matchSubtypeGroup ::
    forall (ground :: GroundTypeKind) dva gta dvb gtb.
    IsDolanSubtypeEntriesGroundType ground =>
    ground dva gta ->
    ground dvb gtb ->
    Maybe (dva :~: dvb, gta :~~: gtb)
matchSubtypeGroup ta tb = do
    let
        ga = getSubtypeGroup ta
        gb = getSubtypeGroup tb
    (Refl, HRefl) <- subtypeGroupTestEquality ga gb
    if sgIsSubtype ga ta tb
        then return (Refl, HRefl)
        else Nothing

type GreaterConversionWit :: GroundTypeKind -> GroundTypeKind
data GreaterConversionWit ground dva gta
    = forall dvb gtb. MkGreaterConversionWit
        (ground dvb gtb)
        (SubtypeConversion ground dva gta dvb gtb)

instance
    forall (ground :: GroundTypeKind) (dv :: CCRVariances) (gt :: CCRVariancesKind dv).
    ShowGroundType ground =>
    Show (GreaterConversionWit ground dv gt)
    where
    show (MkGreaterConversionWit gt _) = show gt

mkGreaterConversionWit :: forall (ground :: GroundTypeKind) dv gt. ground dv gt -> GreaterConversionWit ground dv gt
mkGreaterConversionWit t = MkGreaterConversionWit t identitySubtypeConversion

type LesserConversionWit :: GroundTypeKind -> GroundTypeKind
data LesserConversionWit ground dvb gtb
    = forall dva gta. MkLesserConversionWit
        (ground dva gta)
        (SubtypeConversion ground dva gta dvb gtb)

instance
    forall (ground :: GroundTypeKind) (dv :: CCRVariances) (gt :: CCRVariancesKind dv).
    ShowGroundType ground =>
    Show (LesserConversionWit ground dv gt)
    where
    show (MkLesserConversionWit t _) = show t

mkLesserConversionWit :: forall (ground :: GroundTypeKind) dv gt. ground dv gt -> LesserConversionWit ground dv gt
mkLesserConversionWit t = MkLesserConversionWit t identitySubtypeConversion

greaterCWGroup ::
    forall (ground :: GroundTypeKind) (dv :: CCRVariances) (gt :: CCRVariancesKind dv).
    IsDolanSubtypeEntriesGroundType ground =>
    GreaterConversionWit ground dv gt ->
    SomeSubtypeGroup ground
greaterCWGroup (MkGreaterConversionWit t _) = MkSomeSubtypeGroup $ getSubtypeGroup t

lesserCWGroup ::
    forall (ground :: GroundTypeKind) (dv :: CCRVariances) (gt :: CCRVariancesKind dv).
    IsDolanSubtypeEntriesGroundType ground =>
    LesserConversionWit ground dv gt ->
    SomeSubtypeGroup ground
lesserCWGroup (MkLesserConversionWit t _) = MkSomeSubtypeGroup $ getSubtypeGroup t

matchGreater ::
    forall (ground :: GroundTypeKind) dva gta.
    IsDolanSubtypeEntriesGroundType ground =>
    Bool ->
    SubtypeConversionEntry ground ->
    GreaterConversionWit ground dva gta ->
    Maybe (GreaterConversionWit ground dva gta)
matchGreater True (MkSubtypeConversionEntry TrustMe _ _ _) _ = Nothing
matchGreater _ (MkSubtypeConversionEntry _ sta stb convE) (MkGreaterConversionWit ta conv) = do
    (Refl, HRefl) <- matchSubtypeGroup ta sta
    return $ MkGreaterConversionWit stb $ composeSubtypeConversion convE conv

matchLesser ::
    forall (ground :: GroundTypeKind) dva gta.
    IsDolanSubtypeEntriesGroundType ground =>
    Bool ->
    SubtypeConversionEntry ground ->
    LesserConversionWit ground dva gta ->
    Maybe (LesserConversionWit ground dva gta)
matchLesser True (MkSubtypeConversionEntry TrustMe _ _ _) _ = Nothing
matchLesser _ (MkSubtypeConversionEntry _ sta stb convE) (MkLesserConversionWit tb conv) = do
    (Refl, HRefl) <- matchSubtypeGroup stb tb
    return $ MkLesserConversionWit sta $ composeSubtypeConversion conv convE

getImmediateGreaters ::
    forall (ground :: GroundTypeKind) dva gta.
    IsDolanSubtypeEntriesGroundType ground =>
    Bool ->
    [SubtypeConversionEntry ground] ->
    GreaterConversionWit ground dva gta ->
    [GreaterConversionWit ground dva gta]
getImmediateGreaters rejectTrustMe entries t = mapMaybe (\entry -> matchGreater rejectTrustMe entry t) entries

getImmediateLessers ::
    forall (ground :: GroundTypeKind) dva gta.
    IsDolanSubtypeEntriesGroundType ground =>
    Bool ->
    [SubtypeConversionEntry ground] ->
    LesserConversionWit ground dva gta ->
    [LesserConversionWit ground dva gta]
getImmediateLessers rejectTrustMe entries t = mapMaybe (\entry -> matchLesser rejectTrustMe entry t) entries

greaterContains ::
    forall (ground :: GroundTypeKind) dva gta dvb gtb.
    IsDolanSubtypeEntriesGroundType ground =>
    ground dvb gtb ->
    GreaterConversionWit ground dva gta ->
    Maybe (SubtypeConversion ground dva gta dvb gtb)
greaterContains tb (MkGreaterConversionWit ta conv) = do
    (Refl, HRefl) <- matchSubtypeGroup ta tb
    return conv

findGreater ::
    forall (ground :: GroundTypeKind) dva gta dvb gtb.
    IsDolanSubtypeEntriesGroundType ground =>
    [SubtypeConversionEntry ground] ->
    [SomeSubtypeGroup ground] ->
    [GreaterConversionWit ground dva gta] ->
    ground dvb gtb ->
    [SubtypeConversion ground dva gta dvb gtb]
findGreater _entries _old [] _target = []
findGreater entries old current target = let
    convs = mapMaybe (greaterContains target) current
    allnew = concatmap (getImmediateGreaters False entries) current
    new = filter (\w -> not $ elem (greaterCWGroup w) old) allnew
    in convs <> findGreater entries (fmap greaterCWGroup current <> old) new target

getAllGreaters ::
    forall (ground :: GroundTypeKind) dv gt.
    IsDolanSubtypeEntriesGroundType ground =>
    [SubtypeConversionEntry ground] ->
    [GreaterConversionWit ground dv gt] ->
    [GreaterConversionWit ground dv gt]
getAllGreaters entries current = let
    allnew = concatmap (getImmediateGreaters True entries) current
    old = fmap greaterCWGroup current
    in case filter (\w -> not $ elem (greaterCWGroup w) old) allnew of
        [] -> current
        new -> getAllGreaters entries $ current <> new

getAllLessers ::
    forall (ground :: GroundTypeKind) dv gt.
    IsDolanSubtypeEntriesGroundType ground =>
    [SubtypeConversionEntry ground] ->
    [LesserConversionWit ground dv gt] ->
    [LesserConversionWit ground dv gt]
getAllLessers entries current = let
    allnew = concatmap (getImmediateLessers True entries) current
    old = fmap lesserCWGroup current
    in case filter (\w -> not $ elem (lesserCWGroup w) old) allnew of
        [] -> current
        new -> getAllLessers entries $ current <> new

getChains ::
    forall (ground :: GroundTypeKind) dva gta dvb gtb.
    IsDolanSubtypeEntriesGroundType ground =>
    [SubtypeConversionEntry ground] ->
    ground dva gta ->
    ground dvb gtb ->
    [SubtypeConversion ground dva gta dvb gtb]
getChains entries gta gtb = findGreater entries [] [mkGreaterConversionWit gta] gtb

type IsDolanSubtypeEntriesGroundType :: GroundTypeKind -> Constraint
class
    ( IsDolanSubtypeGroundType ground
    , Eq (DolanSubtypeHint ground)
    , Show (DolanSubtypeHint ground)
    , Semigroup (DolanSubtypeHint ground)
    ) =>
    IsDolanSubtypeEntriesGroundType ground
    where
    getSubtypeGroup ::
        forall (dv :: CCRVariances) (gt :: CCRVariancesKind dv). ground dv gt -> SubtypeGroup ground dv gt
    getSubtypeGroup = singletonSubtypeGroup

subtypeConversionAsGeneralAs ::
    forall (ground :: GroundTypeKind) (dva :: CCRVariances) (gta :: CCRVariancesKind dva) (dvb :: CCRVariances) (gtb :: CCRVariancesKind dvb).
    IsDolanSubtypeGroundType ground =>
    ground dva gta ->
    ground dvb gtb ->
    SubtypeConversion ground dva gta dvb gtb ->
    SubtypeConversion ground dva gta dvb gtb ->
    DolanTypeM ground Bool
subtypeConversionAsGeneralAs _ _ (GeneralSubtypeConversion _ NilSubtypeChain) _ = return True
subtypeConversionAsGeneralAs _ _ _ (GeneralSubtypeConversion _ NilSubtypeChain) = return False
subtypeConversionAsGeneralAs _ _ CoerceSubtypeConversion _ = return True
subtypeConversionAsGeneralAs _ _ _ CoerceSubtypeConversion = return False
subtypeConversionAsGeneralAs ga gb (GeneralSubtypeConversion _ (ConsSubtypeChain link1 chain1)) (GeneralSubtypeConversion _ (ConsSubtypeChain link2 chain2)) =
    runRenamer @(DolanTypeSystem ground) [] []
        $ getChainArguments link1 chain1
        $ \args1a args1b ->
            getChainArguments link2 chain2 $ \rawargs2a rawargs2b ->
                -- cs1 is as general as cs2 if cs1 can subsume to cs2
                do
                    (args2a, args2b) <-
                        namespace @(DolanTypeSystem ground) [] RigidName
                            $ unEndoM (dolanNamespaceRenameArguments <***> dolanNamespaceRenameArguments) (rawargs2a, rawargs2b)
                    shortAnd
                        [ checkCrumbleArguments (groundTypeVarianceMap ga) args2a args1a
                        , checkCrumbleArguments (groundTypeVarianceMap gb) args1b args2b
                        ]

entries_getSubtypeChain ::
    forall (ground :: GroundTypeKind).
    IsDolanSubtypeEntriesGroundType ground =>
    [SubtypeConversionEntry ground] -> GetSubtypeChain ground
entries_getSubtypeChain entries = let
    gsc = MkGetSubtypeChain $ \ga gb -> runDolanTypeM gsc $ do
        let conversions = getChains entries ga gb
        bestConversions <- bestM (subtypeConversionAsGeneralAs ga gb) conversions
        case bestConversions of
            [sconv] -> return $ subtypeConversionChain sconv
            [] -> throw $ NoGroundConvertTypeError ga gb
            _ -> throw $ IncoherentGroundConvertTypeError ga gb
    in gsc

runDolanTypeMEntries ::
    forall (ground :: GroundTypeKind).
    IsDolanSubtypeEntriesGroundType ground =>
    [SubtypeConversionEntry ground] -> DolanTypeM ground --> TypeResult ground
runDolanTypeMEntries entries = runDolanTypeM $ entries_getSubtypeChain entries

type SubtypeConversionPair :: GroundTypeKind -> Type
data SubtypeConversionPair ground
    = forall dva gta dvb gtb. MkSubtypeConversionPair
        (ground dva gta)
        (ground dvb gtb)
        (SubtypeConversion ground dva gta dvb gtb)
        (SubtypeConversion ground dva gta dvb gtb)

type SubtypeConversionCandidate :: GroundTypeKind -> Type
data SubtypeConversionCandidate ground
    = forall dva gta dvb gtb. MkSubtypeConversionCandidate
        (ground dva gta)
        (ground dvb gtb)
        (SubtypeConversion ground dva gta dvb gtb)
        (SubtypeConversion ground dva gta dvb gtb)
        [SomeGroundType ground]
        [SomeGroundType ground]

type GreaterConversionClosure :: GroundTypeKind -> Type
-- Keep the typed closure for candidate construction and its endpoint-only
-- projection for the implication frontier.
data GreaterConversionClosure ground
    = forall dv gt. MkGreaterConversionClosure
        (ground dv gt)
        [GreaterConversionWit ground dv gt]
        [SomeGroundType ground]

greaterConversionGroundType ::
    forall (ground :: GroundTypeKind) dv gt.
    GreaterConversionWit ground dv gt ->
    SomeGroundType ground
greaterConversionGroundType (MkGreaterConversionWit groundType _) = MkSomeGroundType groundType

getGroundTypeClosure ::
    forall (ground :: GroundTypeKind).
    IsDolanSubtypeEntriesGroundType ground =>
    [SubtypeConversionEntry ground] ->
    SomeGroundType ground ->
    [SomeGroundType ground]
getGroundTypeClosure entries (MkSomeGroundType groundType) =
    fmap greaterConversionGroundType $ getAllGreaters entries [mkGreaterConversionWit groundType]

mkGreaterConversionClosure ::
    forall (ground :: GroundTypeKind).
    IsDolanSubtypeEntriesGroundType ground =>
    [SubtypeConversionEntry ground] ->
    SomeGroundType ground ->
    GreaterConversionClosure ground
mkGreaterConversionClosure entries (MkSomeGroundType groundType) =
    let
        closure = getAllGreaters entries [mkGreaterConversionWit groundType]
        in MkGreaterConversionClosure groundType closure $ fmap greaterConversionGroundType closure

lookupGreaterConversionClosure ::
    forall (ground :: GroundTypeKind) dv gt.
    IsDolanGroundType ground =>
    ground dv gt ->
    [GreaterConversionClosure ground] ->
    Maybe ([GreaterConversionWit ground dv gt], [SomeGroundType ground])
lookupGreaterConversionClosure _ [] = Nothing
lookupGreaterConversionClosure target (MkGreaterConversionClosure source closure groundTypes : closures) =
    case groundTypeTestEquality target source of
        Just (Refl, HRefl) -> Just (closure, groundTypes)
        Nothing -> lookupGreaterConversionClosure target closures

groundTypesFromGreaterConversionClosure ::
    forall (ground :: GroundTypeKind).
    GreaterConversionClosure ground ->
    (SomeGroundType ground, [SomeGroundType ground])
groundTypesFromGreaterConversionClosure (MkGreaterConversionClosure source _ groundTypes) =
    (MkSomeGroundType source, groundTypes)

groundTypeClosureContains ::
    forall (ground :: GroundTypeKind) dv gt.
    IsDolanSubtypeEntriesGroundType ground =>
    ground dv gt ->
    [SomeGroundType ground] ->
    Bool
groundTypeClosureContains target =
    any $ \(MkSomeGroundType groundType) -> isJust $ matchSubtypeGroup groundType target

subtypeConversionCandidateImplies ::
    forall (ground :: GroundTypeKind).
    IsDolanSubtypeEntriesGroundType ground =>
    SubtypeConversionCandidate ground ->
    SubtypeConversionCandidate ground ->
    Bool
-- Commutativity of (source1, target1) implies commutativity of
-- (source2, target2) by precomposition when source2 <: source1 and
-- postcomposition when target1 <: target2.
subtypeConversionCandidateImplies
    (MkSubtypeConversionCandidate source1 _ _ _ _ target1Greaters)
    (MkSubtypeConversionCandidate _ target2 _ _ source2Greaters _) =
        groundTypeClosureContains source1 source2Greaters
            && groundTypeClosureContains target2 target1Greaters

subtypeConversionCandidateStrictlyImplies ::
    forall (ground :: GroundTypeKind).
    IsDolanSubtypeEntriesGroundType ground =>
    SubtypeConversionCandidate ground ->
    SubtypeConversionCandidate ground ->
    Bool
subtypeConversionCandidateStrictlyImplies c1 c2 =
    subtypeConversionCandidateImplies c1 c2
        && not (subtypeConversionCandidateImplies c2 c1)

subtypeConversionCandidateEquivalent ::
    forall (ground :: GroundTypeKind).
    IsDolanSubtypeEntriesGroundType ground =>
    SubtypeConversionCandidate ground ->
    SubtypeConversionCandidate ground ->
    Bool
subtypeConversionCandidateEquivalent c1 c2 =
    subtypeConversionCandidateImplies c1 c2
        && subtypeConversionCandidateImplies c2 c1

subtypeConversionCandidateIsKnownConsistent ::
    forall (ground :: GroundTypeKind).
    IsDolanSubtypeEntriesGroundType ground =>
    SubtypeConversionCandidate ground ->
    Bool
subtypeConversionCandidateIsKnownConsistent (MkSubtypeConversionCandidate _ _ conv1 conv2 _ _) =
    conv1 == conv2

findInconsistentPair ::
    forall (ground :: GroundTypeKind).
    IsDolanSubtypeEntriesGroundType ground =>
    [SubtypeConversionEntry ground] ->
    SubtypeConversionEntry ground ->
    Maybe (SubtypeConversionPair ground)
findInconsistentPair entries (MkSubtypeConversionEntry _ (ta :: ground dva gta) (tb :: ground dvb gtb) tconv) = let
    bgreaters :: [GreaterConversionWit ground dvb gtb]
    bgreaters = getAllGreaters entries [mkGreaterConversionWit tb]
    alessers :: [LesserConversionWit ground dva gta]
    alessers = getAllLessers entries [mkLesserConversionWit ta]
    sourceGroundTypes :: [SomeGroundType ground]
    sourceGroundTypes = nub $ do
        MkLesserConversionWit source _ <- alessers
        return $ MkSomeGroundType source
    groundTargetType :: SomeGroundType ground
    groundTargetType = MkSomeGroundType tb
    availableConversionClosures :: [GreaterConversionClosure ground]
    availableConversionClosures =
        MkGreaterConversionClosure tb bgreaters (fmap greaterConversionGroundType bgreaters)
            : fmap (mkGreaterConversionClosure entries) (filter (/= groundTargetType) sourceGroundTypes)
    pairClosures :: [(SubtypeConversionPair ground, [SomeGroundType ground])]
    pairClosures = do
        MkLesserConversionWit (ea :: ground dva' gta') aconv <- alessers
        let
            calculatedClosure :: ([GreaterConversionWit ground dva' gta'], [SomeGroundType ground])
            calculatedClosure =
                let
                    closure = getAllGreaters entries [mkGreaterConversionWit ea]
                    in (closure, fmap greaterConversionGroundType closure)
            (allagreaters, sourceClosure) =
                fromMaybe
                    calculatedClosure
                    $ lookupGreaterConversionClosure ea availableConversionClosures
        MkGreaterConversionWit eb conv <- allagreaters
        MkGreaterConversionWit maxb bconv <- bgreaters
        (Refl, HRefl) <- mpure $ matchSubtypeGroup eb maxb
        return
            ( MkSubtypeConversionPair ea eb conv
                $ composeSubtypeConversion bconv
                $ composeSubtypeConversion tconv aconv
            , sourceClosure
            )
    availableClosures :: [(SomeGroundType ground, [SomeGroundType ground])]
    availableClosures = fmap groundTypesFromGreaterConversionClosure availableConversionClosures
    targets :: [SomeGroundType ground]
    targets = nub $ do
        (MkSubtypeConversionPair _ target _ _, _) <- pairClosures
        return $ MkSomeGroundType target
    targetClosures :: [(SomeGroundType ground, [SomeGroundType ground])]
    targetClosures =
        fmap
            (\target -> (target, fromMaybe (getGroundTypeClosure entries target) $ lookup target availableClosures))
            targets
    candidates :: [SubtypeConversionCandidate ground]
    candidates = do
        (MkSubtypeConversionPair source target conv1 conv2, sourceClosure) <- pairClosures
        let
            groundTarget = MkSomeGroundType target
            targetClosure =
                fromMaybe (getGroundTypeClosure entries groundTarget) $ lookup groundTarget targetClosures
        return $ MkSubtypeConversionCandidate source target conv1 conv2 sourceClosure targetClosure
    -- A strictly implied square follows by pre/postcomposition, so only the
    -- implication frontier contains independent consistency obligations.
    isCritical candidate =
        not $ any (`subtypeConversionCandidateStrictlyImplies` candidate) candidates
    -- Mutually implying squares are the same obligation. One known-consistent
    -- representative proves the class, even if the others have UnknownSK.
    equivalentClassIsKnownConsistent candidate =
        any
            ( \candidate' ->
                subtypeConversionCandidateEquivalent candidate candidate'
                    && subtypeConversionCandidateIsKnownConsistent candidate'
            )
            candidates
    in case find (\candidate -> isCritical candidate && not (equivalentClassIsKnownConsistent candidate)) candidates of
        Just (MkSubtypeConversionCandidate source target conv1 conv2 _ _) ->
            Just $ MkSubtypeConversionPair source target conv1 conv2
        Nothing -> Nothing

checkSubtypeConsistency ::
    forall (ground :: GroundTypeKind).
    IsDolanSubtypeEntriesGroundType ground =>
    [SubtypeConversionEntry ground] ->
    SubtypeConversionEntry ground ->
    Maybe (SomeGroundType ground, SomeGroundType ground)
checkSubtypeConsistency _ (MkSubtypeConversionEntry TrustMe _ _ _) = Nothing
checkSubtypeConsistency entries sce@(MkSubtypeConversionEntry Verify _ _ _) = do
    MkSubtypeConversionPair ta tb _ _ <- findInconsistentPair entries sce
    return (MkSomeGroundType ta, MkSomeGroundType tb)
