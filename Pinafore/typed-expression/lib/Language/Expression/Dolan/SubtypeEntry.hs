{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Dolan.SubtypeEntry
    ( SubtypeConversionEntry(..)
    , subtypeConversionEntry
    , IsDolanSubtypeEntriesGroundType(..)
    , entries_subtypeGroundedTypes
    , SubtypeGroupTest(..)
    , testEqualitySubtypeGroupTest
    , SubtypeGroup(..)
    , singletonSubtypeGroup
    ) where

import Control.Applicative.Wrapped
import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Subtype
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Variance
import Shapes

type SubtypeConversionEntry :: GroundTypeKind -> Type
data SubtypeConversionEntry ground =
    forall dva gta dvb gtb. MkSubtypeConversionEntry (ground dva gta)
                                                     (ground dvb gtb)
                                                     (SubtypeConversion ground dva gta dvb gtb)

instance forall (ground :: GroundTypeKind). DebugIsDolanGroundType ground => Show (SubtypeConversionEntry ground) where
    show (MkSubtypeConversionEntry ta tb _) = show ta <> " <: " <> show tb

subtypeConversionEntry ::
       forall (ground :: GroundTypeKind) a b. IsDolanSubtypeGroundType ground
    => DolanGroundedShimWit ground 'Negative a
    -> DolanGroundedShimWit ground 'Positive b
    -> TSOpenExpression (DolanTypeSystem ground) (DolanShim ground a b)
    -> SubtypeConversionEntry ground
subtypeConversionEntry (MkShimWit (MkDolanGroundedType gta argsa) conva) (MkShimWit (MkDolanGroundedType gtb argsb) convb) conv =
    MkSubtypeConversionEntry gta gtb $ subtypeConversion gta (MkShimWit argsa conva) gtb (MkShimWit argsb convb) conv

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
data SubtypeGroup ground = forall (dv :: DolanVariance) (gt :: DolanVarianceKind dv). MkSubtypeGroup
    { sgWitness :: ground dv gt
    , sgIsSubtype :: SubtypeGroupTest ground
    }

instance forall (ground :: GroundTypeKind). IsDolanGroundType ground => Eq (SubtypeGroup ground) where
    MkSubtypeGroup ta _ == MkSubtypeGroup tb _ = isJust $ groundTypeTestEquality ta tb

instance forall (ground :: GroundTypeKind). DebugIsDolanGroundType ground => Show (SubtypeGroup ground) where
    show (MkSubtypeGroup t _) = show t

singletonSubtypeGroup ::
       forall (ground :: GroundTypeKind) dv gt. IsDolanGroundType ground
    => ground dv gt
    -> SubtypeGroup ground
singletonSubtypeGroup gt = MkSubtypeGroup gt testEqualitySubtypeGroupTest

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

type SubtypeConversionWit :: GroundTypeKind -> forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type
data SubtypeConversionWit ground dva gta =
    forall dvb gtb. MkSubtypeConversionWit (ground dvb gtb)
                                           (SubtypeConversion ground dva gta dvb gtb)

instance forall (ground :: GroundTypeKind) (dv :: DolanVariance) (gt :: DolanVarianceKind dv). DebugIsDolanGroundType ground =>
             Show (SubtypeConversionWit ground dv gt) where
    show (MkSubtypeConversionWit t _) = show t

scwSubgroup ::
       forall (ground :: GroundTypeKind) (dv :: DolanVariance) (gt :: DolanVarianceKind dv).
       IsDolanSubtypeEntriesGroundType ground
    => SubtypeConversionWit ground dv gt
    -> SubtypeGroup ground
scwSubgroup (MkSubtypeConversionWit t _) = getSubtypeGroup t

instance forall (ground :: GroundTypeKind) dva gta. IsDolanSubtypeEntriesGroundType ground =>
             Eq (SubtypeConversionWit ground dva gta) where
    p == q = scwSubgroup p == scwSubgroup q

matchSupertype ::
       forall (ground :: GroundTypeKind) dva gta. IsDolanSubtypeEntriesGroundType ground
    => SubtypeConversionEntry ground
    -> SubtypeConversionWit ground dva gta
    -> Maybe (SubtypeConversionWit ground dva gta)
matchSupertype (MkSubtypeConversionEntry sta stb convE) (MkSubtypeConversionWit ta conv) = do
    sconv <- matchBySubtypeGroup ta sta
    return $ MkSubtypeConversionWit stb $ composeSubtypeConversion convE $ composeSubtypeConversion sconv conv

getImmediateSupertypes ::
       forall (ground :: GroundTypeKind) dva gta. IsDolanSubtypeEntriesGroundType ground
    => [SubtypeConversionEntry ground]
    -> SubtypeConversionWit ground dva gta
    -> [SubtypeConversionWit ground dva gta]
getImmediateSupertypes entries t = mapMaybe (\entry -> matchSupertype entry t) entries

expandSupertypes ::
       forall (ground :: GroundTypeKind) dva gta. IsDolanSubtypeEntriesGroundType ground
    => [SubtypeConversionEntry ground]
    -> [SubtypeConversionWit ground dva gta]
    -> [SubtypeConversionWit ground dva gta]
expandSupertypes entries tt = let
    ttt = fmap (getImmediateSupertypes entries) tt
    in nub $ mconcat $ tt : ttt

contains ::
       forall (ground :: GroundTypeKind) dva gta dvb gtb. IsDolanSubtypeEntriesGroundType ground
    => [SubtypeConversionWit ground dva gta]
    -> ground dvb gtb
    -> Maybe (SubtypeConversion ground dva gta dvb gtb)
contains [] _ = Nothing
contains (MkSubtypeConversionWit ta conv:_) tb
    | Just convm <- matchBySubtypeGroup ta tb = Just $ composeSubtypeConversion convm conv
contains (_:aa) tb = contains aa tb

isSupertype ::
       forall (ground :: GroundTypeKind) dva gta dvb gtb. IsDolanSubtypeEntriesGroundType ground
    => [SubtypeConversionEntry ground]
    -> [SubtypeConversionWit ground dva gta]
    -> ground dvb gtb
    -> Maybe (SubtypeConversion ground dva gta dvb gtb)
isSupertype _entries aa a
    | Just mconv <- contains aa a = Just mconv
isSupertype entries aa a = let
    aa' = expandSupertypes entries aa
    in if length aa' > length aa
           then isSupertype entries aa' a
           else Nothing

getSubtypeShim ::
       forall (ground :: GroundTypeKind) dva gta dvb gtb. IsDolanSubtypeEntriesGroundType ground
    => [SubtypeConversionEntry ground]
    -> ground dva gta
    -> ground dvb gtb
    -> Maybe (SubtypeConversion ground dva gta dvb gtb)
getSubtypeShim entries gta gtb = isSupertype entries [MkSubtypeConversionWit gta IdentitySubtypeConversion] gtb

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
