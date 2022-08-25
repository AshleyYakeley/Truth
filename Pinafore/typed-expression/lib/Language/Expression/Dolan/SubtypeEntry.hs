{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Dolan.SubtypeEntry
    ( SubtypeConversionEntry(..)
    , subtypeConversionEntry
    , simpleSubtypeConversionEntry
    , IsDolanSubtypeEntriesGroundType(..)
    , entries_subtypeGroundedTypes
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
    forall dvb gtb. MkSubtypeConversionEntry (ground dvb gtb)
                                             (forall dva gta.
                                                      ground dva gta -> Maybe (SubtypeConversion ground dva gta dvb gtb))

subtypeConversionEntry ::
       forall (ground :: GroundTypeKind) a b. IsDolanSubtypeGroundType ground
    => DolanGroundedShimWit ground 'Negative a
    -> DolanGroundedShimWit ground 'Positive b
    -> TSOpenExpression (DolanTypeSystem ground) (DolanShim ground a b)
    -> SubtypeConversionEntry ground
subtypeConversionEntry (MkShimWit (MkDolanGroundedType gta argsa) conva) (MkShimWit (MkDolanGroundedType gtb argsb) convb) conv =
    simpleSubtypeConversionEntry gta gtb $
    subtypeConversion gta (MkShimWit argsa conva) gtb (MkShimWit argsb convb) conv

simpleSubtypeConversionEntry ::
       forall (ground :: GroundTypeKind) dva gta dvb gtb. IsDolanGroundType ground
    => ground dva gta
    -> ground dvb gtb
    -> SubtypeConversion ground dva gta dvb gtb
    -> SubtypeConversionEntry ground
simpleSubtypeConversionEntry gta gtb sconv =
    MkSubtypeConversionEntry gtb $ \gta' -> do
        (Refl, HRefl) <- groundTypeTestEquality gta gta'
        return sconv

type SubtypeConversionWit :: GroundTypeKind -> forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type
data SubtypeConversionWit ground dva gta =
    forall dvb gtb. MkSubtypeConversionWit (ground dvb gtb)
                                           (SubtypeConversion ground dva gta dvb gtb)

instance forall (ground :: GroundTypeKind) dva gta. IsDolanGroundType ground => Eq (SubtypeConversionWit ground dva gta) where
    (MkSubtypeConversionWit tp _) == (MkSubtypeConversionWit tq _) = isJust $ groundTypeTestEquality tp tq

matchSupertype ::
       forall (ground :: GroundTypeKind) dva gta. IsDolanSubtypeGroundType ground
    => SubtypeConversionEntry ground
    -> SubtypeConversionWit ground dva gta
    -> Maybe (SubtypeConversionWit ground dva gta)
matchSupertype (MkSubtypeConversionEntry tb f) (MkSubtypeConversionWit ta conv) = do
    convE <- f ta
    return $ MkSubtypeConversionWit tb $ composeSubtypeConversion convE conv

getImmediateSupertypes ::
       forall (ground :: GroundTypeKind) dva gta. IsDolanSubtypeGroundType ground
    => [SubtypeConversionEntry ground]
    -> SubtypeConversionWit ground dva gta
    -> [SubtypeConversionWit ground dva gta]
getImmediateSupertypes entries t = mapMaybe (\entry -> matchSupertype entry t) entries

expandSupertypes ::
       forall (ground :: GroundTypeKind) dva gta. IsDolanSubtypeGroundType ground
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
    | Just convm <- subtypeConversionMatchType ta tb = Just $ composeSubtypeConversion convm conv
contains (_:aa) tb = contains aa tb

isSupertype ::
       forall (ground :: GroundTypeKind) dva gta dvb gtb. IsDolanSubtypeEntriesGroundType ground
    => [SubtypeConversionEntry ground]
    -> [SubtypeConversionWit ground dva gta]
    -> ground dvb gtb
    -> Maybe (SubtypeConversion ground dva gta dvb gtb)
isSupertype _entries aa a
    | Just conv <- contains aa a = Just conv
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
getSubtypeShim entries ta tb = isSupertype entries [MkSubtypeConversionWit ta idSubtypeConversion] tb

type IsDolanSubtypeEntriesGroundType :: GroundTypeKind -> Constraint
class IsDolanSubtypeGroundType ground => IsDolanSubtypeEntriesGroundType ground where
    subtypeConversionEntries :: DolanM ground [SubtypeConversionEntry ground]
    subtypeConversionMatchType ::
           forall (dva :: DolanVariance) (ta :: DolanVarianceKind dva) (dvb :: DolanVariance) (tb :: DolanVarianceKind dvb).
           ground dva ta
        -> ground dvb tb
        -> Maybe (SubtypeConversion ground dva ta dvb tb)
    subtypeConversionMatchType gta gtb = do
        (Refl, HRefl) <- groundTypeTestEquality gta gtb
        return idSubtypeConversion
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
            Just (MkSubtypeConversion sconv) -> sconv sc argsa
            Nothing -> lift $ throwGroundTypeConvertError ta tb
    in wbind margswit $ \(MkSubtypeArguments argsb' sargsconv) ->
           (\p q -> p . q) <$> subtypeDolanArguments sc tb argsb' argsb <*> sargsconv
