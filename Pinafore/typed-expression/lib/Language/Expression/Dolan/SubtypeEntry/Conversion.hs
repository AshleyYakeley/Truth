module Language.Expression.Dolan.SubtypeEntry.Conversion where

import Data.Shim
import Shapes

import Language.Expression.Dolan.SubtypeChain
import Language.Expression.Dolan.SubtypeEntry.Knowledge
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem

type SubtypeConversion ::
    GroundTypeKind ->
    forall (dva :: CCRVariances) ->
    CCRVariancesKind dva -> forall (dvb :: CCRVariances) -> CCRVariancesKind dvb -> Type
data SubtypeConversion ground dva gta dvb gtb where
    GeneralSubtypeConversion ::
        forall (ground :: GroundTypeKind) (dva :: CCRVariances) (gta :: CCRVariancesKind dva) (dvb :: CCRVariances) (gtb :: CCRVariancesKind dvb).
        SubtypeKnowledge ground ->
        SubtypeChain ground dva gta dvb gtb ->
        SubtypeConversion ground dva gta dvb gtb
    CoerceSubtypeConversion ::
        forall (ground :: GroundTypeKind) (gta :: Type) (gtb :: Type).
        Coercible gta gtb =>
        SubtypeConversion ground '[] gta '[] gtb

subtypeConversionChain ::
    forall (ground :: GroundTypeKind) (dva :: CCRVariances) (gta :: CCRVariancesKind dva) (dvb :: CCRVariances) (gtb :: CCRVariancesKind dvb).
    IsDolanGroundType ground =>
    SubtypeConversion ground dva gta dvb gtb ->
    SubtypeChain ground dva gta dvb gtb
subtypeConversionChain (GeneralSubtypeConversion _ chain) = chain
subtypeConversionChain CoerceSubtypeConversion =
    linkSubtypeChain NilCCRVariancesMap NilCCRArguments NilCCRArguments $ pure $ coercionToShim MkCoercion

subtypeConversionKnowledge ::
    forall (ground :: GroundTypeKind) (dva :: CCRVariances) (gta :: CCRVariancesKind dva) (dvb :: CCRVariances) (gtb :: CCRVariancesKind dvb).
    SubtypeConversion ground dva gta dvb gtb ->
    SubtypeKnowledge ground
subtypeConversionKnowledge (GeneralSubtypeConversion sk _) = sk
subtypeConversionKnowledge CoerceSubtypeConversion = NeutralSK

instance
    forall (ground :: GroundTypeKind) (dva :: CCRVariances) (gta :: CCRVariancesKind dva) (dvb :: CCRVariances) (gtb :: CCRVariancesKind dvb).
    ( IsDolanGroundType ground
    , Eq (DolanSubtypeHint ground)
    ) =>
    Eq (SubtypeConversion ground dva gta dvb gtb)
    where
    sc1 == sc2 = subtypeConversionKnowledge sc1 == subtypeConversionKnowledge sc2

instance
    forall (ground :: GroundTypeKind) (dva :: CCRVariances) (gta :: CCRVariancesKind dva) (dvb :: CCRVariances) (gtb :: CCRVariancesKind dvb).
    ( IsDolanGroundType ground
    , Show (DolanSubtypeHint ground)
    ) =>
    Show (SubtypeConversion ground dva gta dvb gtb)
    where
    show (GeneralSubtypeConversion sk chain) = "general (" <> show sk <> "): " <> show chain
    show CoerceSubtypeConversion = "coerce"

identitySubtypeConversion ::
    forall (ground :: GroundTypeKind) (dv :: CCRVariances) (gt :: CCRVariancesKind dv).
    SubtypeConversion ground dv gt dv gt
identitySubtypeConversion = GeneralSubtypeConversion NeutralSK NilSubtypeChain

coerceSubtypeConversion ::
    forall (ground :: GroundTypeKind) (gta :: Type) (gtb :: Type).
    Coercible gta gtb =>
    SubtypeConversion ground '[] gta '[] gtb
coerceSubtypeConversion = CoerceSubtypeConversion

singleSubtypeConversion ::
    forall (ground :: GroundTypeKind) a b.
    Maybe (DolanSubtypeHint ground) ->
    DolanOpenExpression ground (DolanShim ground a b) ->
    SubtypeConversion ground '[] a '[] b
singleSubtypeConversion hint conv =
    GeneralSubtypeConversion (maybe UnknownSK HintSK hint)
        $ linkSubtypeChain NilCCRVariancesMap NilCCRArguments NilCCRArguments conv

composeSubtypeConversion ::
    forall (ground :: GroundTypeKind) dva gta dvb gtb dvc gtc.
    (IsDolanGroundType ground, Semigroup (DolanSubtypeHint ground)) =>
    SubtypeConversion ground dvb gtb dvc gtc ->
    SubtypeConversion ground dva gta dvb gtb ->
    SubtypeConversion ground dva gta dvc gtc
composeSubtypeConversion (GeneralSubtypeConversion _ NilSubtypeChain) ab = ab
composeSubtypeConversion bc (GeneralSubtypeConversion _ NilSubtypeChain) = bc
composeSubtypeConversion CoerceSubtypeConversion CoerceSubtypeConversion = CoerceSubtypeConversion
composeSubtypeConversion (GeneralSubtypeConversion hintbc bc) (GeneralSubtypeConversion hintab ab) =
    GeneralSubtypeConversion (hintab <> hintbc) $ composeSubtypeChain bc ab
composeSubtypeConversion (GeneralSubtypeConversion hint bc) sc@CoerceSubtypeConversion =
    GeneralSubtypeConversion hint $ composeSubtypeChain bc (subtypeConversionChain sc)
composeSubtypeConversion sc@CoerceSubtypeConversion (GeneralSubtypeConversion hint ab) =
    GeneralSubtypeConversion hint $ composeSubtypeChain (subtypeConversionChain sc) ab

subtypeConversion ::
    forall (ground :: GroundTypeKind) dva gta a dvb gtb b.
    IsDolanGroundType ground =>
    SubtypeKnowledge ground ->
    ground dva gta ->
    CCRPolarArgumentsShimWit (DolanPolyShim ground) dva (DolanType ground) gta 'Negative a ->
    CCRPolarArgumentsShimWit (DolanPolyShim ground) dvb (DolanType ground) gtb 'Positive b ->
    DolanOpenExpression ground (DolanShim ground a b) ->
    SubtypeConversion ground dva gta dvb gtb
subtypeConversion sk ga (MkShimWit argsa (MkPolarShim conva)) (MkShimWit argsb (MkPolarShim convb)) convexpr =
    GeneralSubtypeConversion sk
        $ linkSubtypeChain (groundTypeVarianceMap ga) argsa argsb
        $ fmap (\conv -> convb . conv . conva) convexpr

matchIdentitySubtypeConversion ::
    forall (ground :: GroundTypeKind) (dva :: CCRVariances) (gta :: CCRVariancesKind dva) (dvb :: CCRVariances) (gtb :: CCRVariancesKind dvb).
    SubtypeConversion ground dva gta dvb gtb ->
    Maybe (dva :~: dvb, gta :~~: gtb)
matchIdentitySubtypeConversion (GeneralSubtypeConversion _ NilSubtypeChain) = Just (Refl, HRefl)
matchIdentitySubtypeConversion _ = Nothing
