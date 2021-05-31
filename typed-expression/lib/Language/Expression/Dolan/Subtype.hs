{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Dolan.Subtype
    ( SubtypeContext(..)
    , subtypeDolanArguments
    , IsDolanSubtypeGroundType(..)
    , SubtypeArguments(..)
    , SubtypeConversion(..)
    , nilSubtypeConversion
    , idSubtypeConversion
    , composeSubtypeConversion
    , SubypeConversionEntry(..)
    , simpleSubtypeConversionEntry
    , saturateGroundType
    , IsDolanSubtypeEntriesGroundType(..)
    ) where

import Control.Applicative.Wrapped
import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Variance
import Shapes

type SubtypeContext :: (Polarity -> Type -> Type) -> ShimKind Type -> (Type -> Type) -> Type
newtype SubtypeContext w shim solver = MkSubtypeContext
    { subtypeConvert :: forall ta tb pola polb.
                            (Is PolarityType pola, Is PolarityType polb) =>
                                    w pola ta -> w polb tb -> solver (shim ta tb)
    }

subtypeVariance ::
       forall (w :: Polarity -> Type -> Type) (shim :: ShimKind Type) solver pola polb sv a b.
       (Applicative solver, Is PolarityType pola, Is PolarityType polb)
    => SubtypeContext w shim solver
    -> VarianceType sv
    -> SingleArgument sv w pola a
    -> SingleArgument sv w polb b
    -> solver (VarianceCategory shim sv a b)
subtypeVariance sc CovarianceType ta tb = subtypeConvert sc ta tb
subtypeVariance sc ContravarianceType ta tb =
    invertPolarity @pola $
    invertPolarity @polb $ do
        ba <- subtypeConvert sc tb ta
        return $ MkCatDual ba
subtypeVariance sc RangevarianceType (MkRangeType tpa tqa) (MkRangeType tpb tqb) =
    invertPolarity @pola $
    invertPolarity @polb $ do
        pba <- subtypeConvert sc tpb tpa
        qab <- subtypeConvert sc tqa tqb
        return $ MkCatRange pba qab

subtypeArguments ::
       forall (w :: Polarity -> Type -> Type) (pshim :: PolyShimKind) solver pola polb dv (gta :: DolanVarianceKind dv) (gtb :: DolanVarianceKind dv) ta tb.
       (DolanVarianceInCategory pshim, Applicative solver, Is PolarityType pola, Is PolarityType polb)
    => SubtypeContext w (pshim Type) solver
    -> DolanVarianceType dv
    -> DolanVarianceMap dv gta
    -> DolanVarianceMap dv gtb
    -> DolanArguments dv w gta pola ta
    -> DolanArguments dv w gtb polb tb
    -> solver (pshim (DolanVarianceKind dv) gta gtb -> pshim Type ta tb)
subtypeArguments _ NilListType NilDolanVarianceMap NilDolanVarianceMap NilDolanArguments NilDolanArguments = pure id
subtypeArguments sc (ConsListType svt dvt) (ConsDolanVarianceMap dvma) (ConsDolanVarianceMap dvmb) (ConsDolanArguments sta dta) (ConsDolanArguments stb dtb) =
    case applyFunctionKindWitness (inKind @_ @gta) sta of
        Dict ->
            case applyFunctionKindWitness (inKind @_ @gtb) stb of
                Dict ->
                    case applyFunctionKindWitness (inKind @_ @gta) stb of
                        Dict ->
                            case varianceCoercibleKind svt of
                                Dict ->
                                    case dolanVarianceInCategory @pshim dvt of
                                        Dict -> do
                                            sfunc <- subtypeVariance @_ @_ @_ @pola @polb sc svt sta stb
                                            f <- subtypeArguments sc dvt dvma dvmb dta dtb
                                            pure $ \conv -> f (applyPolyShim svt conv sfunc)

subtypeDolanArguments ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) solver pola polb dv gt argsa argsb.
       ( IsDolanGroundType ground
       , DolanVarianceInCategory pshim
       , Applicative solver
       , Is PolarityType pola
       , Is PolarityType polb
       )
    => SubtypeContext (DolanType ground) (pshim Type) solver
    -> ground dv gt
    -> DolanArguments dv (DolanType ground) gt pola argsa
    -> DolanArguments dv (DolanType ground) gt polb argsb
    -> solver (pshim Type argsa argsb)
subtypeDolanArguments sc gt argsa argsb = let
    vkt = groundTypeVarianceType gt
    dvm = groundTypeVarianceMap gt
    in case dolanVarianceMapInKind dvm of
           Dict ->
               case dolanVarianceInCategory @pshim vkt of
                   Dict -> fmap (\f -> f cid) $ subtypeArguments sc vkt dvm dvm argsa argsb

type DebugIsDolanGroundType :: GroundTypeKind -> Constraint
class ( IsDolanGroundType ground
      , MonadIO (DolanM ground)
      , MonadCatch ErrorCall (DolanM ground)
      , forall polarity t. Is PolarityType polarity => Show (DolanType ground polarity t)
      ) => DebugIsDolanGroundType ground

instance forall (ground :: GroundTypeKind). ( IsDolanGroundType ground
         , MonadIO (DolanM ground)
         , MonadCatch ErrorCall (DolanM ground)
         , forall polarity t. Is PolarityType polarity => Show (DolanType ground polarity t)
         ) => DebugIsDolanGroundType ground

type IsDolanSubtypeGroundType :: GroundTypeKind -> Constraint
class IsDolanGroundType ground => IsDolanSubtypeGroundType ground where
    subtypeGroundTypes ::
           forall solver pola polb dva gta a dvb gtb b.
           ( WrappedApplicative solver
           , WAInnerM solver ~ DolanTypeCheckM ground
           , Is PolarityType pola
           , Is PolarityType polb
           )
        => SubtypeContext (DolanType ground) (DolanShim ground) solver
        -> ground dva gta
        -> DolanArguments dva (DolanType ground) gta pola a
        -> ground dvb gtb
        -> DolanArguments dvb (DolanType ground) gtb polb b
        -> solver (DolanShim ground a b)
    default subtypeGroundTypes ::
        forall solver pola polb dva gta a dvb gtb b.
            ( IsDolanSubtypeEntriesGroundType ground
            , WrappedApplicative solver
            , WAInnerM solver ~ DolanTypeCheckM ground
            , Is PolarityType pola
            , Is PolarityType polb
            ) =>
                    SubtypeContext (DolanType ground) (DolanShim ground) solver -> ground dva gta -> DolanArguments dva (DolanType ground) gta pola a -> ground dvb gtb -> DolanArguments dvb (DolanType ground) gtb polb b -> solver (DolanShim ground a b)
    subtypeGroundTypes sc ta argsa tb argsb = let
        margswit :: DolanTypeCheckM ground (SubtypeArguments ground solver pola dvb gtb a)
        margswit = do
            entries <- lift subtypeConversionEntries
            case getSubtypeShim entries ta tb of
                Just (MkSubtypeConversion sconv) -> sconv sc argsa
                Nothing -> lift $ throwGroundTypeConvertError ta tb
        in wbind margswit $ \(MkSubtypeArguments argsb' sargsconv) ->
               (<.>) <$> subtypeDolanArguments sc tb argsb' argsb <*> sargsconv
    tackOnTypeConvertError ::
           (Is PolarityType pola, Is PolarityType polb)
        => DolanType ground pola ta
        -> DolanType ground polb tb
        -> DolanM ground a
        -> DolanM ground a
    throwTypeNotInvertible :: Is PolarityType polarity => DolanType ground polarity t -> DolanM ground a

type SubtypeArguments :: GroundTypeKind -> (Type -> Type) -> Polarity -> forall (dv :: DolanVariance) ->
                                                                                 DolanVarianceKind dv -> Type -> Type
data SubtypeArguments ground solver polarity dv gt a =
    forall b. MkSubtypeArguments (DolanArguments dv (DolanType ground) gt polarity b)
                                 (solver (DolanShim ground a b))

type SubtypeConversion :: GroundTypeKind -> forall (dva :: DolanVariance) ->
                                                    DolanVarianceKind dva -> forall (dvb :: DolanVariance) ->
                                                                                     DolanVarianceKind dvb -> Type
newtype SubtypeConversion ground dva gta dvb gtb =
    MkSubtypeConversion (forall solver pola a.
                             (WrappedApplicative solver, WAInnerM solver ~ DolanTypeCheckM ground, Is PolarityType pola) =>
                                     SubtypeContext (DolanType ground) (DolanShim ground) solver -> DolanArguments dva (DolanType ground) gta pola a -> DolanTypeCheckM ground (SubtypeArguments ground solver pola dvb gtb a))

generateVarType ::
       forall (ground :: GroundTypeKind) polarity. Monad (DolanM ground)
    => DolanTypeCheckM ground (AnyInKind (DolanType ground polarity))
generateVarType = do
    n <- renamerGenerate FreeName []
    MkAnyVar v <- return $ newUVarAny n
    return $ MkAnyInKind $ singleDolanType $ VarDolanSingularType v

saturateSingleArgument ::
       forall (ground :: GroundTypeKind) polarity sv. Monad (DolanM ground)
    => VarianceType sv
    -> DolanTypeCheckM ground (AnyInKind (SingleArgument sv (DolanType ground) polarity))
saturateSingleArgument CovarianceType = generateVarType
saturateSingleArgument ContravarianceType = generateVarType
saturateSingleArgument RangevarianceType = do
    MkAnyInKind ta <- generateVarType
    MkAnyInKind tb <- generateVarType
    return $ MkAnyInKind $ MkRangeType ta tb

saturateDolanArguments ::
       forall (ground :: GroundTypeKind) polarity dv gt. Monad (DolanM ground)
    => DolanVarianceType dv
    -> DolanTypeCheckM ground (AnyW (DolanArguments dv (DolanType ground) gt polarity))
saturateDolanArguments NilListType = return $ MkAnyW NilDolanArguments
saturateDolanArguments (ConsListType t1 tr) =
    saturateSingleArgument @ground @polarity t1 >>= \(MkAnyInKind arg) ->
        saturateDolanArguments tr >>= \(MkAnyW args) -> return $ MkAnyW $ ConsDolanArguments arg args

saturateGroundType ::
       forall (ground :: GroundTypeKind) polarity dv gt. IsDolanGroundType ground
    => ground dv gt
    -> DolanTypeCheckM ground (AnyW (DolanArguments dv (DolanType ground) gt polarity))
saturateGroundType gt = saturateDolanArguments $ groundTypeVarianceType gt

nilSubtypeConversion ::
       forall (ground :: GroundTypeKind) (a :: Type) (b :: Type).
       DolanShim ground a b
    -> SubtypeConversion ground '[] a '[] b
nilSubtypeConversion conv =
    MkSubtypeConversion $ \_ NilDolanArguments -> return $ MkSubtypeArguments NilDolanArguments $ pure $ conv

idSubtypeConversion ::
       forall (ground :: GroundTypeKind) dv gt. IsDolanGroundType ground
    => SubtypeConversion ground dv gt dv gt
idSubtypeConversion = MkSubtypeConversion $ \_ args -> return $ MkSubtypeArguments args $ pure id

composeSubtypeConversion ::
       forall (ground :: GroundTypeKind) dva gta dvb gtb dvc gtc. IsDolanGroundType ground
    => SubtypeConversion ground dvb gtb dvc gtc
    -> SubtypeConversion ground dva gta dvb gtb
    -> SubtypeConversion ground dva gta dvc gtc
composeSubtypeConversion (MkSubtypeConversion bc) (MkSubtypeConversion ab) =
    MkSubtypeConversion $ \sc argsa -> do
        MkSubtypeArguments argsb sconvab <- ab sc argsa
        MkSubtypeArguments argsc sconvbc <- bc sc argsb
        return $ MkSubtypeArguments argsc $ (<.>) <$> sconvbc <*> sconvab

type SubypeConversionEntry :: GroundTypeKind -> Type
data SubypeConversionEntry ground =
    forall dvb gtb. MkSubypeConversionEntry (ground dvb gtb)
                                            (forall dva gta.
                                                     ground dva gta -> Maybe (SubtypeConversion ground dva gta dvb gtb))

simpleSubtypeConversionEntry ::
       forall (ground :: GroundTypeKind) dva gta dvb gtb. IsDolanGroundType ground
    => ground dva gta
    -> ground dvb gtb
    -> SubtypeConversion ground dva gta dvb gtb
    -> SubypeConversionEntry ground
simpleSubtypeConversionEntry ta tb conv =
    MkSubypeConversionEntry tb $ \ta' -> do
        (Refl, HRefl) <- groundTypeTestEquality ta ta'
        return conv

type SubtypeConversionWit :: GroundTypeKind -> forall (dv :: DolanVariance) -> DolanVarianceKind dv -> Type
data SubtypeConversionWit ground dva gta =
    forall dvb gtb. MkSubtypeConversionWit (ground dvb gtb)
                                           (SubtypeConversion ground dva gta dvb gtb)

instance forall (ground :: GroundTypeKind) dva gta. IsDolanGroundType ground => Eq (SubtypeConversionWit ground dva gta) where
    (MkSubtypeConversionWit tp _) == (MkSubtypeConversionWit tq _) = isJust $ groundTypeTestEquality tp tq

matchSupertype ::
       forall (ground :: GroundTypeKind) dva gta. IsDolanGroundType ground
    => SubypeConversionEntry ground
    -> SubtypeConversionWit ground dva gta
    -> Maybe (SubtypeConversionWit ground dva gta)
matchSupertype (MkSubypeConversionEntry tb f) (MkSubtypeConversionWit ta conv) = do
    convE <- f ta
    return $ MkSubtypeConversionWit tb $ composeSubtypeConversion convE conv

getImmediateSupertypes ::
       forall (ground :: GroundTypeKind) dva gta. IsDolanGroundType ground
    => [SubypeConversionEntry ground]
    -> SubtypeConversionWit ground dva gta
    -> [SubtypeConversionWit ground dva gta]
getImmediateSupertypes entries t = mapMaybe (\entry -> matchSupertype entry t) entries

expandSupertypes ::
       forall (ground :: GroundTypeKind) dva gta. IsDolanGroundType ground
    => [SubypeConversionEntry ground]
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
    => [SubypeConversionEntry ground]
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
    => [SubypeConversionEntry ground]
    -> ground dva gta
    -> ground dvb gtb
    -> Maybe (SubtypeConversion ground dva gta dvb gtb)
getSubtypeShim entries ta tb = isSupertype entries [MkSubtypeConversionWit ta idSubtypeConversion] tb

type IsDolanSubtypeEntriesGroundType :: GroundTypeKind -> Constraint
class IsDolanSubtypeGroundType ground => IsDolanSubtypeEntriesGroundType ground where
    subtypeConversionEntries :: DolanM ground [SubypeConversionEntry ground]
    subtypeConversionMatchType ::
           forall (dva :: DolanVariance) (ta :: DolanVarianceKind dva) (dvb :: DolanVariance) (tb :: DolanVarianceKind dvb).
           ground dva ta
        -> ground dvb tb
        -> Maybe (SubtypeConversion ground dva ta dvb tb)
    subtypeConversionMatchType gta gtb = do
        (Refl, HRefl) <- groundTypeTestEquality gta gtb
        return idSubtypeConversion
    throwGroundTypeConvertError :: ground dva gta -> ground dvb gtb -> DolanM ground a
