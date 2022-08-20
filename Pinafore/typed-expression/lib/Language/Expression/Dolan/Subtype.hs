{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Dolan.Subtype
    ( SubtypeContext(..)
    , DolanSubtypeContext
    , subtypeDolanArguments
    , DolanMappable
    , IsDolanSubtypeGroundType(..)
    , DolanShim
    , SubtypeArguments(..)
    , SubtypeConversion(..)
    , subtypeConversion
    , simpleSubtypeConversion
    , nilSubtypeConversion
    , idSubtypeConversion
    , composeSubtypeConversion
    , SubtypeConversionEntry(..)
    , subtypeConversionEntry
    , simpleSubtypeConversionEntry
    , IsDolanSubtypeEntriesGroundType(..)
    ) where

import Control.Applicative.Wrapped
import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Argument
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.Rename
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Variance
import Shapes

type SubtypeContext :: Type -> (Polarity -> Type -> Type) -> ShimKind Type -> (Type -> Type) -> Type
data SubtypeContext varid w shim solver = MkSubtypeContext
    { subtypeConvert :: forall ta tb pola polb.
                            (Is PolarityType pola, Is PolarityType polb) =>
                                    w pola ta -> w polb tb -> solver (shim ta tb)
    , subtypeLiftExpression :: forall a. NamedExpression varid (PolarShimWit shim (w 'Negative) 'Negative) a -> solver a
    }

subtypeVariance ::
       forall varid (w :: Polarity -> Type -> Type) (shim :: ShimKind Type) solver pola polb sv a b.
       (Applicative solver, Is PolarityType pola, Is PolarityType polb)
    => SubtypeContext varid w shim solver
    -> CCRPolarArgument w pola sv a
    -> CCRPolarArgument w polb sv b
    -> solver (CCRVarianceCategory shim sv a b)
subtypeVariance sc (CoCCRPolarArgument ta) (CoCCRPolarArgument tb) = subtypeConvert sc ta tb
subtypeVariance sc (ContraCCRPolarArgument ta) (ContraCCRPolarArgument tb) =
    invertPolarity @pola $
    invertPolarity @polb $ do
        ba <- subtypeConvert sc tb ta
        return $ MkCatDual ba
subtypeVariance sc (RangeCCRPolarArgument tpa tqa) (RangeCCRPolarArgument tpb tqb) =
    invertPolarity @pola $
    invertPolarity @polb $ do
        pba <- subtypeConvert sc tpb tpa
        qab <- subtypeConvert sc tqa tqb
        return $ MkCatRange pba qab

subtypeArguments ::
       forall varid (w :: Polarity -> Type -> Type) (pshim :: PolyShimKind) solver pola polb dv (gta :: DolanVarianceKind dv) (gtb :: DolanVarianceKind dv) ta tb.
       ( DolanVarianceCategory pshim
       , Applicative solver
       , Is PolarityType pola
       , Is PolarityType polb
       , TestEquality (w 'Positive)
       , TestEquality (w 'Negative)
       )
    => SubtypeContext varid w (pshim Type) solver
    -> DolanVarianceType dv
    -> DolanVarianceMap dv gta
    -> DolanVarianceMap dv gtb
    -> DolanArguments dv w gta pola ta
    -> DolanArguments dv w gtb polb tb
    -> solver (pshim (DolanVarianceKind dv) gta gtb -> pshim Type ta tb)
subtypeArguments _ NilListType NilDolanVarianceMap NilDolanVarianceMap NilCCRArguments NilCCRArguments = pure id
subtypeArguments sc (ConsListType svt dvt) (ConsDolanVarianceMap ccrva dvma) (ConsDolanVarianceMap ccrvb dvmb) (ConsCCRArguments sta dta) (ConsCCRArguments stb dtb) =
    case ccrVarianceCoercibleKind svt of
        Dict ->
            case dolanVarianceCategory @pshim dvt of
                Dict -> do
                    sfunc <- subtypeVariance @_ @_ @_ @_ @pola @polb sc sta stb
                    f <- subtypeArguments sc dvt dvma dvmb dta dtb
                    pure $ \conv -> f (applyPolyShim svt ccrva ccrvb conv sfunc)

subtypeDolanArguments ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) solver pola polb dv gt argsa argsb.
       ( IsDolanGroundType ground
       , DolanVarianceCategory pshim
       , Applicative solver
       , Is PolarityType pola
       , Is PolarityType polb
       )
    => SubtypeContext (DolanVarID ground) (DolanType ground) (pshim Type) solver
    -> ground dv gt
    -> DolanArguments dv (DolanType ground) gt pola argsa
    -> DolanArguments dv (DolanType ground) gt polb argsb
    -> solver (pshim Type argsa argsb)
subtypeDolanArguments sc gt argsa argsb = let
    dvt = groundTypeVarianceType gt
    dvm = groundTypeVarianceMap gt
    in case dolanVarianceCategory @pshim dvt of
           Dict -> fmap (\f -> f id) $ subtypeArguments sc dvt dvm dvm argsa argsb

type DolanMappable :: GroundTypeKind -> Type -> Constraint
type DolanMappable ground = TSMappable (DolanTypeSystem ground)

type DolanSubtypeContext :: GroundTypeKind -> (Type -> Type) -> Type
type DolanSubtypeContext ground = SubtypeContext (DolanVarID ground) (DolanType ground) (DolanShim ground)

type IsDolanSubtypeGroundType :: GroundTypeKind -> Constraint
class IsDolanGroundType ground => IsDolanSubtypeGroundType ground where
    subtypeGroundTypes ::
           forall solver pola polb dva gta a dvb gtb b.
           ( WrappedApplicative solver
           , WAInnerM solver ~ DolanTypeCheckM ground
           , Is PolarityType pola
           , Is PolarityType polb
           )
        => DolanSubtypeContext ground solver
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
                    DolanSubtypeContext ground solver -> ground dva gta -> DolanArguments dva (DolanType ground) gta pola a -> ground dvb gtb -> DolanArguments dvb (DolanType ground) gtb polb b -> solver (DolanShim ground a b)
    subtypeGroundTypes sc ta argsa tb argsb = let
        margswit :: DolanTypeCheckM ground (SubtypeArguments ground solver dvb gtb a)
        margswit = do
            entries <- lift subtypeConversionEntries
            case getSubtypeShim entries ta tb of
                Just (MkSubtypeConversion sconv) -> sconv sc argsa
                Nothing -> lift $ throwGroundTypeConvertError ta tb
        in wbind margswit $ \(MkSubtypeArguments argsb' sargsconv) ->
               (\p q -> p . q) <$> subtypeDolanArguments sc tb argsb' argsb <*> sargsconv
    tackOnTypeConvertError ::
           (Is PolarityType pola, Is PolarityType polb)
        => DolanType ground pola ta
        -> DolanType ground polb tb
        -> DolanM ground a
        -> DolanM ground a
    throwTypeNotInvertible :: Is PolarityType polarity => DolanType ground polarity t -> DolanM ground a

type SubtypeArguments :: GroundTypeKind -> (Type -> Type) -> forall (dv :: DolanVariance) ->
                                                                     DolanVarianceKind dv -> Type -> Type
data SubtypeArguments ground solver dv gt a =
    forall polarity b. Is PolarityType polarity =>
                           MkSubtypeArguments (DolanArguments dv (DolanType ground) gt polarity b)
                                              (solver (DolanShim ground a b))

type SubtypeConversion :: GroundTypeKind -> forall (dva :: DolanVariance) ->
                                                    DolanVarianceKind dva -> forall (dvb :: DolanVariance) ->
                                                                                     DolanVarianceKind dvb -> Type
newtype SubtypeConversion ground dva gta dvb gtb =
    MkSubtypeConversion (forall solver pola a.
                             (WrappedApplicative solver, WAInnerM solver ~ DolanTypeCheckM ground, Is PolarityType pola) =>
                                     DolanSubtypeContext ground solver -> DolanArguments dva (DolanType ground) gta pola a -> DolanTypeCheckM ground (SubtypeArguments ground solver dvb gtb a))

simpleSubtypeConversion ::
       forall (ground :: GroundTypeKind) a b. DolanShim ground a b -> SubtypeConversion ground '[] a '[] b
simpleSubtypeConversion conv =
    MkSubtypeConversion $ \_ (NilCCRArguments :: DolanArguments _ _ _ polarity _) ->
        return $ MkSubtypeArguments (NilCCRArguments :: DolanArguments _ _ _ polarity _) $ pure conv

subtypeConversion ::
       forall (ground :: GroundTypeKind) dva gta a dvb gtb b. IsDolanSubtypeGroundType ground
    => ground dva gta
    -> DolanArgumentsShimWit (DolanPolyShim ground) dva (DolanType ground) gta 'Negative a
    -> ground dvb gtb
    -> DolanArgumentsShimWit (DolanPolyShim ground) dvb (DolanType ground) gtb 'Positive b
    -> TSOpenExpression (DolanTypeSystem ground) (DolanShim ground a b)
    -> SubtypeConversion ground dva gta dvb gtb
subtypeConversion gta (MkShimWit rawargsa (MkPolarMap conva)) gtb (MkShimWit rawargsb (MkPolarMap convb)) convexpr =
    MkSubtypeConversion $ \sc argsa -> do
        (sargsa, sargsb) <-
            namespace @(DolanTypeSystem ground) FreeName $ do
                sargsa <- dolanNamespaceRenameArguments (groundTypeVarianceMap gta) rawargsa
                sargsb <- dolanNamespaceRenameArguments (groundTypeVarianceMap gtb) rawargsb
                return (sargsa, sargsb)
        return $
            MkSubtypeArguments sargsb $
            liftA2
                (\conv uconv -> convb . conv . conva . uconv)
                (subtypeLiftExpression sc convexpr)
                (subtypeDolanArguments sc gta argsa sargsa)

nilSubtypeConversion ::
       forall (ground :: GroundTypeKind) (a :: Type) (b :: Type).
       DolanShim ground a b
    -> SubtypeConversion ground '[] a '[] b
nilSubtypeConversion = simpleSubtypeConversion

idSubtypeConversion ::
       forall (ground :: GroundTypeKind) dv gt. IsDolanSubtypeGroundType ground
    => SubtypeConversion ground dv gt dv gt
idSubtypeConversion = MkSubtypeConversion $ \_ args -> return $ MkSubtypeArguments args $ pure id

composeSubtypeConversion ::
       forall (ground :: GroundTypeKind) dva gta dvb gtb dvc gtc. IsDolanSubtypeGroundType ground
    => SubtypeConversion ground dvb gtb dvc gtc
    -> SubtypeConversion ground dva gta dvb gtb
    -> SubtypeConversion ground dva gta dvc gtc
composeSubtypeConversion (MkSubtypeConversion bc) (MkSubtypeConversion ab) =
    MkSubtypeConversion $ \sc argsa -> do
        MkSubtypeArguments argsb sconvab <- ab sc argsa
        MkSubtypeArguments argsc sconvbc <- bc sc argsb
        return $ MkSubtypeArguments argsc $ (.) <$> sconvbc <*> sconvab

type SubtypeConversionEntry :: GroundTypeKind -> Type
data SubtypeConversionEntry ground =
    forall dvb gtb. MkSubtypeConversionEntry (ground dvb gtb)
                                             (forall dva gta.
                                                      ground dva gta -> Maybe (SubtypeConversion ground dva gta dvb gtb))

subtypeConversionEntry ::
       forall (ground :: GroundTypeKind) dva gta a dvb gtb b. IsDolanSubtypeGroundType ground
    => ground dva gta
    -> DolanArgumentsShimWit (DolanPolyShim ground) dva (DolanType ground) gta 'Negative a
    -> ground dvb gtb
    -> DolanArgumentsShimWit (DolanPolyShim ground) dvb (DolanType ground) gtb 'Positive b
    -> TSOpenExpression (DolanTypeSystem ground) (DolanShim ground a b)
    -> SubtypeConversionEntry ground
subtypeConversionEntry gta argsa gtb argsb conv =
    simpleSubtypeConversionEntry gta gtb $ subtypeConversion gta argsa gtb argsb conv

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
