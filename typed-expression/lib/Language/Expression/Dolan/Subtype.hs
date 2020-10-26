{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Dolan.Subtype
    ( SubtypeContext(..)
    , subtypeDolanArguments
    , IsDolanSubtypeGroundType(..)
    , SubypeConversionEntry(..)
    , simpleSubtypeConversionEntry
    , IsDolanSubtypeEntriesGroundType(..)
    ) where

import Control.Applicative.Wrapped
import Data.Shim
import Language.Expression.Dolan.Arguments
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Language.Expression.Dolan.Variance
import Shapes

type SubtypeContext :: (Polarity -> Type -> Type) -> ShimKind Type -> (Type -> Type) -> Polarity -> Polarity -> Type
data SubtypeContext w shim solver pola polb = MkSubtypeContext
    { subtypeConvert :: forall ta tb. w pola ta -> w polb tb -> solver (shim ta tb)
    , subtypeInverted :: SubtypeContext w shim solver (InvertPolarity polb) (InvertPolarity pola)
    }

subtypeVariance ::
       forall (w :: Polarity -> Type -> Type) (shim :: ShimKind Type) solver pola polb sv a b.
       (Applicative solver, Is PolarityType pola, Is PolarityType polb)
    => SubtypeContext w shim solver pola polb
    -> VarianceType sv
    -> SingleArgument sv w pola a
    -> SingleArgument sv w polb b
    -> solver (VarianceCategory shim sv a b)
subtypeVariance sc CovarianceType ta tb = subtypeConvert sc ta tb
subtypeVariance sc ContravarianceType ta tb = do
    ba <- subtypeConvert (subtypeInverted sc) tb ta
    return $ MkCatDual ba
subtypeVariance sc RangevarianceType (MkRangeType tpa tqa) (MkRangeType tpb tqb) = do
    pba <- subtypeConvert (subtypeInverted sc) tpb tpa
    qab <- subtypeConvert sc tqa tqb
    return $ MkCatRange pba qab

subtypeArguments ::
       forall (w :: Polarity -> Type -> Type) (pshim :: PolyShimKind) solver pola polb dv (gta :: DolanVarianceKind dv) (gtb :: DolanVarianceKind dv) ta tb.
       (DolanVarianceInCategory pshim, Applicative solver, Is PolarityType pola, Is PolarityType polb)
    => SubtypeContext w (pshim Type) solver pola polb
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
                                            sfunc <- subtypeVariance sc svt sta stb
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
    => SubtypeContext (DolanType ground) (pshim Type) solver pola polb
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

type IsDolanSubtypeGroundType :: GroundTypeKind -> Constraint
class IsDolanGroundType ground => IsDolanSubtypeGroundType ground where
    subtypeGroundTypes ::
           forall solver pola polb dva gta a dvb gtb b.
           (WrappedApplicative solver, WAWrapper solver ~ DolanM ground, Is PolarityType pola, Is PolarityType polb)
        => SubtypeContext (DolanType ground) (DolanPolyShim ground Type) solver pola polb
        -> ground dva gta
        -> DolanArguments dva (DolanType ground) gta pola a
        -> ground dvb gtb
        -> DolanArguments dvb (DolanType ground) gtb polb b
        -> solver (DolanPolyShim ground Type a b)
    default subtypeGroundTypes ::
        forall solver pola polb dva gta a dvb gtb b.
            ( IsDolanSubtypeEntriesGroundType ground
            , WrappedApplicative solver
            , WAWrapper solver ~ DolanM ground
            , Is PolarityType pola
            , Is PolarityType polb
            ) =>
                    SubtypeContext (DolanType ground) (DolanPolyShim ground Type) solver pola polb -> ground dva gta -> DolanArguments dva (DolanType ground) gta pola a -> ground dvb gtb -> DolanArguments dvb (DolanType ground) gtb polb b -> solver (DolanPolyShim ground Type a b)
    subtypeGroundTypes sc ta argsa tb argsb = let
        margswit ::
               DolanM ground (ShimWit (DolanPolyShim ground Type) (DolanArguments dvb (DolanType ground) gtb pola) 'Positive a)
        margswit = do
            entries <- subtypeConversionEntries
            let
                MkSubtypeMatch smatch = getSubtypeShim entries nullSubtypeMatch
                msconv :: Maybe (SubtypeConversion ground dva gta dvb gtb)
                msconv = smatch ta tb
            case msconv of
                Just (MkSubtypeConversion sconv) -> sconv sc argsa
                Nothing -> throwTypeConvertError ta tb
        in wbind margswit $ \(MkShimWit argsb' (MkPolarMap argsconv)) ->
               fmap (\conv -> conv <.> argsconv) $ subtypeDolanArguments sc tb argsb' argsb
    throwTypeConvertError :: ground dva gta -> ground dvb gtb -> DolanM ground a
    throwTypeConvertInverseError :: DolanType ground 'Negative p -> DolanType ground 'Positive q -> DolanM ground a
    throwTypeSubsumeError ::
           Is PolarityType polarity
        => DolanSingularType ground polarity tinf
        -> DolanType ground polarity tdecl
        -> DolanM ground a
    throwTypeNotInvertible :: Is PolarityType polarity => DolanType ground polarity t -> DolanM ground a

type SubtypeConversion :: GroundTypeKind -> forall (dva :: DolanVariance) ->
                                                    DolanVarianceKind dva -> forall (dvb :: DolanVariance) ->
                                                                                     DolanVarianceKind dvb -> Type
newtype SubtypeConversion ground dva gta dvb gtb =
    MkSubtypeConversion (forall solver polarity polarity' a.
                             ( WrappedApplicative solver
                             , WAWrapper solver ~ DolanM ground
                             , Is PolarityType polarity
                             , Is PolarityType polarity'
                             ) =>
                                     SubtypeContext (DolanType ground) (DolanPolyShim ground Type) solver polarity polarity' -> DolanArguments dva (DolanType ground) gta polarity a -> DolanM ground (ShimWit (DolanPolyShim ground Type) (DolanArguments dvb (DolanType ground) gtb polarity) 'Positive a))

idSubtypeConversion ::
       forall (ground :: GroundTypeKind) dv gt. IsDolanGroundType ground
    => SubtypeConversion ground dv gt dv gt
idSubtypeConversion = MkSubtypeConversion $ \_ args -> return $ mkShimWit args

-- subtypeDolanArguments sc t argsa argsb
composeSubtypeConversion ::
       forall (ground :: GroundTypeKind) dva gta dvb gtb dvc gtc. IsDolanGroundType ground
    => SubtypeConversion ground dvb gtb dvc gtc
    -> SubtypeConversion ground dva gta dvb gtb
    -> SubtypeConversion ground dva gta dvc gtc
composeSubtypeConversion (MkSubtypeConversion bc) (MkSubtypeConversion ab) =
    MkSubtypeConversion $ \sc argsa -> do
        argsb <- ab sc argsa
        chainShimWitM (bc sc) argsb

type SubypeConversionEntry :: GroundTypeKind -> Type
data SubypeConversionEntry ground =
    forall dvb gtb. MkSubypeConversionEntry (ground dvb gtb)
                                            (forall dva gta.
                                                     ground dva gta -> Maybe (SubtypeConversion ground dva gta dvb gtb))

type SubtypeMatch :: GroundTypeKind -> Type
newtype SubtypeMatch ground =
    MkSubtypeMatch (forall dva gta dvb gtb.
                            ground dva gta -> ground dvb gtb -> Maybe (SubtypeConversion ground dva gta dvb gtb))

nullSubtypeMatch :: forall (ground :: GroundTypeKind). SubtypeMatch ground
nullSubtypeMatch = MkSubtypeMatch $ \_ _ -> Nothing

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

eqShimWit ::
       forall (ground :: GroundTypeKind) dva gta. IsDolanGroundType ground
    => SubtypeMatch ground
    -> SubtypeConversionWit ground dva gta
    -> SubtypeConversionWit ground dva gta
    -> Bool
eqShimWit (MkSubtypeMatch smatch) (MkSubtypeConversionWit ta _) (MkSubtypeConversionWit tb _) =
    isJust $ do
        _ <- smatch ta tb
        _ <- smatch tb ta
        return ()

expandSupertypes ::
       forall (ground :: GroundTypeKind) dva gta. IsDolanGroundType ground
    => SubtypeMatch ground
    -> [SubypeConversionEntry ground]
    -> [SubtypeConversionWit ground dva gta]
    -> [SubtypeConversionWit ground dva gta]
expandSupertypes smatch entries tt = let
    ttt = fmap (getImmediateSupertypes entries) tt
    in nubBy (eqShimWit smatch) $ mconcat $ tt : ttt

contains ::
       forall (ground :: GroundTypeKind) dva gta dvb gtb. IsDolanGroundType ground
    => [SubtypeConversionWit ground dva gta]
    -> ground dvb gtb
    -> Maybe (SubtypeConversion ground dva gta dvb gtb)
contains [] _ = Nothing
contains (MkSubtypeConversionWit ta conv:_) tb
    | Just (Refl, HRefl) <- groundTypeTestEquality ta tb = Just conv
contains (_:aa) tb = contains aa tb

isSupertype ::
       forall (ground :: GroundTypeKind) dva gta dvb gtb. IsDolanGroundType ground
    => SubtypeMatch ground
    -> [SubypeConversionEntry ground]
    -> [SubtypeConversionWit ground dva gta]
    -> ground dvb gtb
    -> Maybe (SubtypeConversion ground dva gta dvb gtb)
isSupertype _ _entries aa a
    | Just conv <- contains aa a = Just conv
isSupertype smatch entries aa a = let
    aa' = expandSupertypes smatch entries aa
    in if length aa' > length aa
           then isSupertype smatch entries aa' a
           else Nothing

getSubtypeShim ::
       forall (ground :: GroundTypeKind). IsDolanGroundType ground
    => [SubypeConversionEntry ground]
    -> SubtypeMatch ground
    -> SubtypeMatch ground
getSubtypeShim entries smatch =
    MkSubtypeMatch $ \ta tb -> isSupertype smatch entries [MkSubtypeConversionWit ta idSubtypeConversion] tb

type IsDolanSubtypeEntriesGroundType :: GroundTypeKind -> Constraint
class IsDolanSubtypeGroundType ground => IsDolanSubtypeEntriesGroundType ground where
    subtypeConversionEntries :: DolanM ground [SubypeConversionEntry ground]
