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
    , ConvertSubtype
    , getSubtypeConversion
    , subtypeConversion
    , nilSubtypeConversion
    , composeSubtypeConversion
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
    subtypeGroundedTypes ::
           forall solver pola polb a b.
           ( WrappedApplicative solver
           , WAInnerM solver ~ DolanTypeCheckM ground
           , Is PolarityType pola
           , Is PolarityType polb
           )
        => DolanSubtypeContext ground solver
        -> DolanGroundedType ground pola a
        -> DolanGroundedType ground polb b
        -> solver (DolanShim ground a b)
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

type ConvertSubtype :: GroundTypeKind -> forall (dva :: DolanVariance) ->
                                                 DolanVarianceKind dva -> forall (dvb :: DolanVariance) ->
                                                                                  DolanVarianceKind dvb -> Type
type ConvertSubtype ground dva gta dvb gtb
     = forall solver pola a.
           (WrappedApplicative solver, WAInnerM solver ~ DolanTypeCheckM ground, Is PolarityType pola) =>
                   DolanSubtypeContext ground solver -> DolanArguments dva (DolanType ground) gta pola a -> DolanTypeCheckM ground (SubtypeArguments ground solver dvb gtb a)

type SubtypeConversion :: GroundTypeKind -> forall (dva :: DolanVariance) ->
                                                    DolanVarianceKind dva -> forall (dvb :: DolanVariance) ->
                                                                                     DolanVarianceKind dvb -> Type
data SubtypeConversion ground dva gta dvb gtb where
    GeneralSubtypeConversion
        :: forall (ground :: GroundTypeKind) (dva :: DolanVariance) (gta :: DolanVarianceKind dva) (dvb :: DolanVariance) (gtb :: DolanVarianceKind dvb).
           ConvertSubtype ground dva gta dvb gtb
        -> SubtypeConversion ground dva gta dvb gtb
    IdentitySubtypeConversion
        :: forall (ground :: GroundTypeKind) (dv :: DolanVariance) (gt :: DolanVarianceKind dv).
           SubtypeConversion ground dv gt dv gt
    CoerceSubtypeConversion
        :: forall (ground :: GroundTypeKind) (gta :: Type) (gtb :: Type). Coercible gta gtb
        => SubtypeConversion ground '[] gta '[] gtb

getSubtypeConversion ::
       forall (ground :: GroundTypeKind) dva gta dvb gtb. IsDolanSubtypeGroundType ground
    => SubtypeConversion ground dva gta dvb gtb
    -> ConvertSubtype ground dva gta dvb gtb
getSubtypeConversion (GeneralSubtypeConversion sc) = sc
getSubtypeConversion IdentitySubtypeConversion =
    \_ (args :: DolanArguments _ _ _ polarity _) ->
        return $ MkSubtypeArguments @ground @_ @_ @_ @_ @polarity args $ pure id
getSubtypeConversion CoerceSubtypeConversion =
    \_ (NilCCRArguments :: DolanArguments _ _ _ polarity _) ->
        return $
        MkSubtypeArguments @ground @_ @_ @_ @_ @polarity NilCCRArguments $ pure $ coercionToShim "subtype" MkCoercion

subtypeConversion ::
       forall (ground :: GroundTypeKind) dva gta a dvb gtb b. IsDolanSubtypeGroundType ground
    => ground dva gta
    -> DolanArgumentsShimWit (DolanPolyShim ground) dva (DolanType ground) gta 'Negative a
    -> ground dvb gtb
    -> DolanArgumentsShimWit (DolanPolyShim ground) dvb (DolanType ground) gtb 'Positive b
    -> TSOpenExpression (DolanTypeSystem ground) (DolanShim ground a b)
    -> SubtypeConversion ground dva gta dvb gtb
subtypeConversion gta (MkShimWit rawargsa (MkPolarMap conva)) gtb (MkShimWit rawargsb (MkPolarMap convb)) convexpr =
    GeneralSubtypeConversion $ \sc argsa -> do
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
       forall (ground :: GroundTypeKind) a b. DolanShim ground a b -> SubtypeConversion ground '[] a '[] b
nilSubtypeConversion conv =
    GeneralSubtypeConversion $ \_ (NilCCRArguments :: DolanArguments _ _ _ polarity _) ->
        return $ MkSubtypeArguments (NilCCRArguments :: DolanArguments _ _ _ polarity _) $ pure conv

composeSubtypeConversion ::
       forall (ground :: GroundTypeKind) dva gta dvb gtb dvc gtc. IsDolanSubtypeGroundType ground
    => SubtypeConversion ground dvb gtb dvc gtc
    -> SubtypeConversion ground dva gta dvb gtb
    -> SubtypeConversion ground dva gta dvc gtc
composeSubtypeConversion IdentitySubtypeConversion ab = ab
composeSubtypeConversion bc IdentitySubtypeConversion = bc
composeSubtypeConversion CoerceSubtypeConversion CoerceSubtypeConversion = CoerceSubtypeConversion
composeSubtypeConversion bc ab =
    GeneralSubtypeConversion $ \sc argsa -> do
        MkSubtypeArguments argsb sconvab <- getSubtypeConversion ab sc argsa
        MkSubtypeArguments argsc sconvbc <- getSubtypeConversion bc sc argsb
        return $ MkSubtypeArguments argsc $ (.) <$> sconvbc <*> sconvab
