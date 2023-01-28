{-# LANGUAGE ApplicativeDo #-}

module Language.Expression.Dolan.Subtype
    ( SubtypeContext(..)
    , DolanSubtypeContext
    , subtypeDolanArguments
    , DolanMappable
    , IsDolanSubtypeGroundType(..)
    , DolanShim
    , SubtypeConversion
    , identitySubtypeConversion
    , coerceSubtypeConversion
    , runSubtypeConversion
    , isNeutralSubtypeConversion
    , subtypeConversion
    , nilSubtypeConversion
    , composeSubtypeConversion
    , subtypeConversionAsGeneralAs
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
    -> DolanVarianceMap dv gt
    -> DolanArguments dv (DolanType ground) gt pola argsa
    -> DolanArguments dv (DolanType ground) gt polb argsb
    -> solver (pshim Type argsa argsb)
subtypeDolanArguments sc dvm argsa argsb = let
    dvt = ccrArgumentsType argsa
    in case dolanVarianceCategory @pshim dvt of
           Dict -> fmap (\f -> f id) $ subtypeArguments sc dvt dvm dvm argsa argsb

type DolanMappable :: GroundTypeKind -> Type -> Constraint
type DolanMappable ground = TSMappable (DolanTypeSystem ground)

type DolanSubtypeContext :: GroundTypeKind -> (Type -> Type) -> Type
type DolanSubtypeContext ground = SubtypeContext (DolanVarID ground) (DolanType ground) (DolanShim ground)

type IsDolanSubtypeGroundType :: GroundTypeKind -> Constraint
class (IsDolanGroundType ground, IsPatternWitness (DolanShimWit ground 'Positive) (DolanPatternWitness ground)) =>
          IsDolanSubtypeGroundType ground where
    type DolanPatternWitness ground :: Type -> Type
    dolanMakePatternWitness :: DolanVarID ground -> DolanShimWit ground 'Positive --> DolanPatternWitness ground
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

type SubtypeArguments :: GroundTypeKind -> (Type -> Type) -> forall (dva :: DolanVariance) ->
                                                                     DolanVarianceKind dva -> forall (dvb :: DolanVariance) ->
                                                                                                      DolanVarianceKind dvb -> Type
data SubtypeArguments ground solver dva gta dvb gtb =
    forall a b. MkSubtypeArguments (DolanVarianceMap dva gta)
                                   (DolanArguments dva (DolanType ground) gta 'Negative a)
                                   (DolanVarianceMap dvb gtb)
                                   (DolanArguments dvb (DolanType ground) gtb 'Positive b)
                                   (solver (DolanShim ground a b))

type ConvertSubtype :: GroundTypeKind -> forall (dva :: DolanVariance) ->
                                                 DolanVarianceKind dva -> forall (dvb :: DolanVariance) ->
                                                                                  DolanVarianceKind dvb -> Type
type ConvertSubtype ground dva gta dvb gtb
     = forall solver.
           (WrappedApplicative solver, WAInnerM solver ~ DolanTypeCheckM ground) =>
                   DolanSubtypeContext ground solver -> DolanTypeCheckM ground (SubtypeArguments ground solver dva gta dvb gtb)

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

identitySubtypeConversion ::
       forall (ground :: GroundTypeKind) (dv :: DolanVariance) (gt :: DolanVarianceKind dv).
       SubtypeConversion ground dv gt dv gt
identitySubtypeConversion = IdentitySubtypeConversion

coerceSubtypeConversion ::
       forall (ground :: GroundTypeKind) (gta :: Type) (gtb :: Type). Coercible gta gtb
    => SubtypeConversion ground '[] gta '[] gtb
coerceSubtypeConversion = CoerceSubtypeConversion

isNeutralSubtypeConversion ::
       forall (ground :: GroundTypeKind) (dva :: DolanVariance) (gta :: DolanVarianceKind dva) (dvb :: DolanVariance) (gtb :: DolanVarianceKind dvb).
       SubtypeConversion ground dva gta dvb gtb
    -> Bool
isNeutralSubtypeConversion IdentitySubtypeConversion = True
isNeutralSubtypeConversion CoerceSubtypeConversion = True
isNeutralSubtypeConversion (GeneralSubtypeConversion _) = False

runSubtypeConversion ::
       forall (ground :: GroundTypeKind) solver pola dva gta a polb dvb gtb b.
       ( IsDolanSubtypeGroundType ground
       , WrappedApplicative solver
       , WAInnerM solver ~ DolanTypeCheckM ground
       , Is PolarityType pola
       , Is PolarityType polb
       )
    => DolanSubtypeContext ground solver
    -> SubtypeConversion ground dva gta dvb gtb
    -> DolanVarianceMap dva gta
    -> DolanArguments dva (DolanType ground) gta pola a
    -> DolanArguments dvb (DolanType ground) gtb polb b
    -> DolanTypeCheckM ground (solver (DolanPolyShim ground Type a b))
runSubtypeConversion sc IdentitySubtypeConversion vma argsa argsb = do return $ subtypeDolanArguments sc vma argsa argsb
runSubtypeConversion _sc CoerceSubtypeConversion NilDolanVarianceMap NilCCRArguments NilCCRArguments = do
    return $ pure $ coercionToShim "subtype" MkCoercion
runSubtypeConversion sc (GeneralSubtypeConversion sconv) _ argsa argsb = do
    MkSubtypeArguments vma argsa' vmb argsb' svconv <- sconv sc
    return $
        (\conva convb conv -> convb . conv . conva) <$> subtypeDolanArguments sc vma argsa argsa' <*>
        subtypeDolanArguments sc vmb argsb' argsb <*>
        svconv

subtypeConversion ::
       forall (ground :: GroundTypeKind) dva gta a dvb gtb b. IsDolanSubtypeGroundType ground
    => ground dva gta
    -> DolanArgumentsShimWit (DolanPolyShim ground) dva (DolanType ground) gta 'Negative a
    -> ground dvb gtb
    -> DolanArgumentsShimWit (DolanPolyShim ground) dvb (DolanType ground) gtb 'Positive b
    -> TSOpenExpression (DolanTypeSystem ground) (DolanShim ground a b)
    -> SubtypeConversion ground dva gta dvb gtb
subtypeConversion gta (MkShimWit rawargsa (MkPolarMap conva)) gtb (MkShimWit rawargsb (MkPolarMap convb)) convexpr =
    GeneralSubtypeConversion $ \sc -> do
        let
            vma = groundTypeVarianceMap gta
            vmb = groundTypeVarianceMap gtb
        (argsa, argsb) <-
            namespace @(DolanTypeSystem ground) FreeName $
            unEndoM (dolanNamespaceRenameArguments <***> dolanNamespaceRenameArguments) (rawargsa, rawargsb)
        return $
            MkSubtypeArguments vma argsa vmb argsb $
            fmap (\conv -> convb . conv . conva) (subtypeLiftExpression sc convexpr)

nilSubtypeConversion ::
       forall (ground :: GroundTypeKind) a b. DolanShim ground a b -> SubtypeConversion ground '[] a '[] b
nilSubtypeConversion conv =
    GeneralSubtypeConversion $ \_ ->
        return $ MkSubtypeArguments NilDolanVarianceMap NilCCRArguments NilDolanVarianceMap NilCCRArguments $ pure conv

composeSubtypeConversion ::
       forall (ground :: GroundTypeKind) dva gta dvb gtb dvc gtc. IsDolanSubtypeGroundType ground
    => SubtypeConversion ground dvb gtb dvc gtc
    -> SubtypeConversion ground dva gta dvb gtb
    -> SubtypeConversion ground dva gta dvc gtc
composeSubtypeConversion IdentitySubtypeConversion ab = ab
composeSubtypeConversion bc IdentitySubtypeConversion = bc
composeSubtypeConversion CoerceSubtypeConversion CoerceSubtypeConversion = CoerceSubtypeConversion
composeSubtypeConversion (GeneralSubtypeConversion bc) (GeneralSubtypeConversion ab) =
    GeneralSubtypeConversion $ \sc -> do
        MkSubtypeArguments _ argsb1 vmc argsc sconvbc <- bc sc
        MkSubtypeArguments vma argsa vmb2 argsb2 sconvab <- ab sc
        return $
            MkSubtypeArguments vma argsa vmc argsc $
            (\conv convbc convab -> convbc . conv . convab) <$> subtypeDolanArguments sc vmb2 argsb2 argsb1 <*> sconvbc <*>
            sconvab
composeSubtypeConversion (GeneralSubtypeConversion bc) CoerceSubtypeConversion =
    GeneralSubtypeConversion $ \sc ->
        fmap
            (\(MkSubtypeArguments NilDolanVarianceMap NilCCRArguments vmc argsc sconvbc) ->
                 MkSubtypeArguments NilDolanVarianceMap NilCCRArguments vmc argsc $
                 fmap (\conv -> conv . coercionToShim "subtype" MkCoercion) sconvbc) $
        bc sc
composeSubtypeConversion CoerceSubtypeConversion (GeneralSubtypeConversion ab) =
    GeneralSubtypeConversion $ \sc ->
        fmap
            (\(MkSubtypeArguments vma argsa NilDolanVarianceMap NilCCRArguments sconvab) ->
                 MkSubtypeArguments vma argsa NilDolanVarianceMap NilCCRArguments $
                 fmap (\conv -> coercionToShim "subtype" MkCoercion . conv) sconvab) $
        ab sc

subtypeConversionAsGeneralAs ::
       forall (ground :: GroundTypeKind) solver (dva :: DolanVariance) (gta :: DolanVarianceKind dva) (dvb :: DolanVariance) (gtb :: DolanVarianceKind dvb).
       (IsDolanSubtypeGroundType ground, WrappedApplicative solver, WAInnerM solver ~ DolanTypeCheckM ground)
    => (forall a. solver a -> WAInnerM solver Bool)
    -> DolanSubtypeContext ground solver
    -> SubtypeConversion ground dva gta dvb gtb
    -> SubtypeConversion ground dva gta dvb gtb
    -> DolanM ground Bool
subtypeConversionAsGeneralAs _ _ IdentitySubtypeConversion _ = return True
subtypeConversionAsGeneralAs _ _ _ IdentitySubtypeConversion = return False
subtypeConversionAsGeneralAs _ _ CoerceSubtypeConversion _ = return True
subtypeConversionAsGeneralAs _ _ _ CoerceSubtypeConversion = return False
subtypeConversionAsGeneralAs runSolver sc (GeneralSubtypeConversion cs1) (GeneralSubtypeConversion cs2) =
    runVarRenamerT [] [] $ do
    -- cs1 is as general as cs2 if cs1 can subsume to cs2
        MkSubtypeArguments _ args1a _ args1b _ <- cs1 sc
        MkSubtypeArguments vma rawargs2a vmb rawargs2b _ <- cs2 sc
        (args2a, args2b) <-
            namespace @(DolanTypeSystem ground) RigidName $
            unEndoM (dolanNamespaceRenameArguments <***> dolanNamespaceRenameArguments) (rawargs2a, rawargs2b)
        let
            sconva = subtypeDolanArguments sc vma args2a args1a
            sconvb = subtypeDolanArguments sc vmb args1b args2b
            sthing = liftA2 (,) sconva sconvb
        runSolver sthing
