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
    , subtypeConversion
    , nilSubtypeConversion
    , composeSubtypeConversion
    , subtypeConversionAsGeneralAs
    ) where

import Control.Applicative.Wrapped
import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan.Rename
import Language.Expression.Dolan.Type
import Language.Expression.Dolan.TypeSystem
import Shapes

type SubtypeContext :: Type -> (Polarity -> Type -> Type) -> ShimKind Type -> (Type -> Type) -> Type
data SubtypeContext varid w shim solver = MkSubtypeContext
    { subtypeConvert :: forall ta tb pola polb.
                            (Is PolarityType pola, Is PolarityType polb) =>
                                    w pola ta -> w polb tb -> solver (shim ta tb)
    , subtypeLiftExpression :: forall a. NamedExpression varid (PShimWit shim w 'Negative) a -> solver a
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
    withInvertPolarity @pola $
    withInvertPolarity @polb $ do
        ba <- subtypeConvert sc tb ta
        return $ MkCatDual ba
subtypeVariance sc (RangeCCRPolarArgument tpa tqa) (RangeCCRPolarArgument tpb tqb) =
    withInvertPolarity @pola $
    withInvertPolarity @polb $ do
        pba <- subtypeConvert sc tpb tpa
        qab <- subtypeConvert sc tqa tqb
        return $ MkCatRange pba qab

subtypeArguments ::
       forall varid (w :: Polarity -> Type -> Type) (pshim :: PolyShimKind) solver pola polb dv (gta :: CCRVariancesKind dv) (gtb :: CCRVariancesKind dv) ta tb.
       ( CCRVariancesShim pshim
       , Applicative solver
       , Is PolarityType pola
       , Is PolarityType polb
       , TestEquality (w 'Positive)
       , TestEquality (w 'Negative)
       )
    => SubtypeContext varid w (pshim Type) solver
    -> CCRVariancesType dv
    -> CCRVariancesMap dv gta
    -> CCRVariancesMap dv gtb
    -> CCRPolarArguments dv w gta pola ta
    -> CCRPolarArguments dv w gtb polb tb
    -> solver (pshim (CCRVariancesKind dv) gta gtb -> pshim Type ta tb)
subtypeArguments _ NilListType NilCCRVariancesMap NilCCRVariancesMap NilCCRArguments NilCCRArguments = pure id
subtypeArguments sc (ConsListType svt dvt) (ConsCCRVariancesMap ccrva dvma) (ConsCCRVariancesMap ccrvb dvmb) (ConsCCRArguments sta dta) (ConsCCRArguments stb dtb) =
    case ccrVarianceCoercibleKind svt of
        Dict ->
            case ccrVariancesCategory @pshim dvt of
                Dict -> do
                    sfunc <- subtypeVariance @_ @_ @_ @_ @pola @polb sc sta stb
                    f <- subtypeArguments sc dvt dvma dvmb dta dtb
                    pure $ \conv -> f (applyPolyShim svt ccrva ccrvb conv sfunc)

subtypeDolanArguments ::
       forall (ground :: GroundTypeKind) (pshim :: PolyShimKind) solver pola polb dv gt argsa argsb.
       ( IsDolanGroundType ground
       , CCRVariancesShim pshim
       , Applicative solver
       , Is PolarityType pola
       , Is PolarityType polb
       )
    => SubtypeContext (DolanVarID ground) (DolanType ground) (pshim Type) solver
    -> CCRVariancesMap dv gt
    -> CCRPolarArguments dv (DolanType ground) gt pola argsa
    -> CCRPolarArguments dv (DolanType ground) gt polb argsb
    -> solver (pshim Type argsa argsb)
subtypeDolanArguments sc dvm argsa argsb = let
    dvt = ccrArgumentsType argsa
    in case ccrVariancesCategory @pshim dvt of
           Dict -> fmap (\f -> f id) $ subtypeArguments sc dvt dvm dvm argsa argsb

type DolanMappable :: GroundTypeKind -> Type -> Constraint
type DolanMappable ground = TSMappable (DolanTypeSystem ground)

type DolanSubtypeContext :: GroundTypeKind -> (Type -> Type) -> Type
type DolanSubtypeContext ground = SubtypeContext (DolanVarID ground) (DolanType ground) (DolanShim ground)

type IsDolanSubtypeGroundType :: GroundTypeKind -> Constraint
class ( DebugIsDolanGroundType ground
      , Eq (DolanSubtypeHint ground)
      , Show (DolanSubtypeHint ground)
      , Semigroup (DolanSubtypeHint ground)
      ) => IsDolanSubtypeGroundType ground where
    type DolanSubtypeHint ground :: Type
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

data SubtypeKnowledge (ground :: GroundTypeKind)
    = UnknownSK
    | NeutralSK
    | HintSK (DolanSubtypeHint ground)

instance forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground => Eq (SubtypeKnowledge ground) where
    NeutralSK == NeutralSK = True
    (HintSK a) == (HintSK b) = a == b
    _ == _ = False

instance forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground => Semigroup (SubtypeKnowledge ground) where
    NeutralSK <> sk = sk
    sk <> NeutralSK = sk
    HintSK a <> HintSK b = HintSK $ a <> b
    _ <> _ = UnknownSK

instance forall (ground :: GroundTypeKind). IsDolanSubtypeGroundType ground => Show (SubtypeKnowledge ground) where
    show UnknownSK = "unknown"
    show NeutralSK = "neutral"
    show (HintSK hint) = "hint: " <> show hint

type SubtypeArguments :: GroundTypeKind -> (Type -> Type) -> forall (dva :: CCRVariances) ->
                                                                     CCRVariancesKind dva -> forall (dvb :: CCRVariances) ->
                                                                                                     CCRVariancesKind dvb -> Type
data SubtypeArguments ground solver dva gta dvb gtb =
    forall a b. MkSubtypeArguments (CCRVariancesMap dva gta)
                                   (CCRPolarArguments dva (DolanType ground) gta 'Negative a)
                                   (CCRVariancesMap dvb gtb)
                                   (CCRPolarArguments dvb (DolanType ground) gtb 'Positive b)
                                   (solver (DolanShim ground a b))

type ConvertSubtype :: GroundTypeKind -> forall (dva :: CCRVariances) ->
                                                 CCRVariancesKind dva -> forall (dvb :: CCRVariances) ->
                                                                                 CCRVariancesKind dvb -> Type
type ConvertSubtype ground dva gta dvb gtb
     = forall solver.
           (WrappedApplicative solver, WAInnerM solver ~ DolanTypeCheckM ground) =>
                   DolanSubtypeContext ground solver -> DolanTypeCheckM ground (SubtypeArguments ground solver dva gta dvb gtb)

type SubtypeConversion :: GroundTypeKind -> forall (dva :: CCRVariances) ->
                                                    CCRVariancesKind dva -> forall (dvb :: CCRVariances) ->
                                                                                    CCRVariancesKind dvb -> Type
data SubtypeConversion ground dva gta dvb gtb where
    GeneralSubtypeConversion
        :: forall (ground :: GroundTypeKind) (dva :: CCRVariances) (gta :: CCRVariancesKind dva) (dvb :: CCRVariances) (gtb :: CCRVariancesKind dvb).
           SubtypeKnowledge ground
        -> ConvertSubtype ground dva gta dvb gtb
        -> SubtypeConversion ground dva gta dvb gtb
    IdentitySubtypeConversion
        :: forall (ground :: GroundTypeKind) (dv :: CCRVariances) (gt :: CCRVariancesKind dv).
           SubtypeConversion ground dv gt dv gt
    CoerceSubtypeConversion
        :: forall (ground :: GroundTypeKind) (gta :: Type) (gtb :: Type). Coercible gta gtb
        => SubtypeConversion ground '[] gta '[] gtb

subtypeConversionKnowledge ::
       forall (ground :: GroundTypeKind) (dva :: CCRVariances) (gta :: CCRVariancesKind dva) (dvb :: CCRVariances) (gtb :: CCRVariancesKind dvb).
       SubtypeConversion ground dva gta dvb gtb
    -> SubtypeKnowledge ground
subtypeConversionKnowledge (GeneralSubtypeConversion sk _) = sk
subtypeConversionKnowledge IdentitySubtypeConversion = NeutralSK
subtypeConversionKnowledge CoerceSubtypeConversion = NeutralSK

instance forall (ground :: GroundTypeKind) (dva :: CCRVariances) (gta :: CCRVariancesKind dva) (dvb :: CCRVariances) (gtb :: CCRVariancesKind dvb). IsDolanSubtypeGroundType ground =>
             Eq (SubtypeConversion ground dva gta dvb gtb) where
    sc1 == sc2 = subtypeConversionKnowledge sc1 == subtypeConversionKnowledge sc2

instance forall (ground :: GroundTypeKind) (dva :: CCRVariances) (gta :: CCRVariancesKind dva) (dvb :: CCRVariances) (gtb :: CCRVariancesKind dvb). IsDolanSubtypeGroundType ground =>
             Show (SubtypeConversion ground dva gta dvb gtb) where
    show (GeneralSubtypeConversion sk _) = "general: " <> show sk
    show IdentitySubtypeConversion = "id"
    show CoerceSubtypeConversion = "coerce"

identitySubtypeConversion ::
       forall (ground :: GroundTypeKind) (dv :: CCRVariances) (gt :: CCRVariancesKind dv).
       SubtypeConversion ground dv gt dv gt
identitySubtypeConversion = IdentitySubtypeConversion

coerceSubtypeConversion ::
       forall (ground :: GroundTypeKind) (gta :: Type) (gtb :: Type). Coercible gta gtb
    => SubtypeConversion ground '[] gta '[] gtb
coerceSubtypeConversion = CoerceSubtypeConversion

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
    -> CCRVariancesMap dva gta
    -> CCRPolarArguments dva (DolanType ground) gta pola a
    -> CCRPolarArguments dvb (DolanType ground) gtb polb b
    -> DolanTypeCheckM ground (solver (DolanPolyShim ground Type a b))
runSubtypeConversion sc IdentitySubtypeConversion vma argsa argsb = do return $ subtypeDolanArguments sc vma argsa argsb
runSubtypeConversion _sc CoerceSubtypeConversion NilCCRVariancesMap NilCCRArguments NilCCRArguments = do
    return $ pure $ coercionToShim "subtype" MkCoercion
runSubtypeConversion sc (GeneralSubtypeConversion _ sconv) _ argsa argsb = do
    MkSubtypeArguments vma argsa' vmb argsb' svconv <- sconv sc
    return $
        (\conva convb conv -> convb . conv . conva) <$> subtypeDolanArguments sc vma argsa argsa' <*>
        subtypeDolanArguments sc vmb argsb' argsb <*>
        svconv

subtypeConversion ::
       forall (ground :: GroundTypeKind) dva gta a dvb gtb b. IsDolanSubtypeGroundType ground
    => Maybe (DolanSubtypeHint ground)
    -> ground dva gta
    -> CCRPolarArgumentsShimWit (DolanPolyShim ground) dva (DolanType ground) gta 'Negative a
    -> ground dvb gtb
    -> CCRPolarArgumentsShimWit (DolanPolyShim ground) dvb (DolanType ground) gtb 'Positive b
    -> DolanOpenExpression ground (DolanShim ground a b)
    -> SubtypeConversion ground dva gta dvb gtb
subtypeConversion hint gta (MkShimWit rawargsa (MkPolarShim conva)) gtb (MkShimWit rawargsb (MkPolarShim convb)) convexpr =
    GeneralSubtypeConversion (maybe UnknownSK HintSK hint) $ \sc -> do
        let
            vma = groundTypeVarianceMap gta
            vmb = groundTypeVarianceMap gtb
        (argsa, argsb) <-
            namespace @(DolanTypeSystem ground) [] FreeName $
            unEndoM (dolanNamespaceRenameArguments <***> dolanNamespaceRenameArguments) (rawargsa, rawargsb)
        return $
            MkSubtypeArguments vma argsa vmb argsb $
            fmap (\conv -> convb . conv . conva) (subtypeLiftExpression sc convexpr)

nilSubtypeConversion ::
       forall (ground :: GroundTypeKind) a b.
       Maybe (DolanSubtypeHint ground)
    -> DolanShim ground a b
    -> SubtypeConversion ground '[] a '[] b
nilSubtypeConversion hint conv =
    GeneralSubtypeConversion (maybe UnknownSK HintSK hint) $ \_ ->
        return $ MkSubtypeArguments NilCCRVariancesMap NilCCRArguments NilCCRVariancesMap NilCCRArguments $ pure conv

composeSubtypeConversion ::
       forall (ground :: GroundTypeKind) dva gta dvb gtb dvc gtc. IsDolanSubtypeGroundType ground
    => SubtypeConversion ground dvb gtb dvc gtc
    -> SubtypeConversion ground dva gta dvb gtb
    -> SubtypeConversion ground dva gta dvc gtc
composeSubtypeConversion IdentitySubtypeConversion ab = ab
composeSubtypeConversion bc IdentitySubtypeConversion = bc
composeSubtypeConversion CoerceSubtypeConversion CoerceSubtypeConversion = CoerceSubtypeConversion
composeSubtypeConversion (GeneralSubtypeConversion hintbc bc) (GeneralSubtypeConversion hintab ab) =
    GeneralSubtypeConversion (hintab <> hintbc) $ \sc -> do
        MkSubtypeArguments _ argsb1 vmc argsc sconvbc <- bc sc
        MkSubtypeArguments vma argsa vmb2 argsb2 sconvab <- ab sc
        return $
            MkSubtypeArguments vma argsa vmc argsc $
            (\conv convbc convab -> convbc . conv . convab) <$> subtypeDolanArguments sc vmb2 argsb2 argsb1 <*> sconvbc <*>
            sconvab
composeSubtypeConversion (GeneralSubtypeConversion hint bc) CoerceSubtypeConversion =
    GeneralSubtypeConversion hint $ \sc ->
        fmap
            (\(MkSubtypeArguments NilCCRVariancesMap NilCCRArguments vmc argsc sconvbc) ->
                 MkSubtypeArguments NilCCRVariancesMap NilCCRArguments vmc argsc $
                 fmap (\conv -> conv . coercionToShim "subtype" MkCoercion) sconvbc) $
        bc sc
composeSubtypeConversion CoerceSubtypeConversion (GeneralSubtypeConversion hint ab) =
    GeneralSubtypeConversion hint $ \sc ->
        fmap
            (\(MkSubtypeArguments vma argsa NilCCRVariancesMap NilCCRArguments sconvab) ->
                 MkSubtypeArguments vma argsa NilCCRVariancesMap NilCCRArguments $
                 fmap (\conv -> coercionToShim "subtype" MkCoercion . conv) sconvab) $
        ab sc

subtypeConversionAsGeneralAs ::
       forall (ground :: GroundTypeKind) solver (dva :: CCRVariances) (gta :: CCRVariancesKind dva) (dvb :: CCRVariances) (gtb :: CCRVariancesKind dvb).
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
subtypeConversionAsGeneralAs runSolver sc (GeneralSubtypeConversion _ cs1) (GeneralSubtypeConversion _ cs2) =
    runVarRenamerT [] [] $ do
    -- cs1 is as general as cs2 if cs1 can subsume to cs2
        MkSubtypeArguments _ args1a _ args1b _ <- cs1 sc
        MkSubtypeArguments vma rawargs2a vmb rawargs2b _ <- cs2 sc
        (args2a, args2b) <-
            namespace @(DolanTypeSystem ground) [] RigidName $
            unEndoM (dolanNamespaceRenameArguments <***> dolanNamespaceRenameArguments) (rawargs2a, rawargs2b)
        let
            sconva = subtypeDolanArguments sc vma args2a args1a
            sconvb = subtypeDolanArguments sc vmb args1b args2b
            sthing = liftA2 (,) sconva sconvb
        runSolver sthing
