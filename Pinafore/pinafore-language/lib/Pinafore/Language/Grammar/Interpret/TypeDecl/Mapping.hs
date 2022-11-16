module Pinafore.Language.Grammar.Interpret.TypeDecl.Mapping
    ( Mapping
    , runMapping
    , VarMapping(..)
    , HasVarMapping(..)
    , DependentMapping(..)
    , dependentMapping
    , dependentVarMapping
    ) where

import Data.Shim
import Language.Expression.Common
import Language.Expression.Dolan
import Pinafore.Language.Interpreter
import Pinafore.Language.Type
import Shapes

type Mapping :: Symbol -> Type -> Type
newtype Mapping n t =
    MkMapping (Kleisli Endo (UVarT n -> UVarT n) t)
    deriving (Semigroup, Monoid, Invariant, Summable, Productable)

mkMapping :: ((UVarT n -> UVarT n) -> t -> t) -> Mapping n t
mkMapping f = MkMapping $ Kleisli $ \vv -> Endo $ f vv

runMapping :: Mapping n t -> (UVarT n -> UVarT n) -> t -> t
runMapping (MkMapping (Kleisli f)) ab = appEndo $ f ab

varMapping :: forall (n :: Symbol). Mapping n (UVarT n)
varMapping = mkMapping id

mapMapping :: ((p -> p) -> (q -> q)) -> Mapping n p -> Mapping n q
mapMapping ff m = mkMapping $ \tt -> ff $ runMapping m tt

joinMapping :: ((p -> p) -> (q -> q) -> (t -> t)) -> Mapping n p -> Mapping n q -> Mapping n t
joinMapping ff mp mq = mkMapping $ \tt -> ff (runMapping mp tt) (runMapping mq tt)

newtype VarMapping t = MkVarMapping
    { runVarMapping :: forall v n. VarianceType v -> SymbolType n -> Maybe (Mapping n t)
    }

liftVarMapping0 :: (forall n. Mapping n a) -> VarMapping a
liftVarMapping0 f = MkVarMapping $ \_ _ -> return f

liftVarMapping1 :: (forall n. Mapping n a -> Mapping n b) -> VarMapping a -> VarMapping b
liftVarMapping1 f (MkVarMapping f1) =
    MkVarMapping $ \v n -> do
        m1 <- f1 v n
        return $ f m1

liftVarMapping2 :: (forall n. Mapping n a -> Mapping n b -> Mapping n c) -> VarMapping a -> VarMapping b -> VarMapping c
liftVarMapping2 f (MkVarMapping f1) (MkVarMapping f2) =
    MkVarMapping $ \v n -> do
        m1 <- f1 v n
        m2 <- f2 v n
        return $ f m1 m2

varVarMapping :: SymbolType n -> VarMapping (UVarT n)
varVarMapping var' =
    MkVarMapping $ \v var ->
        case testEquality var var' of
            Just Refl ->
                case v of
                    CoVarianceType -> return varMapping
                    ContraVarianceType -> Nothing
            Nothing -> return mempty

mapVarMapping :: ((p -> p) -> (q -> q)) -> VarMapping p -> VarMapping q
mapVarMapping ff = liftVarMapping1 $ mapMapping ff

joinVarMapping :: ((p -> p) -> (q -> q) -> (t -> t)) -> VarMapping p -> VarMapping q -> VarMapping t
joinVarMapping ff = liftVarMapping2 $ joinMapping ff

invertVarMapping :: VarMapping t -> VarMapping t
invertVarMapping (MkVarMapping f) = MkVarMapping $ \v -> invertVarianceType v f

instance Semigroup (VarMapping t) where
    (<>) = liftVarMapping2 (<>)

instance Monoid (VarMapping t) where
    mempty = liftVarMapping0 mempty

instance Invariant VarMapping where
    invmap ab ba = liftVarMapping1 (invmap ab ba)

instance Summable VarMapping where
    rVoid = liftVarMapping0 rVoid
    (<+++>) = liftVarMapping2 (<+++>)

instance Productable VarMapping where
    rUnit = liftVarMapping0 rUnit
    (<***>) = liftVarMapping2 (<***>)

getArgumentMapping ::
       forall (t :: Type) (sv :: CCRVariance) dv (f :: CCRVarianceKind sv -> DolanVarianceKind dv) (a :: CCRVarianceKind sv).
       CCRVariation sv f
    -> NonpolarArgument QGroundType sv a
    -> NonpolarArguments QGroundType dv (f a) t
    -> VarMapping t
getArgumentMapping svm (CoNonpolarArgument t) args =
    mapVarMapping (\aa -> ccrArgumentsEndo args (ccrvMap svm aa)) $ getVarMapping t
getArgumentMapping svm (ContraNonpolarArgument t) args =
    mapVarMapping (\aa -> ccrArgumentsEndo args (ccrvMap svm $ MkCatDual aa)) $ invertVarMapping $ getVarMapping t
getArgumentMapping svm (RangeNonpolarArgument tp tq) args =
    joinVarMapping
        (\pp qq -> ccrArgumentsEndo args (ccrvMap svm $ MkCatRange pp qq))
        (invertVarMapping $ getVarMapping tp)
        (getVarMapping tq)

getArgumentsMapping ::
       forall (t :: Type) dv gt. DolanVarianceMap dv gt -> NonpolarArguments QGroundType dv gt t -> VarMapping t
getArgumentsMapping NilDolanVarianceMap NilCCRArguments = mempty
getArgumentsMapping (ConsDolanVarianceMap ccrv dvm) (ConsCCRArguments arg args) =
    getArgumentMapping ccrv arg args <> getArgumentsMapping dvm args

getCCRArgumentMapping ::
       forall (t :: Type) polarity (sv :: CCRVariance) dv (f :: CCRVarianceKind sv -> DolanVarianceKind dv) (a :: CCRVarianceKind sv).
       Is PolarityType polarity
    => CCRVariation sv f
    -> CCRPolarArgument QType polarity sv a
    -> DolanArguments dv QType (f a) polarity t
    -> VarMapping t
getCCRArgumentMapping svm (CoCCRPolarArgument t) args =
    mapVarMapping (\aa -> ccrArgumentsEndo args (ccrvMap svm aa)) $ getVarMapping t
getCCRArgumentMapping svm (ContraCCRPolarArgument t) args =
    invertPolarity @polarity $
    mapVarMapping (\aa -> ccrArgumentsEndo args (ccrvMap svm $ MkCatDual aa)) $ invertVarMapping $ getVarMapping t
getCCRArgumentMapping svm (RangeCCRPolarArgument tp tq) args =
    invertPolarity @polarity $
    joinVarMapping
        (\pp qq -> ccrArgumentsEndo args (ccrvMap svm $ MkCatRange pp qq))
        (invertVarMapping $ getVarMapping tp)
        (getVarMapping tq)

getCCRArgumentsMapping ::
       forall (t :: Type) polarity dv gt. Is PolarityType polarity
    => DolanVarianceMap dv gt
    -> DolanArguments dv QType gt polarity t
    -> VarMapping t
getCCRArgumentsMapping NilDolanVarianceMap NilCCRArguments = mempty
getCCRArgumentsMapping (ConsDolanVarianceMap ccrv dvm) (ConsCCRArguments arg args) =
    getCCRArgumentMapping ccrv arg args <> getCCRArgumentsMapping dvm args

class HasVarMapping w where
    getVarMapping :: w t -> VarMapping t

instance HasVarMapping w => HasVarMapping (Compose ((,) x) w) where
    getVarMapping (Compose (_, wt)) = getVarMapping wt

instance HasVarMapping w => HasVarMapping (ListProductType w) where
    getVarMapping (MkListProductType NilListType) = rUnit
    getVarMapping (MkListProductType (ConsListType t1 tr)) = getVarMapping t1 <***> getVarMapping (MkListProductType tr)

instance HasVarMapping w1 => HasVarMapping (PairType w1 w2) where
    getVarMapping (MkPairType w _) = getVarMapping w

instance HasVarMapping QNonpolarType where
    getVarMapping (VarNonpolarType var) = varVarMapping var
    getVarMapping (GroundedNonpolarType gt args) = getArgumentsMapping (pgtVarianceMap gt) args

instance Is PolarityType polarity => HasVarMapping (QGroundedType polarity) where
    getVarMapping (MkDolanGroundedType gt args) = getCCRArgumentsMapping (pgtVarianceMap gt) args

instance Is PolarityType polarity => HasVarMapping (QSingularType polarity) where
    getVarMapping (VarDolanSingularType var) = varVarMapping var
    getVarMapping (GroundedDolanSingularType t) = getVarMapping t
    getVarMapping (RecursiveDolanSingularType nr t) = let
        vm = getVarMapping t
        in case runVarMapping vm CoVarianceType nr of
               Just mr ->
                   MkVarMapping $ \v na -> do
                       ma <- runVarMapping vm v na
                       return $
                           mkMapping $ \vv -> let
                               tt = runMapping mr tt
                               in runMapping ma vv . tt
               Nothing -> vm

instance Is PolarityType polarity => HasVarMapping (QType polarity) where
    getVarMapping NilDolanType = mempty
    getVarMapping (ConsDolanType t tt) =
        case polarityType @polarity of
            PositiveType -> joinVarMapping iJoinPair (getVarMapping t) (getVarMapping tt)
            NegativeType -> joinVarMapping iMeetPair (getVarMapping t) (getVarMapping tt)

instance HasVarMapping w => HasVarMapping (ListVProductType w) where
    getVarMapping (MkListVProductType (MkListVType lvt :: _ tt)) =
        MkVarMapping $ \v n ->
            fmap (MkMapping . Kleisli) $
            getCompose $
            fmap (endoListVProduct @tt) $
            mapListMVProduct
                @_
                @w
                @Endo
                @tt
                (\w -> Compose $ fmap (\(MkMapping (Kleisli mm)) -> mm) $ runVarMapping (getVarMapping w) v n)
                lvt

instance Is PolarityType polarity => HasVarMapping (QSignature polarity) where
    getVarMapping (ValueSignature _ t) = getVarMapping t

data DependentMapping n t =
    forall a. MkDependentMapping a
                                 (a -> t)
                                 (Mapping n a)

dependentMapping :: (t -> DependentMapping n t) -> Mapping n t
dependentMapping tdm =
    mkMapping $ \vv t ->
        case tdm t of
            MkDependentMapping a at (MkMapping (Kleisli f)) -> at (appEndo (f vv) a)

dependentVarMapping :: TestEquality w => [SomeFor VarMapping w] -> VarMapping (SomeOf w)
dependentVarMapping vmaps =
    MkVarMapping $ \v n -> do
        mdict <- witnessMapForMapM (\(MkVarMapping gm) -> gm v n) $ witnessMapForFromList vmaps
        return $
            dependentMapping $ \(MkSomeOf wit a) ->
                MkDependentMapping a (MkSomeOf wit) $
                fromMaybe (error "missing mapping") $ witnessMapForLookup wit mdict
