module Pinafore.Language.Grammar.Interpret.TypeDecl.Mapping
    ( Mapping
    , runMapping
    , VarMapping(..)
    , HasVarMapping(..)
    , DependentMapping(..)
    , dependentMapping
    ) where

import Pinafore.Language.Type
import Shapes

type Mapping :: Symbol -> Type -> Type
newtype Mapping n t =
    MkMapping (Kleisli Endo (UVarT n -> UVarT n) t)
    deriving (Semigroup, Monoid, IsoVariant, Summish, Productish)

varMapping :: forall (n :: Symbol). Mapping n (UVarT n)
varMapping = MkMapping $ Kleisli $ \ab -> Endo ab

runMapping :: Mapping n t -> (UVarT n -> UVarT n) -> t -> t
runMapping (MkMapping (Kleisli f)) ab = appEndo $ f ab

mapMapping :: ((p -> p) -> (q -> q)) -> Mapping n p -> Mapping n q
mapMapping ff (MkMapping (Kleisli f)) = MkMapping $ Kleisli $ \tt -> Endo $ ff $ appEndo $ f tt

joinMapping :: ((p -> p) -> (q -> q) -> (t -> t)) -> Mapping n p -> Mapping n q -> Mapping n t
joinMapping ff (MkMapping (Kleisli fp)) (MkMapping (Kleisli fq)) =
    MkMapping $ Kleisli $ \tt -> Endo $ ff (appEndo $ fp tt) (appEndo $ fq tt)

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

instance IsoVariant VarMapping where
    isoMap ab ba = liftVarMapping1 (isoMap ab ba)

instance Summish VarMapping where
    pNone = liftVarMapping0 pNone
    (<+++>) = liftVarMapping2 (<+++>)

instance Productish VarMapping where
    pUnit = liftVarMapping0 pUnit
    (<***>) = liftVarMapping2 (<***>)

getArgumentMapping ::
       forall (t :: Type) (sv :: CCRVariance) dv (f :: CCRVarianceKind sv -> DolanVarianceKind dv) (a :: CCRVarianceKind sv).
       CCRVariation sv f
    -> NonpolarArgument PinaforeGroundType sv a
    -> NonpolarArguments PinaforeGroundType dv (f a) t
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
       forall (t :: Type) dv gt. DolanVarianceMap dv gt -> NonpolarArguments PinaforeGroundType dv gt t -> VarMapping t
getArgumentsMapping NilDolanVarianceMap NilCCRArguments = mempty
getArgumentsMapping (ConsDolanVarianceMap ccrv dvm) (ConsCCRArguments arg args) =
    getArgumentMapping ccrv arg args <> getArgumentsMapping dvm args

class HasVarMapping w where
    getVarMapping :: w t -> VarMapping t

instance HasVarMapping PinaforeNonpolarType where
    getVarMapping (VarNonpolarType var) = varVarMapping var
    getVarMapping (GroundedNonpolarType gt args) = getArgumentsMapping (pgtVarianceMap gt) args

data DependentMapping n t =
    forall a. MkDependentMapping a
                                 (a -> t)
                                 (Mapping n a)

dependentMapping :: (t -> DependentMapping n t) -> Mapping n t
dependentMapping tdm =
    MkMapping $
    Kleisli $ \vv ->
        Endo $ \t ->
            case tdm t of
                MkDependentMapping a at (MkMapping (Kleisli f)) -> at (appEndo (f vv) a)

instance HasVarMapping w => HasVarMapping (HListWit w) where
    getVarMapping (MkHListWit NilListType) = pUnit
    getVarMapping (MkHListWit (ConsListType t1 tr)) = getVarMapping t1 <***> getVarMapping (MkHListWit tr)
