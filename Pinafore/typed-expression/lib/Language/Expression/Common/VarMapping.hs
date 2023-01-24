module Language.Expression.Common.VarMapping where

import Data.Shim
import Language.Expression.Common.TypeVariable
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

newtype VarMapping expr t = MkVarMapping
    { runVarMapping :: forall v n. VarianceType v -> SymbolType n -> Maybe (expr (Mapping n t))
    }

liftVarMapping0 :: Applicative expr => (forall n. expr (Mapping n a)) -> VarMapping expr a
liftVarMapping0 f = MkVarMapping $ \_ _ -> return f

liftVarMapping1 ::
       Applicative expr
    => (forall n. expr (Mapping n a) -> expr (Mapping n b))
    -> VarMapping expr a
    -> VarMapping expr b
liftVarMapping1 f (MkVarMapping f1) =
    MkVarMapping $ \v n -> do
        m1 <- f1 v n
        return $ f m1

liftVarMapping2 ::
       Applicative expr
    => (forall n. expr (Mapping n a) -> expr (Mapping n b) -> expr (Mapping n c))
    -> VarMapping expr a
    -> VarMapping expr b
    -> VarMapping expr c
liftVarMapping2 f (MkVarMapping f1) (MkVarMapping f2) =
    MkVarMapping $ \v n -> do
        m1 <- f1 v n
        m2 <- f2 v n
        return $ f m1 m2

varVarMapping :: Applicative expr => SymbolType n -> VarMapping expr (UVarT n)
varVarMapping var' =
    MkVarMapping $ \v var ->
        case testEquality var var' of
            Just Refl ->
                case v of
                    CoVarianceType -> return $ pure varMapping
                    ContraVarianceType -> Nothing
            Nothing -> return $ pure mempty

mapVarMapping :: Applicative expr => expr ((p -> p) -> (q -> q)) -> VarMapping expr p -> VarMapping expr q
mapVarMapping eff = liftVarMapping1 $ liftA2 mapMapping eff

joinVarMapping ::
       Applicative expr
    => expr ((p -> p) -> (q -> q) -> (t -> t))
    -> VarMapping expr p
    -> VarMapping expr q
    -> VarMapping expr t
joinVarMapping eff = liftVarMapping2 $ \emp emq -> joinMapping <$> eff <*> emp <*> emq

invertVarMapping :: VarMapping expr t -> VarMapping expr t
invertVarMapping (MkVarMapping f) = MkVarMapping $ \v -> invertVarianceType v f

instance Applicative expr => Semigroup (VarMapping expr t) where
    (<>) = liftVarMapping2 $ liftA2 (<>)

instance Applicative expr => Monoid (VarMapping expr t) where
    mempty = liftVarMapping0 $ pure mempty

instance Applicative expr => Invariant (VarMapping expr) where
    invmap ab ba = liftVarMapping1 $ fmap (invmap ab ba)

instance Applicative expr => Summable (VarMapping expr) where
    rVoid = liftVarMapping0 $ pure rVoid
    (<+++>) = liftVarMapping2 $ liftA2 (<+++>)

instance Applicative expr => Productable (VarMapping expr) where
    rUnit = liftVarMapping0 $ pure rUnit
    (<***>) = liftVarMapping2 $ liftA2 (<***>)

class Applicative expr => HasVarMapping expr w where
    getVarMapping :: w t -> VarMapping expr t

instance HasVarMapping expr w => HasVarMapping expr (Compose ((,) x) w) where
    getVarMapping (Compose (_, wt)) = getVarMapping wt

instance HasVarMapping expr w => HasVarMapping expr (ListProductType w) where
    getVarMapping (MkListProductType NilListType) = rUnit
    getVarMapping (MkListProductType (ConsListType t1 tr)) = getVarMapping t1 <***> getVarMapping (MkListProductType tr)

instance HasVarMapping expr w => HasVarMapping expr (ListVProductType w) where
    getVarMapping (MkListVProductType (MkListVType lvt :: _ tt)) =
        MkVarMapping $ \v n ->
            getCompose $
            fmap (MkMapping . Kleisli) $
            getCompose $
            fmap (endoListVProduct @tt) $
            mapListMVProduct
                @_
                @w
                @Endo
                @tt
                (\w -> Compose $ fmap (\(MkMapping (Kleisli mm)) -> mm) $ Compose $ runVarMapping (getVarMapping w) v n)
                lvt

instance HasVarMapping expr w1 => HasVarMapping expr (PairType w1 w2) where
    getVarMapping (MkPairType w _) = getVarMapping w

data DependentMapping n t =
    forall a. MkDependentMapping a
                                 (a -> t)
                                 (Mapping n a)

dependentMapping :: (t -> DependentMapping n t) -> Mapping n t
dependentMapping tdm =
    mkMapping $ \vv t ->
        case tdm t of
            MkDependentMapping a at (MkMapping (Kleisli f)) -> at (appEndo (f vv) a)

dependentVarMapping :: (Applicative expr, TestEquality w) => [SomeFor (VarMapping expr) w] -> VarMapping expr (SomeOf w)
dependentVarMapping vmaps = let
    depmap mdict =
        dependentMapping $ \(MkSomeOf wit a) ->
            MkDependentMapping a (MkSomeOf wit) $ fromMaybe (error "missing mapping") $ witnessMapForLookup wit mdict
    in MkVarMapping $ \v n ->
           getCompose $
           fmap depmap $ witnessMapForMapM (\(MkVarMapping gm) -> Compose $ gm v n) $ witnessMapForFromList vmaps
