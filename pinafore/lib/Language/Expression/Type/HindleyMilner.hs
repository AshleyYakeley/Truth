module Language.Expression.Type.HindleyMilner where

import Language.Expression.Expression
import Language.Expression.Typed
import Shapes

data PickWitness (z :: k) (s :: k -> k) (t :: k) where
    FirstPickWitness :: PickWitness z s z
    NextPickWitness :: PickWitness z s t -> PickWitness z s (s t)

instance TestEquality (PickWitness (z :: k) (s :: k -> k)) where
    testEquality FirstPickWitness FirstPickWitness = return Refl
    testEquality (NextPickWitness w1) (NextPickWitness w2) = do
        Refl <- testEquality w1 w2
        return Refl
    testEquality _ _ = Nothing

type VarBasis = [Type]

data FirstVarT (vars :: VarBasis) where
    MkFirstVarT :: t -> FirstVarT (t ': r)

unFirstVarT :: FirstVarT (t ': r) -> t
unFirstVarT (MkFirstVarT x) = x

biFirstVarT :: Bijection (FirstVarT (t ': r)) t
biFirstVarT = MkBijection unFirstVarT MkFirstVarT

data NextVarT (n :: VarBasis -> Type) (vars :: VarBasis) where
    MkNextVarT :: n vars -> NextVarT n (t ': vars)

unNextVarT :: NextVarT n (t ': vars) -> n vars
unNextVarT (MkNextVarT x) = x

biNextVarT :: Bijection (NextVarT n (t ': vars)) (n vars)
biNextVarT = MkBijection unNextVarT MkNextVarT

type VarWitness = PickWitness FirstVarT NextVarT

type family Pick (z :: k) (s :: k -> k) (n :: Nat) :: k where
    Pick z s 'Zero = z
    Pick z s ('Succ n) = s (Pick z s n)

type PickVar n = Pick FirstVarT NextVarT n

data TestTypeWit :: Type -> Type where
    IntTTW :: TestTypeWit Int
    BoolTTW :: TestTypeWit Bool

instance TestEquality TestTypeWit where
    testEquality IntTTW IntTTW = Just Refl
    testEquality BoolTTW BoolTTW = Just Refl
    testEquality _ _ = Nothing

data HMType :: (VarBasis -> Type) -> Type where
    FunctionHMType :: HMType a -> HMType b -> HMType (KindFunction (VarBasis -> Type) a b)
    GroundHMType :: TestTypeWit t -> HMType (Const t)
    VarHMType :: NatType i -> HMType (PickVar i)

instance TestEquality HMType where
    testEquality (FunctionHMType a1 b1) (FunctionHMType a2 b2) = do
        Refl <- testEquality a1 a2
        Refl <- testEquality b1 b2
        return Refl
    testEquality (GroundHMType w1) (GroundHMType w2) = do
        Refl <- testEquality w1 w2
        return Refl
    testEquality (VarHMType n1) (VarHMType n2) = do
        Refl <- testEquality n1 n2
        return Refl
    testEquality _ _ = Nothing

{-
data Expression (varwit :: k -> Type) (a :: k)
    = ClosedExpression (KindLimit k a)
    | forall (t :: k). OpenExpression Name
                                      (varwit t)
                                      (Expression varwit (KindFunction k t a))


ClosedExpression
t basis
OpenExpression




KindFunction (VarBasis -> Type) a b basis ~ a basis -> b basis
KindLimit (VarBasis -> Type) a :: forall basis. a basis


data Expression a = ClosedExpression (forall basis. ListLengthWitness n basis -> a basis)
    | OpenExpression Name (Expression (KindFunction k t a))

Expression :: (k -> Type) -> (k -> Type) -> k -> Type
k = VarBasis -> Type
HMType :: (VarBasis -> Type) -> Type

Expression HMType :: (Var -> Type) -> Var -> Type

Expression HMType (AllF (ListLengthWitness n)) t :: Type

-}
-- witness that t is length n
data ListLengthWitness (n :: Nat) (t :: [k]) where
    ZeroLengthWitness :: ListLengthWitness 'Zero '[]
    SuccLengthWitness :: ListLengthWitness n t -> ListLengthWitness ('Succ n) (x ': t)

type Var = VarBasis -> Type

data TypeBox (tpwit :: Var -> Type) (bc :: (Var -> Type) -> Var -> Type) =
    forall (n :: Nat) (t :: Var). MkTypeBox (NatType n)
                                            (tpwit t)
                                            (bc (AllF (ListLengthWitness n)) t)

type family BasisMap (f :: Type) (basis :: VarBasis) :: VarBasis

type Rebasis (varwit :: Var -> Type) (basiswit :: VarBasis -> Type) (bmap :: Type)
     = forall r t1.
               varwit t1 -> (forall t2.
                                     varwit t2 -> (forall basis2.
                                                           basiswit basis2 -> Bijection (t1 (BasisMap bmap basis2)) (t2 basis2)) -> r) -> r

newtype RebasisW varwit basiswit bmap = MkRebasisW
    { unRebasisW :: Rebasis varwit basiswit bmap
    }




rebasisExpression ::
       RebasisW varwit basiswit bmap
    -> (forall a r (t1 :: Var). f1 (KindFunction Var a t1) -> (forall (t2 :: Var). f2 (KindFunction Var a t2) -> r) -> r)
    -> Expression varwit f1 t1
    -> (forall t2. Expression varwit f2 t2 -> r)
    -> r
rebasisExpression _ mv (ClosedExpression fa) cont =
    mv (kfmap catToUnitFunc fa) $ \f2t2 -> cont $ ClosedExpression $ kfmap catFromUnitFunc f2t2
rebasisExpression rebasis mv (OpenExpression name vw expr) cont =
    unRebasisW rebasis vw $ \vw' wbij ->
        rebasisExpression rebasis mv expr $ \expr' -> cont $ OpenExpression name vw' $ kfmap ab expr'

{-
data Expression (varwit :: k -> Type) (f :: k -> Type) (a :: k)
    = ClosedExpression (f a)
    | forall (t :: k). OpenExpression Name
                                      (varwit t)
                                      (Expression varwit f (KindFunction k t a))
rebasisExpression ::
       RebasisW varwit basiswit bmap
    -> (forall r (t1 :: Var). f1 t1 -> (forall (t2 :: Var). f2 t2 -> r) -> r)
    -> Expression varwit f1 t1
    -> (forall t2. Expression varwit f2 t2 -> r)
    -> r
rebasisExpression _ mv (ClosedExpression fa) cont = mv fa $ \f2t2 -> cont $ ClosedExpression f2t2
rebasisExpression rebasis mv (OpenExpression name vw expr) cont = unRebasisW rebasis vw $ \vw' wbij -> rebasisExpression rebasis mv expr $ \expr' -> cont $ wanted
mv :: forall r (t1 :: Var). f1 t1 -> (forall (t2 :: Var). f2 t2 -> r) -> r
tv :: Var
name :: Name
expr :: Expression varwit f1 (KindFunction k tv t1)
t2 :: Var
vw' :: varwit t2
wbij :: forall basis2. basiswit basis2 -> Bijection (tv (BasisMap bmap basis2)) (t2 basis2)
t2' :: Var
expr' :: Expression varwit f2 t2'
wanted :: Expression varwit f2 ??
-}
class BoxContents' (bc :: (k -> Type) -> k -> Type) where
    mapBoxContents' ::
           forall bwit1 bwit2 t1 t2. (KindFunctor bwit1, KindFunctor bwit2)
        => (forall a. bwit1 (KindFunction k a t1) -> bwit2 (KindFunction k a t2))
        -> bc bwit1 t1
        -> bc bwit2 t2

class BoxContents (bc :: (Var -> Type) -> (Var -> Type) -> Var -> Type) where
    mapBoxContents ::
           forall varwit basiswit bmap f1 f2.
           RebasisW varwit basiswit bmap
        -> (forall a r (t1 :: Var).
                    f1 (KindFunction Var a t1) -> (forall (t2 :: Var). f2 (KindFunction Var a t2) -> r) -> r)
        -> (forall r t1. bc varwit f1 t1 -> (forall t2. bc varwit f2 t2 -> r) -> r)

instance BoxContents Expression where
    mapBoxContents = rebasisExpression

data PairBoxContents bc1 bc2 (varwit :: Var -> Type) (f :: Var -> Type) (t :: Var) =
    MkPairBoxContents (bc1 varwit f t)
                      (bc2 varwit f t)

instance (BoxContents bc1, BoxContents bc2) => BoxContents (PairBoxContents bc1 bc2) where
    mapBoxContents rebasis mv (MkPairBoxContents bc1 bc2) cont =


mapBoxContents ::
           forall varwit basiswit bmap f1 f2.
           RebasisW varwit basiswit bmap
        -> (forall a r (t1 :: Var).
                    f1 (KindFunction Var a t1) -> (forall (t2 :: Var). f2 (KindFunction Var a t2) -> r) -> r)
        -> (forall r t1. bc varwit f1 t1 -> (forall t2. bc varwit f2 t2 -> r) -> r)

mapBoxContents rebasis mv :: forall r t1. bc varwit f1 t1 -> (forall t2. bc varwit f2 t2 -> r) -> r


{-
instance CartesianClosedCategory (KindMorphism k) => BoxContents (Expression (varwit :: k -> Type)) where
    mapBoxContents ff (ClosedExpression fa) = ClosedExpression $ kfmap catFromUnitFunc $ ff $ kfmap catToUnitFunc fa
    mapBoxContents ff (OpenExpression name vw expr) =
        OpenExpression name vw $ mapBoxContents (kfmap catFromProductFunc . ff . kfmap catToProductFunc) expr
-}
{-
data Expression (varwit :: k -> Type) (f :: k -> Type) (a :: k)
    = ClosedExpression (f a)
    | forall (t :: k). OpenExpression Name
                                      (varwit t)
                                      (Expression varwit f (KindFunction k t a))
-}
{-
instance CartesianClosedCategory (KindMorphism k) => BoxContents (Expression (varwit :: k -> Type)) where
    mapBoxContents ff (ClosedExpression fa) = ClosedExpression $ foo fa
    mapBoxContents ff (OpenExpression name vw expr) =
        OpenExpression name (foov vw) $ foo2 expr
-}
data FFF varwit bwit1 bwit2 (t1 :: k) (t2 :: k) vt1 =
    forall vt2. MkFFF (Bijection (varwit vt1) (varwit vt2))
                      (Rebasis' varwit bwit1 bwit2 (KindFunction k vt1 t1) (KindFunction k vt2 t2))

data Rebasis' varwit bwit1 bwit2 t1 t2 =
    MkRebasis' (Bijection (bwit1 t1) (bwit2 t2))
               (forall vt1. FFF varwit bwit1 bwit2 t1 t2 vt1)

rebasisExpression' ::
       forall varwit bwit1 bwit2 t1 t2. (KindFunctor bwit1, KindFunctor bwit2)
    => Rebasis' varwit bwit1 bwit2 t1 t2
    -> Expression varwit bwit1 t1
    -> Expression varwit bwit2 t2
rebasisExpression' (MkRebasis' ff _) (ClosedExpression fa) = ClosedExpression $ biForwards ff fa
rebasisExpression' (MkRebasis' _ fff) (OpenExpression name vw expr) =
    case fff of
        MkFFF vmap rebasis -> OpenExpression name (biForwards vmap vw) $ rebasisExpression' rebasis expr

{-
liftRebasis :: (KindFunctor bwit1, KindFunctor bwit2) => (->) (bwit1 t1) (bwit2 t2) -> (->) (bwit1 (KindFunction k vt1 t1)) (bwit2 (KindFunction k vt2 t2))
liftRebasis bmap b1 =     kfmap :: KindMorphism k a b -> f a -> f b


mapBoxContents ::
    Rebasis' varwit bwit1 bwit2 t1
    -> bc bwit1 t1
    -> (forall t2. tpwit t2 -> bc bwit2 t2 -> r)
    -> r
-}
{-
rebasisExpression (MkRebasis ff _) (ClosedExpression fa) = ClosedExpression $ biForwards ff fa
rebasisExpression (MkRebasis _ fff) (OpenExpression name vw expr) = case fff of
    MkFFF vmap rebasis -> OpenExpression name (biForwards vmap vw) $ rebasisExpression rebasis expr
-}
{- example
type X = forall a b. a -> Int -> b

type Func = KindFunction Var

varType1 :: HMType FirstVarT
varType1 = VarHMType ZeroType

varType2 :: HMType (NextVarT FirstVarT)
varType2 = VarHMType $ SuccType ZeroType

intType :: HMType (Const Int)
intType = GroundHMType IntTTW

type XType = Func FirstVarT (Func (Const Int) (NextVarT FirstVarT))

xType :: HMType XType
xType = FunctionHMType varType1 $ FunctionHMType intType varType2

xToT :: X -> (forall (basis :: VarBasis). ListLengthWitness ('Succ ('Succ 'Zero)) basis -> XType basis)
xToT x (SuccLengthWitness (SuccLengthWitness ZeroLengthWitness)) =
    MkKindFunction1 $ \(MkFirstVarT a) -> MkKindFunction1 $ \(Const i) -> MkNextVarT $ MkFirstVarT $ x a i

xToThing :: X -> TypeBox HMType (Expression HMType)
xToThing x = MkTypeBox representative xType $ MkAllF $ xToT x

tToX ::
       forall a b.
       (forall (basis :: VarBasis). ListLengthWitness ('Succ ('Succ 'Zero)) basis -> XType basis)
    -> a
    -> Int
    -> b
tToX kf a i =
    case kf (SuccLengthWitness (SuccLengthWitness ZeroLengthWitness) :: ListLengthWitness _ '[ a, b]) of
        MkKindFunction1 f ->
            case unKindFunction1 (f (MkFirstVarT a)) (Const i) of
                MkNextVarT (MkFirstVarT b) -> b

thingToXOrError :: TypeBox HMType (Expression HMType) -> X
thingToXOrError (MkTypeBox nwit tp (MkAllF val)) =
    case nwit of
        SuccType (SuccType ZeroType) ->
            case testEquality xType tp of
                Just Refl -> tToX val
                Nothing -> error "nope"
        _ -> error "nope"
-}
kindFunction1Bijection :: Bijection (KindFunction Type (a p) (b p)) (KindFunction1 a b p)
kindFunction1Bijection = MkBijection MkKindFunction1 unKindFunction1

type family Discharge (n :: Nat) (v :: Type) (basis :: VarBasis) :: VarBasis where
    Discharge 'Zero v r = (v ': r)
    Discharge ('Succ n) v (t ': r) = t ': (Discharge n v r)

dischargeLength ::
       forall i t n basis.
       NatType i
    -> GreaterEqual n i
    -> ListLengthWitness n basis
    -> ListLengthWitness ('Succ n) (Discharge i t basis)
dischargeLength ZeroType ZeroGreaterEqual w = SuccLengthWitness w
dischargeLength (SuccType i) (SuccGreaterEqual al) (SuccLengthWitness lw) =
    SuccLengthWitness $ dischargeLength @_ @t i al lw

data ListAtLeastLengthWitness (i :: Nat) (l :: [k]) where
    MkListAtLeastLengthWitness :: ListLengthWitness n t -> GreaterEqual n i -> ListAtLeastLengthWitness i t

dischargeBelowBijection ::
       forall basis t n a b.
       ListAtLeastLengthWitness ('Succ a) basis
    -> NatType a
    -> NatType b
    -> GreaterEqual a b
    -> Bijection (PickVar b (Discharge ('Succ a) t basis)) (PickVar b basis)
dischargeBelowBijection (MkListAtLeastLengthWitness lwit al) _ ZeroType _ =
    case al of
        SuccGreaterEqual _ ->
            case lwit of
                SuccLengthWitness _ -> invertBijection biFirstVarT . biFirstVarT
dischargeBelowBijection (MkListAtLeastLengthWitness lwit al) a (SuccType b') ab =
    case al of
        SuccGreaterEqual al' ->
            case lwit of
                SuccLengthWitness lwit' ->
                    case ab of
                        SuccGreaterEqual ab' ->
                            case a of
                                SuccType a' ->
                                    case dischargeBelowBijection @_ @t (MkListAtLeastLengthWitness lwit' al') a' b' ab' of
                                        bij -> invertBijection biNextVarT . bij . biNextVarT

dischargeOnBijection ::
       forall basis t n a.
       ListAtLeastLengthWitness a basis
    -> NatType a
    -> Bijection (PickVar a (Discharge a t basis)) t
dischargeOnBijection _ ZeroType = biFirstVarT
dischargeOnBijection (MkListAtLeastLengthWitness lwit al) (SuccType a') =
    case al of
        SuccGreaterEqual al' ->
            case lwit of
                SuccLengthWitness lwit' ->
                    case dischargeOnBijection (MkListAtLeastLengthWitness lwit' al') a' of
                        bij -> bij . biNextVarT

dischargeAboveBijection ::
       forall basis t n a b.
       ListAtLeastLengthWitness a basis
    -> NatType a
    -> NatType b
    -> GreaterEqual b a
    -> Bijection (PickVar ('Succ b) (Discharge a t basis)) (PickVar b basis)
dischargeAboveBijection _ ZeroType _ _ = biNextVarT
dischargeAboveBijection (MkListAtLeastLengthWitness lwit al) (SuccType a') b ba =
    case al of
        SuccGreaterEqual al' ->
            case lwit of
                SuccLengthWitness lwit' ->
                    case ba of
                        SuccGreaterEqual ba' ->
                            case b of
                                SuccType b' ->
                                    case dischargeAboveBijection @_ @t (MkListAtLeastLengthWitness lwit' al') a' b' ba' of
                                        bij -> invertBijection biNextVarT . bij . biNextVarT

data DischargeVar (i :: Nat) (v :: Var)

type instance BasisMap (DischargeVar i v) basis =
     Discharge i (v basis) basis

dischargeVarVarT ::
       forall v (k :: Nat) (i :: Nat) r x.
       NatType k
    -> NatType i
    -> NatType x
    -> (forall t' x'.
                Either (t' :~: v) (PickVar x' :~: t', NatType x') -> (forall basis.
                                                                              ListAtLeastLengthWitness (Add k i) basis -> Bijection (PickVar (Add k x) (Discharge (Add k i) (v basis) basis)) (t' basis)) -> r)
    -> r
dischargeVarVarT kt ZeroType ZeroType cont =
    cont (Left Refl) $ \(lalwit :: ListAtLeastLengthWitness _ basis) ->
        case addZeroWit kt of
            Refl -> dischargeOnBijection @basis @(v basis) lalwit kt
dischargeVarVarT kt (SuccType it) ZeroType cont =
    cont (Right (Refl, kt)) $ \(lalwit :: ListAtLeastLengthWitness _ basis) ->
        case (addZeroWit kt, succAddWit kt it) of
            (Refl, Refl) ->
                dischargeBelowBijection @basis @(v basis) lalwit (addWit kt it) kt (addGreaterEqualWit kt it)
dischargeVarVarT kt ZeroType (SuccType xt) cont =
    cont (Right (Refl, addWit kt xt)) $ \(lalwit :: ListAtLeastLengthWitness _ basis) ->
        case (addZeroWit kt, succAddWit kt xt) of
            (Refl, Refl) ->
                dischargeAboveBijection @basis @(v basis) lalwit kt (addWit kt xt) (addGreaterEqualWit kt xt)
dischargeVarVarT kt (SuccType it) (SuccType xt) cont =
    case (succAddWit kt it, succAddWit kt xt) of
        (Refl, Refl) -> dischargeVarVarT @v (SuccType kt) it xt cont

remapType :: HMType t1 -> (forall t2. HMType t2 -> Bijection (t1 basis1) (t2 basis2) -> r) -> r
remapType (GroundHMType tw) cont = cont (GroundHMType tw) $ MkBijection (Const . getConst) (Const . getConst)
remapType (FunctionHMType ta tb) cont =
    remapType ta $ \ta' bija ->
        remapType tb $ \tb' bijb ->
            cont (FunctionHMType ta' tb') $
            mapBijectionIn kindFunction1Bijection $ mapBijectionOut kindFunction1Bijection $ funcBijection bija bijb

--remapType (VarHMType tt) cont = cont
dischargeVarRebasis ::
       forall (i :: Nat) v. NatType i -> HMType v -> Rebasis HMType (ListAtLeastLengthWitness i) (DischargeVar i v)
dischargeVarRebasis _it _tv (GroundHMType tw) cont =
    cont (GroundHMType tw) $ \_ -> MkBijection (Const . getConst) (Const . getConst)
dischargeVarRebasis it tv (VarHMType tt) cont =
    dischargeVarVarT @v ZeroType it tt $ \mtt' bij ->
        case mtt' of
            Left Refl -> cont tv bij
            Right (Refl, tt') -> cont (VarHMType tt') bij
dischargeVarRebasis it tv (FunctionHMType ta tb) cont =
    dischargeVarRebasis it tv ta $ \ta' bija ->
        dischargeVarRebasis it tv tb $ \tb' bijb ->
            cont (FunctionHMType ta' tb') $ \lwal ->
                mapBijectionIn kindFunction1Bijection $
                mapBijectionOut kindFunction1Bijection $ funcBijection (bija lwal) (bijb lwal)

--data TypeBox (tpwit :: Var -> Type) (f :: (Var -> Type) -> Var -> Type) =
--    forall (n :: Nat) (t :: Var). MkTypeBox (NatType n) (tpwit t) (f (AllF (ListLengthWitness n)) t)
dischargeVar ::
       forall bc (i :: Nat) v. BoxContents' bc
    => NatType i
    -> HMType v
    -> TypeBox HMType bc
    -> Maybe (TypeBox HMType bc)
dischargeVar it tv (MkTypeBox (SuccType (nt :: NatType n)) (tp :: HMType t) ft) = do
    al <- natGreaterEqual nt it
    return $
        dischargeVarRebasis it tv tp $ \(tp' :: HMType t') wbij -> let
            ftmap ::
                   forall a.
                   AllF (ListLengthWitness (Succ n)) (KindFunction Var a t)
                -> AllF (ListLengthWitness n) (KindFunction Var a t')
            ftmap (MkAllF fv) =
                MkAllF $ \(lw :: ListLengthWitness n basis) ->
                    MkKindFunction1 $
                    foo
                        (biForwards (wbij $ MkListAtLeastLengthWitness lw al))
                        (unKindFunction1 $ fv $ dischargeLength @_ @(v basis) it al lw)
            in MkTypeBox nt tp' $ mapBoxContents' ftmap ft
dischargeVar _ _ _ = Nothing

{-
data FFF varwit bwit1 bwit2 (t1 :: k) (t2 :: k) vt1 = forall vt2. MkFFF (varwit vt1 -> varwit vt2) (Rebasis varwit bwit1 bwit2  (KindFunction k vt1 t1) (KindFunction k vt2 t2))

data Rebasis varwit bwit1 bwit2 t1 t2 = MkRebasis (bwit1 t1 -> bwit2 t2) (forall vt1. FFF varwit bwit1 bwit2 t1 t2 vt1)

--data Rebasis varwit bwit1 bwit2 t1 t2 where
--    MkRebasis :: (bwit1 t1 -> bwit2 t2) -> (forall vt1. FFF varwit bwit1 bwit2 t1 t2 vt1) -> Rebasis varwit bwit1 bwit2 t1 t2

rebasisExpression :: forall varwit bwit1 bwit2 t1 t2. (KindFunctor bwit1, KindFunctor bwit2)
        => Rebasis varwit bwit1 bwit2 t1 t2
        -> Expression varwit bwit1 t1
        -> Expression varwit bwit2 t2

-}
{-
type Var = VarBasis -> Type
foo :: (t x -> t' t2) -> KindFunction Var a t x -> KindFunction Var a t' t2
newtype KindFunction1 (a :: kp -> Type) (b :: kp -> Type) (p :: kp) = MkKindFunction1
    { unKindFunction1 :: KindFunction Type (a p) (b p)
    }
-}
type family Append (basis1 :: VarBasis) (basis2 :: VarBasis) where
    Append '[] basis2 = basis2
    Append (t ': r) basis2 = t ': (Append r basis2)

{-
exprMapSymbol ::
       FunctionKind k
    => (forall (r :: Type) (t1 :: k). varwit1 t1 -> (forall (t2 :: k). varwit2 t2 -> KindMorphism k t2 t1 -> r) -> r)
    -> Expression varwit1 a
    -> Expression varwit2 a


specify :: VarWitness vt -> HMType t -> Expression a -> Expression HMType a -> Expression HMType a'

aw :: forall (r :: Type) (ab :: VarBasis -> Type) (a :: VarBasis -> Type).
           HMType ab
        -> HMType a
        -> (
                forall (a' :: VarBasis -> Type) (b' :: VarBasis -> Type).
                HMType b' ->
                    (forall (basis :: VarBasis). ab basis -> a' basis -> b' basis) ->
                    (forall (basis :: VarBasis). a basis -> a' basis) ->
                    r
            )
        -> Result Text r
-}
instance FunctionWitness HMType
    --applyWitness MkUniType MkUniType cont = return $ cont MkUniType applyValue id

{-
    applyWitness ::
           forall (r :: Type) (ab :: VarBasis -> Type) (a :: VarBasis -> Type).
           HMType ab
        -> HMType a
        -> (forall (a' :: VarBasis -> Type) (b' :: VarBasis -> Type).
                    HMType b' -> KindMorphism (VarBasis -> Type) ab (KindFunction (VarBasis -> Type) a' b') -> KindMorphism (VarBasis -> Type) a a' -> r)
        -> Result Text r

    applyWitness (FunctionHMType (tfa :: HMType fa) (tfb :: HMType fb)) taa cont = do
        -- ta1 ::
        return $ cont trb mf ma
    applyWitness _ _ _ = fail "cannot apply non-function"
-}
instance UnifyWitness HMType where
    freeWitness cont = cont $ VarHMType ZeroType
    --unifyWitnesses MkUniType MkUniType cont = cont MkUniType id id

data HMTypeSystem

type instance TypeSystemKind HMTypeSystem = VarBasis -> Type

-- to break mutual recursion
$(return [])

instance TypeSystem HMTypeSystem where
    type PositiveWitness HMTypeSystem = HMType
    type NegativeWitness HMTypeSystem = HMType
