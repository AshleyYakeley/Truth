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

class BoxContents (bc :: (k -> Type) -> k -> Type) where
    mapBoxContents ::
           forall bwit1 bwit2 t1 t2. (KindFunctor bwit1, KindFunctor bwit2)
        => (forall a. bwit1 (KindFunction k a t1) -> bwit2 (KindFunction k a t2))
        -> bc bwit1 t1
        -> bc bwit2 t2

{-
foo1 :: cat t (CatFunction cat (TerminalObject cat) t)

foo2 :: cat (CatFunction cat (TerminalObject cat) t) t

foo3 :: cat (CatFunction cat a (CatFunction cat b c)) (CatFunction cat (CatProduct cat a b) c)


foo4 :: cat (CatFunction cat (CatProduct cat a b) c) (CatFunction cat a (CatFunction cat b c))


x1 :: cat (CatFunction cat a b) (CatFunction cat p q)
x2' :: CatLimit cat (CatFunction cat a b) -> CatLimit cat (CatFunction cat p q)

kapply :: cat a b -> CatLimit cat a -> CatLimit cat b

convert1 :: CatLimit cat (CatFunction cat a b) -> cat a b
convert2 :: cat a b -> CatLimit cat (CatFunction cat a b)

class Category (KindMorphism k) => CategoryKind k where
    type KindMorphism k :: k -> k -> Type
    type KindLimit k :: k -> Type
    kapply :: forall (a :: k) (b :: k). KindMorphism k a b -> KindLimit k a -> KindLimit k b
    kconst :: KindLimit k b -> KindMorphism k a b






    fromCombinator :: forall (a :: k) (b :: k). KindLimit k (KindFunction k a b) -> KindMorphism k a b
    toCombinator :: forall (a :: k) (b :: k). KindMorphism k a b -> KindLimit k (KindFunction k a b)



    catApplyPair :: cat (CatProduct cat (CatFunction cat a b) a) b
    catCurry :: cat (CatProduct cat a b) c -> cat a (CatFunction cat b c)
    catUncurry :: cat a (CatFunction cat b c) -> cat (CatProduct cat a b) c
-}
instance BoxContents (Expression varwit) where
    mapBoxContents ff (ClosedExpression fa) = ClosedExpression $ kfmap catFromUnitFunc $ ff $ kfmap catToUnitFunc fa
    mapBoxContents ff (OpenExpression name vw expr) =
        OpenExpression name vw $ mapBoxContents (kfmap catFromProductFunc . ff . kfmap catToProductFunc) expr

--class CategoryKind k => KindFunctor (f :: k -> Type) where
--    kfmap :: KindMorphism k a b -> f a -> f b
-- newtype AllF (w :: k -> *) (f :: k -> *) = MkAllF { getAllF :: forall (t :: k). w t -> f t }
{-
f (a -> x) -> a -> f x
(a -> f x) -> f (a -> x)


kfmap' ::

(t -> t1) -> t1
t2 -> (t -> t2)


foo :: (bwit1 t1 -> bwit2 t2) -> bwit1 (KindFunction k t t1) -> bwit2 (KindFunction k t t2)
foo ff w1tt1 = wanted
ff :: bwit1 t1 -> bwit2 t2
w1tt1 :: bwit1 (KindFunction k t t1)
kfmap :: KindMorphism k (KindFunction t t1) t1 -> bwit1 (KindFunction t t1) -> bwit1 t1
kfmap :: KindMorphism k t2 (KindFunction k t t2) -> bwit2 t2 -> bwit2 (KindFunction k t t2)

\km -> kfmap km w1tt1 :: KindMorphism k (KindFunction t t1) t1 -> bwit1 t1


wanted :: bwit2 (KindFunction k t t2)
-}
{-
data Expression (varwit :: k -> Type) (f :: k -> Type) (a :: k)
    = ClosedExpression (f a)
    | forall (t :: k). OpenExpression Name
                                      (varwit t)
                                      (Expression varwit f (KindFunction k t a))
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

dischargeBelowBijection ::
       forall basis t n a b.
       ListLengthWitness n basis
    -> GreaterEqual n ('Succ a)
    -> NatType a
    -> NatType b
    -> GreaterEqual a b
    -> Bijection (PickVar b (Discharge ('Succ a) t basis)) (PickVar b basis)
dischargeBelowBijection lwit al _ ZeroType _ =
    case al of
        SuccGreaterEqual _ ->
            case lwit of
                SuccLengthWitness _ -> invertBijection biFirstVarT . biFirstVarT
dischargeBelowBijection lwit al a (SuccType b') ab =
    case al of
        SuccGreaterEqual al' ->
            case lwit of
                SuccLengthWitness lwit' ->
                    case ab of
                        SuccGreaterEqual ab' ->
                            case a of
                                SuccType a' ->
                                    case dischargeBelowBijection @_ @t lwit' al' a' b' ab' of
                                        bij -> invertBijection biNextVarT . bij . biNextVarT

dischargeOnBijection ::
       forall basis t n a.
       ListLengthWitness n basis
    -> GreaterEqual n a
    -> NatType a
    -> Bijection (PickVar a (Discharge a t basis)) t
dischargeOnBijection _ _ ZeroType = biFirstVarT
dischargeOnBijection lwit al (SuccType a') =
    case al of
        SuccGreaterEqual al' ->
            case lwit of
                SuccLengthWitness lwit' ->
                    case dischargeOnBijection lwit' al' a' of
                        bij -> bij . biNextVarT

dischargeAboveBijection ::
       forall basis t n a b.
       ListLengthWitness n basis
    -> GreaterEqual n a
    -> NatType a
    -> NatType b
    -> GreaterEqual b a
    -> Bijection (PickVar ('Succ b) (Discharge a t basis)) (PickVar b basis)
dischargeAboveBijection _ _ ZeroType _ _ = biNextVarT
dischargeAboveBijection lwit al (SuccType a') b ba =
    case al of
        SuccGreaterEqual al' ->
            case lwit of
                SuccLengthWitness lwit' ->
                    case ba of
                        SuccGreaterEqual ba' ->
                            case b of
                                SuccType b' ->
                                    case dischargeAboveBijection @_ @t lwit' al' a' b' ba' of
                                        bij -> invertBijection biNextVarT . bij . biNextVarT

dischargeVarVarT ::
       forall v (k :: Nat) (i :: Nat) r x.
       NatType k
    -> NatType i
    -> NatType x
    -> (forall t' x'.
                Either (t' :~: v) (PickVar x' :~: t', NatType x') -> (forall n basis.
                                                                              ListLengthWitness n basis -> GreaterEqual n (Add k i) -> Bijection (PickVar (Add k x) (Discharge (Add k i) (v basis) basis)) (t' basis)) -> r)
    -> r
dischargeVarVarT kt ZeroType ZeroType cont =
    cont (Left Refl) $ \(lwit :: ListLengthWitness n basis) al ->
        case addZeroWit kt of
            Refl -> dischargeOnBijection @basis @(v basis) lwit al kt
dischargeVarVarT kt (SuccType it) ZeroType cont =
    cont (Right (Refl, kt)) $ \(lwit :: ListLengthWitness n basis) al ->
        case (addZeroWit kt, succAddWit kt it) of
            (Refl, Refl) ->
                dischargeBelowBijection @basis @(v basis) lwit al (addWit kt it) kt (addGreaterEqualWit kt it)
dischargeVarVarT kt ZeroType (SuccType xt) cont =
    cont (Right (Refl, addWit kt xt)) $ \(lwit :: ListLengthWitness n basis) al ->
        case (addZeroWit kt, succAddWit kt xt) of
            (Refl, Refl) ->
                dischargeAboveBijection @basis @(v basis) lwit al kt (addWit kt xt) (addGreaterEqualWit kt xt)
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

dischargeVarT ::
       forall (i :: Nat) v r t.
       NatType i
    -> HMType v
    -> HMType t
    -> (forall t'.
                HMType t' -> (forall n basis.
                                      ListLengthWitness n basis -> GreaterEqual n i -> Bijection (t (Discharge i (v basis) basis)) (t' basis)) -> r)
    -> r
dischargeVarT _it _tv (GroundHMType tw) cont =
    cont (GroundHMType tw) $ \_ _ -> MkBijection (Const . getConst) (Const . getConst)
dischargeVarT it tv (VarHMType tt) cont =
    dischargeVarVarT @v ZeroType it tt $ \mtt' bij ->
        case mtt' of
            Left Refl -> cont tv bij
            Right (Refl, tt') -> cont (VarHMType tt') bij
dischargeVarT it tv (FunctionHMType ta tb) cont =
    dischargeVarT it tv ta $ \ta' bija ->
        dischargeVarT it tv tb $ \tb' bijb ->
            cont (FunctionHMType ta' tb') $ \lw al ->
                mapBijectionIn kindFunction1Bijection $
                mapBijectionOut kindFunction1Bijection $ funcBijection (bija lw al) (bijb lw al)

--data TypeBox (tpwit :: Var -> Type) (f :: (Var -> Type) -> Var -> Type) =
--    forall (n :: Nat) (t :: Var). MkTypeBox (NatType n) (tpwit t) (f (AllF (ListLengthWitness n)) t)
dischargeVar ::
       forall bc (i :: Nat) v. BoxContents bc
    => NatType i
    -> HMType v
    -> TypeBox HMType bc
    -> Maybe (TypeBox HMType bc)
dischargeVar it tv (MkTypeBox (SuccType nt) tp ft) = do
    al <- natGreaterEqual nt it
    return $
        dischargeVarT it tv tp $ \tp' wbij -> let
            ftmap (MkAllF fv) =
                MkAllF $ \(lw :: ListLengthWitness n basis) ->
                    biForwards (wbij lw al) (fv $ dischargeLength @_ @(v basis) it al lw)
            in MkTypeBox nt tp' $ mapBoxContents ftmap ft
dischargeVar _ _ _ = Nothing

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
