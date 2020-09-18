module Pinafore.Language.Value.RefOrder where

import Changes.Core
import Data.Shim
import Pinafore.Base
import Pinafore.Language.Value.FiniteSetRef
import Pinafore.Language.Value.Morphism
import Pinafore.Language.Value.WholeRef
import Shapes

type Order t = t -> t -> Ordering

noOrder :: Order a
noOrder _ _ = EQ

reverseOrder :: forall a. Order a -> Order a
reverseOrder o p q = o q p

joinOrderings :: Ordering -> Ordering -> Ordering
joinOrderings EQ ob = ob
joinOrderings oa _ = oa

joinOrders :: Order a -> Order a -> Order a
joinOrders oa ob p q = joinOrderings (oa p q) (ob p q)

concatOrders :: forall a. [Order a] -> Order a
concatOrders [] = noOrder
concatOrders (o:oo) = joinOrders o $ concatOrders oo

data LangRefOrder a =
    forall t. MkLangRefOrder (PinaforeFunctionMorphism PinaforeStorageUpdate (Know a) t)
                             (Order t)

instance Semigroup (LangRefOrder a) where
    MkLangRefOrder fa oa <> MkLangRefOrder fb ob =
        MkLangRefOrder (liftA2 (,) fa fb) $ \(a1, b1) (a2, b2) -> joinOrderings (oa a1 a2) (ob b1 b2)

instance Monoid (LangRefOrder a) where
    mempty = MkLangRefOrder (pure ()) $ compare @()
    mappend = (<>)

instance Contravariant (LangRefOrder) where
    contramap ba (MkLangRefOrder ef o) = MkLangRefOrder (ef . (arr $ fmap ba)) o

instance HasVariance 'Contravariance (LangRefOrder) where
    varianceRepresentational = Nothing

pureRefOrder :: forall a. Order a -> LangRefOrder a
pureRefOrder cmp = MkLangRefOrder id $ knowOrder cmp

refOrderOn :: forall a b. LangMorphism '( a, TopType) '( BottomType, b) -> LangRefOrder b -> LangRefOrder a
refOrderOn f (MkLangRefOrder ef o) = MkLangRefOrder (ef . langMorphismFunction f) o

refOrders :: forall a. [LangRefOrder a] -> LangRefOrder a
refOrders = mconcat

reverseRefOrder :: forall a. LangRefOrder a -> LangRefOrder a
reverseRefOrder (MkLangRefOrder ef o) = MkLangRefOrder ef $ reverseOrder o

qOrderSet ::
       forall a. (?pinafore :: PinaforeContext)
    => LangRefOrder a
    -> PinaforeROWRef (FiniteSet a)
    -> PinaforeROWRef (Know [a])
qOrderSet (MkLangRefOrder (ofunc :: PinaforeFunctionMorphism PinaforeStorageUpdate (Know a) t) oord) pset = let
    cmp :: Order (a, t)
    cmp (_, t1) (_, t2) = oord t1 t2
    ofuncpair :: PinaforeFunctionMorphism PinaforeStorageUpdate a (a, t)
    ofuncpair =
        proc a -> do
            kt <- ofunc -< Known a
            returnA -< (a, kt)
    upairs :: PinaforeROWRef (FiniteSet (a, t))
    upairs = applyPinaforeFunction pinaforeEntityModel (cfmap ofuncpair) pset
    sortpoints :: FiniteSet (a, t) -> [a]
    sortpoints (MkFiniteSet pairs) = fmap fst $ sortBy cmp pairs
    in eaMapReadOnlyWhole (Known . sortpoints) upairs

langRefOrderCompare ::
       forall a. (?pinafore :: PinaforeContext)
    => LangRefOrder a
    -> PinaforeImmutableWholeRef a
    -> PinaforeImmutableWholeRef a
    -> PinaforeImmutableWholeRef Ordering
langRefOrderCompare (MkLangRefOrder ef o) fv1 fv2 =
    o <$> (applyImmutableRef pinaforeEntityModel (fmap Known ef) fv1) <*>
    (applyImmutableRef pinaforeEntityModel (fmap Known ef) fv2)

pinaforeSetGetOrdered ::
       forall a. (?pinafore :: PinaforeContext)
    => LangRefOrder a
    -> LangFiniteSetRef '( BottomType, a)
    -> LangWholeRef '( TopType, [a])
pinaforeSetGetOrdered order set = pinaforeROWRefToWholeRef $ qOrderSet order $ langFiniteSetRefFunctionValue set

pinaforeUpdateOrder :: LangRefOrder a -> UpdateOrder (ContextUpdate PinaforeStorageUpdate (WholeUpdate (Know a)))
pinaforeUpdateOrder (MkLangRefOrder m cmp) =
    MkUpdateOrder cmp $ changeLensToFloating $ pinaforeFunctionMorphismContextChangeLens m
